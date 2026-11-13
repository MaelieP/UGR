# =============================================================================
# Atlas de distribución de renta de los hogares (INE) — niveau DISTRICT
# Années 2016-2023, via l'API JSON de l'INE (package ineapir)
# -> un fichier Parquet par indicateur, envoyé directement dans le bucket S3
#
# Les districts n'existent que dans les tables PAR PROVINCE (52 provinces,
# 10 tables chacune, chacune avec son propre identifiant). Le script :
#   1. lit la page INE de chaque province pour récupérer les identifiants
#   2. pour chaque table, repère dans les métadonnées la variable "district"
#      (codes à 7 chiffres) et, si besoin, la valeur "Total" du sexe
#   3. interroge l'API avec ce filtre, sur 2016-2023
#   4. empile les 52 provinces et écrit un Parquet par indicateur dans S3
# =============================================================================

# install.packages(c("ineapir", "dplyr", "purrr", "stringr", "arrow", "aws.s3"))
library(ineapir)
library(dplyr)
library(purrr)
library(stringr)

# -----------------------------------------------------------------------------
# 0. PARAMÈTRES
# -----------------------------------------------------------------------------
BUCKET  <- "mperier"                 # juste le nom, sans "s3://" ni "/"
DOSSIER <- "ine_adrh/districts"
DEBUT   <- "2016/01/01"
FIN     <- "2023/12/31"

# Les 6 tables voulues : mot-clé du titre sur la page INE + position de repli
# (ordre observé sur la page d'Albacete) + faut-il filtrer Sexo = Total ?
INDICATEURS <- tibble::tribble(
  ~nom,                       ~motif_titre,                                   ~position, ~total_sexe,
  "renta_media_mediana",      "renta media y mediana",                         1,        FALSE,
  "fuente_ingresos",          "fuente de ingresos",                            2,        FALSE,
  "umbrales_fijos",           "umbrales fijos por sexo$",                      3,        TRUE,
  "umbrales_relativos",       "umbrales relativos por sexo$",                  6,        TRUE,
  "gini_p80p20",              "gini",                                          9,        FALSE,
  "indicadores_demograficos", "demogr",                                       10,        FALSE
)

# Pages INE des 52 provinces (paramètre capsel), section
# "Resultados por municipios, distritos y secciones censales por provincias"
CAPSELS <- c(5609, 5652:5659, 5670:5712)   # Albacete, Alicante ... Melilla

# -----------------------------------------------------------------------------
# 1. IDENTIFIANTS DES TABLES DE CHAQUE PROVINCE (lecture de la page INE)
# -----------------------------------------------------------------------------
tables_province <- function(capsel) {
  url  <- paste0("https://ine.es/dynt3/inebase/index.htm?padre=12385&capsel=", capsel)
  html <- paste(readLines(url, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  
  # liens de tables avec leur titre : ...Tabla.htm?t=30656...>Titre</a>
  m <- str_match_all(html, "Tabla\\.htm\\?t=(\\d+)[^>]*>\\s*([^<]+?)\\s*<")[[1]]
  liens <- tibble(id = as.integer(m[, 2]), titre = str_squish(m[, 3])) |>
    distinct(id, .keep_all = TRUE)
  
  if (nrow(liens) == 0) {           # repli : identifiants seuls, dans l'ordre
    ids   <- unique(as.integer(str_match_all(html, "[?&]t=(\\d+)")[[1]][, 2]))
    liens <- tibble(id = ids, titre = NA_character_)
  }
  
  map_dfr(seq_len(nrow(INDICATEURS)), function(i) {
    ind <- INDICATEURS[i, ]
    hit <- liens |> filter(str_detect(tolower(titre), ind$motif_titre))
    id  <- if (nrow(hit) >= 1) hit$id[1] else liens$id[ind$position]
    tibble(capsel = capsel, nom = ind$nom, id = id, total_sexe = ind$total_sexe,
           par_titre = nrow(hit) >= 1)
  })
}

catalogue <- map_dfr(CAPSELS, function(c) { Sys.sleep(0.5); tables_province(c) })
print(count(catalogue, nom, par_titre))   # vérif : 52 tables par indicateur

# -----------------------------------------------------------------------------
# 2. FILTRE API POUR UNE TABLE : variable "district" + Sexo = Total
# -----------------------------------------------------------------------------
construire_filtre <- function(id, total_sexe) {
  groupes <- get_metadata_table_groups(idTable = id, validate = FALSE)
  filtre  <- list()
  
  # a) variable des districts = celle dont les codes ont 7 chiffres
  g_terr <- groupes$Id[str_detect(groupes$Nombre, regex("territ", ignore_case = TRUE))][1]
  terr   <- get_metadata_table_values(idTable = id, idGroup = g_terr, validate = FALSE)
  var_d  <- terr |> filter(str_detect(Codigo, "^\\d{7}$")) |> pull(Fk_Variable) |> unique()
  if (length(var_d) != 1) stop("variable district introuvable (table ", id, ")")
  filtre[[as.character(var_d)]] <- ""            # "" = tous les districts
  
  # b) Sexo = Total
  if (total_sexe) {
    g_sexe <- groupes$Id[str_detect(groupes$Nombre, regex("sexo", ignore_case = TRUE))][1]
    sexe   <- get_metadata_table_values(idTable = id, idGroup = g_sexe, validate = FALSE)
    tot    <- sexe |> filter(str_detect(Nombre, regex("^total$|ambos", ignore_case = TRUE)))
    if (nrow(tot) == 0) stop("valeur Total du sexe introuvable (table ", id, ")")
    filtre[[as.character(tot$Fk_Variable[1])]] <- as.character(tot$Id[1])
  }
  filtre
}

# -----------------------------------------------------------------------------
# 3. TÉLÉCHARGEMENT D'UNE TABLE (toutes les années 2016-2023 en une requête)
# -----------------------------------------------------------------------------
telecharger <- function(id, total_sexe) {
  filtre <- construire_filtre(id, total_sexe)
  appel <- function(d, f) tryCatch(
    get_data_table(idTable = id, filter = filtre, dateStart = d, dateEnd = f,
                   tip = "AM", metanames = TRUE, metacodes = TRUE,
                   unnest = TRUE, validate = FALSE),
    error = function(e) NULL)
  
  df <- NULL
  for (k in 1:3) {                                   # 3 essais
    df <- appel(DEBUT, FIN)
    if (!is.null(df) && nrow(df) > 0) break
    Sys.sleep(5 * k)
  }
  if (is.null(df) || nrow(df) == 0) {                # repli : année par année
    message("   table ", id, " : requête découpée par année")
    df <- map_dfr(2016:2023, function(a) {
      Sys.sleep(1); appel(paste0(a, "/01/01"), paste0(a, "/12/31"))
    })
  }
  if (nrow(df) == 0) stop("réponse vide, filtre = ",
                          paste(names(filtre), filtre, sep = ":", collapse = " | "))
  
  col_code <- names(df)[str_detect(names(df), "territ.*\\.Codigo$")][1]
  col_nom  <- str_remove(col_code, "\\.Codigo$")
  df |>
    select(-any_of("MetaData")) |>
    rename(cod_distrito = all_of(col_code), distrito = all_of(col_nom)) |>
    filter(str_detect(cod_distrito, "^\\d{7}$")) |>
    mutate(cod_municipio = str_sub(cod_distrito, 1, 5),
           cod_provincia = str_sub(cod_distrito, 1, 2),
           id_table = id)
}

# -----------------------------------------------------------------------------
# 4. BOUCLE : pour chaque indicateur, les 52 provinces, puis envoi dans S3
# -----------------------------------------------------------------------------
vers_s3 <- function(df, nom) {
  objet <- paste0(DOSSIER, "/", nom, "_2016_2023.parquet")
  aws.s3::s3write_using(df, FUN = arrow::write_parquet,
                        object = objet, bucket = BUCKET, opts = list(region = ""))
  message("   -> s3://", BUCKET, "/", objet)
}

erreurs <- list()
for (n in INDICATEURS$nom) {
  message("\n=== ", n, " ===")
  cat_n <- filter(catalogue, nom == n)
  df_n <- map_dfr(seq_len(nrow(cat_n)), function(i) {
    Sys.sleep(1)
    tryCatch(telecharger(cat_n$id[i], cat_n$total_sexe[i]),
             error = function(e) {
               message("   [ERREUR] table ", cat_n$id[i], " : ", conditionMessage(e))
               erreurs[[length(erreurs) + 1]] <<- tibble(nom = n, id = cat_n$id[i],
                                                         msg = conditionMessage(e))
               NULL
             })
  })
  message("   ", nrow(df_n), " lignes | ", n_distinct(df_n$cod_distrito), " districts | ",
          n_distinct(df_n$cod_provincia), " provinces | années ",
          paste(range(df_n$Anyo), collapse = "-"))
  if (nrow(df_n) > 0) vers_s3(df_n, n)
}

bind_rows(erreurs)   # tables à relancer, le cas échéant
