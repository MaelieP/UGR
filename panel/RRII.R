# =============================================================================
# read_rrii.R — Lecture des fichiers IRPF_RRII (apartado C, 1 ligne = personne × bien)
# Panel de Hogares IEF/AEAT/INE, 2016–2023. Indépendant du script PANEL.
# NON TESTÉ : écrit sans accès aux données. Lancer d'abord check_format() et
# read_rrii(y, n_max = 2000) sur une année avant la boucle complète.
# =============================================================================
library(data.table)
library(readxl)
library(arrow)

# ---- 0. Paramètres -----------------------------------------------------------
# Tout vient du bucket et y retourne. Le disque local ne sert que de zone
# temporaire de la session (RAR + designs), perdue au redémarrage.
bkt     <- s3_bucket("mperier", endpoint_override = Sys.getenv("AWS_S3_ENDPOINT"))
ARCH_S3 <- "P HOGARES 2023.rar"          # clé du RAR dans le bucket : À ADAPTER
FINAL_S3 <- "panel_hogares/rrii_2016_2023.parquet"     # SORTIE : un seul fichier, toutes années
TMP_S3   <- "panel_hogares/rrii_tmp"                    # étapes intermédiaires par année (reprise si crash)
CHK_S3   <- "panel_hogares/rrii_checks"                 # CSV de contrôles
YEARS   <- 2016:2023
TMP     <- file.path(tempdir(), "rrii")
dir.create(TMP, recursive = TRUE, showWarnings = FALSE)

s3_exists <- function(key) {
  d <- dirname(key)
  isTRUE(tryCatch(basename(key) %in% basename(bkt$ls(d)), error = function(e) FALSE))
}
rrii_key <- function(y) sprintf("%s/rrii_%d.parquet", TMP_S3, y)

# Designs : copiés depuis le bucket car readxl ne lit pas sur S3.
DIR_DISENO <- file.path(TMP, "design_files")
dir.create(DIR_DISENO, recursive = TRUE, showWarnings = FALSE)
src <- bkt$ls("panel_hogares/design_files")
src <- src[grepl("\\.xlsx$", src, ignore.case = TRUE)]
for (k in src) {
  dest <- file.path(DIR_DISENO, iconv(basename(k), to = "ASCII//TRANSLIT", sub = "_"))
  if (!file.exists(dest)) {
    con <- bkt$OpenInputFile(k)
    writeBin(as.raw(con$Read()), dest)
    con$close()
  }
}
print(list.files(DIR_DISENO))
stopifnot(length(list.files(DIR_DISENO, pattern = "\\.xlsx$")) >= 5)

# RAR : unrar exige un fichier local -> copie (~13 Go) uniquement s'il reste des
# années à lire. Vérifier l'espace libre avant (system("df -h", tempdir())).
ARCH <- file.path(TMP, basename(ARCH_S3))
get_arch <- function() {
  if (file.exists(ARCH)) return(invisible(ARCH))
  free_gb <- as.numeric(system(sprintf("df --output=avail -B1G %s | tail -1", shQuote(TMP)), intern = TRUE))
  if (free_gb < 15) stop("Espace disque insuffisant dans ", TMP, " : ", free_gb, " Go")
  # mc est préinstallé sur Onyxia (alias "s3") ; sinon : copy_files(bkt$path(ARCH_S3), ARCH)
  st <- system2("mc", c("cp", shQuote(paste0("s3/mperier/", ARCH_S3)), shQuote(ARCH)))
  if (st != 0 || !file.exists(ARCH)) stop("Échec de la copie du RAR depuis le bucket")
  invisible(ARCH)
}

# Nombre d'enregistrements attendus (Resumen 2021–2023, feuille 8_IRPF_RRII en 2020).
# Absent du design 2016–2019 -> estimé par n_expected_size() (taille / longueur de ligne).
N_EXPECTED <- c(`2020` = 4243009, `2021` = 4351030, `2022` = 4532969, `2023` = 5229865)

# ---- 1. Layout depuis le xlsx de design ---------------------------------------
# Positions recalculées par cumul des longueurs (les "Posición inicial" sont des
# formules Excel). En 2016–2019 les 4 années sont côte à côte, colonnes A/H/O/V.
# Recherche par motif (évite les soucis d'encodage du "ñ" dans les noms S3)
design_file <- function(y) {
  pat <- if (y <= 2019) "Registro_2016_2019\\.xlsx$" else sprintf("Registro_%d\\.xlsx$", y)
  f <- list.files(DIR_DISENO, pattern = pat, full.names = TRUE, recursive = TRUE)
  if (length(f) != 1) stop("Design introuvable ou ambigu pour ", y)
  f
}

layout_rrii <- function(y) {
  raw <- as.data.table(read_excel(design_file(y), sheet = "8_IRPF_RRII", col_names = FALSE,
                                  col_types = "text", .name_repair = "minimal"))
  c0  <- if (y <= 2019) 1 + 7 * (y - 2016) else 1
  hdr <- which(raw[[c0]] == "Posición inicial")[1]
  b   <- raw[(hdr + 1):.N, c0:(c0 + 5)]
  setnames(b, c("pos", "var", "type", "len", "dec", "desc"))
  b <- b[!is.na(var) & !is.na(len)]
  b[, `:=`(len = as.integer(len), dec = fifelse(is.na(dec), 0L, as.integer(dec)))]
  b[, end := cumsum(len)][, start := end - len + 1L]
  stopifnot(b$var[1] == "IDENPER", b$var[3] == "RefCatastral")
  b[, .(var, type, start, end, len, dec)]
}

# ---- 2. Nom du membre dans l'archive -----------------------------------------
# 2016–2019 : 4_IRPF{a}_RRII.txt ; 2020+ : 8_IRPF{a}_RRII.txt
.ls_cache <- NULL
member_name <- function(y) {
  get_arch()
  if (is.null(.ls_cache)) .ls_cache <<- trimws(system2("lsar", shQuote(path.expand(ARCH)), stdout = TRUE))
  m <- grep(sprintf("IRPF%d_RRII\\.txt$", y), .ls_cache, ignore.case = TRUE, value = TRUE)
  if (length(m) != 1) stop("Membre RRII introuvable ou ambigu pour ", y, " : ", length(m))
  m
}

# Flux : unrar -> (head) -> (filtre lignes "bien") -> cut sur les colonnes gardées.
# Structure du fichier (vérifiée 2016–2021 par comptage sur fichier entier) :
#   - 1 ligne vide par personne SANS bien déclaré (seuls IDENPER, IDENHOG remplis)
#   - 1 ligne par personne × bien pour les propriétaires
# Ligne "bien" = au moins un caractère utile après la position 22 (espaces, ".",
# ",", "0" et fin de ligne \r ignorés ; "." = manquant à partir de 2022).
AWK_BIENS <- "awk '{ d = substr($0, 23); gsub(/[ .,0\\r]/, \"\", d); if (d != \"\") print }'"
stream_cmd <- function(y, ranges, n_max = NULL, only_biens = FALSE) {
  paste0("unrar p -inul ", shQuote(path.expand(ARCH)), " ", shQuote(member_name(y)),
         if (!is.null(n_max)) sprintf(" | head -n %d", n_max) else "",
         if (only_biens) paste0(" | ", AWK_BIENS) else "",
         " | cut -c ", ranges)
}

# ---- 3. Conversion numérique -------------------------------------------------
# RRII 2016 : point décimal EXPLICITE ("50.00", "4.73") -> pas de ÷100.
# Règle : ≤2021, ÷10^dec seulement si la valeur n'a pas de point ;
# ≥2022, littéral ("." = manquant, "," = milliers). 2022+ À CONFIRMER avec fill_rate(2023).
to_num_rrii <- function(x, dec, y) {
  x <- trimws(x)
  x[x %in% c("", ".")] <- NA_character_
  x <- gsub(",", "", x, fixed = TRUE)
  out <- as.numeric(x)
  if (y <= 2021) {
    imp <- !is.na(out) & !grepl(".", x, fixed = TRUE)
    out[imp] <- out[imp] / 10^dec
  }
  out
}

# ---- 4. Diagnostic de format (aucun identifiant affiché) ----------------------
check_format <- function(y, n = 2000) {
  L <- layout_rrii(y)
  x <- readLines(pipe(stream_cmd(y, "1-100000", n_max = n)))
  cat(y, ": largeur ligne lue =", unique(nchar(sub("\r$", "", x)))[1:3],
      "| largeur design =", max(L$end), "\n")
  num <- L[type == "Num" & dec > 0][1:min(5, .N)]
  for (i in seq_len(nrow(num))) {
    v <- substr(x, num$start[i], num$end[i])
    cat(sprintf("  %-8s dec=%d  vide=%.0f%%  avec '.'=%.0f%%  avec ','=%.0f%%  ex: [%s]\n",
                num$var[i], num$dec[i], 100 * mean(trimws(v) == ""),
                100 * mean(grepl(".", v, fixed = TRUE)), 100 * mean(grepl(",", v, fixed = TRUE)),
                paste(head(unique(trimws(v[trimws(v) != ""])), 3), collapse = "] [")))
  }
  invisible(NULL)
}

# ---- 5. Lecture ---------------------------------------------------------------
# keep : variables PAR à garder en plus des identifiants (NULL = toutes ;
# toutes = ~90 colonnes × ~5 M lignes, compter plusieurs Go de RAM par année).
# Ne lit QUE les lignes "bien" (filtre awk) -> 1 ligne = personne × bien.
read_rrii <- function(y, keep = NULL, n_max = NULL) {
  L <- layout_rrii(y)
  if (!is.null(keep)) L <- L[var %in% unique(c("IDENPER", "IDENHOG", "RefCatastral", keep))]
  x <- readLines(pipe(stream_cmd(y, paste(sprintf("%d-%d", L$start, L$end), collapse = ","),
                                 n_max, only_biens = TRUE)))
  e <- cumsum(L$len); s <- e - L$len + 1L
  DT <- setDT(lapply(seq_len(nrow(L)), function(i) substr(x, s[i], e[i])))
  setnames(DT, L$var); rm(x); gc()
  
  for (v in c("IDENPER", "IDENHOG")) set(DT, j = v, value = as.numeric(DT[[v]]))
  DT[, RefCatastral := trimws(RefCatastral)]
  DT[RefCatastral %in% c("", ".") | grepl("^0+$", RefCatastral), RefCatastral := NA_character_]
  for (i in which(L$type == "Num" & !L$var %in% c("IDENPER", "IDENHOG", "RefCatastral")))
    set(DT, j = L$var[i], value = to_num_rrii(DT[[L$var[i]]], L$dec[i], y))
  for (i in which(L$type == "Char")) set(DT, j = L$var[i], value = trimws(DT[[L$var[i]]]))
  DT[, year := y]
  DT[]
}

# ---- 5a. Table personnes : toutes les personnes du fichier, avec ou sans bien --
# Lit seulement IDENPER/IDENHOG + un drapeau "ligne bien" calculé dans awk (léger).
read_rrii_personas <- function(y) {
  awk <- "awk '{ d = substr($0, 23); gsub(/[ .,0\\r]/, \"\", d); print substr($0, 1, 22) (d != \"\" ? 1 : 0) }'"
  cmd <- paste0("unrar p -inul ", shQuote(path.expand(ARCH)), " ", shQuote(member_name(y)), " | ", awk)
  l <- readLines(pipe(cmd))
  P <- data.table(IDENPER = as.numeric(substr(l, 1, 11)),
                  IDENHOG = as.numeric(substr(l, 12, 22)),
                  bien    = substr(l, 23, 23) == "1")
  rm(l)
  P[, .(n_biens = sum(bien)), by = .(IDENPER, IDENHOG)][, `:=`(sans_bien = n_biens == 0L, year = y)][]
}

# ---- 5b. Nombre attendu estimé par la taille du membre (2016–2019) ------------
# n = taille décompressée / (longueur design + fin de ligne). Fin de ligne : 2 si
# CRLF, 1 si LF -> si le résultat n'est pas entier, essayer l'autre valeur.
n_expected_size <- function(y, eol = 2L) {
  j  <- jsonlite::fromJSON(paste(system2("lsar", c("-j", shQuote(path.expand(ARCH))), stdout = TRUE),
                                 collapse = ""))$lsarContents
  sz <- as.numeric(j$XADFileSize[j$XADFileName == member_name(y)])
  if (length(sz) != 1 || is.na(sz) || sz > 1e15) return(NA_real_)
  sz / (max(layout_rrii(y)$end) + eol)
}

# ---- 6. Contrôles (comptages uniquement) -------------------------------------
check_rrii <- function(DT, P, y) {
  per_ref <- DT[!is.na(RefCatastral), .(n_per = uniqueN(IDENPER)), by = RefCatastral]
  data.table(
    year            = y,
    n_lignes_fichier = sum(pmax(P$n_biens, 1L)),   # lignes vides + lignes bien
    n_attendu       = if (as.character(y) %in% names(N_EXPECTED)) N_EXPECTED[[as.character(y)]] else NA_real_,
    n_personnes_tot = nrow(P),
    n_sans_bien     = sum(P$sans_bien),
    n_lignes_bien   = nrow(DT),
    n_proprietaires = uniqueN(DT$IDENPER),
    n_menages       = uniqueN(DT$IDENHOG),
    n_refcat        = uniqueN(DT$RefCatastral, na.rm = TRUE),
    pct_refcat_na   = round(100 * mean(is.na(DT$RefCatastral)), 2),
    biens_par_pers  = round(nrow(DT) / uniqueN(DT$IDENPER), 2),
    pct_ref_multi   = round(100 * mean(per_ref$n_per > 1), 2),   # refs à ≥2 IDENPER
    pct_doublons    = round(100 * mean(duplicated(DT[, .(IDENPER, RefCatastral)])), 2)
  )
}

# ---- 7. Boucle ----------------------------------------------------------------
# Étape conseillée avant : check_format(2016); check_format(2022); read_rrii(2023, n_max = 2000)
# 7a. Une étape par année dans rrii_tmp/ (sert de cache : une année déjà écrite
# n'est pas relue depuis le RAR ; le RAR n'est copié que s'il en manque une).
pers_key <- function(y) sprintf("%s/personas_%d.parquet", TMP_S3, y)
checks <- list()
for (y in YEARS) {
  if (s3_exists(rrii_key(y))) {
    DT <- setDT(read_parquet(bkt$path(rrii_key(y))))
  } else {
    DT <- read_rrii(y)
    write_parquet(DT, bkt$path(rrii_key(y)), compression = "zstd")
  }
  if (s3_exists(pers_key(y))) {
    P <- setDT(read_parquet(bkt$path(pers_key(y))))
  } else {
    P <- read_rrii_personas(y)
    write_parquet(P, bkt$path(pers_key(y)), compression = "zstd")
  }
  checks[[as.character(y)]] <- check_rrii(DT, P, y)
  print(checks[[as.character(y)]])
  rm(DT, P); gc()
}
write_csv_arrow(rbindlist(checks), bkt$path(paste0(CHK_S3, "/checks_rrii.csv")))
# Contrôle à lire : n_lignes_fichier doit être égal à n_attendu (2020–2023) ;

# 7a-bis. Table personnes unique (petite : 5 colonnes) -> 1 ligne = personne × année,
# n_biens = nb de lignes bien, sans_bien = TRUE si aucun bien déclaré.
PERS_S3 <- "panel_hogares/rrii_personas_2016_2023.parquet"
if (!s3_exists(PERS_S3)) {
  P_all <- rbindlist(lapply(YEARS, function(y) setDT(read_parquet(bkt$path(pers_key(y))))))
  P_all[, year := as.integer(year)]
  setcolorder(P_all, c("year", "IDENPER", "IDENHOG", "n_biens", "sans_bien"))
  write_parquet(P_all, bkt$path(PERS_S3), compression = "zstd")
  rm(P_all); gc()
}
# 7a-ter. Colonnes harmonisées : même sens toutes années (les PARxx brutes changent
# de sens d'une année à l'autre). Correspondance : projet, claude/RRII_correspondance_cases.md
# Rupture 2018/2019 : résidence principale EXCLUE de RRII en 2016–2018, déclarée à partir de 2019
# -> uso_vivienda_habitual = NA (non observable) avant 2019.
H_NUM <- c("num_orden", "pct_prop", "pct_usuf", "dias_disposicion", "renta_imputada",
           "ingresos_alquiler", "rend_neto_alquiler", "rend_neto_reducido")
H_LGL <- c("urbano", "uso_alquiler", "uso_disposicion", "uso_accesorio",
           "uso_vh_exconyuge", "uso_vivienda_habitual", "uso_actividad")
H_CHR <- "situacion"   # "ES" (régime commun, avec réf.), "PV_NAV", "ES_sin_ref", "extranjero"

add_harmonized <- function(DT, y) {
  g <- function(v) {                       # accepte "PAR65" ou "P65" (2019 = préfixe P)
    if (is.na(v)) return(rep(NA_real_, nrow(DT)))
    alt <- c(v, sub("^PAR", "P", v), sub("^P(?!AR)", "PAR", v, perl = TRUE))
    hit <- intersect(alt, names(DT))
    if (length(hit)) DT[[hit[1]]] else rep(NA_real_, nrow(DT))
  }
  x <- function(v) {                       # case cochée = tout sauf vide, "0", ".", "N"
    z <- trimws(as.character(g(v)))
    !is.na(z) & !(z %in% c("", "0", ".", "N"))
  }
  if (y <= 2018) {                                               # usage = clé unique 1–6
    m <- switch(as.character(y),
                "2016" = list(no=NA,    p="P51", u="P52", nat="P53", uso="P54", sit="P55",
                              d="P58", ri="P59", ing="P60", rn="P70", rnr="P74"),
                "2017" = list(no="P55", p="P57", u="P58", nat="P59", uso="P60", sit="P61",
                              d="P67", ri="P68", ing="P69", rn="P79", rnr="P83"),
                "2018" = list(no="P61", p="P63", u="P64", nat="P65", uso="P66", sit="P67",
                              d="P73", ri="P74", ing="P79", rn="P89", rnr="P93"))
    uso <- g(m$uso); s <- g(m$sit)
    sit_lab <- if (y <= 2017) c("ES", "PV_NAV", "ES_sin_ref", "extranjero")
    else           c("ES", "PV_NAV", "PV_NAV", "ES_sin_ref", "extranjero")
    DT[, `:=`(num_orden = g(m$no), pct_prop = g(m$p), pct_usuf = g(m$u),
              urbano = g(m$nat) == 1,
              uso_alquiler = uso %in% c(1, 3), uso_disposicion = uso %in% c(2, 3, 5),
              uso_accesorio = uso %in% c(4, 5), uso_vh_exconyuge = uso %in% 6,
              uso_vivienda_habitual = NA, uso_actividad = NA,
              situacion = sit_lab[s],
              dias_disposicion = g(m$d), renta_imputada = g(m$ri),
              ingresos_alquiler = g(m$ing), rend_neto_alquiler = g(m$rn),
              rend_neto_reducido = g(m$rnr))]
  } else {                                                       # 2019+ : cases à cocher
    s <- g("PAR65")
    DT[, `:=`(num_orden = g("PAR61"), pct_prop = g("PAR63"), pct_usuf = g("PAR64"),
              urbano = x("PAR67"),
              uso_vivienda_habitual = x("PAR70"), uso_vh_exconyuge = x("PAR71"),
              uso_actividad = x("PAR72"), uso_disposicion = x("PAR73"),
              uso_accesorio = x("PAR74"), uso_alquiler = x("PAR75"),
              situacion = c("ES", "PV_NAV", "PV_NAV", "ES_sin_ref", "extranjero")[s],
              dias_disposicion = g("PAR85"), renta_imputada = g("PAR89"),
              # Cases de location 2019+ ([0102]–[0154]) non détaillées dans les manuels :
              # à remplir après identification (section 9, id_par()).
              ingresos_alquiler = NA_real_, rend_neto_alquiler = NA_real_,
              rend_neto_reducido = NA_real_)]
  }
  for (v in H_NUM) set(DT, j = v, value = suppressWarnings(as.numeric(DT[[v]])))
  for (v in H_LGL) set(DT, j = v, value = as.logical(DT[[v]]))
  set(DT, j = H_CHR, value = as.character(DT[[H_CHR]]))
  DT[]
}

# 7b. Assemblage en UN SEUL fichier, écrit année par année (jamais les 8 années
# en mémoire en même temps). 1 ligne = personne × bien, propriétaires uniquement.
# Colonnes : identifiants, puis colonnes HARMONISÉES (à utiliser en priorité),
# puis PARxx brutes (union de toutes les années ; absente une année = NA).
# ATTENTION : une même PARxx peut désigner un concept différent selon l'année
# -> pour les brutes, toujours filtrer sur `year` (voir la correspondance).
if (!s3_exists(FINAL_S3)) {
  ID_VARS <- c("year", "IDENPER", "IDENHOG", "RefCatastral")
  H_VARS  <- c(H_CHR, H_LGL, H_NUM)
  pq_reader <- function(key) ParquetFileReader$create(bkt$OpenInputFile(key))
  schemas  <- lapply(YEARS, function(y) pq_reader(rrii_key(y))$GetSchema())  
  raw_vars <- setdiff(unique(unlist(lapply(schemas, names))), c(ID_VARS, H_VARS))
  all_vars <- c(ID_VARS, H_VARS, raw_vars)
  is_chr <- vapply(all_vars, function(v) any(vapply(schemas, function(s)
    !is.null(s$GetFieldByName(v)) && s$GetFieldByName(v)$type$ToString() %in% c("string", "large_string"),
    logical(1))), logical(1))
  is_chr[c("year", H_LGL, H_NUM)] <- FALSE
  is_chr[c("RefCatastral", H_CHR)] <- TRUE
  sch <- schema(lapply(all_vars, function(v)
    field(v, if (v == "year") int32()
          else if (v %in% H_LGL) boolean()
          else if (is_chr[[v]]) utf8()
          else float64())))
  
  sink   <- bkt$OpenOutputStream(FINAL_S3)
  writer <- ParquetFileWriter$create(sch, sink,
                                     properties = ParquetWriterProperties$create(names(sch), compression = "zstd"))
  n_tot <- 0
  for (y in YEARS) {
    DT <- setDT(read_parquet(bkt$path(rrii_key(y))))
    DT <- add_harmonized(DT, y)
    for (v in setdiff(all_vars, names(DT)))
      set(DT, j = v, value = if (v %in% H_LGL) NA else if (is_chr[[v]]) NA_character_ else NA_real_)
    for (v in all_vars[is_chr])  if (!is.character(DT[[v]])) set(DT, j = v, value = as.character(DT[[v]]))
    for (v in setdiff(all_vars[!is_chr], c("year", H_LGL)))
      if (!is.double(DT[[v]])) set(DT, j = v, value = as.numeric(DT[[v]]))
    DT[, year := as.integer(year)]
    setcolorder(DT, all_vars)
    writer$WriteTable(arrow_table(DT[, ..all_vars], schema = sch), chunk_size = 1e6)
    n_tot <- n_tot + nrow(DT)
    cat(y, ":", nrow(DT), "lignes écrites\n")
    rm(DT); gc()
  }
  writer$Close(); sink$close()
  
  # Contrôle : même nombre de lignes que la somme des étapes annuelles
  n_final <- pq_reader(FINAL_S3)$num_rows  
  stopifnot(n_final == n_tot)
  cat("Fichier unique écrit :", FINAL_S3, "—", n_final, "lignes,", length(all_vars), "colonnes\n")
  # Une fois vérifié, les étapes annuelles peuvent être supprimées à la main :
  for (y in YEARS) { bkt$DeleteFile(rrii_key(y)); bkt$DeleteFile(pers_key(y)) }
}

# 7c. Contrôles des colonnes harmonisées (comptages uniquement)
H <- setDT(read_parquet(bkt$path(FINAL_S3), col_select = c("year", H_CHR, H_LGL, "renta_imputada",
                                                           "ingresos_alquiler")))
chk_h <- H[, .(n = .N,
               pct_urbano       = round(100 * mean(urbano, na.rm = TRUE), 1),
               pct_alquiler     = round(100 * mean(uso_alquiler, na.rm = TRUE), 1),
               pct_alquiler_hors_vh   = 
                 round(100 * sum(uso_alquiler & !(uso_vivienda_habitual %in% TRUE), na.rm = TRUE) /
                                                sum(!(uso_vivienda_habitual %in% TRUE)), 1), 
                    # % de biens en loc PARMI ceux qui ne sont aps des résidences principales 
               pct_disposicion  = round(100 * mean(uso_disposicion, na.rm = TRUE), 1),
               pct_disposicion_hors_vh = round(100 * sum(uso_disposicion & !(uso_vivienda_habitual %in% TRUE), na.rm = TRUE) /
                                                 sum(!(uso_vivienda_habitual %in% TRUE)), 1),
                # % de biens à disposition PARMI ceux qui ne sont aps des résidences principales 
               pct_vivienda_hab = round(100 * mean(uso_vivienda_habitual, na.rm = TRUE), 1),
               pct_situ_ES      = round(100 * mean(situacion %in% "ES"), 1),
               pct_renta_imp    = round(100 * mean(!is.na(renta_imputada) & renta_imputada != 0), 1),
               pct_ingresos     = round(100 * mean(!is.na(ingresos_alquiler) & ingresos_alquiler != 0), 1)),
           by = year][order(year)]
print(chk_h)   # pct_vivienda_hab = NaN avant 2019 (non observable) ; pct_ingresos = 0 en 2019+ (à identifier)
write_csv_arrow(chk_h, bkt$path(paste0(CHK_S3, "/checks_rrii_harmonise.csv")))
rm(H); gc()

# ---- 8. Stabilité de la clé RefCatastral entre années -------------------------
# Part des couples (IDENPER, RefCatastral) de y-1 retrouvés en y.
# Hors résidence principale : elle entre dans RRII en 2019, ce qui ferait chuter
# artificiellement la transition 2018->2019.
KEYS <- setDT(read_parquet(bkt$path(FINAL_S3),
                           col_select = c("year", "IDENPER", "RefCatastral", "uso_vivienda_habitual")))[
                             !is.na(RefCatastral) & !(uso_vivienda_habitual %in% TRUE)]
read_keys <- function(y) KEYS[year == y, .(IDENPER, RefCatastral)]
stab <- rbindlist(lapply(YEARS[-1], function(y) {
  a <- read_keys(y - 1)
  b <- read_keys(y)
  pers <- intersect(a$IDENPER, b$IDENPER)
  a <- unique(a[IDENPER %in% pers]); b <- unique(b[IDENPER %in% pers])
  data.table(transition = paste0(y - 1, "->", y),
             pct_couples_retrouves = round(100 * nrow(fintersect(a, b)) / nrow(a), 1))
}))
print(stab)
write_csv_arrow(stab, bkt$path(paste0(CHK_S3, "/checks_rrii_stabilite.csv")))
rm(KEYS); gc()

# ---- 9. Identification des cases de location 2019+ ----------------------------
# Les manuels 2019+ disent seulement « cases [0102] à [0154] ». On repère les cases
# en comparant, par personne, la somme de chaque PAR candidate à own_rent du panel :
# le déflateur étant constant dans l'année, la bonne case est PROPORTIONNELLE à own_rent.
# Étalonnage sur 2018 (P79 = recettes, P89 = revenu net, P93 = revenu net réduit connus).
# Nécessite PANEL en mémoire (voir script principal). Sorties = statistiques agrégées.
id_par <- function(y, rng = 100:154) {
  D <- setDT(read_parquet(bkt$path(rrii_key(y))))
  cand <- intersect(c(paste0("P", rng), paste0("PAR", rng)), names(D))
  cand <- cand[vapply(cand, function(v) is.numeric(D[[v]]), logical(1))]
  S <- D[, lapply(.SD, sum, na.rm = TRUE), by = IDENPER, .SDcols = cand]; rm(D)
  X <- merge(S, PANEL[year == y & !is.na(own_rent) & own_rent != 0, .(IDENPER, own_rent)],
             by = "IDENPER")
  rbindlist(lapply(cand, function(v) {
    r <- X$own_rent / X[[v]]
    k <- median(r[is.finite(r)])
    data.table(var = v,
               pct_non_nul       = round(100 * mean(X[[v]] != 0), 1),
               ratio             = round(k, 3),
               cor               = round(suppressWarnings(cor(X[[v]], X$own_rent)), 3),
               pct_proportionnel = round(100 * mean(abs(X$own_rent - k * X[[v]]) <=
                                                      0.01 * abs(X$own_rent) + 2), 1))
  }))[order(-pct_proportionnel)]
}
if (exists("PANEL")) {
  print(id_par(2018, 79:93))   # étalonnage : quelle case 2018 colle à own_rent ?
  print(id_par(2019))          # puis la même chose en 2019 (et 2020–2023 si besoin)
}
# Une fois les cases trouvées, compléter la branche 2019+ de add_harmonized()
# (ingresos_alquiler, rend_neto_alquiler, rend_neto_reducido), supprimer FINAL_S3 et relancer 7b.
