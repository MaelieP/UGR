library(data.table)
library(jsonlite)
library(arrow)
options(scipen = 999)

# ================= RÉGLAGES (à modifier) =================
bucket_nom     <- "mperier"
nom_bucket_rar <- "P HOGARES 2023.rar"                       # nom du .rar dans le bucket
archive        <- path.expand("~/work/UGR/Panel_Hogares.rar") # copie locale
dossier_data   <- path.expand("~/work/UGR/data")
tmp_dir        <- path.expand("~/work/UGR/tmp_extraction")

max_taille_Mo   <- 200         # on ignore les fichiers plus gros
motif           <- "\\.(txt|csv)$"
filtre_chemin   <- NULL        # ex: "000 IDEN" pour viser un sous-dossier
sep             <- ";"
encodage        <- "unknown"   # "Latin-1" si les accents sont cassés
header          <- "auto"      # FALSE si les .txt n'ont pas d'en-tête
types_forces    <- NULL        # "character" = tout en texte (garde les zéros initiaux)
convertir       <- FALSE       # TRUE = écrit aussi un parquet par fichier
retenter_echecs <- FALSE       # TRUE = retente les fichiers déjà en échec
reinitialiser   <- FALSE       # TRUE = efface les anciens résultats d'exploration
max_fichiers    <- 5           # Inf pour tout traiter
# ==========================================================

dir.create(dossier_data, showWarnings = FALSE, recursive = TRUE)
fichier_res <- file.path(dossier_data, "exploration_fichiers.csv")

# ---------- Fonctions ----------

# Outils : disparaissent à chaque redémarrage du service
installer_outils <- function() {
  if (Sys.which("lsar") == "" || Sys.which("unrar") == "")
    system("sudo apt-get update -qq && sudo apt-get install -y -qq unar unrar")
  stopifnot("lsar/unrar introuvables" = Sys.which("lsar") != "" && Sys.which("unrar") != "")
}

# Copie bucket -> disque local, avec contrôle de taille
copier_depuis_bucket <- function(nom_bucket, local, bucket_nom) {
  bucket  <- s3_bucket(bucket_nom, endpoint_override = Sys.getenv("AWS_S3_ENDPOINT"))
  attendu <- bucket$GetFileInfo(nom_bucket)[[1]]$size
  cat("Taille dans le bucket :", attendu, "octets\n")
  
  if (file.exists(local) && file.size(local) == attendu) {
    cat("Copie locale déjà complète, rien à faire.\n")
  } else {
    unlink(local)
    libre <- as.numeric(system(paste("df -B1 --output=avail", shQuote(dirname(local)),
                                     "| tail -n 1"), intern = TRUE))
    stopifnot("Pas assez de place sur le disque" = libre > attendu * 1.02)
    entree <- bucket$OpenInputStream(nom_bucket)
    sortie <- file(local, "wb")
    total <- 0; prochain <- 1e9
    repeat {
      m <- entree$Read(64 * 1024^2)
      if (m$size == 0) break
      writeBin(m$data(), sortie)
      total <- total + m$size
      if (total >= prochain) { cat(round(total / 1e9), "Go\n"); prochain <- prochain + 1e9 }
    }
    close(sortie); entree$close()
  }
  stopifnot("Copie incomplète : taille locale différente du bucket" = file.size(local) == attendu)
  cat("OK : copie complète,", file.size(local), "octets\n")
}

taille_lisible <- function(x) {
  unites <- c("o", "Ko", "Mo", "Go", "To")
  i <- ifelse(is.na(x) | x < 1, 1, pmin(floor(log(x, 1000)), 4) + 1)
  valeur <- round(x / 1000^(i - 1), 2)
  txt <- vapply(valeur, function(v)
    if (is.na(v)) NA_character_ else
      format(v, scientific = FALSE, decimal.mark = ",", big.mark = " ",
             trim = TRUE, drop0trailing = TRUE), character(1))
  ifelse(is.na(txt), NA_character_, paste(txt, unites[i]))
}

# Index de l'archive (lsar) : un tableau, utilisé pour le CSV ET pour l'exploration
lire_index <- function(archive) {
  json <- tempfile(fileext = ".json")
  system(paste("lsar -j", shQuote(archive), ">", shQuote(json)))
  stopifnot("lsar n'a rien produit" = file.exists(json) && file.size(json) > 0)
  x <- fromJSON(json, flatten = TRUE)$lsarContents
  stopifnot("lsar ne trouve aucun fichier dans l'archive" = is.data.frame(x) && nrow(x) > 0)
  
  col <- function(nm) if (nm %in% names(x)) x[[nm]] else rep(NA, nrow(x))
  idx <- data.table(
    chemin            = gsub("\\\\", "/", col("XADFileName")),
    taille            = as.numeric(col("XADFileSize")),
    taille_compressee = as.numeric(col("XADCompressedSize")),
    date_modif        = as.character(col("XADLastModificationDate")),
    est_dossier       = col("XADIsDirectory") %in% 1
  )
  # tailles aberrantes lues par lsar : on les marque et on les remplace par NA
  idx[, taille_suspecte := (taille > 1e15) %in% TRUE | (taille_compressee > 1e15) %in% TRUE]
  idx[taille > 1e15, taille := NA]
  idx[taille_compressee > 1e15, taille_compressee := NA]
  
  # hiérarchie et tailles lisibles
  idx[, `:=`(nom = basename(chemin),
             dossier_parent = ifelse(dirname(chemin) == ".", "", dirname(chemin)),
             profondeur = lengths(strsplit(chemin, "/", fixed = TRUE)),
             extension = tolower(tools::file_ext(chemin)),
             taille_decompressee_lisible = taille_lisible(taille),
             taille_compressee_lisible = taille_lisible(taille_compressee))]
  for (k in 1:4)
    idx[[paste0("niveau", k)]] <- vapply(strsplit(idx$chemin, "/", fixed = TRUE),
                                         function(p) if (length(p) >= k) p[k] else NA_character_, "")
  idx
}

# Extraction d'UN fichier avec unrar (garde l'arborescence dans tmp_dir)
extraire_un <- function(chemin_rel) {
  unlink(tmp_dir, recursive = TRUE); dir.create(tmp_dir, recursive = TRUE)
  sortie <- suppressWarnings(
    system2("unrar", c("x", "-o+", "-y", shQuote(archive), shQuote(chemin_rel),
                       shQuote(paste0(tmp_dir, "/"))), stdout = TRUE, stderr = TRUE))
  st <- attr(sortie, "status"); if (is.null(st)) st <- 0L
  list(status = st, message = paste(tail(sortie, 3), collapse = " / "))
}

# Extraire, vérifier la taille, lire, (convertir), résumer
traiter_un <- function(chemin_rel, attendu) {
  t0 <- Sys.time()
  res <- list(chemin = chemin_rel, taille_attendue = attendu, taille_octets = NA_real_,
              n_lignes = NA_integer_, n_colonnes = NA_integer_, colonnes = NA_character_,
              types = NA_character_, statut = NA_character_, detail = NA_character_,
              duree_s = NA_real_)
  ext <- extraire_un(chemin_rel)
  f <- file.path(tmp_dir, chemin_rel)
  
  if (!file.exists(f)) {
    res$statut <- "echec_extraction"; res$detail <- ext$message
  } else if (file.size(f) != attendu) {
    res$taille_octets <- file.size(f)
    res$statut <- "incomplet"
    res$detail <- paste0("attendu ", attendu, ", obtenu ", file.size(f), ". ", ext$message)
  } else {
    res$taille_octets <- file.size(f)
    lu <- tryCatch({
      df <- fread(f, sep = sep, encoding = encodage, header = header, colClasses = types_forces)
      if (convertir) {
        p <- file.path(dossier_data, "parquet", sub("\\.[^.]+$", ".parquet", chemin_rel))
        dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE)
        write_parquet(df, p, compression = "zstd")
      }
      list(n_lignes = nrow(df), n_colonnes = ncol(df),
           colonnes = paste(names(df), collapse = " | "),
           types = paste(vapply(df, function(c) class(c)[1], ""), collapse = " | "),
           statut = "ok")
    }, error = function(e) list(statut = "erreur_lecture", detail = conditionMessage(e)))
    res <- modifyList(res, lu)
  }
  res$duree_s <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  res
}

# ---------- Exécution ----------

installer_outils()
copier_depuis_bucket(nom_bucket_rar, archive, bucket_nom)

# Tableau du contenu de l'archive (consultable et exporté en CSV)
idx <- lire_index(archive)
cat("Entrées :", nrow(idx), "| tailles aberrantes :", sum(idx$taille_suspecte), "\n")
write.csv2(idx, file.path(dossier_data, "Panel_Hogares_content.csv"), row.names = FALSE)

# Fichiers à explorer
cand <- idx[!est_dossier & grepl(motif, chemin, ignore.case = TRUE)]
if (!is.null(filtre_chemin)) cand <- cand[grepl(filtre_chemin, chemin, fixed = TRUE)]

ignores <- rbind(cand[!is.na(taille) & taille > max_taille_Mo * 1e6, .(chemin, taille, raison = "trop gros")],
                 cand[is.na(taille), .(chemin, taille, raison = "taille inconnue")])
fwrite(ignores, file.path(dossier_data, "fichiers_ignores.csv"), sep = ";")
a_traiter <- cand[!is.na(taille) & taille <= max_taille_Mo * 1e6]
cat("Candidats :", nrow(cand), "| à traiter :", nrow(a_traiter), "| ignorés :", nrow(ignores), "\n")

# Reprise
if (reinitialiser) unlink(fichier_res)
if (file.exists(fichier_res)) {
  anciens <- fread(fichier_res, sep = ";")
  deja <- if (retenter_echecs) anciens[statut == "ok", chemin] else anciens$chemin
  a_traiter <- a_traiter[!chemin %in% deja]
}
a_traiter <- head(a_traiter, max_fichiers)

# Boucle : un fichier à la fois
for (i in seq_len(nrow(a_traiter))) {
  res <- traiter_un(a_traiter$chemin[i], a_traiter$taille[i])
  fwrite(as.data.table(res), fichier_res, sep = ";", append = file.exists(fichier_res))
  cat(sprintf("[%d/%d] %s (%ss) - %s\n", i, nrow(a_traiter), res$statut, res$duree_s, res$chemin))
  unlink(tmp_dir, recursive = TRUE)
}

if (file.exists(fichier_res)) View(fread(fichier_res, sep = ";"))



### 
# Les infos quant aux nmb de lignes et colonnes / dico des variables pour chacun des fichiers 
# sont à retrouver dans les excel diseño 
# /Users/maelieperier/OneDrive/Granada2627/UGR/DIME Vivienda/Panel de hogares/DISENO/00_DiseñoRegistro_2016_2019.xlsx