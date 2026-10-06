library(data.table)
library(jsonlite)
library(arrow)    

# ================= RÉGLAGES (à modifier) =================
archive        <- path.expand("~/work/UGR/Panel_Hogares.rar")
dossier_sortie <- path.expand("~/work/UGR/data")
tmp_dir        <- path.expand("~/work/UGR/tmp_extraction")

max_taille_Mo  <- 200            # on ignore les fichiers plus gros que ça
budget_lot_Go  <- 1              # espace max extrait en une fois (reste < ton disque libre)
motif          <- "\\.(txt|csv)$"  # types de fichiers visés
filtre_chemin  <- NULL           # ex: "000 IDEN" pour viser un seul sous-dossier
sep            <- ";"
encodage       <- "unknown"      # mettre "Latin-1" si les accents sont cassés
convertir      <- FALSE          # TRUE = écrit aussi un fichier parquet par fichier
max_fichiers   <- 2              # TEST : 5 fichiers. Mettre Inf pour tout traiter
# ==========================================================

dir.create(dossier_sortie, showWarnings = FALSE, recursive = TRUE)
fichier_res <- file.path(dossier_sortie, "exploration_fichiers.csv")

# --- 1. Index de l'archive ---
json <- tempfile(fileext = ".json")
system(paste("lsar -j", shQuote(archive), ">", shQuote(json)))
x <- fromJSON(json, flatten = TRUE)$lsarContents

est_dossier <- if (is.null(x$XADIsDirectory)) rep(FALSE, nrow(x)) else x$XADIsDirectory %in% 1
index <- data.table(
  chemin = gsub("\\\\", "/", x$XADFileName),
  taille = if (is.null(x$XADFileSize)) NA_real_ else as.numeric(x$XADFileSize),
  est_dossier = est_dossier
)
index[taille > 1e15, taille := NA]            # tailles aberrantes lues par lsar

cand <- index[!est_dossier & grepl(motif, chemin, ignore.case = TRUE)]
if (!is.null(filtre_chemin)) cand <- cand[grepl(filtre_chemin, chemin, fixed = TRUE)]

trop_gros <- cand[!is.na(taille) & taille >  max_taille_Mo * 1e6]
inconnue  <- cand[is.na(taille)]
a_traiter <- cand[!is.na(taille) & taille <= max_taille_Mo * 1e6]

cat("Candidats :", nrow(cand), "| à traiter :", nrow(a_traiter),
    "| trop gros :", nrow(trop_gros), "| taille inconnue :", nrow(inconnue), "\n")

fwrite(rbind(trop_gros[, .(chemin, taille, raison = "trop gros")],
             inconnue[,  .(chemin, taille, raison = "taille inconnue")]),
       file.path(dossier_sortie, "fichiers_ignores.csv"), sep = ";")

# --- 2. Reprise : on saute ce qui a déjà été fait ---
if (file.exists(fichier_res)) {
  deja <- fread(fichier_res, sep = ";")[statut == "ok", chemin]
  a_traiter <- a_traiter[!chemin %in% deja]
}
a_traiter <- head(a_traiter, max_fichiers)
a_traiter[, lot := cumsum(taille) %/% (budget_lot_Go * 1e9)]   # lots d'environ budget_lot_Go

# --- 3. Exploration d'un fichier déjà extrait ---
explorer <- function(chemin_rel) {
  out <- data.table(chemin = chemin_rel, taille_octets = NA_real_, n_lignes = NA_integer_,
                    n_colonnes = NA_integer_, colonnes = NA_character_,
                    types = NA_character_, statut = NA_character_)
  f <- file.path(tmp_dir, chemin_rel)
  if (!file.exists(f)) { out$statut <- "introuvable après extraction"; return(out) }
  tryCatch({
    df <- fread(f, sep = sep, encoding = encodage)
    out[, `:=`(taille_octets = file.size(f), n_lignes = nrow(df), n_colonnes = ncol(df),
               colonnes = paste(names(df), collapse = " | "),
               types = paste(vapply(df, function(c) class(c)[1], ""), collapse = " | "),
               statut = "ok")]
    if (convertir) {
      p <- file.path(dossier_sortie, "parquet", sub("\\.[^.]+$", ".parquet", chemin_rel))
      dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE)
      write_parquet(df, p, compression = "zstd")
    }
    out
  }, error = function(e) { out$statut <- paste("erreur:", conditionMessage(e)); out })
}

# --- 4. Boucle par lots ---
for (l in unique(a_traiter$lot)) {
  lot <- a_traiter[lot == l]
  unlink(tmp_dir, recursive = TRUE); dir.create(tmp_dir, recursive = TRUE)
  system2("unar", c("-f", "-D", "-o", shQuote(tmp_dir), shQuote(archive), shQuote(lot$chemin)))
  
  for (ch in lot$chemin) {
    res <- explorer(ch)
    fwrite(res, fichier_res, sep = ";", append = file.exists(fichier_res))
    cat(res$statut, "-", ch, "\n")
  }
  unlink(tmp_dir, recursive = TRUE)     # on libère l'espace avant le lot suivant
}

# --- 5. Résultat consultable ---
resultats <- fread(fichier_res, sep = ";")
View(resultats)
