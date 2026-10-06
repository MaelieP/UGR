# Open .rar files without entirely unzipping them 

# à mettre dans le terminal : 
# sudo apt-get update && sudo apt-get install -y unar
# which lsar unar

library(arrow)
library(data.table)
library(jsonlite)
library(dplyr)

bucket <- s3_bucket("mperier",
                    endpoint_override = Sys.getenv("AWS_S3_ENDPOINT"))
work <- path.expand("~/work/UGR")

traiter_rar <- function(nom_rar) {
  base   <- tools::file_path_sans_ext(basename(nom_rar))
  local  <- file.path(work, "archive.rar")
  tmpdir <- file.path(work, "tmp")
  
  # 1. copie depuis le bucket, par morceaux de 64 Mo
  entree <- bucket$OpenInputStream(nom_rar)
  sortie <- file(local, "wb")
  repeat {
    m <- entree$Read(64 * 1024^2)
    if (m$size == 0) break
    writeBin(m$data(), sortie)
  }
  close(sortie); entree$close()
  
  # 2. extraction
  system2("unar", c("-f", "-o", shQuote(tmpdir), shQuote(local)))
  
  # 3. conversion en parquet et envoi vers le bucket
  fichiers <- list.files(tmpdir, pattern = "\\.(txt|csv)$",
                         full.names = TRUE, recursive = TRUE,
                         ignore.case = TRUE)
  for (f in fichiers) {
    df  <- fread(f, sep = ";")
    out <- paste0("parquet/", base, "/",
                  tools::file_path_sans_ext(basename(f)), ".parquet")
    write_parquet(df, bucket$path(out))
    rm(df); gc()
  }
  
  # 4. nettoyage
  unlink(c(local, tmpdir), recursive = TRUE)
  cat("OK :", nom_rar, "-", length(fichiers), "fichiers\n")
}

# Liste des archives à traiter
archives <- c("MODULO INMOBILIARIO 2023.rar")  # ajoute les autres ici

for (a in archives) {
  tryCatch(traiter_rar(a),
           error = function(e) cat("ÉCHEC :", a, "-", conditionMessage(e), "\n"))
}


# Panel Hogares 
entree <- bucket$OpenInputStream("P HOGARES 2023.rar")
sortie <- file(path.expand("~/work/UGR/Panel_Hogares.rar"), "wb")
total <- 0
repeat {
  m <- entree$Read(64 * 1024^2)
  if (m$size == 0) break
  writeBin(m$data(), sortie)
  total <- total + m$size
  if (total %% (1024^3) < 64 * 1024^2) cat(round(total / 1024^3, 1), "Go\n")
}
close(sortie); entree$close()

##
# Convertir le contenu du dossier en tableau 
##

options(scipen = 999) 

archive <- path.expand("~/work/UGR/Panel_Hogares.rar")

# lsar écrit le contenu de l'archive en JSON dans un fichier temporaire
json <- tempfile(fileext = ".json")
system(paste("lsar -j", shQuote(archive), ">", shQuote(json)))

x <- fromJSON(json, flatten = TRUE)$lsarContents

# on garde les colonnes utiles (celles qui existent)
cols <- c(chemin   = "XADFileName",
          taille   = "XADFileSize",
          taille_compressee = "XADCompressedSize",
          date_modif = "XADLastModificationDate",
          dossier  = "XADIsDirectory")
cols <- cols[cols %in% names(x)]
contenu <- x[, cols, drop = FALSE]
names(contenu) <- names(cols)

contenu$taille_Go <- round(contenu$taille / 1024^3, 3)


taille_lisible <- function(x) {
  unites <- c("o", "Ko", "Mo", "Go", "To")
  i <- ifelse(is.na(x) | x < 1, 1, pmin(floor(log(x, 1000)), 4) + 1)
  valeur <- round(x / 1000^(i - 1), 2)
  txt <- vapply(valeur, function(v)
    if (is.na(v)) NA_character_ else
      format(v, scientific = FALSE, decimal.mark = ",",
             big.mark = " ", trim = TRUE, drop0trailing = TRUE),
    character(1))
  ifelse(is.na(txt), NA_character_, paste(txt, unites[i]))
}

taille_lisible(c(0, 850, 1500000, 3.2e9, 472100600500))

contenu <- contenu |>
  mutate(
    Taille_decompressee = taille_lisible(taille),
    Taille_compressee = taille_lisible(taille_compressee)
  ) |>
  select(-any_of("taille_Go"))   # on retire l'ancienne colonne en Go

dim(contenu)        # nombre de lignes
head(contenu)

dossier_data <- path.expand("~/work/UGR/data")
dir.create(dossier_data, showWarnings = FALSE, recursive = TRUE)

write.csv2(contenu, file.path(dossier_data, "Panel_Hogares_content.csv"), row.names = FALSE)


# => Attention certains fichiers (17) ont une taille aberrante, pas de similitude entre ces fichiers... 