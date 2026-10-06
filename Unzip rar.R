# Open .rar files without entirely unzipping them 
library(arrow)
library(data.table)

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