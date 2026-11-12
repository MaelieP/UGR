# =============================================================================
# 09_idealista.R
# Traitement Idealista seul (aucune microdonnée fiscale lue ici).
#   1) Charge et fusionne les trois bases (section x trimestre)
#   2) Contrôle le découpage des sections
#   3) Agrège section -> district et commune, trimestre -> année
#   4) Déflate en €2023 (IPC INE 50934), retards t-1, ratio prix / loyer
# Sorties (bucket) :
#   idealista/merged/idealista_sec_quarter.parquet  section x trimestre
#   idealista/merged/idealista_geo_year.parquet     (district ou commune) x année
# Jointure au panel : fonction join_idealista() dans fn_idealista.R
# NON TESTÉ : à exécuter section par section dans la console Onyxia.
# =============================================================================

library(data.table)
library(arrow)

# ---- 0. Paramètres ----------------------------------------------------------
bkt <- s3_bucket("mperier", endpoint_override = Sys.getenv("AWS_S3_ENDPOINT"))

CHK_DIR <- "~/work/UGR/checks_idealista"
dir.create(CHK_DIR, showWarnings = FALSE, recursive = TRUE)

# Motif du nom de chaque base dans le dossier idealista/ du bucket
SRC <- c(`2012_2016` = "idealista_2012_2016",
         `2017_2022` = "idealista_2017_2022",
         `2022_2026` = "202606_residential_metrics")

OUT_SECQ <- "idealista/merged/idealista_sec_quarter.parquet"
OUT_GEOY <- "idealista/merged/idealista_geo_year.parquet"

# Seuils (appliqués en dur : valeur mise à NA si non atteint)
MIN_ADS_Q <- 1L    # stock minimum pour qu'un trimestre compte
MIN_Q     <- 2L    # trimestres minimum pour une valeur annuelle
MIN_ADS   <- 6L   # stock annuel minimum (somme des stocks trimestriels)

# IPC : INE table 50934, IPC general, media anual (base 2025 = 100)
# f23 = I_2023 / I_t  (montant nominal de t x f23 -> €2023)
IPC_IDX <- c(`2012` = 78.298, `2013` = 79.401, `2014` = 79.281, `2015` = 78.885,
             `2016` = 78.725, `2017` = 80.265, `2018` = 81.609, `2019` = 82.180,
             `2020` = 81.915, `2021` = 84.448, `2022` = 91.534, `2023` = 94.768,
             `2024` = 97.397, `2025` = 100.000)
IPC <- data.table(year = as.integer(names(IPC_IDX)), f23 = IPC_IDX[["2023"]] / unname(IPC_IDX))


# ---- 1. Chargement et fusion ------------------------------------------------
# Vrais fichiers (pas les marqueurs de dossier) sous idealista/
infos <- bkt$GetFileInfo(FileSelector$create("idealista", recursive = TRUE))
FILES <- data.table(path = vapply(infos, function(i) i$path, ""),
                    type = vapply(infos, function(i) as.integer(i$type), 0L),
                    size = vapply(infos, function(i) as.numeric(i$size), 0))
FILES <- FILES[type == as.integer(FileType$File) & size > 0 & !grepl("/merged/", path)]

# Tout est lu en texte côté Arrow (types homogènes, pas d'integer64, zéros de tête gardés)
read_src <- function(pattern) {
  fs <- FILES[grepl(pattern, path, fixed = TRUE), path]
  if (!length(fs)) stop("Aucun fichier pour le motif : ", pattern)
  cat("  ", pattern, "->", paste(fs, collapse = " | "), "\n")
  rbindlist(lapply(fs, function(f) {
    t <- read_csv_arrow(bkt$path(f), as_data_frame = FALSE)
    for (nm in names(t)) t[[nm]] <- t[[nm]]$cast(string())
    as.data.table(t)
  }), use.names = TRUE)
}

cat("Fichiers lus :\n")
IDL <- rbindlist(lapply(names(SRC), function(s) read_src(SRC[[s]])[, src := s]), use.names = TRUE)

setnames(IDL, c("LOCATIONID", "PERIOD",
                "UNITPRICE_RESIDENTIAL_SALE_ALL", "UNITPRICE_RESIDENTIAL_RENT_ALL",
                "STOCK_RESIDENTIAL_SALE_ALL",     "STOCK_RESIDENTIAL_RENT_ALL"),
         c("sec_raw", "period_raw", "sale", "rent", "n_sale", "n_rent"))



# PERIOD -> date -> année / trimestre (ISO puis jj/mm/aaaa)
parse_period <- function(x) {
  x <- substr(trimws(x), 1, 10)
  d <- as.IDate(x, format = "%Y-%m-%d")
  i <- is.na(d) & !is.na(x)
  if (any(i)) d[i] <- as.IDate(x[i], format = "%d/%m/%Y")
  d
}
IDL[, period := parse_period(period_raw)]
print(IDL[!is.na(period), .(n_rows = .N, first = min(period), last = max(period),
                            months = paste(sort(unique(month(period))), collapse = ",")), by = src])
IDL[, `:=`(year = year(period), quarter = quarter(period))]

# Section : normalisation en code INE à 10 chiffres
norm_sec <- function(x) {
  x <- gsub("\\.0+$", "", trimws(x))
  i9 <- !is.na(x) & grepl("^[0-9]{9}$", x)
  x[i9] <- paste0("0", x[i9])                 # zéro de tête perdu (provinces 01–09)
  x[!grepl("^[0-9]{10}$", x)] <- NA_character_
  x
}
IDL[, sec := norm_sec(sec_raw)]

IDL[, `:=`(dist = substr(sec, 1, 7), mun = substr(sec, 1, 5))]


IDL[, t := year * 4L + quarter - 1L]   # index de trimestre

# ---- 2. Contrôle du découpage -----------------------------------------------

# 2b. Dynamique des codes d'un trimestre à l'autre
P <- IDL[!is.na(sec), .(codes = list(unique(sec)), src = src[1L]), by = t][order(t)]
P[, `:=`(n      = lengths(codes),
         n_new  = c(NA, mapply(function(a, b) length(setdiff(a, b)), codes[-1], codes[-.N])),
         n_gone = c(NA, mapply(function(a, b) length(setdiff(b, a)), codes[-1], codes[-.N])))]
dyn <- P[, .(year = t %/% 4L, quarter = t %% 4L + 1L, src, n, n_new, n_gone)]
print(dyn); fwrite(dyn, file.path(CHK_DIR, "sec_dynamics.csv"))

# 2c. Continuité aux jonctions entre bases
bounds <- IDL[, .(tmin = min(t), tmax = max(t)), by = src][order(tmin)]
c2c <- rbindlist(lapply(seq_len(nrow(bounds) - 1L), function(k) {
  a <- IDL[src == bounds$src[k]      & t == bounds$tmax[k]      & !is.na(sec), unique(sec)]
  b <- IDL[src == bounds$src[k + 1L] & t == bounds$tmin[k + 1L] & !is.na(sec), unique(sec)]
  data.table(from = paste(bounds$src[k], bounds$tmax[k] %/% 4L, "T", bounds$tmax[k] %% 4L + 1L),
             to   = paste(bounds$src[k + 1L], bounds$tmin[k + 1L] %/% 4L, "T", bounds$tmin[k + 1L] %% 4L + 1L),
             n_from = length(a), n_to = length(b), common = length(intersect(a, b)),
             only_from = length(setdiff(a, b)), only_to = length(setdiff(b, a)),
             only_to_in_2012_2016 = sum(setdiff(b, a) %in% IDL[src == "2012_2016", sec]))
}))
print(c2c); fwrite(c2c, file.path(CHK_DIR, "sec_junctions.csv"))

# 2d. Sections perdues au passage à la base 2022_2026 : où ? et cmb 
newb <- IDL[src == "2022_2026", unique(sec)]
lost <- IDL[src == "2017_2022" & t == max(t[src == "2017_2022"]) & !(sec %in% newb)]
c2d <- lost[, .(n_sec = .N, stock_sale = sum(n_sale, na.rm = TRUE)), by = .(prov = substr(sec, 1, 2))][order(-n_sec)]
cat("\nSections perdues en 2022T2 par province :\n"); print(c2d)
fwrite(c2d, file.path(CHK_DIR, "sec_lost_2022_by_prov.csv"))
# En somme très peu de sections perdues et encore moins de logements 


# ---- 3. Sauvegarde section x trimestre --------------------------------------
SQ <- IDL[!is.na(sec) & !is.na(period),
          .(sec, dist, mun, year, quarter, period, sale, rent, n_sale, n_rent, src)]
stopifnot(!anyDuplicated(SQ, by = c("sec", "year", "quarter")))
setorder(SQ, sec, year, quarter)
tbl <- arrow_table(SQ); tbl$metadata$r <- NULL
write_parquet(tbl, bkt$path(OUT_SECQ), compression = "zstd")
cat("Écrit :", OUT_SECQ, "-", nrow(SQ), "lignes\n")

# ---- 4. Section -> district / commune, par trimestre ------------------------
A <- copy(SQ)
# Prix manquant ou non positif -> poids nul ; stock NA -> 0
A[is.na(n_sale) | is.na(sale) | sale <= 0, n_sale := 0]
A[is.na(n_rent) | is.na(rent) | rent <= 0, n_rent := 0]

# Moyenne pondérée par le stock = moyenne sur les annonces
# (exacte si la valeur de section est une moyenne, approchée si c'est une médiane)
wm <- function(x, n) { s <- sum(n); if (s > 0) sum(x * n, na.rm = TRUE) / s else NA_real_ }

q_agg <- function(code_col, level) {
  Q <- A[, .(sale = wm(sale, n_sale), rent = wm(rent, n_rent),
             n_sale = sum(n_sale), n_rent = sum(n_rent), n_sec = uniqueN(sec)),
         by = c(code_col, "year", "quarter")]
  setnames(Q, code_col, "geo_code")
  Q[n_sale < MIN_ADS_Q, sale := NA_real_]
  Q[n_rent < MIN_ADS_Q, rent := NA_real_]
  Q[, geo_level := level][]
}
GQ <- rbind(q_agg("dist", "dist"), q_agg("mun", "mun"))

# ---- 5. Trimestre -> année --------------------------------------------------
# Moyenne simple des trimestres disponibles ; les lignes restent même si le prix est NA
GY <- GQ[, .(sale    = if (sum(!is.na(sale)) >= MIN_Q) mean(sale, na.rm = TRUE) else NA_real_,
             rent    = if (sum(!is.na(rent)) >= MIN_Q) mean(rent, na.rm = TRUE) else NA_real_,
             nq_sale = sum(!is.na(sale)), nq_rent = sum(!is.na(rent)),
             n_sale  = sum(n_sale), n_rent = sum(n_rent), n_sec = max(n_sec)),
         by = .(geo_level, geo_code, year)]
GY[n_sale < MIN_ADS, sale := NA_real_]
GY[n_rent < MIN_ADS, rent := NA_real_]

# ---- 6. Déflation (€2023) ---------------------------------------------------
miss <- setdiff(unique(GY$year), IPC$year)
if (length(miss)) warning("Pas d'indice IPC pour : ", paste(sort(miss), collapse = ", "),
                          " -> années écartées")
GY <- IPC[GY, on = "year", nomatch = 0L]
GY[, `:=`(sale = sale * f23, rent = rent * f23)][, f23 := NULL]

# ---- 7. Retards t-1 et ratio prix / loyer -----------------------------------
L <- GY[, .(geo_level, geo_code, year = year + 1L, sale_l1 = sale, rent_l1 = rent)]
GY <- L[GY, on = .(geo_level, geo_code, year)]
GY[, ptr := sale / (12 * rent)]   # années de loyer (vente €/m², loyer €/m²/mois)

setcolorder(GY, c("geo_level", "geo_code", "year", "sale", "rent", "sale_l1", "rent_l1", "ptr",
                  "n_sale", "n_rent", "nq_sale", "nq_rent", "n_sec"))
setorder(GY, geo_level, geo_code, year)
stopifnot(!anyDuplicated(GY, by = c("geo_level", "geo_code", "year")))

# ---- 8. Contrôles et sauvegarde ---------------------------------------------
chk_prices <- GY[, .(n = .N, n_sale_ok = sum(!is.na(sale)), med_sale = median(sale, na.rm = TRUE),
                     n_rent_ok = sum(!is.na(rent)), med_rent = median(rent, na.rm = TRUE),
                     med_ptr = median(ptr, na.rm = TRUE)),
                 by = .(geo_level, year)][order(geo_level, year)]
print(chk_prices)
fwrite(chk_prices, file.path(CHK_DIR, "geo_year_prices.csv"))

tbl <- arrow_table(GY); tbl$metadata$r <- NULL
write_parquet(tbl, bkt$path(OUT_GEOY), compression = "zstd")
cat("Écrit :", OUT_GEOY, "-", nrow(GY), "lignes\n")
