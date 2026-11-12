# =============================================================================
# 10_idealista_district.R
# Module Idealista : prix de vente et de location au niveau district, par personne-année.
#   sections 2011 -> district (moyenne pondérée par le nb d'annonces)
#   trimestre -> année (moyenne simple des trimestres, >= MIN_Q trimestres)
#   déflation vers €2023 avec l'IPC INE (table 50934, media anual)
#   repli au niveau commune si le district ne matche pas
# Entrées  : bucket mperier (panel avec section + CSV Idealista)
# Sortie   : panel_hogares/mod_idealista.parquet (IDENPER, year, idl_*)
#            + cache/ et CSV de contrôle dans OUT_DIR
# NON TESTÉ : à exécuter section par section dans la console Onyxia.
# =============================================================================
library(aws.s3)
library(readr)
library(data.table)
library(arrow)
library(dplyr)   # select()/mutate()/collect() sur open_dataset

# ---- 0. Paramètres ----------------------------------------------------------
bkt <- s3_bucket("mperier", endpoint_override = Sys.getenv("AWS_S3_ENDPOINT"))

OUT_DIR  <- "~/work/UGR/panel_idealista"                          # local : cache/ et CSV
PANEL_IN <- "panel_hogares/panel_persons_2016_2023_sec.parquet"           # bucket
IDEA_IN  <- "idealista/202606_residential_metrics_idealistadata_v2.csv"   # bucket
MOD_OUT  <- "panel_hogares/mod_idealista.parquet"                         # bucket
SEC_COL  <- "SECCION" # section dans le panel

# Colonnes Idealista -> noms standard
IDEA_MAP <- c(
  sec    = "LOCATIONID",                       # section censale 2011 (à vérifier)
  period = "PERIOD",                           # date, une par trimestre
  sale   = "UNITPRICE_RESIDENTIAL_SALE_ALL",   # prix de vente (€/m²)
  rent   = "UNITPRICE_RESIDENTIAL_RENT_ALL",   # loyer (€/m²/mois)
  n_sale = "STOCK_RESIDENTIAL_SALE_ALL",       # stock d'annonces de vente
  n_rent = "STOCK_RESIDENTIAL_RENT_ALL"        # stock d'annonces de location
)

MIN_ADS_Q <- 3L    # annonces minimum pour qu'un trimestre compte
MIN_Q     <- 2L    # trimestres minimum pour une valeur annuelle
MIN_ADS   <- 10L   # annonces annuelles minimum dans l'unité géographique
FORCE_MUN_IF_REDRAWN <- TRUE   # communes avec codes district nouveaux -> niveau commune

# IPC : INE table 50934, IPC general, media anual (base 2025 = 100)
# f23 = I_2023 / I_t  (multiplier un montant nominal de t par f23 -> €2023)
IPC_IDX <- c(`2012` = 78.298, `2013` = 79.401, `2014` = 79.281, `2015` = 78.885,
             `2016` = 78.725, `2017` = 80.265, `2018` = 81.609, `2019` = 82.180,
             `2020` = 81.915, `2021` = 84.448, `2022` = 91.534, `2023` = 94.768)
IPC <- data.table(year = as.integer(names(IPC_IDX)), f23 = IPC_IDX[["2023"]] / unname(IPC_IDX))
print(IPC)

# ---- 1. Lecture Idealista ---------------------------------------------------
norm_sec <- function(x) {
  if (is.numeric(x)) x <- ifelse(is.na(x), NA_character_, sprintf("%010.0f", x))
  x <- trimws(as.character(x))
  i9 <- !is.na(x) & nchar(x) == 9L
  x[i9] <- paste0("0", x[i9])                 # zéro de tête perdu (provinces 01–09)
  x[!grepl("^[0-9]{10}$", x)] <- NA_character_
  x
}

# La section est convertie en texte côté Arrow (en int64 elle perdrait son zéro de tête)
ID <- read_csv_arrow(bkt$path(IDEA_IN), as_data_frame = FALSE)
stopifnot(all(IDEA_MAP %in% names(ID)))
ID <- as.data.table(ID)
sc <- IDEA_MAP[["sec"]]
if (inherits(ID[[sc]], "integer64")) {      
  loadNamespace("bit64")
  ID[, (sc) := as.character(get(sc))]
}
ID <- ID[, unname(IDEA_MAP), with = FALSE]
setnames(ID, names(IDEA_MAP))

# PERIOD (date) -> année et trimestre
ID[, period := as.IDate(period)]
print(ID[, .N, by = .(mois = month(period))][order(mois)])   # attendu : 4 mois distincts
ID[, `:=`(year = year(period), quarter = quarter(period))]

ID[, `:=`(sec = norm_sec(sec),
          sale = as.numeric(sale), rent = as.numeric(rent),
          n_sale = as.numeric(n_sale), n_rent = as.numeric(n_rent))]

cat("Sections invalides :", ID[is.na(sec), .N], "\n")
cat("Prix sans nb d'annonces (vente / loc) :",
    ID[!is.na(sale) & is.na(n_sale), .N], "/", ID[!is.na(rent) & is.na(n_rent), .N], "\n")
ID <- ID[!is.na(sec)]
stopifnot(ID[, .N, by = .(sec, year, quarter)][, max(N)] == 1L)
print(ID[, .N, by = year][order(year)])

# Prix manquant ou non positif -> poids nul ; nb d'annonces NA -> 0
ID[is.na(n_sale) | is.na(sale) | sale <= 0, n_sale := 0]
ID[is.na(n_rent) | is.na(rent) | rent <= 0, n_rent := 0]
ID[, `:=`(dist = substr(sec, 1, 7), mun = substr(sec, 1, 5))]

# ---- 2. Section -> district / commune, par trimestre ------------------------
# Moyenne pondérée par le nb d'annonces = moyenne sur les annonces
# (exacte si la valeur de section est une moyenne, approchée si c'est une médiane).
wm <- function(x, n) { s <- sum(n); if (s > 0) sum(x * n, na.rm = TRUE) / s else NA_real_ }

q_agg <- function(by) {
  Q <- ID[, .(sale = wm(sale, n_sale), rent = wm(rent, n_rent),
              n_sale = sum(n_sale), n_rent = sum(n_rent),
              n_sec = uniqueN(sec)),
          by = c(by, "year", "quarter")]
  Q[n_sale < MIN_ADS_Q, sale := NA_real_]
  Q[n_rent < MIN_ADS_Q, rent := NA_real_]
  Q[]
}
DQ <- q_agg("dist")
MQ <- q_agg("mun")

# ---- 3. Trimestre -> année --------------------------------------------------
y_agg <- function(Q, by) {
  Y <- Q[, .(sale    = if (sum(!is.na(sale)) >= MIN_Q) mean(sale, na.rm = TRUE) else NA_real_,
             rent    = if (sum(!is.na(rent)) >= MIN_Q) mean(rent, na.rm = TRUE) else NA_real_,
             nq_sale = sum(!is.na(sale)), nq_rent = sum(!is.na(rent)),
             n_sale  = sum(n_sale),       n_rent  = sum(n_rent)),
         by = c(by, "year")]
  Y[n_sale < MIN_ADS, sale := NA_real_]
  Y[n_rent < MIN_ADS, rent := NA_real_]
  Y[]
}
DY <- y_agg(DQ, "dist")
MY <- y_agg(MQ, "mun")

# ---- 4. Déflation (€2023) ---------------------------------------------------
miss <- setdiff(unique(DY$year), IPC$year)
if (length(miss)) warning("Pas d'indice IPC pour : ", paste(sort(miss), collapse = ", "),
                          " -> années écartées")
defl <- function(Y) {
  Y <- IPC[Y, on = "year", nomatch = 0L]
  Y[, `:=`(sale = sale * f23, rent = rent * f23)][, f23 := NULL][]
}
DY <- defl(DY)
MY <- defl(MY)

# ---- 5. Retard t-1 (même code géographique) ---------------------------------
add_lag <- function(Y, key) {
  L <- Y[, c(key, "year", "sale", "rent"), with = FALSE][, year := year + 1L]
  setnames(L, c("sale", "rent"), c("sale_l1", "rent_l1"))
  L[Y, on = c(key, "year")]
}
DY <- add_lag(DY, "dist")
MY <- add_lag(MY, "mun")

dir.create(file.path(OUT_DIR, "cache"), showWarnings = FALSE, recursive = TRUE)
saveRDS(DY, file.path(OUT_DIR, "cache/idealista_district_year.rds"))
saveRDS(MY, file.path(OUT_DIR, "cache/idealista_mun_year.rds"))

# ---- 6. Jointure au PANEL ---------------------------------------------------
# Seules les colonnes utiles sont lues (fichier unique ou dossier partitionné)
PANEL <- open_dataset(bkt$path(PANEL_IN)) |>
  select(all_of(c("IDENPER", "year", "w", "CPRO", "CMUN", SEC_COL))) |>
  mutate(across(all_of(SEC_COL), as.character)) |>   # texte côté Arrow (évite integer64)
  collect() |>
  as.data.table()
PANEL[, year := as.integer(year)]
stopifnot(PANEL[, .N, by = .(IDENPER, year)][, max(N)] == 1L)

PANEL[, idl_dist := substr(norm_sec(get(SEC_COL)), 1, 7)]
PANEL[, idl_mun  := substr(idl_dist, 1, 5)]

# Cohérence section / CPRO+CMUN (doit être ~0)
cat("Part section incohérente avec CPRO+CMUN :",
    PANEL[!is.na(idl_mun),
          mean(idl_mun != paste0(formatC(as.integer(CPRO), width = 2, flag = "0"),
                                 formatC(as.integer(CMUN), width = 3, flag = "0")))], "\n")

# Communes-années où le panel a des codes district absents du découpage 2011
codes11 <- unique(ID[, .(mun, dist)])
redrawn <- unique(PANEL[!is.na(idl_dist), .(year, mun = idl_mun, dist = idl_dist)]
)[!codes11, on = .(mun, dist)][, .(n_new = uniqueN(dist)), by = .(mun, year)]
PANEL[, idl_redrawn := FALSE]
PANEL[redrawn, on = .(idl_mun = mun, year), idl_redrawn := TRUE]

vars <- c("sale", "rent", "sale_l1", "rent_l1", "n_sale", "n_rent", "nq_sale", "nq_rent")
PANEL[DY, on = .(idl_dist = dist, year), paste0("idl_", vars, "_d") := mget(paste0("i.", vars))]
PANEL[MY, on = .(idl_mun  = mun,  year), paste0("idl_", vars, "_m") := mget(paste0("i.", vars))]

if (FORCE_MUN_IF_REDRAWN)
  PANEL[idl_redrawn == TRUE, paste0("idl_", vars, "_d") := NA]

# Valeur finale : district, sinon commune
for (v in c("sale", "rent", "sale_l1", "rent_l1")) {
  d <- paste0("idl_", v, "_d"); m <- paste0("idl_", v, "_m")
  PANEL[, paste0("idl_", v) := fifelse(!is.na(get(d)), get(d), get(m))]
}
PANEL[, idl_geo_sale := fcase(!is.na(idl_sale_d), "dist", !is.na(idl_sale_m), "mun",
                              default = NA_character_)]
PANEL[, idl_geo_rent := fcase(!is.na(idl_rent_d), "dist", !is.na(idl_rent_m), "mun",
                              default = NA_character_)]

# Ratio prix / loyer annuel (vente €/m², loyer €/m²/mois), seulement si même niveau
PANEL[, idl_ptr := fifelse(idl_geo_sale == idl_geo_rent,
                           idl_sale / (12 * idl_rent), NA_real_)]

# On garde : valeurs finales, versions district pur, comptages district, drapeaux
drop <- c(grep("^idl_.*_m$", names(PANEL), value = TRUE),
          paste0("idl_", c("sale_l1", "rent_l1"), "_d"))
PANEL[, (drop) := NULL]

# ---- 7. Contrôles -----------------------------------------------------------
sh <- function(cond, w) sum(w[cond], na.rm = TRUE) / sum(w, na.rm = TRUE)
chk_match <- PANEL[, .(
  w_tot        = sum(w, na.rm = TRUE),
  sh_sec_ok    = sh(!is.na(idl_dist), w),
  sh_redrawn   = sh(idl_redrawn, w),
  sh_sale_dist = sh(idl_geo_sale %in% "dist", w),
  sh_sale_mun  = sh(idl_geo_sale %in% "mun", w),
  sh_sale_na   = sh(is.na(idl_geo_sale), w),
  sh_rent_dist = sh(idl_geo_rent %in% "dist", w),
  sh_rent_mun  = sh(idl_geo_rent %in% "mun", w),
  sh_rent_na   = sh(is.na(idl_geo_rent), w)
), by = year][order(year)]
print(chk_match)
fwrite(chk_match, file.path(OUT_DIR, "check_idealista_match.csv"))

# Districts du panel non matchés, les plus lourds (fichier local, pas dans git)
chk_unm <- PANEL[year <= 2022 & !is.na(idl_dist) & is.na(idl_sale_d),
                 .(w = sum(w, na.rm = TRUE), redrawn = any(idl_redrawn)),
                 by = .(year, idl_dist)][order(-w)][seq_len(min(.N, 100L))]
fwrite(chk_unm, file.path(OUT_DIR, "check_idealista_unmatched.csv"))

# Trajectoire des prix déflatés (€2023)
chk_prices <- DY[, .(n_dist_sale = sum(!is.na(sale)), med_sale = median(sale, na.rm = TRUE),
                     n_dist_rent = sum(!is.na(rent)), med_rent = median(rent, na.rm = TRUE)),
                 by = year][order(year)]
print(chk_prices)
fwrite(chk_prices, file.path(OUT_DIR, "check_idealista_prices.csv"))

# ---- 8. Sauvegarde du module ------------------------------------------------
# Même convention que mod_moves : IDENPER, year + colonnes idl_*
keep_cols <- grep("^idl_", names(PANEL), value = TRUE)
mod_idealista <- PANEL[, c("IDENPER", "year", keep_cols), with = FALSE]
stopifnot(!anyDuplicated(mod_idealista, by = c("IDENPER", "year")))   # one row per person-year

tbl <- arrow_table(mod_idealista); tbl$metadata$r <- NULL
write_parquet(tbl, bkt$path(MOD_OUT), compression = "zstd")

# ---- Usage dans un script d'analyse -----------------------------------------
# M <- as.data.table(read_parquet(bkt$path("panel_hogares/mod_idealista.parquet")))
# cols <- setdiff(names(M), c("IDENPER", "year"))
# PANEL[M, on = .(IDENPER, year), (cols) := mget(paste0("i.", cols))]   # par référence
