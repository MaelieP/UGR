# =============================================================================
# Ce script crée une base au niveau districtXannée avec les infos issues de Idealista et du Panel 
# Base district x année : caractéristiques issues du panel + prix Idealista.
# Entrées (bucket) : panel_persons_2016_2023_sec.parquet, mod_moves.parquet,
#                    idealista/merged/idealista_geo_year.parquet (via fn_idealista.R)
# Sortie  (bucket) : panel_hogares/district_year.parquet 
# Une ligne = district (code panel, découpage de l'année) x année.
# CONFIDENTIALITÉ : agrégats de microdonnées fiscales. Garder n_obs, ne pas
# diffuser les cases avec peu d'observations (voir MIN_OBS), pas dans git.
# NON TESTÉ : à exécuter section par section dans la console Onyxia.
# =============================================================================

library(data.table)
library(arrow)

# ---- 0. Paramètres ----------------------------------------------------------
bkt <- s3_bucket("mperier", endpoint_override = Sys.getenv("AWS_S3_ENDPOINT"))
source("~/work/UGR/idealista/id_panel.R")

PANEL_IN <- "panel_hogares/panel_persons_2016_2023_sec.parquet"
MOVES_IN <- "panel_hogares/mod_moves.parquet"
OUT_PATH <- "panel_hogares/district_level.parquet"

MIN_OBS    <- 15L   # seuil indicatif : cases à ne pas diffuser en dessous
SEX_FEMALE <- "6"   # À VÉRIFIER dans la feuille 1_CodIDEN (codage de SEXO)

# ---- 1. Chargement ----------------------------------------------------------
PANEL <- as.data.table(read_parquet(bkt$path(PANEL_IN), col_select = c(
  "IDENPER", "IDENHOG", "year", "w", "hh_w", "is_ref", "CCAA", "CPRO", "CMUN",
  "age", "sex", "n_members", "eq_rb", "eq_rbd", "hh_any_pens", "hh_any_rent", "obs_prev")))

MV <- as.data.table(read_parquet(bkt$path(MOVES_IN),
                                 col_select = c("IDENPER", "year", "DISTRITO", "move_dist")))
PANEL[MV, on = .(IDENPER, year), `:=`(DISTRITO = i.DISTRITO, move_dist = i.move_dist)]
rm(MV); gc()

cat("Part (pondérée) sans code district :",
    PANEL[, sum(w[is.na(DISTRITO)], na.rm = TRUE) / sum(w, na.rm = TRUE)], "\n")
PANEL <- PANEL[!is.na(DISTRITO) & !is.na(w)]

# ---- 2. Outils pondérés ------------------------------------------------------
wq <- function(x, w, p) {                       # quantile pondéré
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(rep(NA_real_, length(p)))
  o <- order(x[ok]); xs <- x[ok][o]; cw <- cumsum(w[ok][o]) / sum(w[ok])
  vapply(p, function(pp) xs[which(cw >= pp)[1L]], numeric(1))
}
wgini <- function(x, w) {                       # Gini pondéré (revenus négatifs gardés)
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) < 2) return(NA_real_)
  o <- order(x[ok]); x <- x[ok][o]; w <- w[ok][o]
  W <- sum(w); mu <- sum(w * x) / W
  if (mu <= 0) return(NA_real_)
  F <- (cumsum(w) - w / 2) / W
  2 * sum(w * x * F) / (W * mu) - 1
}
wmean <- function(x, w) { ok <- !is.na(x) & !is.na(w); if (any(ok)) sum(x[ok] * w[ok]) / sum(w[ok]) else NA_real_ }

# Quintiles nationaux de revenu par année (pour mesurer la composition des districts)
PANEL[, q_nat := {
  b <- wq(eq_rbd, w, c(.2, .4, .6, .8))
  fifelse(is.na(eq_rbd), NA_integer_, findInterval(eq_rbd, b, left.open = TRUE) + 1L)
}, by = year]

# ---- 3. Agrégation district x année -----------------------------------------
D <- PANEL[, {
  q <- wq(eq_rbd, w, c(.1, .5, .9))
  .(
    # 1. Effectifs
    n_obs        = .N,                                    # personnes dans l'échantillon
    n_hh         = uniqueN(IDENHOG),                      # ménages dans l'échantillon
    pop_w        = sum(w),                                # population estimée
    hh_w         = sum(hh_w[is_ref %in% TRUE], na.rm = TRUE),  # ménages estimés
    # 2. Géographie (valeur la plus fréquente, contrôle plus bas)
    CCAA         = names(which.max(table(CCAA))),
    CPRO         = names(which.max(table(CPRO))),
    CMUN         = names(which.max(table(CMUN))),
    n_geo_incoh  = uniqueN(paste(CCAA, CPRO, CMUN)) - 1L,
    # 3. Revenu (revenu équivalent du ménage, porté par chaque personne, €2023)
    rbd_mean     = wmean(eq_rbd, w),
    rbd_p10      = q[1], rbd_med = q[2], rbd_p90 = q[3],
    rbd_gini     = wgini(eq_rbd, w),
    rb_mean      = wmean(eq_rb, w),
    sh_inc_na    = sum(w[is.na(eq_rbd)]) / sum(w),
    sh_q1_nat    = sum(w[q_nat %in% 1L]) / sum(w[!is.na(q_nat)]), # part de la pop du district qui appartient au 1er quintile national 
    sh_q5_nat    = sum(w[q_nat %in% 5L]) / sum(w[!is.na(q_nat)]),
    # 4. Socio-démographie
    age_mean     = wmean(age, w),
    sh_0_17      = sum(w[age < 18],  na.rm = TRUE) / sum(w[!is.na(age)]),
    sh_65plus    = sum(w[age >= 65], na.rm = TRUE) / sum(w[!is.na(age)]),
    sh_female    = sum(w[as.character(sex) == SEX_FEMALE], na.rm = TRUE) / sum(w[!is.na(sex)]),
    hh_size_mean = wmean(n_members, w),            # taille du ménage vécue par les personnes
    sh_hh_pens   = sum(w[hh_any_pens %in% TRUE]) / sum(w),
    sh_hh_rent   = sum(w[hh_any_rent %in% TRUE]) / sum(w),   # ménages percevant des loyers
    # 5. Mobilité
    sh_mover_in  = sum(w[move_dist %in% TRUE]) / sum(w[!is.na(move_dist)]),
    sh_obs_prev  = sum(w[obs_prev %in% TRUE]) / sum(w)
  )
}, by = .(DISTRITO, year)]

D[, `:=`(mun = substr(DISTRITO, 1, 5), small = n_obs < MIN_OBS)]
cat("Districts avec une géographie incohérente (CCAA/CPRO/CMUN multiples) :", D[n_geo_incoh > 0, .N], "\n")
cat("Part des districts-années sous", MIN_OBS, "observations :", D[, mean(small)], "\n")

# ---- 4. Idealista -----------------------------------------------------------
IDY <- load_idealista(bkt)
join_idealista(D, IDY, dist_col = "DISTRITO")   # idl_* : district, sinon commune

# Stocks d'annonces au niveau district (pour juger de la précision des prix)
D[IDY[geo_level == "dist"], on = .(DISTRITO = geo_code, year),
  `:=`(idl_n_sale_d = i.n_sale, idl_n_rent_d = i.n_rent, idl_n_sec_d = i.n_sec)]

# ---- 5. Contrôles et sauvegarde ---------------------------------------------
print(D[, .(n_dist = .N, pop_M = sum(pop_w) / 1e6, med_n_obs = median(n_obs),
            sh_small = mean(small), sh_sale_dist = mean(idl_geo_sale %in% "dist"),
            sh_rent_dist = mean(idl_geo_rent %in% "dist")), by = year][order(year)])

setcolorder(D, c("DISTRITO", "year", "CCAA", "CPRO", "CMUN", "mun"))
setorder(D, DISTRITO, year)
stopifnot(!anyDuplicated(D, by = c("DISTRITO", "year")))

## Labels

LABELS <- c(
  rbd_p90      = "Eq. disposable income, P90 (2023 EUR)",
  rbd_gini     = "Eq. disposable income, Gini",
  rb_mean      = "Eq. gross income, mean (2023 EUR)",
  sh_inc_na    = "Share with missing income",
  sh_q1_nat    = "Share in national bottom quintile",
  sh_q5_nat    = "Share in national top quintile",
  age_mean     = "Mean age",
  sh_0_17      = "Share aged 0-17",
  sh_65plus    = "Share aged 65+",
  sh_female    = "Share female",
  hh_size_mean = "Mean household size",
  sh_hh_pens   = "Share in household with pension",
  sh_hh_rent   = "Share in household with rental income",
  sh_mover_in  = "Share moved in this year",
  sh_obs_prev  = "Share observed previous year",
  small        = "Fewer than MIN_OBS obs.",
  idl_n_sale_d = "Idealista sale stock (district)",
  idl_n_rent_d = "Idealista rent stock (district)",
  idl_n_sec_d  = "Idealista sections (district)",
  idl_redrawn  = "District code not in 2011 map",
  idl_sale_d   = "Sale price, district only (EUR/m2)",
  idl_rent_d   = "Rent, district only (EUR/m2/month)",
  idl_sale     = "Sale price, district or municipality (EUR/m2)",
  idl_rent     = "Rent, district or municipality (EUR/m2/month)",
  idl_sale_l1  = "Sale price, t-1 (EUR/m2)",
  idl_rent_l1  = "Rent, t-1 (EUR/m2/month)",
  idl_geo_sale = "Sale price level (dist/mun)",
  idl_geo_rent = "Rent level (dist/mun)",
  idl_ptr      = "Price-to-annual-rent ratio"
)

for (v in intersect(names(LABELS), names(D))) setattr(D[[v]], "label", LABELS[[v]])
DICT <- data.table(variable = names(LABELS), label = unname(LABELS))
fwrite(DICT, "~/work/UGR/data/district_level_dict.csv")
#write_csv_arrow(DICT, bkt$path("panel_hogares/district_level_dict.csv"))


tbl <- arrow_table(D); tbl$metadata$r <- NULL
write_parquet(tbl, bkt$path(OUT_PATH), compression = "zstd")
cat("Écrit :", OUT_PATH, "-", nrow(D), "lignes\n")
