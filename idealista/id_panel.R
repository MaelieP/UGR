# =============================================================================
# fn_idealista.R
# Fonctions pour joindre les prix Idealista (sortie de merge_idealita.R) au panel.
# Usage :
#   source("fn_idealista.R")
#   IDY <- load_idealista(bkt)
#   join_idealista(PANEL, IDY, dist_col = "DISTRITO")   # ajoute les colonnes idl_* par référence
#   check_idealista_match(PANEL)                       # part du poids matchée par année
# =============================================================================

library(data.table)

load_idealista <- function(bkt, path = "idealista/merged/idealista_geo_year.parquet") {
  as.data.table(arrow::read_parquet(bkt$path(path)))
}

# PANEL : data.table avec `year` et un code district à 7 caractères (CPRO+CMUN+DIST).
# Ajoute par référence :
#   idl_sale, idl_rent, idl_sale_l1, idl_rent_l1  district, sinon commune (€2023)
#   idl_sale_d, idl_rent_d                         district uniquement (sans repli)
#   idl_ptr                                        prix / loyer annuel, si vente et loyer au même niveau
#   idl_geo_sale, idl_geo_rent                     "dist", "mun" ou NA
#   idl_redrawn                                    commune-année avec un code district absent du découpage 2011
join_idealista <- function(PANEL, IDY, dist_col = "DISTRITO", force_mun_if_redrawn = TRUE) {
  stopifnot(is.data.table(PANEL), "year" %in% names(PANEL), dist_col %in% names(PANEL))
  
  PANEL[, idl_tmp_dist := as.character(get(dist_col))]
  PANEL[!grepl("^[0-9]{7}$", idl_tmp_dist), idl_tmp_dist := NA_character_]
  PANEL[, idl_tmp_mun := substr(idl_tmp_dist, 1, 5)]
  if (PANEL[, mean(is.na(idl_tmp_dist))] > 0.5)
    warning("Plus de la moitié des codes district ne sont pas à 7 chiffres : vérifier ", dist_col)
  
  D <- IDY[geo_level == "dist"]
  M <- IDY[geo_level == "mun"]
  
  # Redécoupage : la commune a, cette année, un code district inconnu d'Idealista (2011)
  codes11 <- unique(D$geo_code)
  bad <- unique(PANEL[!is.na(idl_tmp_dist) & !(idl_tmp_dist %in% codes11), .(idl_tmp_mun, year)])
  PANEL[, idl_redrawn := FALSE]
  PANEL[bad, on = .(idl_tmp_mun, year), idl_redrawn := TRUE]
  
  vars <- c("sale", "rent", "sale_l1", "rent_l1", "ptr")
  td <- paste0("idl_tmp_", vars, "_d"); tm <- paste0("idl_tmp_", vars, "_m")
  PANEL[, c(td, tm) := NA_real_]
  PANEL[D, on = .(idl_tmp_dist = geo_code, year), (td) := mget(paste0("i.", vars))]
  PANEL[M, on = .(idl_tmp_mun  = geo_code, year), (tm) := mget(paste0("i.", vars))]
  
  PANEL[, `:=`(idl_sale_d = idl_tmp_sale_d, idl_rent_d = idl_tmp_rent_d)]
  if (force_mun_if_redrawn) PANEL[idl_redrawn == TRUE, (td) := NA_real_]
  
  for (v in c("sale", "rent", "sale_l1", "rent_l1")) {
    d <- paste0("idl_tmp_", v, "_d"); m <- paste0("idl_tmp_", v, "_m")
    PANEL[, paste0("idl_", v) := fifelse(!is.na(get(d)), get(d), get(m))]
  }
  PANEL[, idl_geo_sale := fcase(!is.na(idl_tmp_sale_d), "dist", !is.na(idl_tmp_sale_m), "mun",
                                default = NA_character_)]
  PANEL[, idl_geo_rent := fcase(!is.na(idl_tmp_rent_d), "dist", !is.na(idl_tmp_rent_m), "mun",
                                default = NA_character_)]
  PANEL[, idl_ptr := fcase(idl_geo_sale == "dist" & idl_geo_rent == "dist", idl_tmp_ptr_d,
                           idl_geo_sale == "mun"  & idl_geo_rent == "mun",  idl_tmp_ptr_m,
                           default = NA_real_)]
  
  PANEL[, (grep("^idl_tmp_", names(PANEL), value = TRUE)) := NULL]
  invisible(PANEL)
}

# Part du poids (w) par année : district, commune, non matché
check_idealista_match <- function(PANEL, w_col = "w") {
  sh <- function(cond, w) sum(w[cond], na.rm = TRUE) / sum(w, na.rm = TRUE)
  PANEL[, {
    w <- get(w_col)
    .(sh_redrawn   = sh(idl_redrawn, w),
      sh_sale_dist = sh(idl_geo_sale %in% "dist", w),
      sh_sale_mun  = sh(idl_geo_sale %in% "mun", w),
      sh_sale_na   = sh(is.na(idl_geo_sale), w),
      sh_rent_dist = sh(idl_geo_rent %in% "dist", w),
      sh_rent_mun  = sh(idl_geo_rent %in% "mun", w),
      sh_rent_na   = sh(is.na(idl_geo_rent), w))
  }, by = year][order(year)]
}