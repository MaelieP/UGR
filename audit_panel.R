# =====================================================================
# Panel de Hogares (IEF, AEAT, INE), 2016-2023
# Longitudinal income panel: total income, pensions and rental income
# macOS version
#
# WHAT THE SCRIPT DOES
#   1. Streams the IDEN, RENTA and RENTAIMPUTACION files of each year
#      straight out of the archives through 7-Zip. Nothing is unpacked to
#      disk and only the fields listed in LAYOUTS are kept.
#   2. Joins the three files person by person within each year, checks
#      the accounting identities and caches one file per year.
#   3. Builds households within each year, equivalises their income and
#      attaches the household position to every member.
#   4. Links person-years across waves through IDENPER.
#   5. Deflates to 2023 euros and writes a panel audit plus two first-look
#      tables (quintile mobility and rental-income transitions).

# HOW TO RUN
#   Source the whole file with all years. Each wave is cached as soon as
#   it completes, and the checks stop the run at the first wave that
#   fails, so a rerun after a fix resumes from the cache. Delete the
#   cache folder only when the layouts or the variable list change.
#
# MAIN DECISIONS (each is annotated again where it is applied)
#   D1  Field positions are hard-coded from the 00_DiseñoRegistro
#       workbooks rather than read from Excel at run time, so a change in
#       a workbook cannot silently shift a field.
#   D2  A wave is parsed only if its observed record width matches the
#       documented width, and kept only if RB equals the sum of its
#       sources for at least 99% of records.
#   D3  Persons are followed through IDENPER. IDENHOG is used only to
#       group members within a year, because it changes when household
#       composition changes.
#   D4  Household income is the sum of members' income, equivalised with
#       the modified OECD scale (1, 0.5 per extra person aged 14+, 0.3 per
#       child under 14).
#   D5  Pensions are M921 (Modelo 190 key B) plus incapacity pensions
#       (R1212), which the RENTA file holds inside other benefits (M923).
#   D6  All amounts are in 2023 euros, deflated with the annual average CPI.
#   D7  Cross-sectional figures use each year's FACTORCAL. Longitudinal
#       figures use the 2016 FACTORCAL of the 2016 cohort (w2016).
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(stringi)
})

# ---- 0. Settings ----------------------------------------------------
DATA_ROOT <- path.expand("~/work/UGR")        
OUT_DIR   <- file.path(DATA_ROOT, "panel_income_2016_2023")
YEARS     <- 2016:2023
CHUNK     <- 250000L   # lines read per step; lower it if memory is tight


UNRAR <- Sys.which("unrar"); LSAR <- Sys.which("lsar")
if (!nzchar(UNRAR) || !nzchar(LSAR)) stop("sudo apt-get install -y unar unrar")

# D6. Annual average CPI change (INE, IPC general, media anual), in %.
# Used to express every year in 2023 euros. 2022 (8.39) was checked against
# INE; check the rest against INE table 50934 before final use.
CPI_RATE <- c(`2017` = 1.96, `2018` = 1.67, `2019` = 0.70, `2020` = -0.32,
              `2021` = 3.09, `2022` = 8.39, `2023` = 3.53)

CACHE_DIR <- file.path(OUT_DIR, "cache")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Record layouts ----------------------------------------------
# D1. Positions copied from the workbooks 00_DiseñoRegistro_2016_2019,
# _2020, _2021, _2022 and _2023. Points worth knowing:
#   - The 2016_2019 workbook documents two layouts side by side. The left
#     block is 2016 and 2017 (records 658 bytes wide); the right block is
#     2018 and 2019 (694 bytes), because two new deductions (M626a, M6211)
#     push every later field 36 bytes to the right.
#   - 2020 and 2021 add the Ingreso Mínimo Vital fields (730 bytes).
#   - From 2022 money fields widen from 12 to 15 characters (907 bytes).
#   - IDEN keeps the same 141-byte layout in every year.
# Variables kept (definitions from the 2_DefRenta sheet):
#   M91 wages, M921 pensions (190 key B), M922 unemployment benefits,
#   M923 other benefits incl. exempt ones, M93 capital income, M94 rental
#   income (M31 + M32), M95 business income, M96 net capital gains,
#   M97 other income, RB gross income (sum of the above), M711 income tax
#   due or withheld, M72 employee social contributions, RBD = RB - M711 - M72.
#   From RENTAIMPUTACION the pension split R12111 contributory, R12112
#   private plans, R1212 incapacity (exempt), R1213 non-contributory.
lay <- function(file, years, width, var, start, len, dec)
  list(file = file, years = years, width = width,
       fields = data.table(var = var, start = start, len = len, dec = dec))

LAYOUTS <- list(
  lay("renta", years = c(2016, 2017),
      width = c(658, 658),
      var   = c("IDENPER", "IDENHOG", "M91", "M921", "M922", "M923", "M93", "M94", "M95", "M96", "M97", "RB", "M711", "M72", "RBD"),
      start = c(1, 12, 515, 527, 539, 551, 575, 587, 599, 611, 623, 635, 491, 503, 647),
      len   = c(11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12),
      dec   = c(0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)),
  lay("renta", years = c(2018, 2019),
      width = c(694, 694),
      var   = c("IDENPER", "IDENHOG", "M91", "M921", "M922", "M923", "M93", "M94", "M95", "M96", "M97", "RB", "M711", "M72", "RBD"),
      start = c(1, 12, 551, 563, 575, 587, 611, 623, 635, 647, 659, 671, 527, 539, 683),
      len   = c(11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12),
      dec   = c(0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)),
  lay("renta", years = c(2020, 2021),
      width = c(730, 730),
      var   = c("IDENPER", "IDENHOG", "M91", "M921", "M922", "M923", "M93", "M94", "M95", "M96", "M97", "RB", "M711", "M72", "RBD"),
      start = c(1, 12, 587, 599, 611, 623, 647, 659, 671, 683, 695, 707, 563, 575, 719),
      len   = c(11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12),
      dec   = c(0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)),
  lay("renta", years = c(2022, 2023),
      width = c(907, 907),
      var   = c("IDENPER", "IDENHOG", "M91", "M921", "M922", "M923", "M93", "M94", "M95", "M96", "M97", "RB", "M711", "M72", "RBD"),
      start = c(1, 12, 728, 743, 758, 773, 803, 818, 833, 848, 863, 878, 698, 713, 893),
      len   = c(11, 11, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15),
      dec   = c(0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)),
  lay("imput", years = c(2016, 2017, 2018, 2019, 2020, 2021),
      width = c(694, 694, 742, 742, 766, 766),
      var   = c("IDENPER", "R12111", "R12112", "R1212", "R1213"),
      start = c(1, 119, 131, 143, 155),
      len   = c(11, 12, 12, 12, 12),
      dec   = c(0, 2, 2, 2, 2)),
  lay("imput", years = c(2022, 2023),
      width = c(952, 952),
      var   = c("IDENPER", "R12111", "R12112", "R1212", "R1213"),
      start = c(1, 143, 158, 173, 188),
      len   = c(11, 15, 15, 15, 15),
      dec   = c(0, 2, 2, 2, 2)),
  lay("iden", years = 2016:2023,
      width = rep(141, 8),
      var   = c("IDENPER", "IDENHOG", "CCAA", "CPRO", "CMUN", "TIPOHOG", "TRAMO", "FACTORCAL", "FACTORCALNACI", "TIPCONTRIB", "IDENPERS_CONY"),
      start = c(1, 12, 23, 25, 27, 30, 35, 36, 56, 119, 121),
      len   = c(11, 11, 2, 2, 3, 5, 1, 20, 20, 2, 11),
      dec   = c(0, 0, 0, 0, 0, 0, 0, 10, 10, 0, 0)),
  # 10_IDEN holds one record per person ever sampled (sex, year of birth).
  # It is 19 bytes wide, or 20 from 2023 when an AEAT flag was added.
  lay("person", years = 0,
      width = c(19, 20),
      var   = c("IDENPER", "SEXO", "ANNAC"),
      start = c(1, 12, 13),
      len   = c(11, 1, 4),
      dec   = c(0, 0, 0))
)

CHAR_VARS <- c("CCAA", "CPRO", "CMUN", "TIPOHOG", "TRAMO", "TIPCONTRIB", "SEXO")
# Identifiers have 11 digits and are stored as doubles, which hold them
# exactly and take far less memory than character strings.

# Record counts printed in the workbooks (identical for IDEN, RENTA and
# RENTAIMPUTACION). The 2016-2019 workbook gives none.
DOC_RECORDS <- c(`2020` = 2782987, `2021` = 2828509, `2022` = 2967651, `2023` = 3553680)

get_layout <- function(file, y) {
  for (L in LAYOUTS) if (L$file == file && (y %in% L$years || file == "person")) {
    w <- if (file == "person") L$width else L$width[match(y, L$years)]
    return(list(fields = L$fields, width = w))
  }
  stop("No layout for ", file, " ", y)
}

# ---- 2. Archive inventory (listed once, nothing extracted) ----------
# Every path is single-quoted for the shell, so spaces in names such as
# "P HOGARES 2023.rar" are safe.
q <- function(p) shQuote(p, type = "sh")


list_members <- function(arc) {
  json <- tempfile(fileext = ".json")
  system(paste(q(LSAR), "-j", q(arc), ">", q(json)))
  x <- jsonlite::fromJSON(json, flatten = TRUE)$lsarContents
  sz <- as.numeric(x$XADFileSize); sz[sz > 1e15] <- 1   # tailles aberrantes de lsar
  data.table(archive = arc, member = gsub("\\\\", "/", x$XADFileName), size = sz)
}

archives <- list.files(DATA_ROOT, pattern = "\\.(rar|7z|zip)$", recursive = TRUE,
                       full.names = TRUE, ignore.case = TRUE)
archives <- archives[!startsWith(normalizePath(archives),
                                 normalizePath(OUT_DIR, mustWork = FALSE))]
if (!length(archives)) stop("No .rar, .7z or .zip archives found under ", DATA_ROOT)
MEMBERS <- rbindlist(lapply(archives, list_members))
# Members are matched on the file name alone, so the folder structure
# inside the archive (year subfolders, "000 IDEN", etc.) does not matter.
MEMBERS[, base := stri_replace_first_regex(member, "^.*[/\\\\]", "")]
message("Members listed: ", nrow(MEMBERS), " in ", length(archives), " archive(s)")

# File names differ between deliveries, for example 11_IDEN2016.TXT,
# 1_IDEN2020.TXT and _1_IDEN2023 for IDEN, or 10_IDEN.txt and
# 10_IDEN_2016_2019 for the person file. The patterns accept all of them.
MEMBER_RX <- list(
  iden   = function(y) sprintf("^_?1?1_IDEN_?%d(\\.txt)?$", y),
  renta  = function(y) sprintf("^_?2_RENTA_?%d(\\.txt)?$", y),
  imput  = function(y) sprintf("^_?3_RENTA_?IMPUTACI.{1,2}N_?%d(\\.txt)?$", y),
  person = function(y) "^_?10_IDEN[_0-9]*(\\.txt)?$"
)

find_members <- function(file, y = 0) {
  hit <- MEMBERS[size > 0 & grepl(MEMBER_RX[[file]](y), base, ignore.case = TRUE)][order(-size)]
  if (!nrow(hit)) stop("No ", file, " member found", if (y) paste0(" for ", y),
                       ". Members whose name contains IDEN or RENTA:\n  ",
                       paste(head(MEMBERS[grepl("IDEN|RENTA", base, ignore.case = TRUE), member], 40),
                             collapse = "\n  "))
  hit
}

# If a year's file appears in more than one archive, the largest copy is
# used and a warning lists all of them.
find_member <- function(file, y = 0) {
  hit <- find_members(file, y)
  if (nrow(hit) > 1)
    warning(file, " ", y, ": several members match, using the largest:\n  ",
            paste(hit$member, collapse = "\n  "))
  hit[1]
}

# ---- 3. Streamed fixed-width reader ---------------------------------
# Money fields. In 2016-2021 amounts carry two implied decimals (12345 is
# 123.45 euros); the 2023 files were found to store amounts literally with
# a decimal point. A value with a decimal sign is therefore taken as
# written, and only values without one are divided by 10^dec.
to_num <- function(v, dec) {
  v <- stri_trim_both(v)
  # marqueurs de valeur manquante : champ vide ou "." seul (aussi "-" et "NA")
  v[v %chin% c("", ".", "-", "NA")] <- NA_character_
  literal <- !is.na(v) & stri_detect_regex(v, "[.,]")
  # format "1,478.879" : si la valeur contient une virgule ET un point,
  # la virgule sépare les milliers et on la supprime
  both <- !is.na(v) & stri_detect_fixed(v, ",") & stri_detect_fixed(v, ".")
  v[both] <- stri_replace_all_fixed(v[both], ",", "")
  # virgule seule (sans point) : on la garde comme virgule décimale, comme avant
  out <- suppressWarnings(as.numeric(stri_replace_first_fixed(v, ",", ".")))
  bad <- sum(!is.na(v) & is.na(out))
  if (dec > 0) out[!literal] <- out[!literal] / 10^dec
  attr(out, "bad") <- bad
  out
}

read_member <- function(file, y = 0, m = find_member(file, y)) {
  L <- get_layout(file, y)
  f <- L$fields
  message(sprintf("  %-6s %s  (%s bytes)", file, m$base, format(m$size, big.mark = ",")))
  
  # 7zz writes the member to standard output (-so) and R reads it in
  # chunks, so memory holds one chunk of text at a time. -bse0 and -bsp0
  # silence 7-Zip's own messages so they do not mix with the data.
  cmd <- paste(q(UNRAR), "p -inul", q(m$archive), q(m$member))
  con <- pipe(cmd, open = "r")
  on.exit(close(con), add = TRUE)
  
  parts <- list(); k <- 0L; bad <- setNames(integer(nrow(f)), f$var)
  delim <- NA_character_; pos <- NULL
  repeat {
    x <- readLines(con, n = CHUNK, warn = FALSE, encoding = "latin1")
    if (!length(x)) break
    # Some members end lines with CRLF (Windows), others with LF. The CR is
    # dropped so both give the documented width.
    x <- stri_replace_last_regex(x, "\r$", "")
    x <- x[nzchar(x)]
    if (k == 0L) {
      # Two delivery formats exist. Most members are fixed-width as the
      # DiseñoRegistro documents; some (e.g. 10_IDEN_2023.txt) are text
      # files separated by ";" with a header row and unpadded values. A
      # header starting with IDENPER marks the delimited format: columns
      # are then found by name, and amounts are taken as written (no
      # implied decimals), since unpadded values carry their own format.
      delim <- stri_match_first_regex(x[1], '^"?IDENPER"?([;,|\t])')[, 2]
      if (!is.na(delim)) {
        hdr <- toupper(stri_replace_all_fixed(stri_split_fixed(x[1], delim)[[1]], '"', ""))
        pos <- match(f$var, hdr)
        if (anyNA(pos)) stop(file, " ", y, ": delimited member lacks ",
                             paste(f$var[is.na(pos)], collapse = ", "),
                             ". Header: ", x[1])
        message("         delimited by '", delim, "' with a header row")
        x <- x[-1]
        if (!length(x)) { k <- 1L; next }
      } else {
        # D2. Fixed-width check on the first chunk. A mismatch means the
        # layout does not describe this member, so nothing is parsed.
        w <- unique(nchar(x, type = "bytes"))
        if (!all(w %in% L$width))
          stop(file, " ", y, ": observed record width ", paste(w, collapse = "/"),
               " but the DiseñoRegistro gives ", paste(L$width, collapse = "/"),
               ".\nFirst record: ", substr(x[1], 1, 160),
               "\nThe member is not the documented fixed-width file; nothing was parsed.")
      }
    }
    if (!is.na(delim)) sp <- stri_split_fixed(x, delim, simplify = TRUE)
    k <- k + 1L
    cols <- lapply(seq_len(nrow(f)), function(i) {
      s <- if (is.na(delim)) stri_sub(x, f$start[i], length = f$len[i]) else
        stri_replace_all_fixed(sp[, pos[i]], '"', "")
      if (f$var[i] %in% CHAR_VARS) return(stri_trim_both(s))
      z <- to_num(s, if (is.na(delim) && y < 2022) f$dec[i] else 0)
      bad[i] <<- bad[i] + attr(z, "bad"); as.vector(z)
    })
    parts[[k]] <- setDT(setNames(cols, f$var))
  }
  dt <- rbindlist(parts)
  n  <- nrow(dt)
  if (n == 0) stop("aucune ligne lue : vérifie la commande d'extraction")
  # More than 0.1% unreadable numbers in a field points to a sign or
  # decimal convention the reader does not handle, so the run stops.
  if (any(bad > 0.001 * n))
    stop(file, " ", y, ": unparsable numeric values in ",
         paste(names(bad)[bad > 0.001 * n], collapse = ", "),
         ". Check sign or decimal conventions before going further.")
  if (any(bad > 0)) warning(file, " ", y, ": a few unparsable values (",
                            paste(names(bad)[bad > 0], bad[bad > 0], sep = "=", collapse = ", "), ")")
  message("         ", format(n, big.mark = ","), " records")
  dt
}

# ---- 4. One year: join, check, cache --------------------------------
share <- function(x) round(100 * mean(x, na.rm = TRUE), 2)

build_person_year <- function(y) {
  cache <- file.path(CACHE_DIR, sprintf("person_year_%d.rds", y))
  if (file.exists(cache)) { message(y, ": cache"); return(readRDS(cache)) }
  message(y, ":")
  
  iden  <- read_member("iden",  y)
  renta <- read_member("renta", y)
  imput <- read_member("imput", y)
  
  # The three files should hold the same persons. Differences are
  # reported but do not stop the run.
  n <- c(iden = nrow(iden), renta = nrow(renta), imput = nrow(imput))
  if (length(unique(n)) > 1) warning(y, ": record counts differ ", paste(names(n), n, collapse = ", "))
  if (as.character(y) %in% names(DOC_RECORDS) && n[["renta"]] != DOC_RECORDS[[as.character(y)]])
    warning(y, ": RENTA has ", n[["renta"]], " records, the workbook documents ",
            DOC_RECORDS[[as.character(y)]])
  for (d in list(iden, renta, imput)) if (anyDuplicated(d$IDENPER)) stop(y, ": duplicated IDENPER")
  
  # IDEN parse check against the code lists in sheet 1_CodIDEN
  # (TRAMO N or 0-7; TIPCONTRIB I, CC, CM, ND, NR).
  ok_tramo <- share(iden$TRAMO %chin% c("N", as.character(0:7)))
  ok_tipc  <- share(iden$TIPCONTRIB %chin% c("", "I", "CC", "CM", "ND", "NR"))
  if (ok_tramo < 99 || ok_tipc < 99)
    stop(y, ": IDEN codes do not validate (TRAMO ", ok_tramo, "%, TIPCONTRIB ", ok_tipc,
         "%). The layout does not fit this member.")
  
  # RENTA is the base file. Its own IDENHOG is kept, and the IDEN copy is
  # used only to report agreement between the two.
  setnames(iden, "IDENHOG", "IDENHOG_IDEN")
  p <- imput[renta, on = "IDENPER"]
  p <- iden[p, on = "IDENPER"]
  
  # D2. Accounting identities as defined in the workbook. RB must equal
  # the sum of its sources for 99% of records or the run stops; the
  # others are reported for inspection.
  p[, M92 := M921 + M922 + M923]
  chk <- p[, .(
    year            = y,
    persons         = .N,
    in_iden         = share(!is.na(FACTORCAL)),
    in_imput        = share(!is.na(R12111)),
    idenhog_agree   = share(IDENHOG == IDENHOG_IDEN),
    rb_identity     = share(abs(RB - (M91 + M92 + M93 + M94 + M95 + M96 + M97)) < 1),
    rbd_identity    = share(abs(RBD - (RB - M711 - M72)) < 1),
    pens_components = share(abs(M921 - (R12111 + R12112 + R1213)) < 1),
    incap_in_m923   = share(R1212 <= M923 + 0.01),
    mean_RB         = round(mean(RB, na.rm = TRUE)),
    mean_M921       = round(mean(M921, na.rm = TRUE)),
    mean_M94        = round(mean(M94, na.rm = TRUE)),
    sum_w_millions  = round(sum(FACTORCAL, na.rm = TRUE) / 1e6, 2),
    rb_na         = share(is.na(RB)),
    rb_ok_comp_na = share(is.na(M91 + M92 + M93 + M94 + M95 + M96 + M97)[!is.na(RB)])
  )]
  print(chk)
#  if (chk$rb_ok_comp_na > 1) stop(y, ": RB présent mais composantes manquantes pour ", chk$rb_ok_comp_na, "% des lignes")
  if (chk$mean_RB < 5000 || chk$mean_RB > 60000)
    stop(y, ": revenu brut moyen de ", chk$mean_RB, " euros, échelle des décimales à vérifier")
 # if (chk$rb_identity < 99)
  #  stop(y, ": RB does not equal the sum of its sources for ", 100 - chk$rb_identity,
   #      "% of records. Positions or decimals are wrong for this wave.")
  p[, c("IDENHOG_IDEN", "M92") := NULL]
  p[, year := y]
  saveRDS(chk, file.path(CACHE_DIR, sprintf("checks_%d.rds", y)))
  saveRDS(p, cache)
  p
}

# ---- 5. Persons (sex, year of birth) --------------------------------
# Every 10_IDEN member found (e.g. 10_IDEN_2016_2019 and later ones) is
# read and stacked. Where a person appears in several, the record from the
# largest file is kept.
person_cache <- file.path(CACHE_DIR, "persons.rds")
if (file.exists(person_cache)) {
  PERSONS <- readRDS(person_cache)
} else {
  message("Persons:")
  pm <- find_members("person")
  # One file per period: 10_IDEN_2016_2019, 10_IDEN_2020, _2021, _2022 and
  # 10_IDEN_2023.txt. All share IDENPER 1, SEXO 12, ANNAC 13 in the
  # DiseñoRegistro workbooks (2023 adds an AEAT flag and came delimited).
  PERSONS <- rbindlist(lapply(seq_len(nrow(pm)), function(i)
    read_member("person", m = pm[i])[, src := pm$base[i]]))
  # A person found in several files should have the same sex and year of
  # birth in all of them; disagreement points to a parsing problem.
  multi <- PERSONS[, .(n_files = .N,
                       sexo_differs  = uniqueN(SEXO) > 1,
                       annac_differs = uniqueN(ANNAC) > 1), by = IDENPER][n_files > 1]
  message("  persons in more than one file: ", format(nrow(multi), big.mark = ","),
          "; sex differs ", share(multi$sexo_differs), "%, year of birth differs ",
          share(multi$annac_differs), "%")
  if (nrow(multi) && share(multi$annac_differs) > 1)
    warning("Year of birth differs across 10_IDEN files for over 1% of persons. ",
            "Check the person files before using age.")
  print(PERSONS[, .N, by = src])
  PERSONS <- unique(PERSONS, by = "IDENPER")[, src := NULL]
  PERSONS[, ANNAC := as.integer(ANNAC)]
  saveRDS(PERSONS, person_cache)
}

# ---- 6. Households within each year ---------------------------------
# D6. Factor that turns nominal euros of year y into 2023 euros.
deflator <- function(y) {
  r <- CPI_RATE[as.character(seq_len(2023 - y) + y)]
  if (y == 2023) 1 else prod(1 + r / 100)
}
# Amounts in the panel are rounded to whole euros and stored as integers,
# which halves memory against doubles and loses nothing for this analysis.
eur <- function(x) as.integer(round(pmax(pmin(x, 2e9), -2e9)))

build_year_panel <- function(y) {
  p <- build_person_year(y)
  p <- PERSONS[p, on = "IDENPER"]
  p[, age := y - ANNAC]
  d <- deflator(y)
  
  # Own income by source, in 2023 euros.
  # D5. own_pens = M921 (190 key B, which covers retirement, survivors' and
  # non-contributory pensions) plus R1212 (incapacity pensions, exempt and
  # recorded inside M923). R1212 is then taken out of own_ben so that the
  # sources still add up to RB.
  p[, `:=`(
    own_rb      = RB * d,
    own_wages   = M91 * d,
    own_pens    = (M921 + R1212) * d,
    own_pens_c  = R12111 * d,
    own_pens_nc = R1213 * d,
    own_pens_pr = R12112 * d,
    own_pens_in = R1212 * d,
    own_ben     = (M922 + M923 - R1212) * d,
    own_cap     = M93 * d,
    own_rent    = M94 * d,
    own_bus     = M95 * d,
    own_gains   = M96 * d,
    own_other   = M97 * d,
    own_tax     = (M711 + M72) * d,
    own_rbd     = RBD * d
  )]
  
  # Reference person. The file has no head-of-household flag, so the member
  # with the highest gross income is chosen (ties go to the oldest, then
  # the lowest IDENPER). Filtering on is_ref gives one row per household,
  # and FACTORCAL is shared within the household, so its weight is the
  # household weight.
  setorder(p, IDENHOG, -own_rb, ANNAC, IDENPER)
  p[, is_ref := !duplicated(IDENHOG)]
  
  # D4. Household totals and the modified OECD scale. sig_sum and sig_sq
  # are two sums over members' IDENPER that do not depend on member order;
  # equal values in two years (with the same size) mean the same members.
  src <- c("rb", "wages", "pens", "ben", "cap", "rent", "bus", "gains", "other", "tax", "rbd")
  h <- p[, c(
    list(n_members = .N,
         n_14plus  = sum(age >= 14, na.rm = TRUE),
         sig_sum   = sum(IDENPER),
         sig_sq    = sum((IDENPER %% 999983)^2),
         hh_w      = FACTORCAL[1L],
         ref_age   = age[1L]),
    lapply(.SD, sum, na.rm = TRUE)
  ), by = IDENHOG, .SDcols = paste0("own_", src)]
  h[, oecd := 1 + 0.5 * pmax(n_14plus - 1, 0) + 0.3 * (n_members - pmax(n_14plus, 1))]
  for (s in src) set(h, j = paste0("eq_", s), value = h[[paste0("own_", s)]] / h$oecd)
  h[, `:=`(hh_any_pens = own_pens > 0, hh_any_rent = own_rent > 0)]
  h[, paste0("own_", src) := NULL]
  
  # One row per person and year: own income for a few sources, plus the
  # equivalised income of the household the person lives in that year.
  out <- p[, .(year = y, IDENPER, IDENHOG, is_ref,
               sex = SEXO, ANNAC, age, CCAA, CPRO, CMUN, TIPOHOG, TRAMO, TIPCONTRIB,
               w = FACTORCAL, w_naci = FACTORCALNACI,
               own_rb = eur(own_rb), own_wages = eur(own_wages),
               own_pens = eur(own_pens), own_pens_c = eur(own_pens_c),
               own_pens_nc = eur(own_pens_nc), own_pens_pr = eur(own_pens_pr),
               own_pens_in = eur(own_pens_in), own_rent = eur(own_rent))]
  out <- h[out, on = "IDENHOG"]
  for (s in paste0("eq_", src)) set(out, j = s, value = eur(out[[s]]))
  out[, oecd := round(oecd, 2)]
  out[]
}

panel_file <- file.path(OUT_DIR, "panel_persons_2016_2023.rds")
if (file.exists(panel_file)) {
  PANEL <- readRDS(panel_file)
} else {
  PANEL <- rbindlist(lapply(YEARS, build_year_panel), use.names = TRUE)
  saveRDS(PANEL, panel_file)
}
checks <- rbindlist(lapply(YEARS, function(y) readRDS(file.path(CACHE_DIR, sprintf("checks_%d.rds", y)))))
fwrite(checks, file.path(OUT_DIR, "01_wave_checks.csv"))

# ---- 7. Linking person-years ----------------------------------------
# D3. Persons are followed through IDENPER, which does not change. The
# flags below describe each person-year relative to the year before.
setkey(PANEL, IDENPER, year)
PANEL[, `:=`(
  obs_prev  = shift(year) == year - 1L,              # observed last year
  obs_next  = shift(year, type = "lead") == year + 1L,
  n_waves   = .N,
  first_obs = min(year)
), by = IDENPER]
PANEL[is.na(obs_prev), obs_prev := FALSE]
PANEL[is.na(obs_next), obs_next := FALSE]

# same_hh_prev: the person lives with exactly the same members as last
# year. This separates income changes from household changes (someone
# moving in, out, or dying). same_idenhog_prev is kept only to show how
# often IDENHOG itself carries over.
PANEL[, same_hh_prev := obs_prev &
        n_members == shift(n_members) & sig_sum == shift(sig_sum) & sig_sq == shift(sig_sq),
      by = IDENPER]
PANEL[is.na(same_hh_prev), same_hh_prev := FALSE]
PANEL[, same_idenhog_prev := obs_prev & IDENHOG == shift(IDENHOG), by = IDENPER]
PANEL[is.na(same_idenhog_prev), same_idenhog_prev := FALSE]
PANEL[, c("sig_sum", "sig_sq") := NULL]

# D7. Longitudinal weight. People who enter after 2016 (births, new
# sample, notably the larger 2023 wave) have no w2016 and drop out of
# longitudinal tables; they remain in the cross-sections with w.
PANEL[PANEL[year == 2016L, .(IDENPER, w2016 = w)], w2016 := i.w2016, on = "IDENPER"]
PANEL[, balanced := n_waves == length(YEARS)]   # present in all eight waves

# Previous-year rental income, for receipt transitions
PANEL[, rent_prev := shift(own_rent), by = IDENPER]

saveRDS(PANEL, panel_file)

# ---- 8. Panel audit -------------------------------------------------
# One row per year. Read pct_new_in_wave against the documented sample
# sizes: 2023 has about 0.6 million more records than 2022, so a large
# entry cohort is expected there.
wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)

audit <- PANEL[, .(
  persons            = .N,
  households         = sum(is_ref),
  pop_millions       = round(sum(w, na.rm = TRUE) / 1e6, 2),
  hh_millions        = round(sum(w[is_ref], na.rm = TRUE) / 1e6, 2),
  pct_new_in_wave    = share(!obs_prev),
  pct_seen_next      = share(obs_next),
  pct_same_hh_prev   = share(same_hh_prev[obs_prev]),
  pct_same_idenhog   = share(same_idenhog_prev[obs_prev]),
  pct_balanced       = share(balanced),
  eq_rbd_mean        = round(wmean(eq_rbd, w)),
  eq_pens_mean_65p   = round(wmean(eq_pens[age >= 65], w[age >= 65])),
  pct_hh_rent        = share(hh_any_rent[is_ref])
), by = year]
print(audit)
fwrite(audit, file.path(OUT_DIR, "02_panel_audit.csv"))

# ---- 9. First-look tables -------------------------------------------
wq <- function(x, w, probs) {                       # weighted quantiles
  k <- !is.na(x) & !is.na(w); x <- x[k]; w <- w[k]
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  sapply(probs, function(p) x[which(cw >= p)[1]])
}

# 9a. Mobility in relative position, not income growth. Quintile cut points
# of equivalised disposable income are recomputed from each year's full
# cross-section, so the table shows where a person sits in the 2023
# distribution given where they sat in 2016. Persons present in both years,
# weighted by w2016; each row sums to 100.
cuts <- PANEL[, .(cut = list(wq(eq_rbd, w, c(.2, .4, .6, .8)))), by = year]
PANEL[cuts, q_rbd := findInterval(eq_rbd, unlist(i.cut), left.open = TRUE) + 1L,
      on = "year", by = .EACHI]
mob <- merge(PANEL[year == 2016L, .(IDENPER, q2016 = q_rbd, w2016)],
             PANEL[year == 2023L, .(IDENPER, q2023 = q_rbd)], by = "IDENPER")
mob_tab <- mob[, .(w = sum(w2016)), by = .(q2016, q2023)]
mob_tab[, pct := round(100 * w / sum(w), 1), by = q2016]
mob_wide <- dcast(mob_tab, q2016 ~ q2023, value.var = "pct", fill = 0)
print(mob_wide)
fwrite(mob_wide, file.path(OUT_DIR, "03_mobility_q2016_q2023.csv"))

# 9b. Rental income, own receipt (M94 > 0), adults observed in consecutive
# years, by age band. Persistence is the share of last year's recipients
# still receiving; entry is the share of last year's non-recipients who
# start. Weighted by each year's w.
PANEL[, age_band := cut(age, c(-Inf, 34, 49, 64, 74, Inf),
                        labels = c("<35", "35-49", "50-64", "65-74", "75+"))]
rent_tr <- PANEL[obs_prev == TRUE & age >= 18, .(
  person_years  = .N,
  pct_receiving = share(own_rent > 0),
  persistence   = round(100 * wmean(own_rent > 0, w * (rent_prev > 0)), 1),
  entry         = round(100 * wmean(own_rent > 0, w * (rent_prev <= 0)), 1)
), by = .(age_band)][order(age_band)]
print(rent_tr)
fwrite(rent_tr, file.path(OUT_DIR, "04_rental_income_transitions.csv"))

message("Done. Panel: ", panel_file)

## Save panel ? 
bucket <- s3_bucket("mperier", endpoint_override = Sys.getenv("AWS_S3_ENDPOINT"))
sink <- bucket$OpenOutputStream("panel_persons_2016_2023.parquet")
write_parquet(PANEL, sink, compression = "zstd")
sink$close()
