## ======================================================================
## Regression-based Shapley for Govt Spending as % of GDP (AU, ABS)
## Decompose changes in spend/GDP into age-group contributions
## Last update: 09/09/2025 
## Incorporate broader government spending with GFCE
## ======================================================================

# -------------------- Packages --------------------
suppressPackageStartupMessages({
  library(data.table)
  library(readabs)
  library(lubridate)
  library(ggplot2)
  library(fixest)
  library(theme61)
})

rm(list = ls()); invisible(gc())

# -------------------- User prefs --------------------
freq            <- "FY"                 # "FY" or "CY"
# measure options now include GFCE_plus_transfers:
#   "GFCE" | "GFCE_plus_GFCF" | "GFCE_plus_transfers" - plus transfer excludes GFCF
measure         <- "GFCE_plus_transfers"               
# when measure == "GFCE_plus_transfers", choose transfers basket:
#   "cash" | "cash_plus_subsidies" | "cash_plus_subsidies_plus_currgrants"
transfers_set   <- "cash"               
share_basis     <- "nominal"            # "nominal" (standard) or "real"
conditional_sv  <- TRUE                 # TRUE = conditional (full Shapley); FALSE = β·Δs
abs_check_local <- TRUE                 # use cached ABS files (fast after first run)
start_year      <- 1980                 # sample start
final_year_opt  <- NA                   # optional end; NA to use all available
baseline_year   <- 2002                 # NA -> first available in sample
features <- c("0_14","15_34","35_54","55_64","65p","tot","rp_g","dln_pop")

# -------------------- Helpers --------------------
fy_end_year <- function(date) { d <- as.Date(date); year(d %m+% months(6)) }

annualise <- function(dt, out_name, mode = c("mean","sum"), freq = c("FY","CY")) {
  mode <- match.arg(mode); freq <- match.arg(freq)
  dt <- as.data.table(copy(dt))                    # <- ensure data.table
  if (!"date" %in% names(dt)) stop("annualise(): 'date' column missing")
  if (!inherits(dt$date, "Date")) dt[, date := as.Date(date)]
  tmp <- if (freq == "FY") {
    if (mode == "mean") dt[, .(val = mean(value, na.rm = TRUE)), by = .(year = fy_end_year(date))]
    else                dt[, .(val = sum(value,  na.rm = TRUE)), by = .(year = fy_end_year(date))]
  } else {
    if (mode == "mean") dt[, .(val = mean(value, na.rm = TRUE)), by = .(year = year(date))]
    else                dt[, .(val = sum(value,  na.rm = TRUE)), by = .(year = year(date))]
  }
  setnames(tmp, "val", out_name)
  setorder(tmp, year)
  tmp
}

### Key thing is to go through and understand if we are picking up the right categories in below - GFCE from the expenditure accounts, then subsides and social benefits received

# ---------- Quick presence check for ABS 5206.0 series names ----------
abs_presence_check <- function(dataset,
                               patterns,
                               cols = c("series","table_title","unit","data_type","series_type"),
                               exact = FALSE) {
  DT <- data.table::as.data.table(dataset)

  # Make sure the text cols exist and are character
  cols <- intersect(cols, names(DT))
  if (!length(cols)) stop("None of the candidate text columns are present in the dataset.")
  for (cc in cols) if (!is.character(DT[[cc]])) data.table::set(DT, j = cc, value = as.character(DT[[cc]]))

  # Some ABS pulls lack series_type; keep code robust
  if (!"series_type" %in% names(DT)) data.table::set(DT, j = "series_type", value = "")

  # Build a combined text field across metadata for regex searching
  DT[, txt_all := do.call(paste, c(.SD, list(sep = " | "))), .SDcols = cols]

  # Ensure patterns is a named list
  if (!is.list(patterns)) patterns <- as.list(patterns)
  if (is.null(names(patterns)) || any(!nzchar(names(patterns)))) {
    names(patterns) <- paste0("pat", seq_along(patterns))
  }

  # Helper for one pattern
  one <- function(label, pat) {
    if (isTRUE(exact) && "series" %in% names(DT)) {
      idx <- tolower(trimws(DT$series)) == tolower(trimws(pat))
    } else {
      rx  <- if (grepl("^\\(\\?i\\)", pat)) pat else paste0("(?i)", pat)
      idx <- grepl(rx, DT$txt_all, perl = TRUE)
    }
    data.table::data.table(
      target    = label,              # <- not 'key'
      pattern   = pat,
      found     = any(idx),
      n_match   = sum(idx),
      samples   = if (any(idx) && "series" %in% names(DT))
        paste(utils::head(unique(DT[idx, series]), 3), collapse = " || ")
      else ""
    )
  }

  res <- data.table::rbindlist(
    lapply(names(patterns), function(nm) one(nm, patterns[[nm]])),
    use.names = TRUE, fill = TRUE
  )
  data.table::setorder(res, -found, target)
  res[]
}

# ---------------- Example usage ----------------
data_check <- read_abs("5206.0", check_local = TRUE)

must_have <- list(
  gdp_nom   = "\\b(gross\\s+domestic\\s+product|gdp)\\b.*(current\\s*prices|\\bcp\\b)",
  gdp_real  = "\\b(gross\\s+domestic\\s+product|gdp)\\b.*(chain\\s*volume|volume\\s*measures|cvm)",
  gfce_nom  = "^\\s*general\\s+government\\s*;\\s*final\\s+consumption\\s+expenditure\\s*;.*(current\\s*prices|\\bcp\\b)",
  gfce_real = "^\\s*general\\s+government\\s*;\\s*final\\s+consumption\\s+expenditure\\s*;.*(chain|volume)",
  gfcf_nom  = "^\\s*general\\s+government\\s*;\\s*gross\\s+fixed\\s+capital\\s+formation\\s*;.*(current\\s*prices|\\bcp\\b)",
  gfcf_real = "^\\s*general\\s+government\\s*;\\s*gross\\s+fixed\\s+capital\\s+formation\\s*;.*(chain|volume)",

  # Transfers (PAYABLE/PAID only — ignore *receivable*)
  cash_sa_pay = "general\\s+government\\s*;.*social\\s+benefits.*social\\s+assistance",
  cash_wc_pay = "general\\s+government\\s*;.*social\\s+benefits.*workers'?\\s*compensation",
  subsidies   = "general\\s+government\\s*;.*\\bsubsidies\\b",
  curr_grants = "general\\s+government\\s*;.*current\\s+(grants|transfers)"
)

chk <- abs_presence_check(data_check, patterns = must_have)
print(chk)

if (any(!chk$found)) {
  warning(sprintf("Missing %d expected series: %s",
                  sum(!chk$found), paste(chk$target[!chk$found], collapse = ", ")))
}
# 
# XXXXXXXXXXXXXX # Line break to stop code if I am dumb - there are missing series to fix
# 
# sum(grepl("(?i)social benefits", as.character(data_check$series)))
# unique(data_check$series[grepl("(?i)social benefits", data_check$series)])[1:5]

##################################################################

## Code for pulling spending data
get_spend_gdp <- function(freq = c("FY","CY"),
                          check_local = TRUE,
                          verbose = TRUE,
                          spend_measure = c("GFCE","GFCE_plus_GFCF","GFCE_plus_transfers"),
                          transfers_set = c("cash","cash_plus_subsidies","cash_plus_subsidies_plus_currgrants")) {
  freq <- match.arg(freq)
  spend_measure <- match.arg(spend_measure)
  transfers_set <- match.arg(transfers_set)
  
  na <- as.data.table(read_abs("5206.0", check_local = check_local))
  if (!nrow(na)) stop("read_abs('5206.0') returned 0 rows.")
  if (!inherits(na$date, "Date")) na[, date := as.Date(date)]
  
  txt_cols <- intersect(c("series","table_title","unit","data_type","series_type"), names(na))
  if (!length(txt_cols)) txt_cols <- "series"
  for (cc in txt_cols) if (!is.character(na[[cc]])) set(na, j = cc, value = as.character(na[[cc]]))
  if (!"series_type" %in% names(na)) na[, series_type := ""]
  na[, txt_all := do.call(paste, c(.SD, list(sep = " | "))), .SDcols = txt_cols]
  
  prefer_sa <- function(DT) {
    if (!nrow(DT)) return(DT)
    sa <- DT[grepl("Seasonally adjusted", series_type, TRUE)]
    if (nrow(sa)) return(sa)
    tr <- DT[grepl("\\bTrend\\b", series_type, TRUE)]
    if (nrow(tr)) return(tr)
    DT[grepl("\\bOriginal\\b", txt_all, TRUE)]
  }
  
  ann_sum <- function(DT, out_name) {
    if (!nrow(DT)) {
      dt0 <- data.table(year = integer(), val = numeric())
      setnames(dt0, "val", out_name)
      return(dt0)
    }
    annualise(DT, out_name = out_name, mode = "sum", freq = freq)
  }
  
  # --- guards & matchers ---
  is_gen_gov_total <- function(s) grepl("^\\s*General\\s+government\\s*;\\s*", s, TRUE) & !grepl("National|State and local", s, TRUE)
  is_gfce   <- function(s) grepl("Final\\s+consumption\\s+expenditure\\s*;", s, TRUE)
  is_gfcf   <- function(s) grepl("Gross\\s+fixed\\s+capital\\s+formation\\s*;", s, TRUE)
  is_badvar <- function(s) grepl("Percentage\\s+changes|Index", s, TRUE)
  is_nom    <- function(s) grepl("Current\\s*prices?|\\bCP\\b", s, TRUE)
  is_real   <- function(s) grepl("Chain\\s*volume|Chain-?volume|Volume\\s*measures|\\bCVM\\b|\\bvolume\\b", s, TRUE)
  is_dollar_m <- function(s) grepl("\\$\\s*m|\\$m", s, TRUE)
  is_pc_cap   <- function(s) grepl("per\\s+capita|proportion\\s+of\\s+gdp|%\\s*of\\s*gdp", s, TRUE)
  is_gdp <- function(s) grepl("\\bGross\\s+domestic\\s+product\\b|\\bGDP\\b", s, TRUE)
  
  is_cash_sa <- function(s)
    grepl("(?i)^\\s*secondary\\s+income\\s+receivable\\s*[-–—]\\s*social\\s+benefits\\s+receivable\\s*[-–—]\\s*social\\s+assistance\\s+benefits\\s*;?\\s*$",
          s, perl = TRUE)
  is_cash_wc <- function(s)
    grepl("(?i)^\\s*secondary\\s+income\\s+receivable\\s*[-–—]\\s*social\\s+benefits\\s+receivable\\s*[-–—]\\s*workers'?\\s*compensation\\s*;?\\s*$",
          s, perl = TRUE)
  is_subsidies    <- function(s) grepl("\\bsubsidies\\b", s, TRUE)
  is_curr_grants  <- function(s) grepl("current\\s+(grants|transfers)", s, TRUE)
  
  rows_gen_total <- vapply(na$series, is_gen_gov_total, logical(1))
  rows_not_bad   <- !vapply(na$series, is_badvar, logical(1))
  gg <- na[rows_gen_total & rows_not_bad]
  
  # GFCE / GFCF
  gg_gfce <- gg[vapply(series, is_gfce, logical(1))]
  gg_gfcf <- gg[vapply(series, is_gfcf, logical(1))]
  gfce_nom_q  <- gg_gfce[vapply(txt_all, is_nom, 1)  & vapply(txt_all, is_dollar_m, 1) & !vapply(txt_all, is_pc_cap, 1)]
  gfce_real_q <- gg_gfce[vapply(txt_all, is_real, 1) & vapply(txt_all, is_dollar_m, 1) & !vapply(txt_all, is_pc_cap, 1)]
  gfcf_nom_q  <- gg_gfcf[vapply(txt_all, is_nom, 1)  & vapply(txt_all, is_dollar_m, 1) & !vapply(txt_all, is_pc_cap, 1)]
  gfcf_real_q <- gg_gfcf[vapply(txt_all, is_real, 1) & vapply(txt_all, is_dollar_m, 1) & !vapply(txt_all, is_pc_cap, 1)]
  
  # GDP
  gdp_nom_q  <- prefer_sa(na[ vapply(na$series, is_gdp, 1) &
                                vapply(na$txt_all, is_nom, 1) &
                                vapply(na$txt_all, is_dollar_m, 1) &
                                !vapply(na$txt_all, is_pc_cap, 1) ])
  gdp_real_q <- prefer_sa(na[ vapply(na$series, is_gdp, 1) &
                                vapply(na$txt_all, is_real, 1) &
                                vapply(na$txt_all, is_dollar_m, 1) &
                                !vapply(na$txt_all, is_pc_cap, 1) ])
  
  # --- Transfers (PAYABLE) ---
  cash_sa_pay_q <- gg[
    vapply(txt_all, function(s) is_cash_sa(s), 1) &
      vapply(txt_all, is_dollar_m, 1) & !vapply(txt_all, is_pc_cap, 1)
  ]
  cash_wc_pay_q <- gg[
    vapply(txt_all, function(s) is_cash_wc(s), 1) &
      vapply(txt_all, is_dollar_m, 1) & !vapply(txt_all, is_pc_cap, 1)
  ]
  # subs_pay_q <- gg[
  #   vapply(txt_all, function(s) is_subsidies(s) && is_payable(s), 1) &
  #     vapply(txt_all, is_dollar_m, 1) & !vapply(txt_all, is_pc_cap, 1)
  # ]
  # cgr_pay_q <- gg[
  #   vapply(txt_all, function(s) is_curr_grants(s) && is_payable(s), 1) &
  #     vapply(txt_all, is_dollar_m, 1) & !vapply(txt_all, is_pc_cap, 1)
  # ]
  
  cash_sa_pay_q <- prefer_sa(cash_sa_pay_q)
  cash_wc_pay_q <- prefer_sa(cash_wc_pay_q)
  # subs_pay_q    <- prefer_sa(subs_pay_q)
  # cgr_pay_q     <- prefer_sa(cgr_pay_q)
  
  if (!nrow(cash_sa_pay_q)) message("Note: no 'Social assistance benefits (payable)' found — cash benefits may be understated.")
  if (!nrow(cash_wc_pay_q)) message("Note: no 'Workers' compensation (payable)' found — cash benefits may be understated.")
  
  # --- Annualise all flows ---
  gfce_nom_a   <- ann_sum(gfce_nom_q,  "gfce_nom")
  gfce_real_a  <- ann_sum(gfce_real_q, "gfce_real")
  gfcf_nom_a   <- ann_sum(gfcf_nom_q,  "gfcf_nom")
  gfcf_real_a  <- ann_sum(gfcf_real_q, "gfcf_real")
  gdp_nom_a    <- ann_sum(gdp_nom_q,   "gdp_nom")
  gdp_real_a   <- ann_sum(gdp_real_q,  "gdp_real")
  
  sa_nom_a  <- ann_sum(cash_sa_pay_q, "sa_nom")
  wc_nom_a  <- ann_sum(cash_wc_pay_q, "wc_nom")
  # subs_a    <- ann_sum(subs_pay_q,    "trans_subs_nom")
  # cgr_a     <- ann_sum(cgr_pay_q,     "trans_cgr_nom")
  
  # Combine nominal cash benefits (social assistance + workers' comp)
  cash_a <- merge(sa_nom_a, wc_nom_a, by = "year", all = TRUE)
  if (!nrow(cash_a)) cash_a <- data.table(year = integer(), sa_nom = numeric(), wc_nom = numeric())
  cash_a[, trans_cash_nom := rowSums(cbind(sa_nom, wc_nom), na.rm = TRUE)]
  cash_a[, `:=`(sa_nom = NULL, wc_nom = NULL)]
  
  out <- Reduce(function(x, y) merge(x, y, by = "year", all = TRUE),
                list(gdp_nom_a, gdp_real_a,
                     gfce_nom_a, gfce_real_a,
                     gfcf_nom_a, gfcf_real_a,
                     cash_a, subs_a, cgr_a))
  setorder(out, year)
  
  # Compose transfers basket
  out[, transfers_nom := fifelse(
    transfers_set == "cash",                            trans_cash_nom,
    fifelse(transfers_set == "cash_plus_subsidies",     rowSums(cbind(trans_cash_nom, trans_subs_nom), na.rm = TRUE),
            rowSums(cbind(trans_cash_nom, trans_subs_nom, trans_cgr_nom), na.rm = TRUE))
  )]
  
  # Build spend aggregates
  out[, `:=`(
    gov_spend_nom = fifelse(
      spend_measure == "GFCE",                    gfce_nom,
      fifelse(spend_measure == "GFCE_plus_GFCF",  gfce_nom + gfcf_nom,
              gfce_nom + transfers_nom)          # GFCE + transfers (no investment)
    ),
    gov_spend_real = fifelse(
      spend_measure == "GFCE",                    gfce_real,
      fifelse(spend_measure == "GFCE_plus_GFCF",  gfcf_real + gfce_real, NA_real_)
    )
  )]
  
  # Shares
  out[, gov_gdp_nom  := gov_spend_nom / gdp_nom]
  out[, gov_gdp_real := ifelse(is.finite(gov_spend_real) & is.finite(gdp_real),
                               gov_spend_real / gdp_real, NA_real_)]
  
  if (isTRUE(verbose)) {
    yr <- function(d) if (nrow(d)) paste(range(d$year, na.rm = TRUE), collapse = " – ") else "∅"
    message("Year ranges — ",
            paste(
              paste0("GDP_nom:",   yr(gdp_nom_a)),
              paste0("GDP_real:",  yr(gdp_real_a)),
              paste0("GFCE_nom:",  yr(gfce_nom_a)),
              paste0("GFCE_real:", yr(gfce_real_a)),
              paste0("GFCF_nom:",  yr(gfcf_nom_a)),
              paste0("GFCF_real:", yr(gfcf_real_a)),
              paste0("Cash benefits (nom):", yr(cash_a)),
              paste0("Subsidies (nom):",     yr(subs_a)),
              paste0("Curr grants (nom):",   yr(cgr_a)),
              sep = " | "))
    message("Transfers set = ", transfers_set,
            "  |  spend_measure = ", spend_measure)
  }
  
  out[]
}

#### Check the core data by downloading - want to check we've selected sensible datasets

data_check <- get_spend_gdp(freq = "FY", check_local = TRUE, verbose = TRUE,
              spend_measure = measure, transfers_set = transfers_set)

data_check

####

# -------------------- ABS 3101.0 Table 59: ERP & age shares --------------------
get_erp_total_and_shares <- function(freq = c("FY","CY"),
                                     table_hint = "59",
                                     check_local = TRUE) {
  freq <- match.arg(freq)
  erp_raw <- as.data.table(read_abs("3101.0", tables = table_hint, check_local = check_local))
  if (!"date" %in% names(erp_raw)) stop("ABS 3101.0 pull missing 'date'.")
  if (!inherits(erp_raw$date, "Date")) erp_raw[, date := as.Date(date)]
  
  age_text_col <- if ("item" %in% names(erp_raw)) "item" else "series"
  if ("sex" %in% names(erp_raw) && any(grepl("^Persons$", erp_raw$sex, TRUE))) {
    erp_raw <- erp_raw[grepl("^Persons$", sex, TRUE)]
  }
  
  parse_age_bounds <- function(x) {
    x <- tolower(x); nums <- regmatches(x, gregexpr("\\d+", x))[[1]]
    if (!length(nums)) return(c(NA_integer_, NA_integer_))
    lo <- as.integer(nums[1]); hi <- if (grepl("over|and over|\\+", x)) 120L else as.integer(nums[length(nums)])
    c(lo, hi)
  }
  bounds <- do.call(rbind, lapply(erp_raw[[age_text_col]], parse_age_bounds))
  erp_raw[, age_lo := bounds[,1]]
  erp_raw[, age_hi := bounds[,2]]
  ages <- erp_raw[!is.na(age_lo) & !is.na(age_hi)]
  
  map_bin <- function(lo, hi) {
    if (hi <= 14) return("0_14")
    if (lo >= 15 && hi <= 34) return("15_34")
    if (lo >= 35 && hi <= 54) return("35_54")
    if (lo >= 55 && hi <= 64) return("55_64")
    if (lo >= 65) return("65p")
    NA_character_
  }
  ages[, group := mapply(map_bin, age_lo, age_hi)]
  ages <- ages[!is.na(group)]
  
  ages_grp <- ages[, .(pop_grp = sum(value, na.rm = TRUE)), by = .(date, group)]
  erp_tot  <- ages_grp[, .(pop_total = sum(pop_grp, na.rm = TRUE)), by = .(date)]
  
  erp_m <- merge(ages_grp, erp_tot, by = "date", all.x = TRUE)
  erp_m[, share := pop_grp / pop_total]
  
  if (freq == "FY") {
    shares_a  <- erp_m[, .(share = mean(share, na.rm = TRUE)), by = .(year = fy_end_year(date), group)]
    erp_tot_a <- erp_tot[, .(pop_total = mean(pop_total, na.rm = TRUE)), by = .(year = fy_end_year(date))]
  } else {
    shares_a  <- erp_m[, .(share = mean(share, na.rm = TRUE)), by = .(year = year(date), group)]
    erp_tot_a <- erp_tot[, .(pop_total = mean(pop_total, na.rm = TRUE)), by = .(year = year(date))]
  }
  
  shares_w <- dcast(shares_a, year ~ group, value.var = "share")
  for (g in c("0_14","15_34","35_54","55_64","65p")) if (!g %in% names(shares_w)) shares_w[, (g) := NA_real_]
  setcolorder(shares_w, c("year","0_14","15_34","35_54","55_64","65p"))
  
  merge(erp_tot_a, shares_w, by = "year", all = TRUE)[order(year)]
}

# -------------------- Terms of trade (annual mean of index) --------------------
get_terms_of_trade <- function(freq = c("FY","CY"), check_local = TRUE) {
  freq <- match.arg(freq)
  bo <- as.data.table(read_abs("5302.0", check_local = check_local))
  if (!inherits(bo$date, "Date")) bo[, date := as.Date(date)]
  
  txt_cols <- intersect(c("series","table_title","unit","data_type","series_type"), names(bo))
  if (!length(txt_cols)) txt_cols <- "series"
  for (cc in txt_cols) if (!is.character(bo[[cc]])) set(bo, j = cc, value = as.character(bo[[cc]]))
  bo[, "__txt" := do.call(paste, c(.SD, list(sep = " | "))), .SDcols = txt_cols]
  
  is_tot <- grepl("terms\\s*of\\s*trade", bo[["__txt"]], TRUE) &
    grepl("index", bo[["__txt"]], TRUE) &
    !grepl("percentage\\s*changes|index\\s*changes", bo[["__txt"]], TRUE)
  
  prefer_sa <- function(DT) {
    sa <- DT[grepl("Seasonally adjusted", DT[["__txt"]], TRUE)]
    if (nrow(sa)) return(sa)
    tr <- DT[grepl("\\bTrend\\b", DT[["__txt"]], TRUE)]
    if (nrow(tr)) return(tr)
    DT[grepl("\\bOriginal\\b", DT[["__txt"]], TRUE)]
  }
  
  tot_q <- prefer_sa(bo[is_tot])
  if (!nrow(tot_q)) stop("Could not locate ToT index in 5302.0")
  
  annualise(tot_q, out_name = "tot_index", mode = "mean", freq = freq)[order(year)]
}

# -------------------- Build dataset (now supports transfers) --------------------
build_share_dataset <- function(freq = c("FY","CY"),
                                measure = c("GFCE","GFCE_plus_GFCF","GFCE_plus_transfers"),
                                share_basis = c("nominal","real"),
                                transfers_set = c("cash","cash_plus_subsidies","cash_plus_subsidies_plus_currgrants"),
                                check_local = TRUE) {
  freq          <- match.arg(freq)
  measure       <- match.arg(measure)
  share_basis   <- match.arg(share_basis)
  transfers_set <- match.arg(transfers_set)
  
  na   <- get_spend_gdp(freq = freq, check_local = check_local, verbose = TRUE,
                        spend_measure = measure, transfers_set = transfers_set)
  erp  <- get_erp_total_and_shares(freq = freq, check_local = check_local)
  dt   <- merge(na, erp, by = "year", all = FALSE)
  if (!nrow(dt)) stop("No overlapping years between 5206.0 pulls and 3101.0.")
  
  # Construct spend/GDP according to chosen basis & measure
  if (share_basis == "nominal") {
    if (!"gov_spend_nom" %in% names(dt) || !"gdp_nom" %in% names(dt))
      stop("Missing nominal spend/GDP.")
    dt[, gov_gdp := gov_spend_nom / gdp_nom]
  } else {
    if (!is.finite(match(measure, c("GFCE","GFCE_plus_GFCF"))))
      stop("Real-basis shares require spend_measure = 'GFCE' or 'GFCE_plus_GFCF'.")
    if (!"gov_spend_real" %in% names(dt) || !"gdp_real" %in% names(dt))
      stop("Missing real spend/GDP.")
    dt[, gov_gdp := gov_spend_real / gdp_real]
  }
  
  # Relative price (Baumol proxy) — keep using GFCE vs GDP deflators
  if (!all(c("gfce_nom","gfce_real","gdp_nom","gdp_real") %in% names(dt)))
    stop("GFCE/GDP nominal & real needed to compute rp_g.")
  dt[, p_gfce := gfce_nom / gfce_real]
  dt[, p_gdp  := gdp_nom  / gdp_real]
  dt[, rp_g   := as.numeric(scale(p_gfce / p_gdp, center = TRUE, scale = TRUE))]
  
  # Population growth (scale effect)
  if (!"pop_total" %in% names(dt)) stop("pop_total not found (ERP merge failed).")
  setorder(dt, year)
  dt[, dln_pop := c(NA_real_, diff(log(pop_total)))]
  
  # Keep required columns
  need_cols <- c("year", "gov_gdp",
                 "0_14","15_34","35_54","55_64","65p",
                 "pop_total", "rp_g", "dln_pop",
                 "gfce_nom","gfce_real","gdp_nom","gdp_real")
  miss <- setdiff(need_cols, names(dt))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  
  dt <- dt[, ..need_cols]
  dt <- dt[complete.cases(dt)]
  setorder(dt, year)
  
  # Age-share sanity
  dt[, age_sum := `0_14` + `15_34` + `35_54` + `55_64` + `65p`]
  if (any(abs(dt$age_sum - 1) > 0.03)) warning("Some years' age shares deviate from 1 by >3%.")
  dt[, age_sum := NULL]
  
  # Sanity on spend/GDP (broad measure can be higher than GFCE)
  ratio_chk <- dt$gov_gdp
  if (max(ratio_chk, na.rm = TRUE) > 0.8 || min(ratio_chk, na.rm = TRUE) < 0.01) {
    warning(sprintf("Spend/GDP share outside expected range (min %.1f%%, max %.1f%%). Check filters.",
                    100*min(ratio_chk, na.rm = TRUE), 100*max(ratio_chk, na.rm = TRUE)))
  }
  
  dt[]
}

# -------------------- Shapley utilities (unchanged) --------------------
.all_perms <- function(v) if (length(v)==1L) list(v) else {
  out <- list()
  for (i in seq_along(v)) for (p in .all_perms(v[-i])) out[[length(out)+1L]] <- c(v[i], p)
  out
}
.all_subsets <- function(v) {
  out <- list(character(0))
  if (length(v)) for (m in 1:length(v)) out <- c(out, combn(v, m, simplify = FALSE))
  out
}
.fit_subset <- function(y, X, S) {
  if (length(S) == 0L) return(numeric(0))
  cols <- as.data.frame(X[, ..S])
  for (j in seq_along(cols)) cols[[j]] <- as.numeric(cols[[j]])
  Xi <- as.matrix(cols)
  fit <- lm.fit(x = Xi, y = y)              # no intercept
  b <- as.numeric(coef(fit))
  keep <- !is.na(b)
  if (!length(b) || !any(keep)) return(numeric(0))
  names(b) <- colnames(Xi)
  b[keep]
}
.delta_hat <- function(bS, x0, x1) {
  if (!length(bS)) return(0)
  common <- intersect(names(bS), names(x0))
  if (!length(common)) return(0)
  v0 <- setNames(as.numeric(unlist(x0[common], use.names = FALSE)), common)
  v1 <- setNames(as.numeric(unlist(x1[common], use.names = FALSE)), common)
  sum(bS[common] * (v1 - v0))
}

shapley_regression_pair_general <- function(dt, y0, y1, features, y_col = "gov_gdp", conditional = TRUE) {
  stopifnot(y0 %in% dt$year, y1 %in% dt$year)
  d0 <- dt[year == y0]; d1 <- dt[year == y1]
  X  <- dt[, ..features]
  y  <- as.numeric(dt[[y_col]])
  
  .all_perms <- function(v) if (length(v)==1L) list(v) else { out <- list(); for (i in seq_along(v)) for (p in Recall(v[-i])) out[[length(out)+1L]] <- c(v[i], p); out }
  .all_subsets <- function(v) { out <- list(character(0)); if (length(v)) for (m in 1:length(v)) out <- c(out, combn(v, m, simplify=FALSE)); out }
  .fit_subset <- function(y, X, S) {
    if (!length(S)) return(numeric(0))
    Xdf <- as.data.frame(X[, ..S]); for (j in seq_along(Xdf)) Xdf[[j]] <- as.numeric(Xdf[[j]])
    Xi <- as.matrix(Xdf); b <- as.numeric(coef(lm.fit(x = Xi, y = y)))
    keep <- !is.na(b); if (!any(keep)) return(numeric(0))
    names(b) <- colnames(Xi); b[keep]
  }
  .delta_hat <- function(bS, x0, x1) {
    if (!length(bS)) return(0)
    common <- intersect(names(bS), names(x0)); if (!length(common)) return(0)
    v0 <- setNames(as.numeric(unlist(x0[common], use.names=FALSE)), common)
    v1 <- setNames(as.numeric(unlist(x1[common], use.names=FALSE)), common)
    sum(bS[common] * (v1 - v0))
  }
  .key <- function(S) if (length(S)) paste(sort(S), collapse="|") else ".EMPTY"
  
  d_actual <- as.numeric(d1[[y_col]] - d0[[y_col]])
  
  if (!conditional) {
    b <- .fit_subset(y, X, features)
    contrib <- setNames(numeric(length(features)), features)
    for (g in features) contrib[g] <- (as.numeric(d1[[g]] - d0[[g]])) * (if (g %in% names(b)) b[g] else 0)
    d_hat <- sum(contrib)
    return(list(d_actual = d_actual, d_hat = d_hat, contrib = contrib, unexplained = d_actual - d_hat))
  }
  
  subsets <- .all_subsets(features)
  subsets <- Filter(function(S) length(S) > 0L, subsets)
  beta_cache <- new.env(parent = emptyenv())
  for (S in subsets) assign(.key(S), .fit_subset(y, X, S), envir = beta_cache)
  
  delta_of <- function(S) {
    if (!length(S)) return(0)
    bS <- get(.key(S), envir = beta_cache, inherits = FALSE)
    x0 <- as.list(d0[, ..features]); x1 <- as.list(d1[, ..features])
    .delta_hat(bS, x0[S], x1[S])
  }
  
  perms <- .all_perms(features)
  contrib_sum <- setNames(numeric(length(features)), features)
  for (p in perms) {
    S <- character(0); prev <- 0
    for (g in p) {
      Snew <- c(S, g)
      val  <- delta_of(Snew)
      contrib_sum[g] <- contrib_sum[g] + (val - prev)
      S <- Snew; prev <- val
    }
  }
  contrib <- contrib_sum / length(perms)
  d_hat   <- sum(contrib)
  list(d_actual = d_actual, d_hat = d_hat, contrib = contrib, unexplained = d_actual - d_hat)
}

age_model_r2 <- function(dt, groups = c("0_14","15_34","35_54","55_64","65p")) {
  cols <- as.data.frame(dt[, ..groups]); for (j in seq_along(cols)) cols[[j]] <- as.numeric(cols[[j]])
  Xi <- as.matrix(cols); yi <- as.numeric(dt$gov_gdp)
  fit <- lm.fit(x = Xi, y = yi)
  yhat <- as.vector(Xi %*% coef(fit))
  sst <- sum((yi - mean(yi))^2, na.rm = TRUE)
  sse <- sum((yi - yhat)^2,     na.rm = TRUE)
  list(r2 = ifelse(sst > 0, 1 - sse/sst, NA_real_), yhat = yhat)
}

coef_full <- function(dt, features, y_col = "gov_gdp") {
  Xdf <- as.data.frame(dt[, ..features]); for (j in seq_along(Xdf)) Xdf[[j]] <- as.numeric(Xdf[[j]])
  Xi  <- as.matrix(Xdf); yi  <- as.numeric(dt[[y_col]])
  b   <- as.numeric(coef(lm.fit(x = Xi, y = yi))); names(b) <- colnames(Xi); b
}
coef_subsets <- function(dt, features, y_col = "gov_gdp") {
  .all_subsets <- function(v) { out <- list(character(0)); if (length(v)) for (m in 1:length(v)) out <- c(out, combn(v, m, simplify=FALSE)); out }
  .fit_subset  <- function(y, X, S) {
    if (!length(S)) return(numeric(0))
    Xdf <- as.data.frame(X[, ..S]); for (j in seq_along(Xdf)) Xdf[[j]] <- as.numeric(Xdf[[j]])
    Xi <- as.matrix(Xdf); b <- as.numeric(coef(lm.fit(x = Xi, y = y)))
    keep <- !is.na(b); if (!any(keep)) return(numeric(0))
    names(b) <- colnames(Xi); b[keep]
  }
  X <- dt[, ..features]; y <- as.numeric(dt[[y_col]])
  subs <- .all_subsets(features)
  subs <- Filter(function(S) length(S) > 0L, subs)
  rbindlist(lapply(subs, function(S){
    bS <- .fit_subset(y, X, S)
    if (!length(bS)) return(NULL)
    data.table(subset = paste(sort(S), collapse="|"), feature = names(bS), beta = as.numeric(bS))
  }))
}

# -------------------- Four-bin + panel wrappers --------------------
run_shapley_blocks <- function(dt, conditional = TRUE, features) {
  setorder(dt, year)
  brks <- unique(as.integer(quantile(unique(dt$year), probs = c(0,.25,.5,.75,1), type = 1)))
  dt[, bin := findInterval(year, brks, all.inside = TRUE)]
  bounds <- dt[, .(y0 = min(year), y1 = max(year)), by = bin][order(bin)]
  if (nrow(bounds) > 4L) bounds <- bounds[1:4]
  
  four_res <- rbindlist(lapply(1:nrow(bounds), function(i) {
    sh <- shapley_regression_pair_general(dt, bounds$y0[i], bounds$y1[i], features = features, conditional = conditional)
    data.table(y0 = bounds$y0[i], y1 = bounds$y1[i], d_actual = sh$d_actual, d_hat = sh$d_hat, unexplained = sh$unexplained, t(sh$contrib))
  }), use.names = TRUE, fill = TRUE)
  
  panel_pairs <- data.table(y0 = head(dt$year, -1L), y1 = tail(dt$year, -1L))
  panel_res <- rbindlist(lapply(1:nrow(panel_pairs), function(i) {
    sh <- shapley_regression_pair_general(dt, panel_pairs$y0[i], panel_pairs$y1[i], features = features, conditional = conditional)
    data.table(y0 = panel_pairs$y0[i], y1 = panel_pairs$y1[i], d_actual = sh$d_actual, d_hat = sh$d_hat, unexplained = sh$unexplained, t(sh$contrib))
  }), use.names = TRUE, fill = TRUE)
  
  list(four_bin = four_res, panel = panel_res)
}

# -------------------- Counterfactual: hold age shares at baseline --------------------
counterfactual_age_path <- function(dt, baseline_year,
                                    groups = c("0_14","15_34","35_54","55_64","65p")) {
  stopifnot(baseline_year %in% dt$year)
  Xi <- as.matrix(dt[, ..groups]); yi <- dt$gov_gdp
  fit <- lm.fit(x = Xi, y = yi); b <- coef(fit); names(b) <- colnames(Xi)[!is.na(b)]
  base <- dt[year == baseline_year, ..groups]
  base_vec <- as.numeric(base[1, ..groups]); names(base_vec) <- groups
  Xcf <- Xi
  for (j in seq_along(groups)) Xcf[, j] <- base_vec[j]
  yhat_actual <- as.vector(Xi  %*% b)
  yhat_cf     <- as.vector(Xcf %*% b)
  data.table(year = dt$year, y_actual = dt$gov_gdp, yhat_actual = yhat_actual, yhat_cf = yhat_cf, diff_cf = yhat_actual - yhat_cf)
}

# ==================== RUN ====================
message("▶ Building dataset (", freq, ", measure=", measure, ", basis=", share_basis, ", transfers=", transfers_set, ") …")
tot_a    <- get_terms_of_trade(freq = freq, check_local = abs_check_local)
share_dt <- build_share_dataset(freq, measure, share_basis, transfers_set, check_local = abs_check_local)
share_dt <- merge(share_dt, tot_a, by = "year", all.x = TRUE)
share_dt[, tot := as.numeric(scale(tot_index, center = TRUE, scale = TRUE))]

# Recompute rp_g (already in build_share_dataset, but harmless)
share_dt[, p_gfce := gfce_nom / gfce_real]
share_dt[, p_gdp  := gdp_nom  / gdp_real]
share_dt[, rp_g   := as.numeric(scale(p_gfce / p_gdp, center = TRUE, scale = TRUE))]
share_dt[, dln_pop := c(NA_real_, diff(log(pop_total)))]

# Sample window
if (is.finite(start_year))     share_dt <- share_dt[year >= start_year]
if (is.finite(final_year_opt)) share_dt <- share_dt[year <= final_year_opt]
stopifnot(nrow(share_dt) >= 2L)
if (!is.finite(baseline_year) || !(baseline_year %in% share_dt$year)) baseline_year <- min(share_dt$year)

# Diagnostics
am <- age_model_r2(share_dt)
message(sprintf("▶ Age-only model R^2 = %.3f", am$r2))

# Shapley
message("▶ Computing Shapley contributions …")
sv_res   <- run_shapley_blocks(share_dt, conditional = conditional_sv, features = features)
four_bin <- sv_res$four_bin
panel    <- sv_res$panel

if (nrow(four_bin)) message("Four-bin: mean |unexplained| = ", sprintf("%.4f", mean(abs(four_bin$unexplained), na.rm = TRUE)))
if (nrow(panel))    message("Panel: median |unexplained| = ", sprintf("%.4f", median(abs(panel$unexplained), na.rm = TRUE)))

# Quick table
if (exists("four_bin") && nrow(four_bin)) {
  tmp <- copy(four_bin)[, Segment := paste0(y0, "→", y1)]
  cols_order <- intersect(c("Segment", "d_actual", "d_hat", "unexplained", features), names(tmp))
  print(tmp[, ..cols_order])
}

# Panel plot
if (exists("panel") && nrow(panel)) {
  panel_measures <- intersect(features, names(panel))
  panel_m <- melt(panel,
                  id.vars = c("y0","y1","d_actual","d_hat","unexplained"),
                  measure.vars = panel_measures,
                  variable.name = "driver",
                  value.name = "contrib")
  panel_m[, year := y1]
  lab_r2 <- if (is.finite(am$r2)) sprintf("R²(age-only)=%.3f", am$r2) else ""
  p <- ggplot(panel_m, aes(x = year, y = contrib, fill = driver)) +
    geom_col() +
    geom_line(data = unique(panel_m[, .(year, d_actual)]), aes(y = d_actual, x = year, group = 1), inherit.aes = FALSE) +
    geom_point(data = unique(panel_m[, .(year, d_actual)]), aes(y = d_actual, x = year), inherit.aes = FALSE) +
    labs(title = "Regression-based Shapley of Δ(spend/GDP): explained contributions",
         subtitle = paste0("Unexplained (residual) not stacked. ", lab_r2),
         x = "Year", y = "Δ share (level points)", fill = "Driver") +
    theme_e61(legend = "bottom")
  print(p)
}

# Four-bin plot
if (exists("four_bin") && nrow(four_bin)) {
  four_measures <- intersect(features, names(four_bin))
  four_m <- melt(four_bin,
                 id.vars = c("y0","y1","d_actual","d_hat","unexplained"),
                 measure.vars = four_measures,
                 variable.name = "driver",
                 value.name = "contrib")
  four_m[, Segment := paste0(y0, "–", y1)]
  seg_levels <- four_bin[, paste0(y0, "–", y1)]
  four_m[, Segment := factor(Segment, levels = seg_levels)]
  p4 <- ggplot(four_m, aes(x = Segment, y = contrib, fill = driver)) +
    geom_col() +
    geom_point(data = unique(four_m[, .(Segment, d_hat)]), aes(y = d_hat, x = Segment), inherit.aes = FALSE, shape = 23, size = 3, stroke = 0.6) +
    geom_point(data = unique(four_m[, .(Segment, d_actual)]), aes(y = d_actual, x = Segment), inherit.aes = FALSE, shape = 16, size = 2.8) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    coord_flip() +
    labs(title = "Regression-based Shapley of Δ(spend/GDP): binned periods",
         subtitle = "Stacks = explained contributions; ● actual Δ, ◇ predicted Δ",
         x = NULL, y = "Δ share (level points)", fill = "Driver") +
    theme_e61(legend = "bottom")
  print(p4)
}

# ---- Collapsed four-bin plot: Residual + 65+ + Other ages + Economic effects ----
if (exists("four_bin") && nrow(four_bin)) {
  measure_vars <- intersect(features, names(four_bin))
  
  four_m <- melt(
    four_bin,
    id.vars       = c("y0","y1","d_actual","d_hat","unexplained"),
    measure.vars  = measure_vars,
    variable.name = "driver",
    value.name    = "contrib"
  )
  
  age_bins <- c("0_14","15_34","35_54","55_64")
  four_m[, group := fcase(
    driver == "65p",              "65+",
    driver %in% age_bins,         "Other ages",
    default =                     "Economic effects"
  )]
  
  four_g <- four_m[, .(contrib = sum(contrib, na.rm = TRUE)),
                   by = .(y0, y1, d_actual, d_hat, unexplained, group)]
  
  residual_dt <- unique(four_m[, .(y0, y1, d_actual, d_hat, unexplained)])
  residual_dt[, `:=`(group = "Residual", contrib = unexplained)]
  four_g <- rbind(four_g, residual_dt[, .(y0, y1, d_actual, d_hat, unexplained, group, contrib)],
                  use.names = TRUE)
  
  four_g[, Segment := paste0(y0, "–", y1)]
  seg_levels <- four_bin[, paste0(y0, "–", y1)]
  four_g[, Segment := factor(Segment, levels = seg_levels)]
  
  four_g[, group := factor(group, levels = c("Residual","65+","Other ages","Economic effects"))]
  
  p4c <- ggplot(four_g, aes(x = Segment, y = contrib, fill = group)) +
    geom_col(width = 0.85) +
    geom_point(
      data = unique(four_g[, .(Segment, d_actual)]),
      aes(y = d_actual, x = Segment),
      inherit.aes = FALSE,
      shape = 16, size = 4, colour = "black"
    ) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    coord_flip() +
    labs_e61(
      title     = "Demographic trends dominate lift in spending",
      subtitle  = "Regression-based Shapley of Δ(spend/GDP)\nΔ share (level points)",
      x = NULL, y = NULL, fill = NULL,
      footnotes = c(
        paste0("Black dot reflects the change in ", measure, " to GDP."),
        "Effects represent association between the change in the category and changes in spending to GDP.",
        "Economic Effects reflect variation explained by changes in population, relative government costs, and terms of trade."
      ),
      sources   = c("ABS","e61")
    )
  print(p4c)
}

# Counterfactual paths by block (optional)
counterfactual_paths_by_block <- function(dt, features, blocks, baseline_year, y_col = "gov_gdp") {
  stopifnot(baseline_year %in% dt$year)
  Xdf <- as.data.frame(dt[, ..features]); for (j in seq_along(Xdf)) Xdf[[j]] <- as.numeric(Xdf[[j]])
  Xi  <- as.matrix(Xdf); yi  <- as.numeric(dt[[y_col]])
  b   <- as.numeric(coef(lm.fit(x = Xi, y = yi))); names(b) <- colnames(Xi)
  yhat_full <- as.vector(Xi %*% b)
  base_row <- dt[year == baseline_year, ..features]
  out <- data.table(year = dt$year, y_actual = yi, yhat_full = yhat_full)
  for (nm in names(blocks)) {
    blk <- intersect(blocks[[nm]], features); if (!length(blk)) next
    Xcf <- Xi
    for (col in blk) Xcf[, col] <- as.numeric(base_row[[col]])
    out[[paste0("yhat_cf_", nm)]] <- as.vector(Xcf %*% b)
    out[[paste0("impact_", nm)]]  <- out$yhat_full - out[[paste0("yhat_cf_", nm)]]
  }
  X0 <- Xi; for (col in features) X0[, col] <- as.numeric(base_row[[col]])
  out[["yhat_cf_AllFixed"]] <- as.vector(X0 %*% b)
  out[]
}

plot_counterfactuals <- function(cf_dt, blocks_to_show = NULL, title = "Counterfactual paths (holding blocks fixed at baseline)") {
  if (is.null(blocks_to_show)) blocks_to_show <- gsub("^yhat_cf_", "", grep("^yhat_cf_", names(cf_dt), value = TRUE))
  keep <- c("year", "y_actual", "yhat_full", paste0("yhat_cf_", blocks_to_show))
  long <- melt(cf_dt[, ..keep], id.vars = "year", variable.name = "series", value.name = "value")
  long[, series := factor(series, levels = c("y_actual","yhat_full", paste0("yhat_cf_", blocks_to_show)),
                          labels = c("Actual","Fitted", paste0("CF: hold ", blocks_to_show, " at baseline")))]
  ggplot(long, aes(year, value, colour = series)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(title = title, x = NULL, y = "Spend / GDP") +
    theme_e61(legend = "bottom")
}

blocks <- list(
  Demography = c("0_14","15_34","35_54","55_64","65p"),
  RelPrices  = c("rp_g"),
  PopGrowth  = c("dln_pop"),
  ToT        = intersect("tot", names(share_dt))
)

cf_paths <- counterfactual_paths_by_block(share_dt, features, blocks, baseline_year, y_col = "gov_gdp")
print(plot_counterfactuals(cf_dt = cf_paths,
                           blocks_to_show = names(blocks),
                           title = sprintf("Counterfactual spend/GDP (baseline %s; sample %s–%s)",
                                           baseline_year, min(share_dt$year), max(share_dt$year))))

# Save key outputs
fwrite(share_dt, "age_share_input.csv")
if (exists("four_bin") && nrow(four_bin)) fwrite(four_bin, "shapley_age_fourbin.csv")
if (exists("panel")    && nrow(panel))    fwrite(panel,    "shapley_age_panel.csv")
fwrite(cf_paths, "counterfactual_paths_generalgov.csv")
message("Done.")



##### Optional: your manual segmented ranges (unchanged) #####

manual_bounds <- data.table(
  label = c("1985–1997", "1998–2010", "2011–2019", "2020–2022"),
  y0    = c(1985,        1998,        2011,        2020),
  y1    = c(1997,        2010,        2019,        2022)
)

run_shapley_for_ranges <- function(dt, bounds_dt, features, conditional = TRUE, snap = TRUE) {
  stopifnot(all(c("y0","y1") %in% names(bounds_dt)))
  yrs <- sort(unique(dt$year))
  snap_year <- function(y, dir = c("down","up")) {
    dir <- match.arg(dir)
    if (dir == "down") { cand <- yrs[yrs <= y]; if (length(cand)) max(cand) else NA_integer_ }
    else               { cand <- yrs[yrs >= y]; if (length(cand)) min(cand) else NA_integer_ }
  }
  rbindlist(lapply(1:nrow(bounds_dt), function(i){
    y0 <- bounds_dt$y0[i]; y1 <- bounds_dt$y1[i]
    lab <- if ("label" %in% names(bounds_dt) && !is.na(bounds_dt$label[i]))
      bounds_dt$label[i] else sprintf("%d–%d", y0, y1)
    if (snap) { y0a <- snap_year(y0, "down"); y1a <- snap_year(y1, "up") } else { y0a <- y0; y1a <- y1 }
    if (!is.finite(y0a) || !is.finite(y1a) || y1a <= y0a) return(NULL)
    sh <- shapley_regression_pair_general(dt, y0a, y1a, features = features, conditional = conditional)
    data.table(Segment = lab, y0 = y0a, y1 = y1a,
               d_actual = sh$d_actual, d_hat = sh$d_hat, unexplained = sh$unexplained,
               t(sh$contrib))
  }), use.names = TRUE, fill = TRUE)
}

manual_res <- run_shapley_for_ranges(share_dt, manual_bounds, features, conditional = conditional_sv)

plot_collapsed_shapley <- function(res_dt, features, measure_label = measure) {
  stopifnot(nrow(res_dt) > 0)
  measure_vars <- intersect(features, names(res_dt))
  four_m <- melt(
    res_dt,
    id.vars       = c("Segment","y0","y1","d_actual","d_hat","unexplained"),
    measure.vars  = measure_vars,
    variable.name = "driver",
    value.name    = "contrib"
  )
  age_bins <- c("0_14","15_34","35_54","55_64")
  four_m[, group := fcase(
    driver == "65p",              "65+",
    driver %in% age_bins,         "Other ages",
    default =                     "Economic effects"
  )]
  four_g <- four_m[, .(contrib = sum(contrib, na.rm = TRUE)),
                   by = .(Segment, y0, y1, d_actual, d_hat, unexplained, group)]
  residual_dt <- unique(four_m[, .(Segment, y0, y1, d_actual, d_hat, unexplained)])
  residual_dt[, `:=`(group = "Residual", contrib = unexplained)]
  four_g <- rbind(four_g, residual_dt[, .(Segment, y0, y1, d_actual, d_hat, unexplained, group, contrib)],
                  use.names = TRUE)
  four_g[, Segment := factor(Segment, levels = manual_bounds$label)]
  four_g[, stack_order := {
    if (all(contrib[group=="Residual"] <= 0)) {
      factor(group, levels = c("Residual","65+","Other ages","Economic effects"))
    } else {
      factor(group, levels = c("65+","Other ages","Economic effects","Residual"))
    }
  }, by = Segment]
  ggplot(four_g, aes(x = Segment, y = contrib, fill = stack_order)) +
    geom_col(width = 0.85) +
    geom_point(
      data = unique(four_g[, .(Segment, d_actual)]),
      aes(y = d_actual, x = Segment),
      inherit.aes = FALSE,
      shape = 16, size = 4, colour = "black"
    ) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    coord_flip() +
    labs_e61(
      title     = "Demographic trends dominate lift in spending",
      subtitle  = "Regression-based Shapley of Δ(spend/GDP)",
      x = NULL, y = "Δ share (level points)", fill = "Component",
      footnotes = c(
        paste0("Black dot reflects the change in ", measure_label, " to GDP."),
        "Effects represent association between the change in the category and changes in spending to GDP.",
        "Economic Effects reflect variation explained by changes in population, relative government costs, and terms of trade."
      ),
      sources   = c("ABS","e61")
    ) +
    theme_e61(legend = "bottom")
}

p_manual <- plot_collapsed_shapley(manual_res, features)
print(p_manual)
# save_e61("Shapley_collapsed_manual.png", plot = p_manual, res = 2)
