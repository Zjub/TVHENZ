## ======================================================================
## Regression-based Shapley for Govt Spending as % of GDP (AU, ABS)
## Decompose changes in spend/GDP into age-group contributions - unlike prior versions we account for outliers during COVID
## with diagnostics so we don't attribute all changes to ageing.
## Last update: 23/10/2025 (to add in projections)
## Note: This is the fixed and final version!!!!
## Note to fix - the population number in this code is double the actual, which doesn't influence the growth rate (the variable used).
## ======================================================================

# -------------------- Packages --------------------
suppressPackageStartupMessages({
  library(data.table)
  library(readabs)
  library(lubridate)
  library(ggplot2)
  library(fixest)
  library(theme61)
  library(readxl)
})

rm(list = ls()); invisible(gc())

# -------------------- User prefs --------------------
freq            <- "FY"                 # "FY" or "CY"
measure         <- "Expenditure"              # "GFCE" or "GFCE_plus_GFCF" or "Expenditure"
share_basis     <- "nominal"           # "nominal" (standard for shares) or "real"
conditional_sv  <- TRUE                # TRUE = conditional (full Shapley); FALSE = β·Δs
abs_check_local <- FALSE                # use cached ABS files (fast after first run)
#features <- c("0_14","15_34","35_54","55_64","65p","tot") # The set of variables included in this run
start_year      <- 1980    # <-- choose your sample start
final_year_opt  <- NA      # optional end year, set to e.g. 2018 or NA to use all. 
baseline_year   <- 1999      # optional: force a baseline year; NA = first year of filtered sample
features <- c("0_14","15_34","35_54","55_64","65p","tot",
              "rp_g","unemp") # ,"dln_pop""unemp",
outlier_years <- c(2020, 2021)   # Years we remove from estimation for being outliers
govt_level    <- "total"        # Either total, or just "Federal" - not currently operational. Only requires a change to one line, but will need to change all the saves.

# -------------------- Helpers --------------------
# AU FY ends in June: add 6m and take year
fy_end_year <- function(date) {
  d <- as.Date(date)
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))
  y + (m >= 7L)   # Jul–Dec -> next year; Jan–Jun -> same year
}

pick_pref <- function(dq) {
  has_col <- "series_type" %in% names(dq)
  sa <- dq[grepl("Seasonally adjusted", if (has_col) series_type else series, TRUE)]
  if (nrow(sa)) return(sa)
  tr <- dq[grepl("\\bTrend\\b",        if (has_col) series_type else series, TRUE)]
  if (nrow(tr)) return(tr)
  dq[grepl("\\bOriginal\\b",           if (has_col) series_type else series, TRUE)]
}

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

# ---- Helpers ----
prefer_sa <- function(DT) {
  if (!nrow(DT)) return(DT)
  has_series_type <- "series_type" %in% names(DT)
  txt <- if (has_series_type) DT$series_type else DT$.__txt
  sa <- DT[grepl("Seasonally adjusted", txt, TRUE)]
  if (nrow(sa)) return(sa)
  tr <- DT[grepl("\\bTrend\\b", txt, TRUE)]
  if (nrow(tr)) return(tr)
  DT[grepl("\\bOriginal\\b", txt, TRUE)]
}

# Dedup by date/value to avoid double counting across tables
dedup_by_date_value <- function(DT) {
  if (!nrow(DT)) return(DT)
  unique(DT[, .(date, value)])[order(date)]
}

ann <- function(DT, out_name) {
  if (!inherits(DT, "data.table")) DT <- as.data.table(DT)
  if (!nrow(DT)) { tmp <- data.table(year = integer(), val = numeric()); setnames(tmp, "val", out_name); return(tmp) }
  # NOTE: for flows you may prefer mode = "sum"
  annualise(DT, out_name = out_name, mode = "mean", freq = freq)
}


################### Tester function ----
## This function is used to check we are selecting unique and sensible series 
#
# preview_gfce_gdp_selection <- function(check_local = TRUE, max_show = 50) {
#   library(data.table)
#   library(readabs)
#   
#   # ---- Load & prep (same as your function) ----
#   na <- as.data.table(read_abs("5206.0", check_local = check_local))
#   if (!nrow(na)) stop("read_abs('5206.0') returned 0 rows.")
#   if (!inherits(na$date, "Date")) na[, date := as.Date(date)]
#   unit_guard <- grepl("\\$\\s*Million(s)?\\b", na$unit, ignore.case = TRUE)
#   na <- na[unit_guard]
#   
#   txt_cols <- intersect(c("series","table_title","unit","data_type","series_type"), names(na))
#   if (!length(txt_cols)) txt_cols <- "series"
#   for (cc in txt_cols) if (!is.character(na[[cc]])) set(na, j = cc, value = as.character(na[[cc]]))
#   na[, .__txt := do.call(paste, c(.SD, list(sep = " | "))), .SDcols = txt_cols]
#   
#   # ---- Same split rules as your function ----
#   is_tbl13 <- function(tt) grepl("^\\s*Table\\s*(3)\\b", tt, TRUE)
#   is_tbl24 <- function(tt) grepl("^\\s*Table\\s*(2)\\b",    tt, TRUE)  # IPD choice
#   is_rev   <- function(s)  grepl("Revisions", s, TRUE)
#   
#   na_nom  <- na[ is_tbl13(table_title) & !is_rev(series) ]
#   na_real <- na[ is_tbl24(table_title) & !is_rev(series) ]
#   
#   # ---- Helpers (same behaviour) ----
#   prefer_sa <- function(DT) {
#     if (!nrow(DT)) return(DT)
#     has_series_type <- "series_type" %in% names(DT)
#     txt <- if (has_series_type) DT$series_type else DT$.__txt
#     sa <- DT[grepl("Seasonally adjusted", txt, TRUE)]
#     if (nrow(sa)) return(sa)
#     tr <- DT[grepl("\\bTrend\\b", txt, TRUE)]
#     if (nrow(tr)) return(tr)
#     DT[grepl("\\bOriginal\\b", txt, TRUE)]
#   }
#   dedup_by_date_value <- function(DT) {
#     if (!nrow(DT)) return(DT)
#     unique(DT[, .(date, value, series, unit, data_type, series_type, table_title)])[order(date)]
#   }
#   
#   # ---- Selectors (exactly as in your loop) ----
#   is_gen_gov_total <- function(s) grepl("^\\s*General\\s+government\\s*;\\s*", s, TRUE) &&
#     !grepl("National|State and local", s, TRUE)
#   is_gfce    <- function(s) grepl("Final\\s+consumption\\s+expenditure\\s*;", s, TRUE)
#   is_gfcf    <- function(s) grepl("Gross\\s+fixed\\s+capital\\s+formation\\s*;", s, TRUE)
#   is_bad_var <- function(s) grepl("Percentage\\s+changes|Index", s, TRUE)
#   
#   is_nom  <- function(s) grepl("Current\\s*prices?", s, TRUE) || grepl("\\bCP\\b", s, TRUE)
#   is_real <- function(s) grepl("Chain\\s*volume|Volume\\s*chain|Chain-?volume|Volume\\s*measures", s, TRUE) ||
#     grepl("\\bCVM\\b|\\bvolume\\b", s, TRUE)
#   is_gdp  <- function(s) grepl("\\bGross\\s+domestic\\s+product\\b|\\bGDP\\b", s, TRUE)
#   
#   # ---- Apply the same filters up to selection (no annualising) ----
#   rows_gen_total_nom <- vapply(na_nom$series,  is_gen_gov_total, 1L)
#   rows_not_bad_nom   <- !vapply(na_nom$series, is_bad_var,       1L)
#   gg_nom  <- na_nom[rows_gen_total_nom & rows_not_bad_nom]
#   
#   rows_gen_total_real <- vapply(na_real$series,  is_gen_gov_total, 1L)
#   rows_not_bad_real   <- !vapply(na_real$series, is_bad_var,       1L)
#   gg_real <- na_real[rows_gen_total_real & rows_not_bad_real]
#   
#   gfce_nom_q  <- gg_nom [ vapply(gg_nom$.__txt,  is_nom,  1L) & vapply(gg_nom$series,  is_gfce, 1L) ]
#   gfcf_nom_q  <- gg_nom [ vapply(gg_nom$.__txt,  is_nom,  1L) & vapply(gg_nom$series,  is_gfcf, 1L) ]
#   gfce_real_q <- gg_real[ vapply(gg_real$.__txt, is_real, 1L) & vapply(gg_real$series, is_gfce, 1L) ]
#   gfcf_real_q <- gg_real[ vapply(gg_real$.__txt, is_real, 1L) & vapply(gg_real$series, is_gfcf, 1L) ]
#   
#   gdp_nom_q  <- na_nom [ vapply(na_nom$series,  is_gdp, 1L) & vapply(na_nom$.__txt,  is_nom,  1L) ]
#   gdp_real_q <- na_real[ vapply(na_real$series, is_gdp, 1L) & vapply(na_real$.__txt, is_real, 1L) ]
#   
#   # Respect your prefer_sa + dedup steps
#   gfce_nom_q  <- dedup_by_date_value(prefer_sa(gfce_nom_q))
#   gfcf_nom_q  <- dedup_by_date_value(prefer_sa(gfcf_nom_q))
#   gfce_real_q <- dedup_by_date_value(prefer_sa(gfce_real_q))
#   gfcf_real_q <- dedup_by_date_value(prefer_sa(gfcf_real_q))
#   gdp_nom_q   <- dedup_by_date_value(prefer_sa(gdp_nom_q))
#   gdp_real_q  <- dedup_by_date_value(prefer_sa(gdp_real_q))
#   
#   # ---- Return the unique series lists (what you wanted) ----
#   uniq <- function(DT) unique(DT[, .(series, unit, data_type, series_type, table_title)])
#   res <- list(
#     GFCE_nom  = uniq(gfce_nom_q),
#     GFCE_real = uniq(gfce_real_q),
#     GFCF_nom  = uniq(gfcf_nom_q),
#     GFCF_real = uniq(gfcf_real_q),
#     GDP_nom   = uniq(gdp_nom_q),
#     GDP_real  = uniq(gdp_real_q)
#   )
#   
#   # quick printed summary
#   cat("\nSelected unique series counts:\n")
#   print(data.table(
#     slice = names(res),
#     n_series = vapply(res, nrow, integer(1))
#   ))
#   
#   # show a few entries from each slice
#   for (nm in names(res)) {
#     cat("\n---", nm, "(showing up to", max_show, ") ---\n")
#     print(head(res[[nm]][order(series)], max_show))
#   }
#   
#   invisible(res)
# }
# 
# sel <- preview_gfce_gdp_selection(check_local = TRUE)
# # See exactly which series will be used:
# sel$GFCE_nom
# sel$GDP_nom
# 
# # Just counts:
# sapply(sel, nrow)
# 
# 
# #base_data[base_data$table_title == "Table 3. Expenditure on Gross Domestic Product (GDP), Current prices"]$series
########

check <- read_abs("5206.0")
setDT(check)

unique(check$table_no)

unique(check[startsWith(table_no,"5206017")]$series) # As we want both household transfers, business transfers, and interest we just want the "total income payable" category

check[table_no =="5206017_gen_govt_income_account" & series == "Total income payable ;"]

########


# -------------------- ABS 5206.0: GFCE, GFCF, GDP --------------------
# Pulls General government totals (excludes National/State & local),
# GFCE + GFCF (both bases where available), and GDP (nominal & chain volume).
get_gfce_gdp <- function(freq = c("FY","CY"), check_local = TRUE, verbose = TRUE) {
  freq <- match.arg(freq)
  
  # ---- Load & prep ----
  na <- as.data.table(read_abs("5206.0", check_local = check_local))
  if (!nrow(na)) stop("read_abs('5206.0') returned 0 rows.")
  if (!inherits(na$date, "Date")) na[, date := as.Date(date)]
  unit_guard <- grepl("\\$\\s*Million(s)?\\b", na$unit, ignore.case = TRUE)
  na <- na[unit_guard]
  
  # Coerce table_no to character for reliable matching
  if (!"table_no" %in% names(na)) stop("Expected column 'table_no' not found.")
  if (!is.character(na$table_no)) na[, table_no := as.character(table_no)]
  
  # Build combined text field across available metadata columns
  txt_cols <- intersect(c("series","table_title","unit","data_type","series_type"), names(na))
  if (!length(txt_cols)) txt_cols <- "series"
  for (cc in txt_cols) if (!is.character(na[[cc]])) set(na, j = cc, value = as.character(na[[cc]]))
  na[, .__txt := do.call(paste, c(.SD, list(sep = " | "))), .SDcols = txt_cols]
  
  # ---- Helpers ----
  is_tbl13 <- function(tt) grepl("^\\s*Table\\s*(3)\\b", tt, TRUE) # Table 1 unnecessary
  is_tbl24 <- function(tt) grepl("^\\s*Table\\s*(2)\\b", tt, TRUE) # Choose IPD instead of chain-price indexes
  is_rev   <- function(s)  grepl("Revisions", s, TRUE)
  
  #
  is_gen_gov_total <- function(s) grepl("^\\s*General\\s+government\\s*;\\s*", s, TRUE) &&
    !grepl("National|State and local", s, TRUE)
  is_gfce    <- function(s) grepl("Final\\s+consumption\\s+expenditure\\s*;", s, TRUE)
  is_gfcf    <- function(s) grepl("Gross\\s+fixed\\s+capital\\s+formation\\s*;", s, TRUE)
  is_bad_var <- function(s) grepl("Percentage\\s+changes|Index", s, TRUE)
  
  is_nom  <- function(s) grepl("Current\\s*prices?", s, TRUE) || grepl("\\bCP\\b", s, TRUE)
  is_real <- function(s) grepl("Chain\\s*volume|Volume\\s*chain|Chain-?volume|Volume\\s*measures", s, TRUE) ||
    grepl("\\bCVM\\b|\\bvolume\\b", s, TRUE)
  is_gdp  <- function(s) grepl("\\bGross\\s+domestic\\s+product\\b|\\bGDP\\b", s, TRUE)
  
  # ---- Filter slices (nominal vs real) ----
  # Nominal (Tables 1/3)
  na_nom  <- na[ is_tbl13(table_title) & !is_rev(series) ]
  rows_gen_total_nom <- vapply(na_nom$series,  is_gen_gov_total, 1L)
  rows_not_bad_nom   <- !vapply(na_nom$series, is_bad_var,       1L)
  gg_nom  <- na_nom[rows_gen_total_nom & rows_not_bad_nom]
  
  # Real (Tables 2/4)
  na_real <- na[ is_tbl24(table_title) & !is_rev(series) ]
  rows_gen_total_real <- vapply(na_real$series,  is_gen_gov_total, 1L)
  rows_not_bad_real   <- !vapply(na_real$series, is_bad_var,       1L)
  gg_real <- na_real[rows_gen_total_real & rows_not_bad_real]
  
  # Basis splits
  gfce_nom_q  <- gg_nom [ vapply(gg_nom$.__txt,  is_nom,  1L) & vapply(gg_nom$series,  is_gfce, 1L) ] |> prefer_sa() |> dedup_by_date_value()
  gfcf_nom_q  <- gg_nom [ vapply(gg_nom$.__txt,  is_nom,  1L) & vapply(gg_nom$series,  is_gfcf, 1L) ] |> prefer_sa() |> dedup_by_date_value()
  
  gfce_real_q <- gg_real[ vapply(gg_real$.__txt, is_real, 1L) & vapply(gg_real$series, is_gfce, 1L) ] |> prefer_sa() |> dedup_by_date_value()
  gfcf_real_q <- gg_real[ vapply(gg_real$.__txt, is_real, 1L) & vapply(gg_real$series, is_gfcf, 1L) ] |> prefer_sa() |> dedup_by_date_value()
  
  # GDP: nominal from 1/3, real from 2/4 (headline, dedup)
  gdp_nom_q  <- na_nom [ vapply(na_nom$series,  is_gdp, 1L) & vapply(na_nom$.__txt,  is_nom,  1L) ] |> prefer_sa() |> dedup_by_date_value()
  gdp_real_q <- na_real[ vapply(na_real$series, is_gdp, 1L) & vapply(na_real$.__txt, is_real, 1L) ] |> prefer_sa() |> dedup_by_date_value()
  
  print(gdp_nom_q)
  print(gfce_nom_q)
  
  # ---- Total income payable (Table 17 / 5206017) ----
  is_tip <- function(s) grepl("^\\s*Total\\s+income\\s+payable\\s*;\\s*$", s, TRUE)
  tip_q  <- na[ table_no == "5206017_gen_govt_income_account"  & !is_rev(series) & series == "Total income payable ;" ] |> 
    prefer_sa() |> dedup_by_date_value()
  
  # ---- Diagnostics if subjects totally missing ----
  if (!nrow(gfce_nom_q) && !nrow(gfce_real_q) && !nrow(gfcf_nom_q) && !nrow(gfcf_real_q))
    stop("No General government GFCE/GFCF rows after filters (Tables 1/3 for nominals, 2/4 for reals).")
  if (!nrow(gdp_nom_q) && !nrow(gdp_real_q))
    stop("No GDP rows after filters.")
  if (!nrow(tip_q))
    warning("No 'Total income payable ;' series found in Table 17 (5206017).")
  
  # ---- Annualise ----
  gfce_nom_a   <- ann(gfce_nom_q,   "gfce_nom")
  gfce_real_a  <- ann(gfce_real_q,  "gfce_real")
  gfcf_nom_a   <- ann(gfcf_nom_q,   "gfcf_nom")
  gfcf_real_a  <- ann(gfcf_real_q,  "gfcf_real")
  gdp_nom_a    <- ann(gdp_nom_q,    "gdp_nom")
  gdp_real_a   <- ann(gdp_real_q,   "gdp_real")
  tip_nom_a    <- if (nrow(tip_q)) ann(tip_q, "total_income_payable_nom") else data.table(year=integer(), total_income_payable_nom=double())
  
  out <- Reduce(function(x, y) merge(x, y, by = "year", all = TRUE),
                list(gfce_nom_a, gfce_real_a, gfcf_nom_a, gfcf_real_a, gdp_nom_a, gdp_real_a, tip_nom_a))
  setorder(out, year)
  
  if (isTRUE(verbose)) {
    yr <- function(d) if (nrow(d)) paste(range(d$year, na.rm = TRUE), collapse = " – ") else "∅"
    message(
      "Year ranges — ",
      paste(
        paste0("GFCE_nom:",  yr(gfce_nom_a)),
        paste0("GFCE_real:", yr(gfce_real_a)),
        paste0("GFCF_nom:",  yr(gfcf_nom_a)),
        paste0("GFCF_real:", yr(gfcf_real_a)),
        paste0("GDP_nom:",   yr(gdp_nom_a)),
        paste0("GDP_real:",  yr(gdp_real_a)),
        paste0("TIP_nom:",   yr(tip_nom_a)),
        sep = " | "
      )
    )
  }
  
  out
}




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
    lo <- as.integer(nums[1]); hi <- if (grepl("over|and over|\\+", x)) 120L else lo
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
    shares_a <- erp_m[, .(share = mean(share, na.rm = TRUE)), by = .(year = fy_end_year(date), group)]
    erp_tot_a <- erp_tot[, .(pop_total = mean(pop_total, na.rm = TRUE)), by = .(year = fy_end_year(date))]
  } else {
    shares_a <- erp_m[, .(share = mean(share, na.rm = TRUE)), by = .(year = year(date), group)]
    erp_tot_a <- erp_tot[, .(pop_total = mean(pop_total, na.rm = TRUE)), by = .(year = year(date))]
  }
  
  shares_w <- dcast(shares_a, year ~ group, value.var = "share")
  for (g in c("0_14","15_34","35_54","55_64","65p")) if (!g %in% names(shares_w)) shares_w[, (g) := NA_real_]
  setcolorder(shares_w, c("year","0_14","15_34","35_54","55_64","65p"))
  
  merge(erp_tot_a, shares_w, by = "year", all = TRUE)[order(year)]
}

get_terms_of_trade <- function(freq = c("FY","CY"), check_local = TRUE) {
  freq <- match.arg(freq)
  bo <- as.data.table(read_abs("5302.0", check_local = check_local))
  if (!inherits(bo$date, "Date")) bo[, date := as.Date(date)]
  
  tot_q <- bo[table_no == "530205" & series == "Terms of Trade ;  Goods and Services ;" & series_type == "Seasonally Adjusted"]
  
  # Build text field & pick ToT index rows (exclude pct changes)
  # txt_cols <- intersect(
  #   c("series","table_title","unit","data_type","series_type"),
  #   names(bo)
  # )
  # if (!length(txt_cols)) txt_cols <- "series"
  # 
  # # Ensure all candidate cols are character
  # for (cc in txt_cols) {
  #   if (!is.character(bo[[cc]])) set(bo, j = cc, value = as.character(bo[[cc]]))
  # }
  # 
  # bo[, "__txt" := do.call(paste, c(.SD, list(sep = " | "))), .SDcols = txt_cols]
  # 
  # is_tot <- grepl("terms\\s*of\\s*trade", bo[["__txt"]], ignore.case = TRUE) &
  #   grepl("index", bo[["__txt"]], ignore.case = TRUE) &
  #   !grepl("percentage\\s*changes|index\\s*changes", bo[["__txt"]], ignore.case = TRUE)
  # 
  # tot_q  <- bo[is_tot]
  # 
  # # Prefer SA → Trend → Original
  # prefer_sa <- function(DT) {
  #   sa <- DT[grepl("Seasonally adjusted", DT[["__txt"]], ignore.case = TRUE)]
  #   if (nrow(sa)) return(sa)
  #   tr <- DT[grepl("\\bTrend\\b", DT[["__txt"]], ignore.case = TRUE)]
  #   if (nrow(tr)) return(tr)
  #   DT[grepl("\\bOriginal\\b", DT[["__txt"]], ignore.case = TRUE)]
  # }
  # 
  # tot_q <- prefer_sa(tot_q)
  # if (!nrow(tot_q)) stop("Could not locate ToT index in 5302.0")
  
  tot_q_unq <- unique(tot_q[, .(date, value)])
  
  print(nrow(tot_q_unq))
  print(nrow(tot_q))
  
  stopifnot(nrow(tot_q) == nrow(tot_q_unq)) 
  
  annualise(tot_q, out_name = "tot_index", mode = "mean", freq = freq)[order(year)]
}



# -------------------- Build dataset: spend/GDP and age shares --------------------
build_share_dataset <- function(freq = c("FY","CY"),
                                measure = c("GFCE","GFCE_plus_GFCF","Expenditures"),
                                share_basis = c("nominal","real"),
                                check_local = TRUE) {
  freq        <- match.arg(freq)
  measure     <- match.arg(measure)
  share_basis <- match.arg(share_basis)
  
  na  <- get_gfce_gdp(freq = freq, check_local = check_local)
  erp <- get_erp_total_and_shares(freq = freq, check_local = check_local)
  dt  <- merge(na, erp, by = "year", all = FALSE)
  if (!nrow(dt)) stop("No overlapping years between 5206.0 and 3101.0.")
  
  # Construct spend/GDP according to chosen basis/measure
  gov_level <- switch(
    measure,
    "GFCE" = {
      if (share_basis == "nominal") dt$gfce_nom else dt$gfce_real
    },
    "GFCE_plus_GFCF" = {
      if (share_basis == "nominal") dt$gfce_nom + dt$gfcf_nom else dt$gfce_real + dt$gfcf_real
    },
    "Expenditures" = {
      # Transfers only available in nominal from Table 17
      if (share_basis == "real") {
        stop("Transfers (‘Total income payable ;’) are only available in nominal terms. ",
             "Select share_basis = 'nominal' or choose a different measure.")
      }
      if (!"total_income_payable_nom" %in% names(dt)) {
        stop("Column 'total_income_payable_nom' not found. ",
             "Ensure get_gfce_gdp() is the updated version that adds Table 17 ‘Total income payable ;’.")
      }
      dt$gfce_nom + dt$gfcf_nom + dt$total_income_payable_nom
    },
    
    stop("Unhandled measure: ", measure)
  )
  
  gdp_level <- if (share_basis == "nominal") dt$gdp_nom else dt$gdp_real
  if (anyNA(gov_level) || anyNA(gdp_level)) {
    stop("Missing series for chosen share_basis/measure in 5206.0 pull.")
  }
  
  dt[, gov_gdp := gov_level / gdp_level]
  
  # ----- drivers that need levels -----
  # Relative price of GFCE vs GDP (Baumol/deflator ratio), standardised
  dt[, p_gfce := gfce_nom / gfce_real]
  dt[, p_gdp  := gdp_nom  / gdp_real]
  dt[, rp_g   := as.numeric(scale(p_gfce / p_gdp, center = TRUE, scale = TRUE))]
  
  # Population growth (scale effect)
  # pop_total comes from get_erp_total_and_shares()
  if (!"pop_total" %in% names(dt)) stop("pop_total not found (ERP merge failed).")
  setorder(dt, year)
  dt[, dln_pop := c(NA_real_, diff(log(pop_total)))]
  
  # Keep required columns (don’t drop what we just created)
  need_cols <- c("year", "gov_gdp",
                 "0_14","15_34","35_54","55_64","65p",
                 "pop_total", "rp_g",# "unemp", #"dln_pop",
                 # keep these in case you need them downstream
                 "gfce_nom","gfce_real","gdp_nom","gdp_real")
  miss <- setdiff(need_cols, names(dt))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  
  dt <- dt[, ..need_cols]
  # First year will have NA dln_pop; complete.cases() will drop it (ok for your pipeline)
  dt <- dt[complete.cases(dt)]
  setorder(dt, year)
  
  # Age-share sanity
  dt[, age_sum := `0_14` + `15_34` + `35_54` + `55_64` + `65p`]
  if (any(abs(dt$age_sum - 1) > 0.03)) warning("Some years' age shares deviate from 1 by >3%.")
  dt[, age_sum := NULL]
  
  dt[]
}

#test <- build_share_dataset(freq = freq,measure = measure,share_basis = share_basis)

# -------------------- Shapley utilities --------------------
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
  # force numeric (defensive against character/factor)
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
  # turn the one-row lists into named numeric vectors
  v0 <- setNames(as.numeric(unlist(x0[common], use.names = FALSE)), common)
  v1 <- setNames(as.numeric(unlist(x1[common], use.names = FALSE)), common)
  sum(bS[common] * (v1 - v0))
}


shapley_regression_pair_general <- function(dt, y0, y1, features, y_col = "gov_gdp", conditional = TRUE) {
  stopifnot(y0 %in% dt$year, y1 %in% dt$year)
  d0 <- dt[year == y0]; d1 <- dt[year == y1]
  X  <- dt[, ..features]
  y  <- as.numeric(dt[[y_col]])
  
  # numeric & robust helpers reused
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
  
  # Conditional Shapley
  subsets <- .all_subsets(features)
  subsets <- Filter(function(S) length(S) > 0L, subsets)  # drop ∅
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


# R^2 of the (no-intercept) regression y ~ shares (age-only model)
age_model_r2 <- function(dt, groups = c("0_14","15_34","35_54","55_64","65p")) {
  cols <- as.data.frame(dt[, ..groups])
  for (j in seq_along(cols)) cols[[j]] <- as.numeric(cols[[j]])
  Xi <- as.matrix(cols)
  yi <- as.numeric(dt$gov_gdp)
  fit <- lm.fit(x = Xi, y = yi)
  yhat <- as.vector(Xi %*% coef(fit))
  sst <- sum((yi - mean(yi))^2, na.rm = TRUE)
  sse <- sum((yi - yhat)^2,     na.rm = TRUE)
  list(r2 = ifelse(sst > 0, 1 - sse/sst, NA_real_), yhat = yhat)
}

##### Coefficients to be extracted
# Full-model coefficients (one fit, no intercept)
coef_full <- function(dt, features, y_col = "gov_gdp") {
  Xdf <- as.data.frame(dt[, ..features]); for (j in seq_along(Xdf)) Xdf[[j]] <- as.numeric(Xdf[[j]])
  Xi  <- as.matrix(Xdf)
  yi  <- as.numeric(dt[[y_col]])
  b   <- as.numeric(coef(lm.fit(x = Xi, y = yi)))
  names(b) <- colnames(Xi)
  b
}

# All subset coefficients used in conditional Shapley (non-empty subsets)
coef_subsets <- function(dt, features, y_col = "gov_gdp") {
  .all_subsets <- function(v) { out <- list(character(0)); if (length(v)) for (m in 1:length(v)) out <- c(out, combn(v, m, simplify=FALSE)); out }
  .fit_subset  <- function(y, X, S) {
    if (!length(S)) return(numeric(0))
    Xdf <- as.data.frame(X[, ..S]); for (j in seq_along(Xdf)) Xdf[[j]] <- as.numeric(Xdf[[j]])
    Xi <- as.matrix(Xdf); b <- as.numeric(coef(lm.fit(x = Xi, y = y)))
    keep <- !is.na(b); if (!any(keep)) return(numeric(0))
    names(b) <- colnames(Xi); b[keep]
  }
  X <- dt[, ..features]
  y <- as.numeric(dt[[y_col]])
  subs <- .all_subsets(features)
  subs <- Filter(function(S) length(S) > 0L, subs)  # drop ∅
  rbindlist(lapply(subs, function(S){
    bS <- .fit_subset(y, X, S)
    if (!length(bS)) return(NULL)
    data.table(subset = paste(sort(S), collapse="|"),
               feature = names(bS),
               beta = as.numeric(bS))
  }))
}

####



# -------------------- Wrappers: four bins & panel --------------------
fourbin_pairs <- function(yrs) {
  brks <- unique(as.integer(quantile(yrs, probs = c(0,.25,.5,.75,1), type = 1)))
  as.data.table(list(bin = findInterval(yrs, brks, all.inside = TRUE), year = yrs))[
    , .(y0 = min(year), y1 = max(year)), by = bin][order(bin)]
}

# Panel / four-bin using your existing run_shapley_blocks but with 'features'
run_shapley_blocks <- function(dt, conditional = TRUE, features) {
  setorder(dt, year)
  # four bins
  brks <- unique(as.integer(quantile(unique(dt$year), probs = c(0,.25,.5,.75,1), type = 1)))
  dt[, bin := findInterval(year, brks, all.inside = TRUE)]
  bounds <- dt[, .(y0 = min(year), y1 = max(year)), by = bin][order(bin)]
  if (nrow(bounds) > 4L) bounds <- bounds[1:4]
  
  four_res <- rbindlist(lapply(1:nrow(bounds), function(i) {
    sh <- shapley_regression_pair_general(dt, bounds$y0[i], bounds$y1[i],
                                          features = features, conditional = conditional)
    data.table(y0 = bounds$y0[i], y1 = bounds$y1[i],
               d_actual = sh$d_actual, d_hat = sh$d_hat, unexplained = sh$unexplained,
               t(sh$contrib))
  }), use.names = TRUE, fill = TRUE)
  
  panel_pairs <- data.table(y0 = head(dt$year, -1L), y1 = tail(dt$year, -1L))
  panel_res <- rbindlist(lapply(1:nrow(panel_pairs), function(i) {
    sh <- shapley_regression_pair_general(dt, panel_pairs$y0[i], panel_pairs$y1[i],
                                          features = features, conditional = conditional)
    data.table(y0 = panel_pairs$y0[i], y1 = panel_pairs$y1[i],
               d_actual = sh$d_actual, d_hat = sh$d_hat, unexplained = sh$unexplained,
               t(sh$contrib))
  }), use.names = TRUE, fill = TRUE)
  
  list(four_bin = four_res, panel = panel_res)
}

# -------------------- Counterfactual: fix age shares at baseline --------------------
# Predict y_t with age shares held at baseline year b0 (using one global fit).
counterfactual_age_path <- function(dt, baseline_year,
                                    groups = c("0_14","15_34","35_54","55_64","65p")) {
  stopifnot(baseline_year %in% dt$year)
  Xi <- as.matrix(dt[, ..groups]); yi <- dt$gov_gdp
  fit <- lm.fit(x = Xi, y = yi)
  b   <- coef(fit); names(b) <- colnames(Xi)[!is.na(b)]
  
  base <- dt[year == baseline_year, ..groups]
  base_vec <- as.numeric(base[1, ..groups]); names(base_vec) <- groups
  
  # Build fixed-share matrix
  Xcf <- Xi
  for (j in seq_along(groups)) Xcf[, j] <- base_vec[j]
  
  yhat_actual <- as.vector(Xi  %*% b)
  yhat_cf     <- as.vector(Xcf %*% b)
  
  data.table(year = dt$year, y_actual = dt$gov_gdp,
             yhat_actual = yhat_actual, yhat_cf = yhat_cf,
             diff_cf = yhat_actual - yhat_cf)
}

# ==================== RUN THE EXERCISE ====================
tot_a   <- get_terms_of_trade(freq = freq, check_local = abs_check_local)
# --- (i) Cycle: Unemployment rate (example placeholder series) ---
# If you have an ABS pull for unemployment, annualise the rate:
dt_unemp <- as.data.table(read_abs("6202.0", tables = "1", check_local = abs_check_local))
dt_unemp <- dt_unemp[series == "Unemployment rate ;  Persons ;" & series_type == "Seasonally Adjusted"]
unemp_a <- annualise(dt_unemp, out_name = "unemp", mode = "mean", freq = freq)

share_dt <- build_share_dataset(freq, measure, share_basis, check_local = abs_check_local)
share_dt <- merge(share_dt, tot_a, by = "year", all.x = TRUE)
share_dt[, tot := as.numeric(scale(tot_index, center = TRUE, scale = TRUE))] # Standardise TOT so it is on a similar scale to other variables
if (exists("unemp_a")) {
  share_dt <- merge(share_dt, unemp_a, by = "year", all.x = TRUE)
}



# Remove the outlier years
share_dt[, is_outlier := year %in% outlier_years]


share_dt


# --- (ii) Relative price of GFCE vs GDP (Baumol effect) ---
share_dt[, p_gfce := gfce_nom / gfce_real]
share_dt[, p_gdp  := gdp_nom  / gdp_real]
share_dt[, rp_g   := as.numeric(scale(p_gfce / p_gdp, center = TRUE, scale = TRUE))]

# --- (iii) Population growth (scale) ---
# pop_total is built inside get_erp_total_and_shares and merged into share_dt
share_dt[, dln_pop := c(0.032, diff(log(pop_total)))] # Set the 1971 value manually


bad_counts <- sapply(share_dt[, ..features], function(x) sum(!is.finite(x)))
bad_counts

# Peek at rows with any problems across y + features
need <- c("gov_gdp", features)
share_dt[!complete.cases(share_dt[, ..need]),
         .(year, across_missing = paste(names(.SD)[colSums(!is.finite(as.matrix(.SD)))>0], collapse=", ")),
         .SDcols = need]

message("   Years: ", paste(range(share_dt$year), collapse = " – "), "  (n=", nrow(share_dt), ")")

# ── Apply sample window ─────────────────────────────────────────────────────────
if (is.finite(start_year))   share_dt <- share_dt[year >= start_year]
if (is.finite(final_year_opt)) share_dt <- share_dt[year <= final_year_opt]
stopifnot(nrow(share_dt) >= 2L)

# Baseline year for counterfactuals and labels:
if (!is.finite(baseline_year)) baseline_year <- min(share_dt$year)
if (!(baseline_year %in% share_dt$year)) {
  warning("baseline_year not in sample; snapping to first available.")
  baseline_year <- min(share_dt$year)
}

dt_est <- share_dt[!(year %in% outlier_years)]          # <- After cutting years
stopifnot(nrow(dt_est) >= 2L)

am <- age_model_r2(dt_est)
message(sprintf("▶ Age-only model R^2 = %.3f (share of variance explained by demographics).", am$r2))

message("▶ Computing Shapley contributions (", if (conditional_sv) "conditional" else "unconditional", ") …")
sv_res   <- run_shapley_blocks(dt_est, conditional = TRUE, features = features)
coef_full(dt_est, features)
coef_subsets(dt_est, features)

four_bin <- sv_res$four_bin
panel    <- sv_res$panel

# Sanity: contributions sum to d_hat; unexplained bridges d_actual - d_hat
if (nrow(four_bin)) {
  message("Four-bin: mean |unexplained| = ", sprintf("%.4f", mean(abs(four_bin$unexplained), na.rm = TRUE)))
}
if (nrow(panel)) {
  message("Panel: median |unexplained| = ", sprintf("%.4f", median(abs(panel$unexplained), na.rm = TRUE)))
}

# ===================== Dynamic table + plots driven by `features` =====================

# Choose measures to show from what's actually present in each table
four_measures  <- if (exists("four_bin"))  intersect(features, names(four_bin)) else character(0)
panel_measures <- if (exists("panel"))     intersect(features, names(panel))    else character(0)

# Label helper for R^2 (ok if `am` doesn't exist)
lab_r2 <- tryCatch({
  if (exists("am") && is.list(am) && is.finite(am$r2)) sprintf("R²(age-only)=%.3f", am$r2) else ""
}, error = function(e) "")

# -------------------- Compact summary table (four-bin) --------------------
if (exists("four_bin") && nrow(four_bin)) {
  tmp <- copy(four_bin)[, Segment := paste0(y0, "→", y1)]
  cols_order <- c("Segment", "d_actual", "d_hat", "unexplained", four_measures)
  # Only keep columns that exist (for safety)
  cols_order <- intersect(cols_order, names(tmp))
  print(tmp[, ..cols_order])
}

# -------------------- Panel plot: explained contributions (dynamic) --------------------
if (exists("panel") && nrow(panel) && length(panel_measures)) {
  panel_m <- melt(
    panel,
    id.vars       = c("y0","y1","d_actual","d_hat","unexplained"),
    measure.vars  = panel_measures,
    variable.name = "driver",
    value.name    = "contrib"
  )
  panel_m[, year := y1]
  
  p <- ggplot(panel_m, aes(x = year, y = contrib, fill = driver)) +
    geom_col() +
    geom_line(
      data = unique(panel_m[, .(year, d_actual)]),
      aes(y = d_actual, x = year, group = 1),
      inherit.aes = FALSE
    ) +
    geom_point(
      data = unique(panel_m[, .(year, d_actual)]),
      aes(y = d_actual, x = year),
      inherit.aes = FALSE
    ) +
    labs(
      title = "Regression-based Shapley of Δ(spend/GDP): explained contributions",
      subtitle = paste0("Unexplained (residual) not stacked. ", lab_r2),
      x = "Year", y = "Δ share (level points)", fill = "Driver"
    ) +
    theme_e61(legend = "bottom")
  print(p)
}

# -------------------- Four-bin (binned period) plot: explained (dynamic) --------------
if (exists("four_bin") && nrow(four_bin) && length(four_measures)) {
  four_m <- melt(
    four_bin,
    id.vars       = c("y0","y1","d_actual","d_hat","unexplained"),
    measure.vars  = four_measures,
    variable.name = "driver",
    value.name    = "contrib"
  )
  
  # Pretty segment labels and ordering
  four_m[, Segment := paste0(y0, "–", y1)]
  seg_levels <- four_bin[, paste0(y0, "–", y1)]
  four_m[, Segment := factor(Segment, levels = seg_levels)]
  
  p4 <- ggplot(four_m, aes(x = Segment, y = contrib, fill = driver)) +
    # Explained contributions (stacked)
    geom_col() +
    # Predicted total explained change (diamond)
    geom_point(
      data = unique(four_m[, .(Segment, d_hat)]),
      aes(y = d_hat, x = Segment),
      inherit.aes = FALSE,
      shape = 23, size = 3, stroke = 0.6
    ) +
    # Actual change (solid dot)
    geom_point(
      data = unique(four_m[, .(Segment, d_actual)]),
      aes(y = d_actual, x = Segment),
      inherit.aes = FALSE,
      shape = 16, size = 2.8
    ) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    coord_flip() +
    labs(
      title = "Regression-based Shapley of Δ(spend/GDP): binned periods",
      subtitle = paste0(
        "Stacks show explained contributions by driver; ● actual Δ, ◇ predicted Δ.  ",
        "Residual = actual − predicted.  ", lab_r2
      ),
      x = NULL, y = "Δ share (level points)", fill = "Driver"
    ) +
    theme_e61(legend = "bottom")
  print(p4)
}
# =====================================================================================

# # -------------------- Collapsed Four-bin plot: 65+ / Other ages / Economic effects / Residual ----
# if (exists("four_bin") && nrow(four_bin)) {
#   # Melt everything in features that is actually present
#   measure_vars <- intersect(features, names(four_bin))
#   four_m <- melt(
#     four_bin,
#     id.vars       = c("y0","y1","d_actual","d_hat","unexplained"),
#     measure.vars  = measure_vars,
#     variable.name = "driver",
#     value.name    = "contrib"
#   )
#   
#   # Collapse into groups
#   age_bins <- c("0_14","15_34","35_54","55_64")
#   four_m[, group := fcase(
#     driver == "65p",              "65+",
#     driver %in% age_bins,         "Other ages",
#     default =                     "Economic effects"
#   )]
#   
#   # Aggregate explained contributions within groups
#   four_g <- four_m[, .(contrib = sum(contrib, na.rm = TRUE)),
#                    by = .(y0, y1, d_actual, d_hat, unexplained, group)]
#   
#   # Add Residual as another category so stacks = d_actual (explained + residual)
#   residual_dt <- unique(four_m[, .(y0, y1, d_actual, d_hat, unexplained)])
#   residual_dt[, `:=`(group = "Residual", contrib = unexplained)]
#   four_g <- rbind(four_g, residual_dt[, .(y0, y1, d_actual, d_hat, unexplained, group, contrib)], use.names = TRUE)
#   
#   # Optional filter (keep your adjustment)
#   four_g <- four_g[y0 != 1972]
#   
#   # Segment labels & ordering
#   four_g[, Segment := paste0(y0, "–", y1)]
#   seg_levels <- four_bin[y0 != 1972, paste0(y0, "–", y1)]
#   four_g[, Segment := factor(Segment, levels = seg_levels)]
#   
#   # Tidy legend order
#   four_g[, group := factor(group, levels = c("65+","Other ages","Economic effects","Residual"))]
#   
#   p4c <- ggplot(four_g, aes(x = Segment, y = contrib, fill = group)) +
#     geom_col() +
#     geom_hline(yintercept = 0, linewidth = 0.4) +
#     coord_flip() +
#     labs(
#       title = "Regression-based Shapley of Δ(spend/GDP): binned periods",
#       subtitle = "Collapsed into 65+, Other ages, Economic effects, and Residual\n(Stack sums to actual Δ)",
#       x = NULL, y = "Δ share (level points)", fill = "Component"
#     ) +
#     theme_e61(legend = "bottom")
#   
#   print(p4c)
# }

# -------------------- Collapsed Four-bin plot with Residual on extremes --------------------
if (exists("four_bin") && nrow(four_bin)) {
  measure_vars <- intersect(features, names(four_bin))
  four_m <- melt(
    four_bin,
    id.vars       = c("y0","y1","d_actual","d_hat","unexplained"),
    measure.vars  = measure_vars,
    variable.name = "driver",
    value.name    = "contrib"
  )
  
  # Collapse into groups
  age_bins <- c("0_14","15_34","35_54","55_64")
  four_m[, group := fcase(
    driver == "65p",              "65+",
    driver %in% age_bins,         "Other ages",
    default =                     "Economic effects"
  )]
  
  # Aggregate explained contributions within groups
  four_g <- four_m[, .(contrib = sum(contrib, na.rm = TRUE)),
                   by = .(y0, y1, d_actual, d_hat, unexplained, group)]
  
  # Add Residual
  residual_dt <- unique(four_m[, .(y0, y1, d_actual, d_hat, unexplained)])
  residual_dt[, `:=`(group = "Residual", contrib = unexplained)]
  four_g <- rbind(four_g, residual_dt[, .(y0, y1, d_actual, d_hat, unexplained, group, contrib)], use.names = TRUE)
  
  # Optional filter
  four_g <- four_g[y0 != 1972]
  
  # Segment labels & ordering
  four_g[, Segment := paste0(y0, "–", y1)]
  seg_levels <- four_bin[y0 != 1972, paste0(y0, "–", y1)]
  four_g[, Segment := factor(Segment, levels = seg_levels)]
  
  # ----- Key: force Residual to extremes -----
  four_g[, stack_order := {
    if (all(contrib[group=="Residual"] <= 0)) {
      factor(group, levels = c("Residual","65+","Other ages","Economic effects"))
    } else {
      factor(group, levels = c("65+","Other ages","Economic effects","Residual"))
    }
  }, by = Segment]
  
  # Build the plot
  p4c <- ggplot(four_g, aes(x = Segment, y = contrib*100, fill = stack_order)) +
    geom_col() +
    # Big solid dot = actual Δ
    geom_point(
      data = unique(four_g[, .(Segment, d_actual)]),
      aes(x = Segment, y = d_actual*100),
      inherit.aes = FALSE,
      shape = 16, size = 5, color = "black"
    ) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    coord_flip() +
    labs_e61(
      title = "Demographic trends dominate lift in spending",
      subtitle = "Regression-based Shapley of Δ(spend/GDP)",
      x = NULL, y = "Δ share (level points)", fill = "Component",
      footnotes = c(paste0("Black dot reflects the change in ",measure," to GDP."),"Effects represent association between the change in the category and changes in spending to GDP.","Economic Effects reflect variation explained by changes in population, relative government costs, and terms of trade."),
      sources = c("e61","ABS")
    ) +
    plab(c("65+","Other ages","Economic effects***","Residual"),y=rep(2,4),x=c(2.2,1.7,1.2,0.7))
  
  print(p4c)
}

save_e61(paste0("Shapley_",measure,".png"),res=2)
save_e61(paste0("Shapley_",measure,".svg"))

# Counterfactual example: hold age structure fixed at earliest year
b0 <- min(dt_est$year)
cf <- counterfactual_age_path(dt_est, baseline_year = b0)
message("▶ Counterfactual vs actual (age fixed at ", b0, "): showing first few rows")
print(head(cf))

# Save key outputs
fwrite(share_dt, paste0("age_share_input_",measure,".csv"))
if (nrow(four_bin)) fwrite(four_bin, paste0("shapley_age_fourbin_",measure,".csv"))
if (nrow(panel))    fwrite(panel,    paste0("shapley_age_panel_",measure,".csv"))
fwrite(cf,          paste0("age_counterfactual_path_",measure,".csv"))

message("Done.")



######### Add counterfactual paths
counterfactual_paths_by_block <- function(dt,
                                          features,
                                          blocks,
                                          baseline_year,
                                          y_col = "gov_gdp") {
  stopifnot(baseline_year %in% dt$year)
  # 1) Fit full model (no intercept)
  Xdf <- as.data.frame(dt[, ..features]); for (j in seq_along(Xdf)) Xdf[[j]] <- as.numeric(Xdf[[j]])
  Xi  <- as.matrix(Xdf)
  yi  <- as.numeric(dt[[y_col]])
  b   <- as.numeric(coef(lm.fit(x = Xi, y = yi)))
  names(b) <- colnames(Xi)
  
  # 2) Actual fitted path (for reference)
  yhat_full <- as.vector(Xi %*% b)
  
  # 3) Build counterfactual for each block
  base_row <- dt[year == baseline_year, ..features]
  out <- data.table(year = dt$year,
                    y_actual = yi,
                    yhat_full = yhat_full)
  
  for (nm in names(blocks)) {
    blk <- intersect(blocks[[nm]], features)
    if (!length(blk)) next
    Xcf <- Xi
    # Set all columns in the block to their baseline values
    for (col in blk) {
      Xcf[, col] <- as.numeric(base_row[[col]])
    }
    out[[paste0("yhat_cf_", nm)]] <- as.vector(Xcf %*% b)
    out[[paste0("impact_", nm)]]  <- out$yhat_full - out[[paste0("yhat_cf_", nm)]]
  }
  
  # 4) Optional: "no changes at all" (everything at baseline)
  X0 <- Xi
  for (col in features) X0[, col] <- as.numeric(base_row[[col]])
  out[["yhat_cf_AllFixed"]] <- as.vector(X0 %*% b)
  
  out[]
}

# -------------------- Quick plot for counterfactuals --------------------
plot_counterfactuals <- function(cf_dt,
                                 blocks_to_show = NULL,
                                 title = "Counterfactual paths (holding blocks fixed at baseline)",
                                 footnotes = "NA") {
  if (is.null(blocks_to_show)) {
    blocks_to_show <- gsub("^yhat_cf_", "", grep("^yhat_cf_", names(cf_dt), value = TRUE))
  }
  keep <- c("year", "y_actual", "yhat_full",
            paste0("yhat_cf_", blocks_to_show))
  long <- melt(cf_dt[, ..keep],
               id.vars = "year",
               variable.name = "series",
               value.name = "value")
  long[, series := factor(series,
                          levels = c("y_actual", "yhat_full",
                                     paste0("yhat_cf_", blocks_to_show)),
                          labels = c("Actual", "Fitted",
                                     paste0("CF: hold ", blocks_to_show, " at baseline")))]
  ggplot(long[series %in% c("Actual","CF: hold Demography at baseline")], aes(year, value*100, colour = series)) +
    geom_line() +
    geom_hline(yintercept = 0) +
    scale_y_continuous_e61() +
    labs_e61(title = title, footnotes = footnotes, x = NULL, y = "Percent of GDP",sources = c("ABS","e61")) +
    plab(c("GFCE/GDP","Demographic Adjusted"),x=c(1980,1980),y=c(21,16))
  #theme_e61(legend = "bottom") 
}

# Define blocks (edit as you like)
blocks <- list(
  Demography = c("0_14","15_34","35_54","55_64","65p"),
  RelPrices  = c("rp_g"),
  #PopGrowth  = c("dln_pop"),
  UR         = c("unemp"),
  ToT        = intersect("tot", names(share_dt))
)

cf_paths <- counterfactual_paths_by_block(
  dt       = share_dt,
  features = features,
  blocks   = blocks,
  baseline_year = baseline_year,
  y_col    = "gov_gdp"
)

print(plot_counterfactuals(
  cf_dt = cf_paths,
  blocks_to_show = names(blocks),
  title = "Counterfactual Government Expenditure to GDP",
  footnotes = c(sprintf("Comparison includes a baseline %s and sample period %s–%s)",
                        baseline_year, min(share_dt$year), max(share_dt$year)),"Government spending estimated from National Accounts data as GFCE + GFCF + Total income payable. This includes interest and current transfers to households and firms.")
))

save_e61(paste0("Demographic_Adjusted_",measure,".png"),res=2)
save_e61(paste0("Demographic_Adjusted_",measure,".svg"))

# Save the table if useful
fwrite(cf_paths, paste0("counterfactual_paths_generalgov_",measure,".csv"))

# ===================== Helpers: manual Shapley by custom ranges =====================
nearest_year <- function(target, years) years[which.min(abs(years - target))]

normalise_segments <- function(ranges, years) {
  # ranges: list(c(y0,y1), c(y0,y1), ...)
  stopifnot(is.list(ranges), length(ranges) >= 1L)
  segs <- rbindlist(lapply(ranges, function(r) {
    stopifnot(length(r) == 2L)
    y0 <- nearest_year(min(r), years)
    y1 <- nearest_year(max(r), years)
    data.table(y0 = y0, y1 = y1)
  }))
  segs <- unique(segs[y0 < y1])  # drop dup / invalid
  if (!nrow(segs)) stop("No valid segments after snapping to available years.")
  segs[]
}

shapley_by_segments <- function(dt, segments, features, conditional = TRUE) {
  # segments: data.table with y0, y1 (inclusive ends)
  setorder(dt, year)
  # compute once per segment
  rbindlist(lapply(1:nrow(segments), function(i) {
    y0 <- segments$y0[i]; y1 <- segments$y1[i]
    sh <- shapley_regression_pair_general(
      dt, y0 = y0, y1 = y1, features = features, conditional = conditional
    )
    data.table(y0 = y0, y1 = y1,
               d_actual = sh$d_actual, d_hat = sh$d_hat, unexplained = sh$unexplained,
               t(sh$contrib))
  }), use.names = TRUE, fill = TRUE)
}
# ================================================================================





# ================================================================================
### Adding an additional projection based on expected future demographic pressures
# Import projections of population and economic variables
# ---- Fit once on historical data, using your current features ----
b_full  <- coef_full(dt_est, features)
Xi_hist <- as.matrix(as.data.frame(dt_est[, ..features]))
yhat_hist <- as.vector(Xi_hist %*% b_full)

# ---- Read projections from Excel ----
proj_path  <- "For_shapely_projections.xlsx"     # << change if needed
proj_sheet <- "Sheet1"

proj_raw <- as.data.table(readxl::read_excel(proj_path, sheet = proj_sheet))
setnames(proj_raw, tolower(names(proj_raw)))
proj_raw <- proj_raw[year >= 2025]

# ---- Column name normalisation (accept common aliases) ----
name_map <- c(
  "year"        = "year",
  "0_14"        = "0_14",
  "15_34"       = "15_34",
  "35_54"       = "35_54",
  "55_64"       = "55_64",
  "65p"         = "65p",
  # rp_g will be provided ALREADY normalised (z). We accept 'rp_g' or 'rp_g_z'.
  "rp_g"        = "rp_g",
  "rp_g_z"      = "rp_g_z",
  # population
  "pop_total"   = "pop_total",
  "dln_pop"     = "dln_pop",
  "unemp"       = "unemp",
  # ToT: either supply tot_z OR tot_index (raw), we'll z-score tot_index
  "tot"         = "tot",         # optional if you already built tot_z into 'tot'
  "tot_z"       = "tot_z",
  "tot_index"   = "tot_index"
)
keep_cols <- intersect(names(proj_raw), names(name_map))
setnames(proj_raw, keep_cols, name_map[keep_cols])

proj <- copy(proj_raw)

# ---- Build dln_pop if only pop_total is provided ----
if (!"dln_pop" %in% names(proj) && "pop_total" %in% names(proj)) {
  setorder(proj, year)
  proj[, dln_pop := c(NA_real_, diff(log(pop_total)))]
  proj <- proj[!is.na(dln_pop)]
}

# ---- rp_g: PASS-THROUGH (already on training z-scale)
# If 'rp_g_z' exists, copy to 'rp_g'. If 'rp_g' exists, assume it's already z-scored.
if ("rp_g_z" %in% names(proj)) {
  proj[, rp_g := as.numeric(rp_g_z)]
} else if ("rp_g" %in% names(proj)) {
  proj[, rp_g := as.numeric(rp_g)]  # assume already z-scored
} else {
  stop("Projection file must include either 'rp_g' (z-scored) or 'rp_g_z'.")
}

# ---- tot: NORMALISE HERE (unless already supplied as tot_z or tot)
# Compute training RAW mean/sd from the historical 'tot_index'
if (!"tot_index" %in% names(share_dt))
  stop("Expected 'tot_index' in share_dt (it is created by get_terms_of_trade).")

mu_tot_raw <- mean(share_dt$tot_index, na.rm = TRUE)
sd_tot_raw <-  sd(share_dt$tot_index,  na.rm = TRUE)
sd_tot_raw <- ifelse(is.finite(sd_tot_raw) && sd_tot_raw > 0, sd_tot_raw, 1)

if ("tot_z" %in% names(proj)) {
  proj[, tot := as.numeric(tot_z)]
} else if ("tot_index" %in% names(proj)) {
  proj[, tot := as.numeric((tot_index - mu_tot_raw) / sd_tot_raw)]
} else if ("tot" %in% names(proj)) {
  # If you exported 'tot' already z-scored on the training scale, pass it through
  proj[, tot := as.numeric(tot)]
} else {
  stop("Provide either 'tot_z' or 'tot_index' (raw) or 'tot' (already z-scored).")
}

# ---- Age shares: softly renormalise if needed
age_vars <- intersect(c("0_14","15_34","35_54","55_64","65p"), names(proj))
if (length(age_vars)) {
  proj[, age_sum := rowSums(.SD), .SDcols = age_vars]
  if (any(abs(proj$age_sum - 1) > 0.03, na.rm = TRUE)) {
    warning("Some projection rows have age shares not summing to 1 (>|0.03|). Renormalising.")
    proj[, (age_vars) := lapply(.SD, \(x) x / age_sum), .SDcols = age_vars]
  }
  proj[, age_sum := NULL]
}

# ---- Keep only projection years beyond the historical max & check features ----
last_hist_year <- max(share_dt$year, na.rm = TRUE)
proj <- proj[year > last_hist_year]
stopifnot(nrow(proj) > 0)

missing_feats <- setdiff(features, names(proj))
if (length(missing_feats)) {
  stop("Projection file missing required feature columns: ",
       paste(missing_feats, collapse = ", "))
}

# ---- Predict (base) ----
predict_with <- function(DT, feat_names) {
  Xi <- as.matrix(as.data.frame(DT[, ..feat_names]))
  as.vector(Xi %*% b_full)
}

hist_dt <- data.table(
  year       = share_dt$year,
  y_actual   = share_dt$gov_gdp,
  yhat_full  = yhat_hist,
  series     = "Historical"
)

base_proj <- copy(proj[, c("year", features), with = FALSE])
base_proj[, yhat_proj := predict_with(base_proj, features)]
base_proj[, series := "Projection (base)"]

proj_all <- base_proj[, .(year, yhat_proj, series)]

# (Optional) scenario handling unchanged…

# ---- Combine for plotting ----
plot_dt <- rbind(
  hist_dt[, .(year, series = "Historical",        value = y_actual)],
  hist_dt[, .(year, series = "Fitted (in-sample)", value = yhat_full)],
  proj_all[, .(year, series, value = yhat_proj)],
  use.names = TRUE
)

# ---- Plot (percent of GDP) ----
p_proj <- ggplot() +
  geom_line(data = plot_dt[series %in% c("Historical","Fitted (in-sample)")],
            aes(year, value*100, linetype = series), linewidth = 0.9) +
  geom_line(data = plot_dt[grepl("^Projection", series)],
            aes(year, value*100, colour = series), linewidth = 1) +
  labs_e61(
    title     = paste0("Government ", measure, " as % of GDP: history, fit, and projections"),
    subtitle  = paste0("Model: no-intercept OLS on features = {", paste(features, collapse=", "),
                       "}"),
    x = NULL, y = "% of GDP",
    sources   = c("ABS", "e61", "Author projections")
  ) +
  theme_e61(legend = "bottom")

print(p_proj)

ggplot() +
  geom_line(data = plot_dt[series %in% c("Historical")],
            aes(year, value*100, linetype = series), linewidth = 0.9) +
  geom_line(data = plot_dt[grepl("^Projection", series)],
            aes(year, value*100, colour = series), linewidth = 1) +
  labs_e61(
    title     = paste0("Government ", measure, " as % of GDP: history, fit, and projections"),
    x = NULL, y = "% of GDP",
    sources   = c("ABS", "e61", "Author projections")
  ) +
  theme_e61(legend = "bottom")


ggplot(plot_dt[series != "Fitted (in-sample)"],aes(x=year,y=value*100,colour=series)) + geom_line() +
  labs_e61(
    title     = paste0("Government ", measure, " as % of GDP"),
    x = NULL, y = "% of GDP",
    sources   = c("ABS", "e61 projections")
  ) +
  plab(c("Historical","Projection"),x=c(1980,2030),y=c(41,36))


save_e61(paste0("GFCE_to_GDP_projection_", measure, ".png"), res = 2)
save_e61(paste0("GFCE_to_GDP_projection_", measure, ".svg"))


# Save projected numbers
fwrite(plot_dt, paste0("gfce_to_gdp_history_fit_projection_",measure,".csv"))

plot_dt

## Explore the data

b <- b_full                          
drv <- features

# Historical contributions
Xi_hist <- as.matrix(as.data.frame(share_dt[, ..drv]))
contrib_hist <- as.data.table(Xi_hist %*% diag(b, nrow=length(b)))
setnames(contrib_hist, drv)
contrib_hist[, year := share_dt$year]

# Projection contributions (using 'proj' you built)
Xi_proj <- as.matrix(as.data.frame(proj[, ..drv]))
contrib_proj <- as.data.table(Xi_proj %*% diag(b, nrow=length(b)))
setnames(contrib_proj, drv)
contrib_proj[, year := proj$year]

# Stack and plot quickly (which drivers bend the path?)
contrib_all <- rbind(
  melt(contrib_hist, id.vars="year", variable.name="driver", value.name="contrib")[, series:="Historical"],
  melt(contrib_proj, id.vars="year", variable.name="driver", value.name="contrib")[, series:="Projection"]
)
ggplot(contrib_all[series=="Projection"], aes(year, contrib*100, fill=driver)) +
  geom_col() +
  geom_line(aes(x=year,y = tapply(contrib, year, sum)[as.character(year)]*100, group=1),
            inherit.aes=FALSE) +
  labs_e61(title="Projected contributions to gov_gdp",
       y="pp of GDP", x=NULL, fill="Driver") + theme_e61(legend = "bottom")

round(cor(share_dt[, .( `0_14`,`15_34`,`35_54`,`55_64`,`65p`)]),2)


##### Alternatives
## ============================================================
## Alternative model: simple age-share regression with TS option
## ============================================================

# ------------------------------------------------------------
# 1. Use the same dataset you already prepared
# ------------------------------------------------------------
# Optional scaling choice:
# Keep raw shares (0–1) or convert to percentage points if clearer:
dt_est[, `:=`(
  `0_14`  = `0_14`,
  `15_34` = `15_34`,
  `35_54` = `35_54`,
  `55_64` = `55_64`,
  `65p`   = `65p`
)]

# ------------------------------------------------------------
# 2. Simple share-based model (levels)
# ------------------------------------------------------------
m_share <- feols(
  gov_gdp ~ `0_14` + `15_34` + `35_54` + `55_64` + `65p` +
    rp_g + tot + dln_pop + unemp,
  data = dt_est
)
summary(m_share)

# ------------------------------------------------------------
# 3. Optional: enable time-series dynamics
# ------------------------------------------------------------
# Example A: add a lag of spending (AR(1) structure)
dt_est[, gov_gdp_l1 := shift(gov_gdp, 1)]

m_share_lag <- feols(
  gov_gdp ~ gov_gdp_l1 +
    `0_14` + `15_34` + `35_54` + `55_64` + `65p` +
    rp_g + tot + dln_pop + unemp,
  data = dt_est
)

summary(m_share_lag)

# Example B: first-difference version
dt_est[, d_gov_gdp := c(NA, diff(gov_gdp))]
age_vars <- c("0_14","15_34","35_54","55_64","65p")
dt_est[, paste0("d_", age_vars) := lapply(.SD, function(x) c(NA, diff(x))), .SDcols = age_vars]
m_share_diff <- feols(
  d_gov_gdp ~ d_0_14 + d_15_34 + d_35_54 + d_55_64 + d_65p +
    rp_g + tot + dln_pop + unemp,
  data = dt_est
)

# ------------------------------------------------------------
# 4. Compare models
etable(m_share, m_share_lag, m_share_diff, dict = c(
  `0_14`="Age 0-14", `15_34`="Age 15-34", `35_54`="Age 35-54",
  `55_64`="Age 55-64", `65p`="Age 65+", rp_g="Relative prices",
  tot="Terms of trade", dln_pop="Pop growth",  unemp = "UR"
))




# ============================================================
# Lag model projection using Excel-based projections to 2071
# ============================================================
# Add lags
setorder(share_dt, year)
share_dt[, gov_gdp_l1 := shift(gov_gdp, 1)]

# Define estimation sample:
#    drop (i) outlier years themselves and (ii) any year whose lag is an outlier
drop_rows <- share_dt$year %in% outlier_years | (share_dt$year - 1L) %in% outlier_years
dt_est <- share_dt[!drop_rows]
stopifnot(nrow(dt_est) >= 2L)

# 3) Estimate the lag model (age shares in 0–1; keep your other Xs)
m_share_lag <- feols(
  gov_gdp ~ gov_gdp_l1 +
    `0_14` + `15_34` + `35_54` + `55_64` + `65p` +
    rp_g + tot + dln_pop + unemp,
  data = dt_est
)

# 4) Fitted values – let fixest align by row
share_dt[, gov_gdp_fitted := as.numeric(predict(m_share_lag, newdata = share_dt))]
# Optional: keep only the fitted values for the estimation window when plotting:
fitted_dt <- dt_est[, .(year, gov_gdp_fitted = as.numeric(predict(m_share_lag, newdata = dt_est)))]

# ---------------- Projections to 2071 ----------------

# Make sure 'proj' has the SAME units as dt_est (0–1 age shares, rp_g already z-scored, tot already z-scored, dln_pop as built)
setorder(proj, year)
target_year <- 2071L
if (max(proj$year) < target_year) {
  drv <- c("0_14","15_34","35_54","55_64","65p","rp_g","tot","dln_pop")
  last <- proj[.N]
  fill <- data.table(year = (max(proj$year)+1L):target_year)
  for (v in drv) fill[[v]] <- last[[v]]  # carry forward
  proj <- rbind(proj, fill, use.names = TRUE)
}

# Start recursion from the LAST year we actually used in estimation
y0_year <- max(dt_est$year)
y0_val  <- dt_est[year == y0_year, gov_gdp]

proj[, gov_gdp_proj := NA_real_]
last_obs <- y0_val

for (i in seq_len(nrow(proj))) {
  row_i <- copy(proj[i])
  row_i[, gov_gdp_l1 := last_obs]                 # supply y_{t-1}
  proj$gov_gdp_proj[i] <- as.numeric(predict(m_share_lag, newdata = row_i))
  last_obs <- proj$gov_gdp_proj[i]
}

# ---------------- Plot ----------------
hist_end <- max(share_dt$year)
plot_dt_AR <- rbindlist(list(
  share_dt[, .(year, value = gov_gdp,        type = "Actual")],
  fitted_dt[, .(year, value = gov_gdp_fitted, type = "Fitted")],
  proj    [, .(year, value = gov_gdp_proj,   type = "Projection")]
), use.names = TRUE)

ggplot() +
  annotate("rect", xmin = hist_end, xmax = target_year,
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.25) +
  geom_line(data = plot_dt_AR[type=="Actual"     & !is.na(value)], aes(year, value, colour = type), linewidth = 1.1) +
  geom_line(data = plot_dt_AR[type=="Fitted"     & !is.na(value)], aes(year, value, colour = type, linetype = type), linewidth = 1.05) +
  geom_line(data = plot_dt_AR[type=="Projection" & !is.na(value)], aes(year, value, colour = type), linewidth = 1.1) +
  scale_color_manual(values = c("Actual"="black","Fitted"="steelblue","Projection"="orange")) +
  scale_linetype_manual(values = c("Fitted"="dashed")) +
  labs(title = "Government spending (% of GDP): Actual and Projection to 2071 (Lag model)",
       subtitle = "Shaded area = projection window",
       x = NULL, y = "Per cent of GDP", color = NULL, linetype = NULL) +
  theme_minimal(base_size = 13)



