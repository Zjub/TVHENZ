## ======================================================================
## Regression-based Shapley for Govt Spending as % of GDP (AU, ABS)
## Decompose changes in spend/GDP into age-group contributions
## with diagnostics so we don't attribute all changes to ageing.
## Last update: 19/08/2025
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
measure         <- "GFCE_plus_GFCF"              # "GFCE" or "GFCE_plus_GFCF"
share_basis     <- "nominal"           # "nominal" (standard for shares) or "real"
conditional_sv  <- TRUE                # TRUE = conditional (full Shapley); FALSE = β·Δs
abs_check_local <- TRUE                # use cached ABS files (fast after first run)
#features <- c("0_14","15_34","35_54","55_64","65p","tot") # The set of variables included in this run
start_year      <- 1980    # <-- choose your sample start
final_year_opt  <- 2019      # optional end year, set to e.g. 2018 or NA to use all
baseline_year   <- 2005      # optional: force a baseline year; NA = first year of filtered sample
features <- c("0_14","15_34","35_54","55_64","65p","tot",
              "rp_g","dln_pop") # ,"dln_pop""unemp",

# -------------------- Helpers --------------------
# AU FY ends in June: add 6m and take year
fy_end_year <- function(date) {
  d <- lubridate::as_date(date); lubridate::year(d %m+% months(6))
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
  if (!inherits(dt$date, "Date")) dt[, date := as.Date(date)]
  tmp <- if (freq == "FY") {
    if (mode == "mean") dt[, .(val = mean(value, na.rm = TRUE)), by = .(year = fy_end_year(date))]
    else                dt[, .(val = sum(value,  na.rm = TRUE)), by = .(year = fy_end_year(date))]
  } else {
    if (mode == "mean") dt[, .(val = mean(value, na.rm = TRUE)), by = .(year = year(date))]
    else                dt[, .(val = sum(value,  na.rm = TRUE)), by = .(year = year(date))]
  }
  setnames(tmp, "val", out_name)[order(year)]
}

# -------------------- ABS 5206.0: GFCE, GFCF, GDP --------------------
# Pulls General government totals (excludes National/State & local),
# GFCE + GFCF (both bases where available), and GDP (nominal & chain volume).
get_gfce_gdp <- function(freq = c("FY","CY"), check_local = TRUE, verbose = TRUE) {
  freq <- match.arg(freq)
  
  # ---- Load & prep ----
  na <- as.data.table(read_abs("5206.0", check_local = check_local))
  if (!nrow(na)) stop("read_abs('5206.0') returned 0 rows.")
  if (!inherits(na$date, "Date")) na[, date := as.Date(date)]
  
  # Build combined text field across available metadata columns
  txt_cols <- intersect(
    c("series","table_title","unit","data_type","series_type"),
    names(na)
  )
  if (!length(txt_cols)) txt_cols <- "series"
  # Coerce to character to avoid factor/NA weirdness, then paste
  for (cc in txt_cols) if (!is.character(na[[cc]])) set(na, j = cc, value = as.character(na[[cc]]))
  na[, .__txt := do.call(paste, c(.SD, list(sep = " | "))), .SDcols = txt_cols]
  
  # ---- Helpers ----
  prefer_sa <- function(DT) {
    if (!nrow(DT)) return(DT)
    has_series_type <- "series_type" %in% names(DT)
    # Use series_type if present, else fallback to combined text
    sa <- DT[grepl("Seasonally adjusted", if (has_series_type) series_type else .__txt, ignore.case = TRUE)]
    if (nrow(sa)) return(sa)
    tr <- DT[grepl("\\bTrend\\b",          if (has_series_type) series_type else .__txt, ignore.case = TRUE)]
    if (nrow(tr)) return(tr)
    DT[grepl("\\bOriginal\\b",             if (has_series_type) series_type else .__txt, ignore.case = TRUE)]
  }
  
  ann <- function(DT, out_name) {
    if (!nrow(DT)) return(data.table(year = integer(), (out_name) := numeric()))
    annualise(DT, out_name = out_name, mode = "mean", freq = freq)
  }
  
  # ---- Selectors (your working logic) ----
  is_gen_gov_total <- function(s) {
    grepl("^\\s*General\\s+government\\s*;\\s*", s, ignore.case = TRUE) &&
      !grepl("National|State and local", s, ignore.case = TRUE)
  }
  is_gfce <- function(s) grepl("Final\\s+consumption\\s+expenditure\\s*;", s, ignore.case = TRUE)
  is_gfcf <- function(s) grepl("Gross\\s+fixed\\s+capital\\s+formation\\s*;", s, ignore.case = TRUE)
  is_bad_var <- function(s) grepl("Percentage\\s+changes|Index", s, ignore.case = TRUE)
  
  is_nom  <- function(s) grepl("Current\\s*prices?", s, ignore.case = TRUE) ||
    grepl("\\bCP\\b", s, ignore.case = TRUE)
  is_real <- function(s) grepl("Chain\\s*volume|Volume\\s*chain|Chain-?volume|Volume\\s*measures", s, ignore.case = TRUE) ||
    grepl("\\bCVM\\b|\\bvolume\\b", s, ignore.case = TRUE)
  
  is_gdp <- function(s) grepl("\\bGross\\s+domestic\\s+product\\b|\\bGDP\\b", s, ignore.case = TRUE)
  
  # ---- Filter slices ----
  rows_gen_total <- vapply(na$series, is_gen_gov_total, logical(1))
  rows_not_bad   <- !vapply(na$series, is_bad_var,    logical(1))
  gg <- na[rows_gen_total & rows_not_bad]
  
  gg_gfce <- gg[vapply(series, is_gfce, logical(1))]
  gg_gfcf <- gg[vapply(series, is_gfcf, logical(1))]
  
  # Basis splits (search in the combined text)
  gfce_nom_q  <- gg_gfce[ vapply(.__txt, is_nom,  logical(1)) ]
  gfce_real_q <- gg_gfce[ vapply(.__txt, is_real, logical(1)) ]
  gfcf_nom_q  <- gg_gfcf[ vapply(.__txt, is_nom,  logical(1)) ]
  gfcf_real_q <- gg_gfcf[ vapply(.__txt, is_real, logical(1)) ]
  
  # Prefer SA → Trend → Original
  gfce_nom_q  <- prefer_sa(gfce_nom_q)
  gfce_real_q <- prefer_sa(gfce_real_q)
  gfcf_nom_q  <- prefer_sa(gfcf_nom_q)
  gfcf_real_q <- prefer_sa(gfcf_real_q)
  
  # GDP (clear wording for price basis)
  gdp_nom_q  <- prefer_sa(na[ vapply(na$series, is_gdp,  logical(1)) &
                                vapply(na$.__txt, is_nom,  logical(1)) ])
  gdp_real_q <- prefer_sa(na[ vapply(na$series, is_gdp,  logical(1)) &
                                vapply(na$.__txt, is_real, logical(1)) ])
  
  # Diagnostics if subjects totally missing
  if (!nrow(gg_gfce)) stop("Could not find General government total GFCE rows (check labels).")
  if (!nrow(gg_gfcf)) stop("Could not find General government total GFCF rows (check labels).")
  if (!nrow(gdp_nom_q) && !nrow(gdp_real_q)) stop("Could not find GDP rows (check labels).")
  
  # ---- Annualise ----
  gfce_nom_a   <- ann(gfce_nom_q,   "gfce_nom")
  gfce_real_a  <- ann(gfce_real_q,  "gfce_real")
  gfcf_nom_a   <- ann(gfcf_nom_q,   "gfcf_nom")
  gfcf_real_a  <- ann(gfcf_real_q,  "gfcf_real")
  gdp_nom_a    <- ann(gdp_nom_q,    "gdp_nom")
  gdp_real_a   <- ann(gdp_real_q,   "gdp_real")
  
  out <- Reduce(function(x, y) merge(x, y, by = "year", all = TRUE),
                list(gfce_nom_a, gfce_real_a, gfcf_nom_a, gfcf_real_a, gdp_nom_a, gdp_real_a))
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
  
  # Build text field & pick ToT index rows (exclude pct changes)
  txt_cols <- intersect(
    c("series","table_title","unit","data_type","series_type"),
    names(bo)
  )
  if (!length(txt_cols)) txt_cols <- "series"
  
  # Ensure all candidate cols are character
  for (cc in txt_cols) {
    if (!is.character(bo[[cc]])) set(bo, j = cc, value = as.character(bo[[cc]]))
  }
  
  bo[, "__txt" := do.call(paste, c(.SD, list(sep = " | "))), .SDcols = txt_cols]
  
  is_tot <- grepl("terms\\s*of\\s*trade", bo[["__txt"]], ignore.case = TRUE) &
    grepl("index", bo[["__txt"]], ignore.case = TRUE) &
    !grepl("percentage\\s*changes|index\\s*changes", bo[["__txt"]], ignore.case = TRUE)
  
  tot_q  <- bo[is_tot]
  
  # Prefer SA → Trend → Original
  prefer_sa <- function(DT) {
    sa <- DT[grepl("Seasonally adjusted", DT[["__txt"]], ignore.case = TRUE)]
    if (nrow(sa)) return(sa)
    tr <- DT[grepl("\\bTrend\\b", DT[["__txt"]], ignore.case = TRUE)]
    if (nrow(tr)) return(tr)
    DT[grepl("\\bOriginal\\b", DT[["__txt"]], ignore.case = TRUE)]
  }
  
  tot_q <- prefer_sa(tot_q)
  if (!nrow(tot_q)) stop("Could not locate ToT index in 5302.0")
  
  annualise(tot_q, out_name = "tot_index", mode = "mean", freq = freq)[order(year)]
}



# -------------------- Build dataset: spend/GDP and age shares --------------------
build_share_dataset <- function(freq = c("FY","CY"),
                                measure = c("GFCE","GFCE_plus_GFCF"),
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
  gov_level <- if (measure == "GFCE") {
    if (share_basis == "nominal") dt$gfce_nom else dt$gfce_real
  } else {
    gf <- if (share_basis == "nominal") dt$gfce_nom else dt$gfce_real
    gc <- if (share_basis == "nominal") dt$gfcf_nom else dt$gfcf_real
    gf + gc
  }
  gdp_level <- if (share_basis == "nominal") dt$gdp_nom else dt$gdp_real
  if (anyNA(gov_level) || anyNA(gdp_level)) {
    stop("Missing series for chosen share_basis/measure in 5206.0 pull.")
  }
  dt[, gov_gdp := gov_level / gdp_level]
  
  # ----- NEW: drivers that need levels -----
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
                 "pop_total", "rp_g", "dln_pop",
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
# Full-model coefficients (one fit, no intercept, numeric & robust)
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
message("▶ Building dataset (", freq, ", measure=", measure, ", basis=", share_basis, ") …")
tot_a   <- get_terms_of_trade(freq = freq, check_local = abs_check_local)
share_dt <- build_share_dataset(freq, measure, share_basis, check_local = abs_check_local)
share_dt <- merge(share_dt, tot_a, by = "year", all.x = TRUE)
share_dt[, tot := as.numeric(scale(tot_index, center = TRUE, scale = TRUE))] # Standardise TOT so it is on a similar scale to other variables

# --- (i) Cycle: Unemployment rate (example placeholder series) ---
# If you have an ABS pull for unemployment, annualise the rate:
dt_unemp <- as.data.table(read_abs("6202.0", tables = "1", check_local = abs_check_local))
unemp_a <- annualise(dt_unemp, out_name = "unemp", mode = "mean", freq = freq)
# For now, we are ignoring unemployment
# if (exists("unemp_a")) {
#   share_dt <- merge(share_dt, unemp_a, by = "year", all.x = TRUE)
# }

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

# Diagnostics: how much of spend/GDP variation do age shares explain?
am <- age_model_r2(share_dt)
message(sprintf("▶ Age-only model R^2 = %.3f (share of variance explained by demographics).", am$r2))

# Shapley blocks
message("▶ Computing Shapley contributions (", if (conditional_sv) "conditional" else "unconditional", ") …")
sv_res   <- run_shapley_blocks(share_dt, conditional = TRUE, features = features)

# Coefficients for intuition:
coef_full(share_dt, features)
coef_subsets(share_dt, features)  # if you want the subset-by-subset β's

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
  p4c <- ggplot(four_g, aes(x = Segment, y = contrib, fill = stack_order)) +
    geom_col() +
    # Big solid dot = actual Δ
    geom_point(
      data = unique(four_g[, .(Segment, d_actual)]),
      aes(x = Segment, y = d_actual),
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
    plab(c("Residual","65+","Other ages","Economic effects***"),y=c(0.07,0.07,0.07,0.07),x=c(0.7,1.2,1.7,2.2)) +
    theme_e61(legend = "bottom")
  
  print(p4c)
}

save_e61(paste0("Shapley_",measure,".png"),res=2)


# Counterfactual example: hold age structure fixed at earliest year
b0 <- min(share_dt$year)
cf <- counterfactual_age_path(share_dt, baseline_year = b0)
message("▶ Counterfactual vs actual (age fixed at ", b0, "): showing first few rows")
print(head(cf))

# Save key outputs
fwrite(share_dt, "age_share_input.csv")
if (nrow(four_bin)) fwrite(four_bin, "shapley_age_fourbin.csv")
if (nrow(panel))    fwrite(panel,    "shapley_age_panel.csv")
fwrite(cf,          "age_counterfactual_path.csv")

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
                                 title = "Counterfactual paths (holding blocks fixed at baseline)") {
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
  ggplot(long, aes(year, value, colour = series)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(title = title, x = NULL, y = "Spend / GDP") +
    theme_e61(legend = "bottom")
}

# Define blocks (edit as you like)
blocks <- list(
  Demography = c("0_14","15_34","35_54","55_64","65p"),
  RelPrices  = c("rp_g"),
  PopGrowth  = c("dln_pop"),
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
  title = sprintf("Counterfactual spend/GDP (baseline %s; sample %s–%s)",
                  baseline_year, min(share_dt$year), max(share_dt$year))
))

# Save the table if useful
fwrite(cf_paths, "counterfactual_paths_generalgov.csv")
