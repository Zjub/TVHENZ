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
})

rm(list = ls()); invisible(gc())

# -------------------- User prefs --------------------
freq            <- "FY"                 # "FY" or "CY"
measure         <- "GFCE"              # "GFCE" or "GFCE_plus_GFCF"
share_basis     <- "nominal"           # "nominal" (standard for shares) or "real"
conditional_sv  <- TRUE                # TRUE = conditional (full Shapley); FALSE = β·Δs
abs_check_local <- TRUE                # use cached ABS files (fast after first run)
features <- c("0_14","15_34","35_54","55_64","65p","tot") # The set of variables included in this run

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
  
  need_cols <- c("year", "gov_gdp", "0_14","15_34","35_54","55_64","65p")
  miss <- setdiff(need_cols, names(dt))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse=", "))
  
  dt <- dt[, ..need_cols]
  dt <- dt[complete.cases(dt)]
  setorder(dt, year)
  dt
  
  # Age-share sanity
  dt[, age_sum := `0_14` + `15_34` + `35_54` + `55_64` + `65p`]
  if (any(abs(dt$age_sum - 1) > 0.03)) warning("Some years' age shares deviate from 1 by >3%.")
  
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
message("   Years: ", paste(range(share_dt$year), collapse = " – "), "  (n=", nrow(share_dt), ")")

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

# Print a small summary
if (nrow(four_bin)) {
  print(four_bin[, .(Segment = paste0(y0,"→",y1),
                     d_actual, d_hat, unexplained,
                     `0_14`,`15_34`,`35_54`,`55_64`,`65p`, `tot`)])
}

# Optional: a quick plot for panel contributions (explained part only)
if (nrow(panel)) {
  panel_m <- melt(panel,
                  id.vars = c("y0","y1","d_actual","d_hat","unexplained"),
                  measure.vars = c("0_14","15_34","35_54","55_64","65p","tot"),
                  variable.name = "age_group", value.name = "contrib")
  panel_m[, year := y1]
  p <- ggplot(panel_m, aes(x = year, y = contrib, fill = age_group)) +
    geom_col() +
    geom_line(aes(y = d_actual, x = year, group = 1), inherit.aes = FALSE) +
    geom_point(aes(y = d_actual, x=year), inherit.aes = FALSE) +
    labs(title = "Regression-based Shapley of Δ(spend/GDP): explained age-group contributions",
         subtitle = paste0("Unexplained (residual) not stacked; R²(age-only) = ", sprintf("%.3f", am$r2)),
         x = "Year", y = "Δ share (level points)") 
  print(p) + theme_e61(legend = "bottom")
}

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




