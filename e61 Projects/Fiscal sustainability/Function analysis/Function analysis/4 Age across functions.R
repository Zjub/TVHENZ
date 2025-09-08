## ======================================================================
## COFOG-by-function Shapley from age shares (aggregate bottom-up)
## No-intercept model; optional time trend; Residual stacked
## Last update: 08/09/2025
## Author: Matt Nolan (+ ChatGPT extension)
## ======================================================================

# -------------------- Packages --------------------
suppressPackageStartupMessages({
  library(data.table)
  library(readabs)
  library(lubridate)
  library(ggplot2)
  library(theme61)
})

rm(list = ls()); invisible(gc())

# -------------------- User prefs --------------------
freq             <- "FY"       # "FY" or "CY"
abs_check_local  <- TRUE
work             <- FALSE      # set TRUE when at work
baseline_year    <- NA         # NA = earliest year in sample
final_year_opt   <- 2019       # optional end year (NA = max available)
start_year       <- NA         # optional start year (NA = min available)

# NEW: toggle time trend in the Shapley features
add_time_trend   <- TRUE       # TRUE = include t & t2; FALSE = age-only

# Paths -- this reflects the ABS provided consolidated data, which reflects our primary data course for thinking about functions.
if (work) {
  consolidate_path <- "C:/Users/MattNolan/Git/TVHENZ/e61 Projects/Fiscal sustainability/Function analysis/Data/abs_gfs_data_clean.csv"
} else {
  consolidate_path <- "~/GitHub/TVHENZ/e61 Projects/Fiscal sustainability/Function analysis/Data/abs_gfs_data_clean.csv"
}

# -------------------- Helpers --------------------
fy_end_year <- function(date) {
  d <- lubridate::as_date(date)
  lubridate::year(d %m+% months(6L))
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

# -------------------- ABS pulls --------------------
# Nominal GDP from 5206.0 (Seasonally Adjusted preferred)
get_gdp_nom <- function(freq = c("FY","CY"), check_local = TRUE) {
  freq <- match.arg(freq)
  
  na <- as.data.table(read_abs("5206.0", check_local = check_local))
  if (!nrow(na)) stop("read_abs('5206.0') returned 0 rows.")
  if (!inherits(na$date, "Date")) na[, date := as.Date(date)]
  
  # Build combined text field across available metadata columns
  txt_cols <- intersect(c("series","table_title","unit","data_type","series_type"), names(na))
  if (!length(txt_cols)) txt_cols <- "series"
  for (cc in txt_cols) {
    if (!is.character(na[[cc]])) set(na, j = cc, value = as.character(na[[cc]]))
  }
  na[, txt_all := do.call(paste, c(.SD, list(sep = " | "))), .SDcols = txt_cols]
  
  # Identify GDP nominal candidates
  is_gdp <- grepl("\\bGross\\s+domestic\\s+product\\b|\\bGDP\\b", na$txt_all, ignore.case = TRUE)
  is_nom <- grepl("Current\\s*prices?|\\bCP\\b", na$txt_all, ignore.case = TRUE)
  gdp_q  <- na[is_gdp & is_nom]
  
  # Prefer SA -> Trend -> Original
  prefer_sa <- function(DT) {
    if (!nrow(DT)) return(DT)
    if ("series_type" %in% names(DT)) {
      sa <- DT[grepl("Seasonally adjusted", series_type, ignore.case = TRUE)]
      if (nrow(sa)) return(sa)
      tr <- DT[grepl("\\bTrend\\b", series_type, ignore.case = TRUE)]
      if (nrow(tr)) return(tr)
    }
    DT[grepl("\\bOriginal\\b", txt_all, ignore.case = TRUE)]
  }
  gdp_q <- prefer_sa(gdp_q)
  
  # Annualise to FY or CY
  annualise(gdp_q, out_name = "gdp_nom", mode = "mean", freq = freq)[order(year)]
}

# ERP & age shares (0_14, 15_34, 35_54, 55_64, 65p) from 3101.0
get_erp_total_and_shares <- function(freq = c("FY","CY"), table_hint = "59", check_local = TRUE) {
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

# -------------------- Read consolidated COFOG dataset --------------------
consolidate_dt <- fread(consolidate_path)
setDT(consolidate_dt)

# Harmonise division names (your mapping)
consolidate_dt[, cofog_group_name := fcase(
  cofog_div_code == "01","01 General Public Service",
  cofog_div_code == "02","02 Defence",
  cofog_div_code == "03","03 Public Order",
  cofog_div_code == "04","04 Economic Affairs",
  cofog_div_code == "05","05 Environmental Protection",
  cofog_div_code == "06","06 Amenities",
  cofog_div_code == "07","07 Health",
  cofog_div_code == "08","08 Culture",
  cofog_div_code == "09","09 Education",
  cofog_div_code == "10","10 Social Protection",
  cofog_div_code == "11","11 Transport",
  default = cofog_group_name
)]

# Keep expenses slice
consolidated_expenses_dt <- consolidate_dt[etf_type_name == "Revenue and expenses"]

# -------------------- Build function-level spend/GDP + age shares --------------------
message("▶ Pulling GDP & ERP/age shares …")
gdp_a  <- get_gdp_nom(freq = freq, check_local = abs_check_local)
erp_a  <- get_erp_total_and_shares(freq = freq, check_local = abs_check_local)

message("▶ Aggregating COFOG function spend …")
fun_yr <- consolidated_expenses_dt[
  , .(nom_expense = sum(gov_expenses_mn, na.rm = TRUE)), by = .(cofog_group_name, fin_year)
][
  gdp_a, on = .(fin_year = year)
][
  !is.na(gdp_nom)
][
  , `:=`(year = fin_year, exp_gdp_f = nom_expense / gdp_nom)
][
  , .(cofog_group_name, year, exp_gdp_f)
]

# Merge age shares
features_age <- c("0_14","15_34","35_54","55_64","65p")
fun_yr <- merge(fun_yr, erp_a[, c("year", features_age), with = FALSE],
                by = "year", all.x = TRUE)
fun_yr <- fun_yr[complete.cases(fun_yr[, c("exp_gdp_f", features_age), with = FALSE])]
setorder(fun_yr, cofog_group_name, year)

# Apply sample window
if (is.finite(start_year))     fun_yr <- fun_yr[year >= start_year]
if (is.finite(final_year_opt)) fun_yr <- fun_yr[year <= final_year_opt]
stopifnot(nrow(fun_yr) > 1L)

# Baseline for counterfactuals
if (!is.finite(baseline_year)) baseline_year <- min(fun_yr$year)
if (!(baseline_year %in% fun_yr$year)) baseline_year <- min(fun_yr$year)

# -------------------- Shapley utilities (no-intercept) --------------------
.all_perms <- function(v) if (length(v)==1L) list(v) else {
  out <- list(); for (i in seq_along(v)) for (p in Recall(v[-i])) out[[length(out)+1L]] <- c(v[i], p); out
}
.all_subsets <- function(v) { out <- list(character(0)); if (length(v)) for (m in 1:length(v)) out <- c(out, combn(v, m, simplify = FALSE)); out }
.fit_subset <- function(y, X, S) {
  if (!length(S)) return(numeric(0))
  Xdf <- as.data.frame(X[, ..S]); for (j in seq_along(Xdf)) Xdf[[j]] <- as.numeric(Xdf[[j]])
  Xi <- as.matrix(Xdf)
  b  <- as.numeric(coef(lm.fit(x = Xi, y = y)))  # no intercept
  keep <- !is.na(b); if (!any(keep)) return(numeric(0))
  names(b) <- colnames(Xi); b[keep]
}
.delta_hat <- function(bS, x0, x1) {
  if (!length(bS)) return(0)
  common <- intersect(names(bS), names(x0)); if (!length(common)) return(0)
  v0 <- setNames(as.numeric(unlist(x0[common], use.names = FALSE)), common)
  v1 <- setNames(as.numeric(unlist(x1[common], use.names = FALSE)), common)
  sum(bS[common] * (v1 - v0))
}

shapley_regression_pair_general <- function(dt, y0, y1, features, y_col = "gov_gdp", conditional = TRUE) {
  stopifnot(y0 %in% dt$year, y1 %in% dt$year)
  d0 <- dt[year == y0]; d1 <- dt[year == y1]
  X  <- dt[, ..features]; y <- as.numeric(dt[[y_col]])
  d_actual <- as.numeric(d1[[y_col]] - d0[[y_col]])
  
  if (!conditional) {
    b <- .fit_subset(y, X, features)
    contrib <- setNames(numeric(length(features)), features)
    for (g in features) contrib[g] <- (as.numeric(d1[[g]] - d0[[g]])) * (if (g %in% names(b)) b[g] else 0)
    d_hat <- sum(contrib)
    return(list(d_actual = d_actual, d_hat = d_hat, contrib = contrib, unexplained = d_actual - d_hat))
  }
  
  subsets <- .all_subsets(features); subsets <- Filter(function(S) length(S) > 0L, subsets)
  beta_cache <- new.env(parent = emptyenv())
  .key <- function(S) if (length(S)) paste(sort(S), collapse = "|") else ".EMPTY"
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
      Snew <- c(S, g); val <- delta_of(Snew)
      contrib_sum[g] <- contrib_sum[g] + (val - prev)
      S <- Snew; prev <- val
    }
  }
  contrib <- contrib_sum / length(perms)
  d_hat   <- sum(contrib)
  list(d_actual = d_actual, d_hat = d_hat, contrib = contrib, unexplained = d_actual - d_hat)
}

fourbin_pairs <- function(yrs) {
  brks <- unique(as.integer(quantile(yrs, probs = c(0,.25,.5,.75,1), type = 1)))
  as.data.table(list(bin = findInterval(yrs, brks, all.inside = TRUE), year = yrs))[
    , .(y0 = min(year), y1 = max(year)), by = bin][order(bin)]
}

# -------------------- Run per function, then aggregate --------------------
run_one_function <- function(dt_fun, age_feats = features_age, conditional = TRUE) {
  # dt_fun: year, exp_gdp_f, age shares...
  setorder(dt_fun, year)
  
  # Build features (age-only OR age + trend)
  feats <- age_feats
  dtX   <- copy(dt_fun)
  if (isTRUE(add_time_trend)) {
    dtX[, t  := year - min(year)]
    dtX[, t2 := t^2]
    feats <- c(feats, "t") #, "t2")
  }
  
  # (a) panel year-to-year
  panel_pairs <- data.table(y0 = head(dtX$year, -1L), y1 = tail(dtX$year, -1L))
  panel_res <- rbindlist(lapply(1:nrow(panel_pairs), function(i) {
    sh <- shapley_regression_pair_general(
      dt = dtX[, c("year","exp_gdp_f", feats), with = FALSE][
        , setnames(.SD, "exp_gdp_f", "gov_gdp")],
      y0 = panel_pairs$y0[i], y1 = panel_pairs$y1[i],
      features = feats, conditional = conditional
    )
    data.table(y0 = panel_pairs$y0[i], y1 = panel_pairs$y1[i],
               d_actual = sh$d_actual, d_hat = sh$d_hat, unexplained = sh$unexplained,
               t(sh$contrib))
  }), use.names = TRUE, fill = TRUE)
  
  # (b) four-bin
  bins <- fourbin_pairs(dtX$year)
  four_res <- rbindlist(lapply(1:nrow(bins), function(i) {
    sh <- shapley_regression_pair_general(
      dt = dtX[, c("year","exp_gdp_f", feats), with = FALSE][
        , setnames(.SD, "exp_gdp_f", "gov_gdp")],
      y0 = bins$y0[i], y1 = bins$y1[i],
      features = feats, conditional = conditional
    )
    data.table(y0 = bins$y0[i], y1 = bins$y1[i],
               d_actual = sh$d_actual, d_hat = sh$d_hat, unexplained = sh$unexplained,
               t(sh$contrib))
  }), use.names = TRUE, fill = TRUE)
  
  list(panel = panel_res, four_bin = four_res)
}

message("▶ Running Shapley by function (", if (add_time_trend) "age + trend" else "age-only", ") …")
by_fun <- split(fun_yr, by = "cofog_group_name", keep.by = FALSE)
res_list <- lapply(names(by_fun), function(f) {
  out <- run_one_function(by_fun[[f]], age_feats = features_age, conditional = TRUE)
  list(fun = f, panel = out$panel[, fun := f], four = out$four_bin[, fun := f])
})

panel_by_fun <- rbindlist(lapply(res_list, `[[`, "panel"), use.names = TRUE, fill = TRUE)
four_by_fun  <- rbindlist(lapply(res_list, `[[`, "four"),  use.names = TRUE, fill = TRUE)

# Aggregate contributions across functions → total
feature_all <- c(features_age, if (add_time_trend) c("t")) #,"t2"
agg_keep    <- c("y0","y1","d_actual","d_hat","unexplained", feature_all)

panel_total <- panel_by_fun[, lapply(.SD, sum, na.rm = TRUE), .SDcols = agg_keep, by = .(y0, y1)]
four_total  <- four_by_fun[,  lapply(.SD, sum, na.rm = TRUE), .SDcols = agg_keep, by = .(y0, y1)]

# -------------------- Counterfactual: hold age shares at baseline (per function → total) --------------------
# (still age-only by design, even if trend is toggled on)
counterfactual_age_path_fun <- function(dt_fun, baseline_year, groups = features_age) {
  stopifnot(baseline_year %in% dt_fun$year)
  Xi <- as.matrix(dt_fun[, ..groups]); yi <- dt_fun$exp_gdp_f
  b  <- coef(lm.fit(x = Xi, y = yi)); names(b) <- colnames(Xi)[!is.na(b)]
  base_vec <- as.numeric(dt_fun[year == baseline_year, ..groups])
  Xcf <- Xi; for (j in seq_along(groups)) Xcf[, j] <- base_vec[j]
  data.table(year = dt_fun$year,
             y_actual_f    = yi,
             yhat_full_f   = as.vector(Xi  %*% b),
             yhat_cf_age_f = as.vector(Xcf %*% b))
}

# Build per-function CF paths and stack
cf_fun_list <- Map(
  function(dt_fun, nm) {
    tmp <- counterfactual_age_path_fun(dt_fun, baseline_year = baseline_year)
    tmp[, fun := nm]
    tmp
  },
  by_fun, names(by_fun)
)
cf_fun_all <- rbindlist(cf_fun_list, use.names = TRUE, fill = TRUE)

# Aggregate to TOTAL series by year (sum across functions)
cf_total <- cf_fun_all[
  ,
  .(
    y_actual     = sum(y_actual_f,    na.rm = TRUE),
    yhat_full    = sum(yhat_full_f,   na.rm = TRUE),
    yhat_cf_age  = sum(yhat_cf_age_f, na.rm = TRUE)
  ),
  by = year
][order(year)]
cf_total[, impact_age := yhat_full - yhat_cf_age]

# -------------------- Output: quick tables & plots --------------------
message("▶ Sample span: ", min(fun_yr$year), "–", max(fun_yr$year))
message("▶ Baseline year (age fixed): ", baseline_year)

# Helper to build plotting tables with Trend + Residual columns
build_plot_tables <- function(TOT, have_trend = add_time_trend) {
  DT <- copy(TOT)
  # Residual column from unexplained
  DT[, Residual := unexplained]
  # Collapse t + t2 into 'Trend' if present
  if (have_trend) {
    missing_t  <- !("t"  %in% names(DT))
    missing_t2 <- !("t2" %in% names(DT))
    if (!missing_t || !missing_t2) {
      DT[, Trend := rowSums(.SD, na.rm = TRUE), .SDcols = intersect(c("t","t2"), names(DT))]
    }
  }
  DT[]
}

# (1) Panel contributions (aggregate) — stacked bars (sum = actual Δ)
if (nrow(panel_total)) {
  panel_plot <- build_plot_tables(panel_total, have_trend = add_time_trend)
  
  # Components to show in stack
  comp_vars <- c(features_age, if (add_time_trend) "Trend", "Residual")
  comp_vars <- comp_vars[comp_vars %in% names(panel_plot)]
  
  panel_m <- melt(
    panel_plot,
    id.vars = c("y0","y1","d_actual","d_hat","unexplained"),
    measure.vars = comp_vars,
    variable.name = "component", value.name = "contrib"
  )
  panel_m[, year := y1]
  
  p <- ggplot(panel_m, aes(x = year, y = contrib, fill = component)) +
    geom_col() +
    # Optional markers: actual & predicted totals
    geom_line(data = unique(panel_m[, .(year, d_actual)]),
              aes(y = d_actual, x = year, group = 1),
              inherit.aes = FALSE, linewidth = 0.4) +
    geom_point(data = unique(panel_m[, .(year, d_actual)]),
               aes(y = d_actual, x = year),
               inherit.aes = FALSE, shape = 16, size = 2.4) +
    geom_point(data = unique(panel_m[, .(year, d_hat)]),
               aes(y = d_hat, x = year),
               inherit.aes = FALSE, shape = 23, size = 2.8, stroke = 0.5) +
    labs(
      title = "Δ(total spend/GDP): age contributions (+ trend) with Residual",
      subtitle = "Stacks include Residual → bars sum to actual Δ; ◇ predicted Δ; ● actual Δ.",
      x = NULL, y = "Δ level points", fill = "Component"
    ) +
    theme_e61(legend = "bottom")
  print(p)
}

# (2) Four-bin contributions (aggregate) — flipped bars (sum = actual Δ)
if (nrow(four_total)) {
  four_plot <- build_plot_tables(four_total, have_trend = add_time_trend)
  comp_vars4 <- c(features_age, if (add_time_trend) "Trend", "Residual")
  comp_vars4 <- comp_vars4[comp_vars4 %in% names(four_plot)]
  
  four_m <- melt(
    four_plot,
    id.vars = c("y0","y1","d_actual","d_hat","unexplained"),
    measure.vars = comp_vars4,
    variable.name = "component", value.name = "contrib"
  )
  
  four_m[, Segment := paste0(y0, "–", y1)]
  seg_levels <- four_plot[, paste0(y0, "–", y1)]
  four_m[, Segment := factor(Segment, levels = seg_levels)]
  
  p4 <- ggplot(four_m, aes(x = Segment, y = contrib, fill = component)) +
    geom_col() +
    # Predicted (◇) and Actual (●) markers
    geom_point(
      data = unique(four_m[, .(Segment, d_hat)]),
      aes(y = d_hat, x = Segment),
      inherit.aes = FALSE, shape = 23, size = 3, stroke = 0.6
    ) +
    geom_point(
      data = unique(four_m[, .(Segment, d_actual)]),
      aes(y = d_actual, x = Segment),
      inherit.aes = FALSE, shape = 16, size = 2.8
    ) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    coord_flip() +
    labs(
      title = "Δ(total spend/GDP): binned periods — age (+ trend) with Residual",
      subtitle = "Stacks include Residual → bars sum to actual Δ; ● actual, ◇ predicted.",
      x = NULL, y = "Δ level points", fill = "Component"
    ) +
    theme_e61(legend = "bottom")
  print(p4)
}

# (3) Counterfactual path: age shares fixed at baseline (aggregate, age-only fit)
cf_long <- melt(
  cf_total[, .(year, y_actual, yhat_full, yhat_cf_age)],
  id.vars = "year", variable.name = "series", value.name = "value"
)
cf_long[, series := factor(series, levels = c("y_actual","yhat_full","yhat_cf_age"),
                           labels = c("Actual","Fitted (age-only)","CF: Age fixed"))]
pcf <- ggplot(cf_long, aes(year, value, colour = series)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = sprintf("Total spend/GDP: counterfactual with age fixed at %s (age-only fit)", baseline_year),
    x = NULL, y = "Spend / GDP", colour = NULL
  ) +
  theme_e61(legend = "bottom")
print(pcf)

# -------------------- Save key outputs --------------------
fwrite(fun_yr,        "cofog_age_input.csv")
if (nrow(panel_by_fun)) fwrite(panel_by_fun, "cofog_shapley_panel_byfun.csv")
if (nrow(four_by_fun))  fwrite(four_by_fun,  "cofog_shapley_fourbin_byfun.csv")
if (nrow(panel_total))  fwrite(panel_total,  "cofog_shapley_panel_total.csv")
if (nrow(four_total))   fwrite(four_total,   "cofog_shapley_fourbin_total.csv")
fwrite(cf_total[, .(year, y_actual, yhat_full, yhat_cf_age, impact_age)],
       "cofog_age_counterfactual_total.csv")

message("Done.")
