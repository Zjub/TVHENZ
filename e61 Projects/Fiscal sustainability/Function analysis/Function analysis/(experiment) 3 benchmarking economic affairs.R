# ---- Packages ----
library(data.table)
library(readabs)
library(lubridate)
library(zoo)
library(ggplot2)
library(theme61)
library(readxl)

# -------------------- User prefs --------------------
freq            <- "FY"                   # "FY" or "CY" (used by ABS pulls)
final_year      <- 2010                   # <-- set your cutoff year here
trend_window    <- 10                     # trailing years for nominal GDP trend at final_year
measure         <- "GFCE_plus_GFCF"       # (not used below, kept for context)
share_basis     <- "nominal"              # (not used below, kept for context)
conditional_sv  <- TRUE                   # (not used below, kept for context)
abs_check_local <- TRUE
features <- c("0_14","15_34","35_54","55_64","65p","tot","rp_g","dln_pop")

# -------------------- Helpers --------------------
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

# ────────────────────────────────────────────────────────────────────────────────
# 1) Your existing ABS pull (unchanged): get_gfce_gdp()
#    ... (assumes it's defined in your session exactly as you provided)
# ────────────────────────────────────────────────────────────────────────────────

# Minimal wrapper to get nominal GDP
get_nominal_gdp <- function(freq = c("CY","FY"),
                            check_local = TRUE,
                            verbose = FALSE) {
  freq <- match.arg(freq)
  if (!exists("get_gfce_gdp")) {
    stop("get_gfce_gdp() not found. Please source/define it before calling get_nominal_gdp().")
  }
  x <- get_gfce_gdp(freq = freq, check_local = check_local, verbose = verbose)
  if (!"gdp_nom" %in% names(x) || all(is.na(x$gdp_nom))) {
    stop("get_gfce_gdp() did not return a usable 'gdp_nom' column.")
  }
  out <- as.data.table(x)[!is.na(gdp_nom), .(year, gdp_nom)]
  setorder(out, year)
  out[]
}

# ────────────────────────────────────────────────────────────────────────────────
# 2) Benchmarks builder with final_year cutoff
#    - calibrates using years <= final_year
#    - anchors at t0 = final_year
#    - projects forward to compare actuals vs pre-2010 baseline
# ────────────────────────────────────────────────────────────────────────────────
build_ea_benchmarks <- function(ea_dt,
                                base_years   = 2000:2019,
                                final_year   = NULL,
                                trend_window = 10,
                                gdp_freq     = "CY",
                                abs_check_local = TRUE) {
  stopifnot(is.data.table(ea_dt), "year" %in% names(ea_dt))
  
  # Nominal GDP (FY or CY per gdp_freq)
  gdp <- get_nominal_gdp(freq = gdp_freq, check_local = abs_check_local, verbose = FALSE)
  setorder(gdp, year)
  gdp[, g_nom := c(NA_real_, diff(log(gdp_nom)))]
  
  # Merge EA with GDP for shares
  dt <- merge(ea_dt, gdp[, .(year, gdp_nom, g_nom)], by = "year", all.x = TRUE)
  
  # Ensure numeric
  if ("ea_nom" %in% names(dt)) dt[, ea_nom := as.numeric(ea_nom)]
  if ("ea_pct_gdp" %in% names(dt)) dt[, ea_pct_gdp := as.numeric(ea_pct_gdp)]
  
  # Reconstruct nominal from %GDP if needed
  if (!"ea_nom" %in% names(dt) || all(is.na(dt$ea_nom))) {
    if (!"ea_pct_gdp" %in% names(dt)) stop("Provide ea_nom or ea_pct_gdp.")
    if (max(dt$ea_pct_gdp, na.rm = TRUE) > 1.01) dt[, ea_share := ea_pct_gdp/100] else dt[, ea_share := ea_pct_gdp]
    dt[, ea_nom := ea_share * gdp_nom]
  }
  # Compute share if missing
  if (!"ea_pct_gdp" %in% names(dt) || all(is.na(dt$ea_pct_gdp))) {
    dt[, ea_share := ea_nom / gdp_nom]
  } else if (!"ea_share" %in% names(dt)) {
    if (max(dt$ea_pct_gdp, na.rm = TRUE) > 1.01) dt[, ea_share := ea_pct_gdp/100] else dt[, ea_share := ea_pct_gdp]
  }
  
  # ---------------- Calibration subset ----------------
  avail_cal <- dt[is.finite(ea_nom) & is.finite(gdp_nom), sort(unique(year))]
  if (is.null(final_year)) {
    cal_years <- intersect(base_years, avail_cal)
  } else {
    cal_years <- intersect(base_years[base_years <= final_year], avail_cal[avail_cal <= final_year])
  }
  if (length(cal_years) < 5L) warning("Few calibration years (<= final_year). Consider widening base_years or raising final_year.")
  
  # Anchor at the end of the calibration window
  t0 <- max(cal_years)
  E0 <- dt[year == t0, ea_nom]
  Y0 <- dt[year == t0, gdp_nom]
  
  # ---------------- Benchmarks ----------------
  # 1) Constant share-of-GDP (calibrated pre-cutoff)
  share_base <- dt[year %in% cal_years, mean(ea_share, na.rm = TRUE)]
  dt[, bench_const_nom   := share_base * gdp_nom]
  dt[, bench_const_share := bench_const_nom / gdp_nom]
  
  # 2) Elasticity to nominal GDP (pre-cutoff)
  reg_dt <- dt[year %in% cal_years,
               .(year,
                 dl_e = c(NA_real_, diff(log(ea_nom))),
                 dl_y = c(NA_real_, diff(log(gdp_nom))))]
  reg_dt <- reg_dt[is.finite(dl_e) & is.finite(dl_y)]
  beta <- if (nrow(reg_dt) >= 8L) as.numeric(coef(lm(dl_e ~ dl_y, data = reg_dt))[["dl_y"]]) else 1.0
  if (nrow(reg_dt) < 8L) warning("Elasticity sample < 8 obs; defaulting beta = 1.")
  dt[, bench_elast_nom   := E0 * (gdp_nom / Y0)^beta]
  dt[, bench_elast_share := bench_elast_nom / gdp_nom]
  
  # 3) Trend-GDP (expenditure-benchmark style, rate estimated up to final_year)
  if (is.null(final_year)) final_year <- max(gdp$year, na.rm = TRUE)
  win_end   <- min(final_year, max(gdp$year, na.rm = TRUE))
  win_start <- max(min(gdp$year, na.rm = TRUE) + 1L, win_end - trend_window + 1L)
  g_trend   <- mean(gdp[year %between% c(win_start, win_end), g_nom], na.rm = TRUE)
  if (!is.finite(g_trend)) g_trend <- mean(gdp[year %in% cal_years, g_nom], na.rm = TRUE)
  if (!is.finite(g_trend)) g_trend <- 0
  
  dt[, bench_trend_nom   := E0 * exp(g_trend * (year - t0))]
  dt[, bench_trend_share := bench_trend_nom / gdp_nom]
  
  # ---------------- Output ----------------
  out <- dt[, .(
    year,
    ea_nom,
    ea_share,
    bench_const_nom,  bench_const_share,
    bench_elast_nom,  bench_elast_share,
    bench_trend_nom,  bench_trend_share
  )]
  
  setattr(out, "calibration", list(
    base_window      = range(cal_years),
    final_year       = final_year,
    anchor_year      = t0,
    const_share      = share_base,
    elasticity_beta  = beta,
    trend_window     = trend_window,
    g_trend_nominal  = g_trend
  ))
  out[]
}

# ────────────────────────────────────────────────────────────────────────────────
# 3) Example usage: reshape OECD → AUS General Government Economic Affairs
# ────────────────────────────────────────────────────────────────────────────────
ea <- read_excel("table22-consolidated-cofog-expenditure-spent-by-approach.xlsx", skip = 1)
setDT(ea)

ea <- ea[`COFOG Area` == "Economic Affairs"
][, `:=`(`1995` = NULL, `1996` = NULL, `1997` = NULL)]

year_cols <- grep("^[0-9]{4}$", names(ea), value = TRUE)

ea_aus <- ea[
  ISO == "AUS" &
    (`COFOG Code` %chin% c("GF04","GF.04") | `COFOG Area` %chin% c("Economic Affairs","G04 Economic affairs")),
  c("Country","ISO","COFOG Area","COFOG Code","Government level","Government level code", year_cols),
  with = FALSE
]

ea_long <- melt(
  ea_aus,
  id.vars = c("Country","ISO","COFOG Area","COFOG Code","Government level","Government level code"),
  measure.vars = year_cols,
  variable.name = "year",
  value.name   = "ea_nom",
  variable.factor = FALSE
)[, `:=`(year = as.integer(year),
         ea_nom = as.numeric(ea_nom))][is.finite(ea_nom)]

ea_gen <- ea_long[, .(ea_nom = sum(ea_nom, na.rm = TRUE)), by = .(ISO, year)][order(year)]
ea_levels <- rbind(
  ea_long[, .(ISO, level = `Government level`, year, ea_nom)],
  ea_gen[, .(ISO, level = "General government", year, ea_nom)]
)[order(level, year)]

ea <- ea_gen[ISO == "AUS", .(year, ea_nom)][order(year)]

# ────────────────────────────────────────────────────────────────────────────────
# 4) Build benchmarks with final_year cutoff and plot
# ────────────────────────────────────────────────────────────────────────────────
ea_bench <- build_ea_benchmarks(
  ea,
  base_years      = 2000:2019,   # wide base, will be truncated at final_year internally
  final_year      = final_year,  # <-- key toggle
  trend_window    = trend_window,
  gdp_freq        = "CY",        # or use freq if your GDP should be FY: gdp_freq = freq
  abs_check_local = abs_check_local
)
attr(ea_bench, "calibration")

plot_ea_share <- function(ea_bench, final_year = NULL) {
  dtp <- melt(
    ea_bench[, .(year,
                 `Actual`            = ea_share,
                 `Constant share`    = bench_const_share,
                 `Elasticity to GDP` = bench_elast_share,
                 `Trend-GDP`         = bench_trend_share)],
    id.vars = "year", variable.name = "Series", value.name = "share"
  )
  p <- ggplot(dtp, aes(year, share, colour = Series)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(size = 1) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(title = "Economic Affairs: actual vs pre-cutoff benchmarks",
         subtitle = if (!is.null(final_year)) paste0("Calibrated using data up to ", final_year) else NULL,
         x = NULL, y = "Share of GDP") +
    theme_bw() +
    theme(legend.position = "bottom")
  if (!is.null(final_year)) {
    p <- p + geom_vline(xintercept = final_year, linetype = "dotted")
  }
  p
}

# Example:
plot_ea_share(ea_bench, final_year = final_year)
