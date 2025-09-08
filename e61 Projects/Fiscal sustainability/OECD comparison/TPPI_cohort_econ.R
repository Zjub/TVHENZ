# Topic: Adjust cohort estimates to include growth (by residualising by growth prior to the process)
# Author: Matt Nolan
# Created: 2/9/2025
# Last edit: 2/9/2025
# Last editor: Matt Nolan


library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(readxl)
library(tidyverse)
library(data.table)
library(Hmisc)
library(tidysynth)
library(readabs)

rm(list = ls()); gc()

## Add data

dt <- read_excel("TTPI main table results.xlsx")
setDT(dt)

dt[,net_trans := sum_transfers - sum_tax]

# --- bring in GDP per capita series from ABS ---
# ABS cat. 5206.0 Table 1 has GDP and population
gdp_data <- read_abs(cat_no = "5206.0", tables = "1")
setDT(gdp_data)

gdp_data <- gdp_data[series == "GDP per capita: Chain volume measures ;" & series_type == "Original"][,year := year(date)]

gdp_dt <- gdp_data[,.(pc_gdp = sum(value)),by=.(year)]
gdp_dt[, `:=`(gdp_lag = shift(pc_gdp),
              yr_lag  = shift(year),
              gap     = year - shift(year))]

# standard year-over-year growth (only when years are consecutive)
gdp_dt[, rgdp_g := fifelse(gap == 1, pc_gdp / gdp_lag - 1, NA_real_)]
gdp_dt <- gdp_dt[year != 2025]

# ------------------ params ------------------
birth_start <- 1943; birth_end <- 1988L
year_end_inp <- 2019
age_min <- 20L; age_max <- 75L
span_loess <- 0.30
center_gdp <- FALSE     # set TRUE to use GDP deviations from mean (removes drift)
use_dgdp   <- FALSE     # set TRUE to use Δg instead of g
# --------------------------------------------

# ------------------ base data ----------------
DT <- as.data.table(dt)
DT[, net_hs := (Haig_simon_income + net_trans)/1000]
DT <- DT[year <= year_end_inp & is.finite(net_hs)]
DT[, age := as.integer(age)]
DT[, birth_year := as.integer(year - age)]
DT <- DT[birth_year %between% c(birth_start, birth_end)]
DT[, cohort5_low := birth_year - ((birth_year - birth_start) %% 5L)]
DT[, cohort5_lab := paste0(cohort5_low, "-", cohort5_low + 4L)]
DT[, cohort5_mid := cohort5_low + 2L]
setorder(DT, cohort5_low, year)

# ---------------- GDP growth -----------------
gdp_dt <- as.data.table(gdp_dt)
stopifnot(all(c("year","rgdp_g") %in% names(gdp_dt)))
if (max(abs(gdp_dt$rgdp_g), na.rm = TRUE) > 1) gdp_dt[, rgdp_g := rgdp_g/100]
setorder(gdp_dt, year)

# -------- smoothed cohort anchors (levels) ---
cohort_year <- DT[, .(net_hs = mean(net_hs, na.rm = TRUE)),
                  by = .(cohort5_low, cohort5_lab, cohort5_mid, year)]
smooth_one <- function(d){
  d <- copy(d)[order(year)]
  if (nrow(d) >= 5 && uniqueN(d$year) >= 4) {
    fit <- loess(net_hs ~ year, d, span = span_loess, degree = 1, na.action = na.exclude)
    d[, net_obs := as.numeric(predict(fit, newdata = data.frame(year = year)))]
  } else d[, net_obs := net_hs]
  d[, age := as.integer(year - cohort5_mid)]
  d[between(age, age_min, age_max),
    .(cohort5_low, cohort5_lab, cohort5_mid, age, year, net_obs)]
}
obs <- rbindlist(lapply(split(cohort_year, by = "cohort5_low"), smooth_one))

# ---------- choose transform for “growth” ----
# log by default; fallback to asinh if any nonpositive anchor
use_log <- all(obs$net_obs > 0)
Tfun  <- if (use_log) function(x) log(x) else function(x) asinh(x)
iTfun <- if (use_log) function(z) exp(z) else function(z) sinh(z)

# ---------- growth regressions ---------------
# ΔT(y) by cohort/age/year
G <- obs[order(cohort5_low, age)][
  , .(age = age, year = year, dTy = Tfun(net_obs) - shift(Tfun(net_obs))),
  by = cohort5_low][!is.na(dTy)]

G <- merge(G, gdp_dt[, .(year, rgdp_g)], by = "year", all.x = TRUE)
G[is.na(rgdp_g), rgdp_g := 0]

if (center_gdp) {
  g_bar <- mean(G$rgdp_g, na.rm = TRUE)
  G[, g_used := rgdp_g - g_bar]
} else G[, g_used := rgdp_g]

if (use_dgdp) {                         # optional: use change in growth
  setorder(gdp_dt, year)
  gdp_dt[, d_rgdp_g := rgdp_g - shift(rgdp_g)]
  G <- merge(G, gdp_dt[, .(year, d_rgdp_g)], by = "year", all.x = TRUE)
  G[is.na(d_rgdp_g), d_rgdp_g := 0]
  G[, g_used := d_rgdp_g]
}

m <- lm(dTy ~ g_used + factor(age), data = G)
beta_gdp <- unname(coef(m)[["g_used"]])

gamma_age <- G[, .(gamma = mean(dTy - beta_gdp * g_used, na.rm = TRUE)), by = age][order(age)]
# (light smoothing of age growth profile)
if (nrow(gamma_age) >= 7) {
  fit_g <- loess(gamma ~ age, data = gamma_age, span = 0.35, degree = 1)
  gamma_age[, gamma := as.numeric(predict(fit_g, newdata = data.frame(age = age)))]
}
gamma_at <- function(a) as.numeric(approx(gamma_age$age, gamma_age$gamma, xout = a, rule = 2)$y)

# ---------- full GDP path for all years ------
years_needed <- seq.int(
  min(as.integer(obs$cohort5_mid) + age_min, na.rm = TRUE),
  max(as.integer(obs$cohort5_mid) + age_max, na.rm = TRUE),
  by = 1L
)
years_grid <- data.table(year = years_needed)
gdp_path <- gdp_dt[years_grid, on = "year"]  
last10 <- gdp_dt[year > max(year) - 10L, mean(rgdp_g, na.rm = TRUE)]
if (!is.finite(last10)) last10 <- 0.02
gdp_path[is.na(rgdp_g), rgdp_g := last10]
gdp_path[, d_rgdp_g := rgdp_g - shift(rgdp_g)]

for (i in 1:10){
  gdp_path[year == 2020, rgdp_g := 0.00]   # or set to your 'normal' year
  gdp_path[year >= 2021, rgdp_g := 0.002*i]   # X% p.a. scenario
  
  # Recompute Δg in case you're using use_dgdp = TRUE elsewhere
  gdp_path[, d_rgdp_g := rgdp_g - shift(rgdp_g)]
  
  setkey(gdp_path, year)
  g_lookup  <- function(y) gdp_path[.(as.integer(y)), on="year", rgdp_g]
  dg_lookup <- function(y) gdp_path[.(as.integer(y)), on="year", d_rgdp_g]
  
  # -------------- fill (compounding) -----------
  fill_one <- function(d){
    d <- copy(d)[order(age)]
    if (!nrow(d)) return(NULL)
    
    ages <- seq(age_min, age_max)
    yrs  <- d$cohort5_mid[1] + ages
    g_t  <- if (use_dgdp) dg_lookup(yrs) else g_lookup(yrs)
    if (center_gdp) {
      g_bar <- mean(G$rgdp_g, na.rm = TRUE); g_t <- g_t - ifelse(use_dgdp, 0, g_bar)
    }
    
    # work in transform space
    Ty <- rep(NA_real_, length(ages)); names(Ty) <- as.character(ages)
    Ty[as.character(d$age)] <- Tfun(d$net_obs)
    
    A0 <- min(d$age); B0 <- max(d$age)
    
    # forward: T(y_a) = T(y_{a-1}) + γ_a + β * g_t
    if (B0 < age_max) {
      for (a in seq(B0 + 1L, age_max)) {
        idx <- match(a, ages)
        Ty[as.character(a)] <- Ty[as.character(a - 1L)] + gamma_at(a) + beta_gdp * g_t[idx]
      }
    }
    # back-cast: T(y_a) = T(y_{a+1}) - (γ_{a+1} + β * g_{t+1})
    if (A0 > age_min) {
      for (a in seq(A0 - 1L, age_min, -1L)) {
        idxp1 <- match(a + 1L, ages)
        Ty[as.character(a)] <- Ty[as.character(a + 1L)] - (gamma_at(a + 1L) + beta_gdp * g_t[idxp1])
      }
    }
    
    out <- data.table(
      cohort5_low = d$cohort5_low[1],
      cohort5_lab = d$cohort5_lab[1],
      cohort5_mid = d$cohort5_mid[1],
      age         = ages,
      year        = as.integer(yrs),
      net_filled  = iTfun(Ty),
      source      = fifelse(ages < A0, "Back-cast",
                            fifelse(ages > B0, "Forecast", "Observed"))
    )
    out
  }
  
  filled_dt <- rbindlist(
    lapply(split(obs[, .(cohort5_low, cohort5_lab, cohort5_mid, age, year, net_obs)],
                 by = "cohort5_low"),
           fill_one),
    use.names = TRUE
  )
  setorder(filled_dt, cohort5_low, age)
  
  # ------------------- plots -------------------
  ggplot() +
    geom_line(data = filled_dt[source=="Observed"],
              aes(age, net_filled, colour = cohort5_lab, group = cohort5_lab), linewidth = 0.9) +
    geom_line(data = filled_dt[source=="Back-cast"],
              aes(age, net_filled, colour = cohort5_lab, group = cohort5_lab, linetype = source),
              linewidth = 0.9) +
    geom_line(data = filled_dt[source=="Forecast"],
              aes(age, net_filled, colour = cohort5_lab, group = cohort5_lab, linetype = source),
              linewidth = 0.9) +
    scale_linetype_manual(values = c("Back-cast"="dashed","Forecast"="dotted")) +
    labs(title="Net HS income by age (growth model: ΔT(y)=γ_age+β·GDP)",
         x="Age", y="Thousands ($000)", colour="5-year cohort", linetype="") +
    theme_minimal()
  
  filled_dt[, cum := cumsum(net_filled), by = cohort5_low]
  ggplot(filled_dt, aes(age, cum, colour = cohort5_lab, group = cohort5_lab)) +
    geom_line(linewidth = 1) +
    labs(title="Cumulative net HS income by age (filled; original units)",
         x="Age", y="Cumulative thousands ($000)", colour="5-year cohort") +
    theme_minimal()
  
  ## Checks
  # 
  # # After you've built filled_dt, gdp_path, and set the flags (center_gdp/use_dgdp):
  # 
  # get_g_used <- function(yy) {
  #   g <- if (use_dgdp) gdp_path[.(as.integer(yy)), on="year", d_rgdp_g]
  #   else          gdp_path[.(as.integer(yy)), on="year", rgdp_g]
  #   if (center_gdp && !use_dgdp) {
  #     g_bar <- mean(G$rgdp_g, na.rm = TRUE)  # from the regression data
  #     g <- g - g_bar
  #   }
  #   g
  # }
  # 
  # chk <- filled_dt[source == "Forecast",
  #                  .(year, cohort5_lab, g_used = get_g_used(year))]
  # 
  # summary(chk$g_used)
  # head(chk[order(year)], 10)
  # 
  # 
  # filled_dt[source == "Forecast",
  #           .(avg_macro = mean(get_g_used(year), na.rm = TRUE)), by = cohort5_lab][order(cohort5_lab)]
  
  plot <- ggplot(filled_dt[cohort5_mid %in% c(1950,1970,1990)], aes(age, cum, colour = cohort5_lab, group = cohort5_lab)) +
    geom_line(linewidth = 1) +
    labs(title=paste0("Cumulative net HS income by age (",i*0.2,"% GDP per cap growth)"),
         x="Age", y="Cumulative thousands ($000)", colour="5-year cohort") + theme_e61(legend = "bottom")
  
  print(plot)
  
  save_e61(paste0("Cohort_income_",20*i,"ppt.png"),plot=plot,res=2)
}

