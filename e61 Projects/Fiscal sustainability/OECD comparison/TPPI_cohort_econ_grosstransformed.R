# Topic: Adjust cohort estimates to be gross and have a tax-transfer function appled.
# Author: Matt Nolan
# Created: 2/9/2025
# Last edit: 2/9/2025
# Last editor: Matt Nolan
## Note, this version is giving nonsense results but the framework is nice - so go through and identify the issues and correct.


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



# ---------------- params ----------------
birth_start <- 1943L; birth_end <- 1988L
year_end_inp <- 2019L              # keep anchors pre-COVID
age_min <- 20L; age_max <- 75L
span_loess <- 0.30
center_gdp <- FALSE                # TRUE -> deviations from mean, removes drift
use_dgdp   <- FALSE                # TRUE -> macro uses Δg instead of g
ratio_cap  <- c(-1, 1)             # clamp r_a into [-100%, +100%]
# ---------------------------------------

# ---------------- base -----------------
DT <- as.data.table(dt)
DT[, gross_hs := Haig_simon_income/1000]                   # thousands
DT[, net_trans_th := (sum_transfers - sum_tax)/1000]       # thousands
DT <- DT[year <= year_end_inp]
DT[, age := as.integer(age)]
DT[, birth_year := as.integer(year - age)]
DT <- DT[birth_year %between% c(birth_start, birth_end)]
DT[, cohort5_low := birth_year - ((birth_year - birth_start) %% 5L)]
DT[, cohort5_lab := paste0(cohort5_low, "-", cohort5_low + 4L)]
DT[, cohort5_mid := cohort5_low + 2L]
setorder(DT, cohort5_low, year)

# --------------- GDP path --------------
gdp_dt <- as.data.table(gdp_dt)
if (max(abs(gdp_dt$rgdp_g), na.rm = TRUE) > 1) gdp_dt[, rgdp_g := rgdp_g/100]
setorder(gdp_dt, year)

years_needed <- seq.int(
  min(as.integer(DT$cohort5_mid) + age_min, na.rm = TRUE),
  max(as.integer(DT$cohort5_mid) + age_max, na.rm = TRUE),
  by = 1L
)
years_grid <- data.table(year = years_needed)

# left-join keeps the full grid
gdp_path <- gdp_dt[years_grid, on = "year"]

# ---- GDP scenario for missing years (EDIT as you like) ----
trend <- gdp_dt[year %between% c(2000, 2019), mean(rgdp_g, na.rm = TRUE)]
if (!is.finite(trend)) trend <- 0.02
gdp_path[is.na(rgdp_g), rgdp_g := trend]
gdp_path[, d_rgdp_g := rgdp_g - shift(rgdp_g)]
setkey(gdp_path, year)

g_lookup  <- function(y) gdp_path[.(as.integer(y)), on="year", rgdp_g]
dg_lookup <- function(y) gdp_path[.(as.integer(y)), on="year", d_rgdp_g]

# --------------- anchors (GROSS, levels) ---------------
cohort_year <- DT[, .(gross = mean(gross_hs, na.rm = TRUE)),
                  by = .(cohort5_low, cohort5_lab, cohort5_mid, year)]

smooth_one <- function(d){
  d <- copy(d)[order(year)]
  if (nrow(d) >= 5 && uniqueN(d$year) >= 4){
    fit <- loess(gross ~ year, d, span = span_loess, degree = 1, na.action = na.exclude)
    d[, gross_obs := as.numeric(predict(fit, data.frame(year = year)))]
  } else d[, gross_obs := gross]
  d[, age := as.integer(year - cohort5_mid)]
  d[between(age, age_min, age_max),
    .(cohort5_low, cohort5_lab, cohort5_mid, age, year, gross_obs)]
}
obs_gross <- rbindlist(lapply(split(cohort_year, by="cohort5_low"), smooth_one))
setorder(obs_gross, cohort5_low, age)

# --------------- growth model on GROSS -------------------
# transform: log by default, fallback to asinh if <=0 exists
use_log <- all(obs_gross$gross_obs > 0)
Tfun  <- if (use_log) function(x) log(x) else function(x) asinh(x)
iTfun <- if (use_log) function(z) exp(z) else function(z) sinh(z)

G <- obs_gross[order(cohort5_low, age)][,
                                        .(age = age, year = year, dTy = Tfun(gross_obs) - shift(Tfun(gross_obs))),
                                        by = cohort5_low][!is.na(dTy)]
G <- merge(G, gdp_dt[, .(year, rgdp_g)], by="year", all.x=TRUE)
G[is.na(rgdp_g), rgdp_g := 0]

if (center_gdp) {
  g_bar <- mean(G$rgdp_g, na.rm = TRUE); G[, g_used := rgdp_g - g_bar]
} else G[, g_used := rgdp_g]
if (use_dgdp) {
  G <- merge(G, gdp_path[, .(year, d_rgdp_g)], by="year", all.x=TRUE)
  G[is.na(d_rgdp_g), d_rgdp_g := 0]; G[, g_used := d_rgdp_g]
}

m <- lm(dTy ~ g_used + factor(age), data = G)
beta_gdp <- unname(coef(m)[["g_used"]])

gamma_age <- G[, .(gamma = mean(dTy - beta_gdp * g_used, na.rm=TRUE)), by=age][order(age)]
if (nrow(gamma_age) >= 7) {
  fit_g <- loess(gamma ~ age, data = gamma_age, span = 0.35, degree = 1)
  gamma_age[, gamma := as.numeric(predict(fit_g, data.frame(age = age)))]
}
gamma_at <- function(a) as.numeric(approx(gamma_age$age, gamma_age$gamma, xout=a, rule=2)$y)

# --------------- fill GROSS by compounding ---------------
fill_one_gross <- function(d){
  d <- copy(d)[order(age)]; if (!nrow(d)) return(NULL)
  ages <- seq(age_min, age_max); yrs <- d$cohort5_mid[1] + ages
  
  g_t <- if (use_dgdp) dg_lookup(yrs) else g_lookup(yrs)
  if (center_gdp && !use_dgdp) g_t <- g_t - mean(G$rgdp_g, na.rm = TRUE)
  
  Ty <- rep(NA_real_, length(ages)); names(Ty) <- as.character(ages)
  Ty[as.character(d$age)] <- Tfun(d$gross_obs)
  
  A0 <- min(d$age); B0 <- max(d$age)
  
  if (B0 < age_max) for (a in seq(B0+1L, age_max)){
    idx <- match(a, ages)
    Ty[as.character(a)] <- Ty[as.character(a-1L)] + gamma_at(a) + beta_gdp * g_t[idx]
  }
  if (A0 > age_min) for (a in seq(A0-1L, age_min, -1L)){
    idxp1 <- match(a+1L, ages)
    Ty[as.character(a)] <- Ty[as.character(a+1L)] - (gamma_at(a+1L) + beta_gdp * g_t[idxp1])
  }
  
  data.table(
    cohort5_low = d$cohort5_low[1],
    cohort5_lab = d$cohort5_lab[1],
    cohort5_mid = d$cohort5_mid[1],
    age = ages,
    year = as.integer(yrs),
    gross_filled = iTfun(Ty),
    source = fifelse(ages < A0, "Back-cast", fifelse(ages > B0, "Forecast", "Observed"))
  )
}

filled_gross_dt <- rbindlist(
  lapply(split(obs_gross[, .(cohort5_low, cohort5_lab, cohort5_mid, age, year, gross_obs)],
               by="cohort5_low"),
         fill_one_gross),
  use.names=TRUE
)
setorder(filled_gross_dt, cohort5_low, age)

# --------------- build AGE net-transfer ratio r_a ---------------
# r = net_trans / gross.  Demean by year, then take median by age; smooth & cap.
R <- DT[year <= year_end_inp & gross_hs > 0,
        .(age, year, gross = gross_hs, net_tr = net_trans_th)]
R[, r := net_tr / pmax(gross, 1e-6)]
R <- R[is.finite(r)]

yr_med <- R[, .(rbar = median(r, na.rm=TRUE)), by=year]
R <- R[yr_med, on="year"]
R[, r_neu := r - rbar]                                  # remove year FE
r_age <- R[, .(r_age = median(r_neu, na.rm=TRUE)), by=age][order(age)]
grand <- median(R$r, na.rm=TRUE)
r_age[, r_use := pmax(ratio_cap[1], pmin(r_age + grand, ratio_cap[2]))]

# smooth over age
if (nrow(r_age) >= 7) {
  fit_r <- loess(r_use ~ age, data = r_age, span = 0.35, degree = 1)
  r_age[, r_use := as.numeric(predict(fit_r, data.frame(age = age)))]
}
setkey(r_age, age)

# --------------- convert GROSS -> NET ----------------------
filled_net_dt <- merge(filled_gross_dt, r_age[, .(age, r_use)], by="age", all.x=TRUE)
filled_net_dt[, net_filled := gross_filled * (1 + r_use)]

# --------------- minimal plots ----------------------------
# (1) Gross levels
ggplot() +
  geom_line(data = filled_gross_dt[source=="Observed"],
            aes(age, gross_filled, colour = cohort5_lab, group = cohort5_lab), linewidth=0.9) +
  geom_line(data = filled_gross_dt[source=="Back-cast"],
            aes(age, gross_filled, colour = cohort5_lab, group = cohort5_lab, linetype=source), linewidth=0.9) +
  geom_line(data = filled_gross_dt[source=="Forecast"],
            aes(age, gross_filled, colour = cohort5_lab, group = cohort5_lab, linetype=source), linewidth=0.9) +
  scale_linetype_manual(values=c("Back-cast"="dashed","Forecast"="dotted")) +
  labs(title="GROSS HS income by age (growth model)", x="Age", y="Thousands ($000)",
       colour="5-year cohort", linetype="") + theme_minimal()

# (2) Net (via age ratio)
ggplot() +
  geom_line(data = filled_net_dt[source=="Observed"],
            aes(age, net_filled, colour = cohort5_lab, group = cohort5_lab), linewidth=0.9) +
  geom_line(data = filled_net_dt[source=="Back-cast"],
            aes(age, net_filled, colour = cohort5_lab, group = cohort5_lab, linetype=source), linewidth=0.9) +
  geom_line(data = filled_net_dt[source=="Forecast"],
            aes(age, net_filled, colour = cohort5_lab, group = cohort5_lab, linetype=source), linewidth=0.9) +
  scale_linetype_manual(values=c("Back-cast"="dashed","Forecast"="dotted")) +
  labs(title="NET HS income by age (gross imputed × age transfer ratio)",
       x="Age", y="Thousands ($000)", colour="5-year cohort", linetype="") +
  theme_minimal()

# --------------- outputs ------------------
# filled_gross_dt : cohort-level gross series (observed/back-cast/forecast), original units
# r_age           : age-specific net-transfer ratio used
# filled_net_dt   : projected NET series derived from gross × (1 + r_age)
