# Topic: Simple debt dynamics scenario using cash balance data from the PBO
# Author: Matt Nolan
# Created: 10/9/2025
# Last edit: 10/9/2025
# Last editor: Matt Nolan
## Note: The gap between payments and revenue does not translate to the change in gross debt for a number of reasons i) capital account transactions ii) issuance that are about liquidity not borrowing iii) the portion of sub-national debt that is included (especially for the Territories) in the Government Gross Debt accounts.  Have projected out a persistent addition to debt on this basis based on historic variance.

## Happy with this structure - just need to update set of projections.

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

## Assumptions

proj_period <- 30

# Elasticity settings (based on this paper - and focusing on the idea that a shock to US bond markets would be the driver behind the interest rate shock in this: https://treasury.gov.au/publication/reconsidering-the-link-between-fiscal-policy-and-interest-rates-in-australia/working-paper-2010-04-reconsidering-the-link-between-fiscal-policy-and-interest-rates-in-australia)
phi_bps_per_ppt <- 3    # 3 bps on r per +1ppt of debt/GDP
phi <- phi_bps_per_ppt / 100  # 3 -> 0.03 because 1ppt=0.01 in ratio

## Import data.

raw <- read_excel("Data_Federal.xlsx")
setDT(raw)

#raw <- raw[Year < 2026] # Use the PBO projections and this can be projections out past that.

# Standardise column names we’ll use (be forgiving to case/spaces)
setnames(raw, tolower(names(raw)))
# Try to detect the year column (often 'unnamed: 0' in your sheet)
if (!("year" %in% names(raw))) {
  ycol <- grep("^unnamed", names(raw), value = TRUE)
  if (length(ycol) == 1) setnames(raw, ycol, "year")
}
stopifnot(all(c("year","receipts","payments","gross_debt","ngdp") %in% names(raw)))

setorder(raw, year)

# Nominal GDP growth (decimal); if provided, trust it; else compute
if (!("ngdp_growth" %in% names(raw))) {
  raw[, ngdp_growth := ngdp / shift(ngdp) - 1]
}

# Effective interest rate r_t (decimal):
# Prefer your interest_rate column; else approximate from cash interest and lagged debt
if (!("interest_rate" %in% names(raw))) {
  if ("interest" %in% names(raw)) {
    raw[, interest_rate := interest / shift(gross_debt)]
  } else {
    raw[, interest_rate := NA_real_]
  }
}

# Interest (gross) level; if missing but we have rate, back it out roughly
if (!("interest" %in% names(raw))) {
  if (!all(is.na(raw$interest_rate))) {
    # Rough back-out: pay interest on prior-year stock; cash basis approximation
    raw[, interest := interest_rate * shift(gross_debt)]
  } else {
    raw[, interest := NA_real_]
  }
}

# Primary balance as % of GDP (surplus +) using gross interest:
# PB = (Receipts - (Payments - Interest)) / NGDP
raw[, pb_gdp := (receipts - (payments - interest)) / ngdp]

# SFA (stock–flow adjustment) – if not provided, derive as residual in levels:
# ΔDebt = (Payments - Receipts) + SFA  =>  SFA = ΔDebt - (Payments - Receipts)
if (!("sfa" %in% names(raw))) {
  raw[, sfa := (gross_debt - shift(gross_debt)) - (payments - receipts)]
}
# Express SFA as % of GDP for dynamics in ratios (use concurrent GDP)
raw[, sfa_gdp := sfa / ngdp]

# Convert stocks/flows to shares of GDP we’ll use
raw[, `:=`(
  b       = gross_debt / ngdp,       # debt ratio (gross)
  int_gdp = interest / ngdp,         # interest as % of GDP
  g       = ngdp_growth,             # nominal GDP growth rate (decimal)
  r       = interest_rate            # effective rate (decimal)
)]

# Keep only the tidy columns we’ll use downstream
hist <- raw[, .(year, g, r, b, pb_gdp, int_gdp, sfa_gdp, ngdp)]
last_hist_year <- max(hist$year, na.rm = TRUE)
b0  <- hist[year == last_hist_year, b][1]   # starting debt/GDP for projections
ngdp0 <- hist[year == last_hist_year, ngdp][1]

# ========= 2) Core simulator (ratios) with debt-sensitive r ===================
# r_t = r_star_t + r_add_t + phi * (b_{t-1} - b_star)
# b_t = ((1 + r_t)/(1 + g_t)) * b_{t-1} - pb_t + sfa_t
# int_gdp_t = r_t * b_{t-1} / (1 + g_t)
simulate_debt <- function(dt, b0, b_star = b0) {
  x <- data.table::copy(dt)
  data.table::setorder(x, year)
  
  # defaults if columns absent
  if (!("phi" %in% names(x)))   x[, phi := 0]
  if (!("r_add" %in% names(x))) x[, r_add := 0]
  if (!("r_star" %in% names(x))) stop("Need r_star in scenario table (baseline rate path).")
  
  n <- nrow(x)
  bt <- numeric(n)
  it <- numeric(n)
  rt <- numeric(n)
  
  b_prev <- b0
  for (i in seq_len(n)) {
    r_eff  <- x$r_star[i] + x$r_add[i] + x$phi[i] * (b_prev - b_star)
    r_eff  <- pmax(r_eff, 0)   # guard against negatives
    rt[i]  <- r_eff
    it[i]  <- r_eff * b_prev / (1 + x$g[i])
    bt[i]  <- ((1 + r_eff)/(1 + x$g[i])) * b_prev - x$pb[i] + x$sfa[i]
    b_prev <- bt[i]
  }
  
  x[, `:=`(r = rt, b = bt, int_gdp = it, overall = pb - it)]
  x[]
}


# ========= 3) Scenario builder ==============================================
# Build scenarios in shares/rates. You pass baseline r in r_star, plus an
# optional additive shock r_add and a phi (bps per ppt mapped below).
make_scenario <- function(years, g_path, r_star_path,
                          receipts_gdp, primary_spend_gdp,
                          phi = 0, r_add = NULL,
                          sfa_rule = c("zero","hist_avg","hist_tail_avg"),
                          hist_sfa = NULL) {
  sfa_rule <- match.arg(sfa_rule)
  sfa <- switch(
    sfa_rule,
    zero = rep(0, length(years)),
    hist_avg = if (!is.null(hist_sfa)) rep(mean(hist_sfa, na.rm = TRUE), length(years)) else rep(0, length(years)),
    hist_tail_avg = if (!is.null(hist_sfa)) {
      tail_n <- min(10, length(hist_sfa))
      rep(mean(tail(hist_sfa, tail_n), na.rm = TRUE), length(years))
    } else rep(0, length(years))
  )
  if (is.null(r_add)) r_add <- rep(0, length(years))
  
  data.table(
    year = years,
    g = g_path,                        # nominal GDP growth (decimal)
    r_star = r_star_path,              # baseline rate (decimal)
    r_add = r_add,                     # additive shock (decimal)
    phi = rep(phi, length(years)),     # sensitivity (decimal per 1.00 in b)
    pb = receipts_gdp - primary_spend_gdp,  # primary bal (% of GDP, + = surplus)
    sfa = sfa
  )
}


# ========= 4) Define projection years & baseline paths ==================
proj_years <- (last_hist_year + 1):(last_hist_year + proj_period)

# Baseline rules:
# - g: average of last 5 years (drop NA/aberrations)
# g_base <- rep(mean(tail(na.omit(hist$g), 5)), length(proj_years))
# g to 5% - 2.5 for inflation, 1.2 for productivity, 1.3 for population growth.
g_base <- rep(0.05, length(proj_years))

# - r: 
# average of last 5 years (effective rate)
#r_star <- rep(mean(tail(na.omit(hist$r), 5)), length(proj_years))
# set to current 10-year bond rate
r_star <- rep(0.042, length(proj_years))

# - receipts and primary spending as % of GDP: use last observed shares
#   receipts_gdp_t = Receipts/NGDP; primary_spend_gdp_t = (Payments - Interest)/NGDP
#   From history:
last_row <- raw[year == last_hist_year]
receipts_last <- last_row$receipts / last_row$ngdp
primary_last  <- (last_row$payments - last_row$interest) / last_row$ngdp

bal_inc <- pmin(seq_along(proj_years) / 2, 1) * 0.0085
rec_base  <- rep(receipts_last , length(proj_years)) + bal_inc # Addition is just lowering what comes from the sfa for nowa
prim_base <- rep(primary_last, length(proj_years)) 

baseline <- make_scenario(
  years = proj_years,
  g_path = g_base,
  r_star_path = r_star,
  receipts_gdp = rec_base,
  primary_spend_gdp = prim_base,
  phi = 0,                               # baseline: keep rate exogenous
  sfa_rule = "hist_tail_avg",
  hist_sfa = hist$sfa_gdp
)[, scenario := "Baseline"]

# Policy scenarios:

# --- 1) Economic shock, NO interest-rate change ----------------------------
# Lower g by 1.5pp for 3y, then back to baseline; no rate response (phi=0)
shock_len <- 3
g_shock   <- g_base - c(rep(0.015, shock_len), rep(0, length(proj_years) - shock_len))

econ_no_r <- make_scenario(
  years = proj_years,
  g_path = g_shock,
  r_star_path = r_star,                   # unchanged
  receipts_gdp = rec_base,
  primary_spend_gdp = prim_base,
  phi = 0,                                # **no** debt-response
  sfa_rule = "hist_tail_avg",
  hist_sfa = hist$sfa_gdp
)[, scenario := "Economic shock (g -1.5pp, 3y) — no IR response"]

## Adjust below to include a literature guided change in GDP, the 150bp shock we have below, and endogenous pass through to revenue and expenditure. [Call this rate shock with economic and fiscal consequences]
# --- 2) Economic shock WITH interest-rate response --------------------------
# Same growth shock, but now the rate responds with phi
econ_with_r <- make_scenario(
  years = proj_years,
  g_path = g_shock,
  r_star_path = r_star,                   # baseline policy/expectations
  receipts_gdp = rec_base,
  primary_spend_gdp = prim_base,
  phi = phi,                              # **endogenous** debt-response
  sfa_rule = "hist_tail_avg",
  hist_sfa = hist$sfa_gdp
)[, scenario := "Economic shock (g -1.5pp, 3y) — IR responds to debt"]

# --- 3) Initial rate shock + endogeneity ------------------------------------
# Add +150bp to r for 3y (r_add), keep g unchanged; keep phi active thereafter
r_add <- c(rep(0.015, shock_len), rep(0.005, length(proj_years) - shock_len))  # your prior shape

rate_shock_endog <- make_scenario(
  years = proj_years,
  g_path = g_base,                        # growth unchanged here
  r_star_path = r_star,
  receipts_gdp = rec_base,
  primary_spend_gdp = prim_base,
  phi = phi,                              # debt-sensitive response
  r_add = r_add,
  sfa_rule = "hist_tail_avg",
  hist_sfa = hist$sfa_gdp
)[, scenario := "Rate shock (+150bp, 3y) + debt-sensitive IR"]

# Bind scenarios
scens <- rbindlist(list(baseline, econ_no_r, econ_with_r, rate_shock_endog), use.names = TRUE)

# ========= 5) Run simulations ==========================================
out <- scens[, simulate_debt(.SD, b0 = b0), by = scenario]

# ========= 6) Plot paths ===============================================

# Debt/GDP
p_debt <- ggplot(out, aes(year, b, colour = scenario)) +
  geom_line(size = 1) +
  labs(title = "Gross debt-to-GDP: scenarios", y = "Debt / GDP", x = NULL, colour = NULL) +
  theme_e61(legend = "bottom")

# Flows (% of GDP): primary, interest, overall balance
flows <- melt(out[, .(scenario, year, pb, int_gdp, overall)],
              id.vars = c("scenario","year"),
              variable.name = "flow", value.name = "share")

p_flows <- ggplot(flows, aes(year, share, colour = flow)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(size = 1) +
  facet_wrap(~ scenario, ncol = 2) +
  labs(title = "Fiscal flows (% of GDP)", y = "% of GDP", x = NULL, colour = NULL) +
  theme_e61(legend = "bottom")

print(p_debt)
#print(p_flows)

# ---- Combine history with projections for a single chart ---------------
hist_line <- hist[!is.na(b), .(scenario = "History", year, b)]
proj_line <- out[, .(scenario, year, b)]
combo_debt <- rbindlist(list(hist_line, proj_line), use.names = TRUE)

scenarios_show <- c("Baseline","Rate shock (+150bp, 3y) + debt-sensitive IR")

p_hist_proj_debt <- ggplot() +
  # history in black
  geom_line(data = hist_line, aes(year, b), size = 1.1, colour = "black") +
  # scenarios in colour
  geom_line(data = proj_line[scenario %in% scenarios_show], aes(year, b, colour = scenario), size = 1) +
  # mark the boundary between history and projections
  geom_vline(xintercept = last_hist_year + 0.5, linetype = "dotted") +
  geom_vline(xintercept = 2025, linetype = "dotted") +
  labs(
    title = "Gross debt-to-GDP scenarios",
    subtitle = "Cash account projections",
    x = NULL, y = "Debt / GDP", colour = NULL
  ) +
  theme_e61(legend = "bottom")

print(p_hist_proj_debt)


out

# # history flows for a cross check: PB, interest, overall = PB - interest
# hist_flows <- hist[!is.na(pb_gdp) & !is.na(int_gdp),
#                    .(scenario = "History", year,
#                      pb = pb_gdp, int_gdp = int_gdp, overall = pb_gdp - int_gdp)]
# 
# proj_flows <- out[, .(scenario, year, pb, int_gdp, overall)]
# 
# flows_comb <- rbindlist(list(hist_flows, proj_flows), use.names = TRUE)
# 
# p_hist_proj_flows <- ggplot(flows_comb, aes(year, pb, colour = "Primary balance")) +
#   geom_line(size = 0.9) +
#   geom_line(aes(y = int_gdp, colour = "Interest")) +
#   geom_line(aes(y = overall, colour = "Overall balance")) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_vline(xintercept = last_hist_year + 0.5, linetype = "dotted") +
#   facet_wrap(~ scenario, ncol = 2) +
#   labs(
#     title = "Fiscal flows (% of GDP): history and scenarios",
#     x = NULL, y = "% of GDP", colour = NULL
#   ) +
#   theme_e61(legend = "bottom")
# 
# print(p_hist_proj_flows)


# # ========= 7) (Optional) Convert ratios back to levels =================
# # If you want nominal debt/interest levels, build an NGDP path from g and b:
# 
# # Build an NGDP path for each scenario (starting from last observed NGDP)
# ngdp_paths <- scens[, .(scenario, year, g)]
# ngdp_paths <- ngdp_paths[, {
#   n <- .N
#   ng <- numeric(n)
#   ng[1] <- ngdp0 * (1 + g[1])
#   if (n > 1) for (i in 2:n) ng[i] <- ng[i-1] * (1 + g[i])
#   .(year = year, ngdp_fwd = ng)
# }, by = scenario]
# 
# out_levels <- merge(out, ngdp_paths, by = c("scenario","year"), all.x = TRUE)
# out_levels[, `:=`(
#   gross_debt_nom = b * ngdp_fwd,
#   interest_nom   = int_gdp * ngdp_fwd
# )]