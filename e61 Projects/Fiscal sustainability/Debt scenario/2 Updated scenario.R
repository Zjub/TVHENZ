# Topic: Simple debt dynamics scenario using cash balance data from the PBO
# Author: Matt Nolan
# Created: 10/9/2025
# Last edit: 11/9/2025
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

## ---------------- Assumptions ----------------
proj_period <- 30

# Debt -> rate semi-elasticity (small, positive)
phi_bps_per_ppt <- 3
phi <- phi_bps_per_ppt / 100   # 0.03 (3 bps per +1ppt of debt/GDP)

## ---------------- Import data ----------------
raw <- read_excel("Data_Federal.xlsx")
setDT(raw)

# Standardise column names
setnames(raw, tolower(names(raw)))
if (!("year" %in% names(raw))) {
  ycol <- grep("^unnamed", names(raw), value = TRUE)
  if (length(ycol) == 1) setnames(raw, ycol, "year")
}
stopifnot(all(c("year","receipts","payments","gross_debt","ngdp") %in% names(raw)))
setorder(raw, year)

# Nominal GDP growth if missing
if (!("ngdp_growth" %in% names(raw))) raw[, ngdp_growth := ngdp / shift(ngdp) - 1]

# Effective interest rate if missing
if (!("interest_rate" %in% names(raw))) {
  if ("interest" %in% names(raw)) raw[, interest_rate := interest / shift(gross_debt)] else raw[, interest_rate := NA_real_]
}

# Interest level if missing but rate exists
if (!("interest" %in% names(raw))) {
  if (!all(is.na(raw$interest_rate))) raw[, interest := interest_rate * shift(gross_debt)] else raw[, interest := NA_real_]
}

# Primary balance % of GDP (surplus +)
raw[, pb_gdp := (receipts - (payments - interest)) / ngdp]

# SFA and as % of GDP
if (!("sfa" %in% names(raw))) raw[, sfa := (gross_debt - shift(gross_debt)) - (payments - receipts)]
raw[, sfa_gdp := sfa / ngdp]

# Tidy ratios/rates
raw[, `:=`(
  b       = gross_debt / ngdp,
  int_gdp = interest / ngdp,
  g       = ngdp_growth,
  r       = interest_rate
)]

hist <- raw[, .(year, g, r, b, pb_gdp, int_gdp, sfa_gdp, ngdp)]
last_hist_year <- max(hist$year, na.rm = TRUE)
b0    <- hist[year == last_hist_year, b][1]
ngdp0 <- hist[year == last_hist_year, ngdp][1]

## ---------------- Core simulator ----------------
# r_t = r_star_t + r_add_t + phi * (b_{t-1} - b_star)
# b_t = ((1 + r_t)/(1 + g_t)) * b_{t-1} - pb_t + sfa_t
# int_gdp_t = r_t * b_{t-1} / (1 + g_t)
simulate_debt <- function(dt, b0, b_star = b0) {
  x <- data.table::copy(dt); data.table::setorder(x, year)
  if (!("phi" %in% names(x)))   x[, phi := 0]
  if (!("r_add" %in% names(x))) x[, r_add := 0]
  if (!("r_star" %in% names(x))) stop("Need r_star in scenario table (baseline rate path).")
  
  n <- nrow(x)
  bt <- numeric(n); it <- numeric(n); rt <- numeric(n)
  rex <- numeric(n); rphi <- numeric(n)
  b_prev <- b0
  
  for (i in seq_len(n)) {
    rex[i]  <- x$r_star[i] + x$r_add[i]
    rphi[i] <- x$phi[i] * (b_prev - b_star)
    r_eff   <- pmax(rex[i] + rphi[i], 0)
    rt[i]   <- r_eff
    it[i]   <- r_eff * b_prev / (1 + x$g[i])
    bt[i]   <- ((1 + r_eff)/(1 + x$g[i])) * b_prev - x$pb[i] + x$sfa[i]
    b_prev  <- bt[i]
  }
  
  x[, `:=`(r_exog = rex, r_phi = rphi, r = rt, b = bt, int_gdp = it, overall = pb - it)]
  x[]
}

## ---------------- Scenario builder ----------------
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
  
  # Guards to avoid silent recycling
  stopifnot(length(years) == length(g_path),
            length(years) == length(r_star_path),
            length(years) == length(r_add))
  
  data.table(
    year = years,
    g = g_path,
    r_star = r_star_path,
    r_add = r_add,
    phi = rep(phi, length(years)),
    pb = receipts_gdp - primary_spend_gdp,
    sfa = sfa
  )
}

## ---------------- Robust transient shock helpers ----------------
# Safe finite-impulse convolution (no ts/length surprises)
fir_apply <- function(x, kernel) {
  n <- length(x); L <- length(kernel)
  out <- numeric(n)
  for (t in seq_len(n)) {
    acc <- 0
    for (k in 0:(L-1)) {
      idx <- t - k
      if (idx >= 1) acc <- acc + kernel[k + 1] * x[idx]
    }
    out[t] <- acc
  }
  out
}

# Transient GDP response driven by contemporaneous + lagged rate shock.
# Kernel sums to zero, so the level gap eventually returns to zero.
make_transient_growth_from_rshock <- function(r_add, g_base,
                                              target_cum_drop = 0.035,
                                              neg_weights = c(0.5, 0.3, 0.2)) {
  stopifnot(length(r_add) == length(g_base))
  wneg <- neg_weights / sum(neg_weights)
  beta_base <- c(-wneg, sum(wneg))  # e.g. [-0.5, -0.3, -0.2, +1.0], sum = 0
  
  # Calibrate on canonical +150bp-for-3y shock
  can <- rep(0, length(r_add)); can[1:3] <- 0.015
  conv1 <- fir_apply(can, beta_base)
  s3_unit <- -sum(conv1[1:3])                 # cumulative growth drop in years 1–3
  gamma <- if (s3_unit > 0) target_cum_drop / s3_unit else 0
  
  delta_g <- gamma * fir_apply(r_add, beta_base)
  g_path  <- g_base + delta_g
  gap     <- cumsum(delta_g)                  # level gap: dips, then reverts to ~0
  
  list(g_path = g_path, gap = gap, delta_g = delta_g, beta = gamma * beta_base, gamma = gamma)
}

# Transient budget pass-through (PB returns to baseline as gap → 0)
# dPB (%GDP) = semi_pb_per_1pct * (gap * 100); allocate mostly to receipts.
apply_fiscal_pass_through_transient <- function(rec_base, prim_base, gap,
                                                semi_pb_per_1pct = 0.003,
                                                share_revenue = 0.85) {
  stopifnot(length(rec_base) == length(gap), length(prim_base) == length(gap))
  dPB <- semi_pb_per_1pct * (gap * 100)      # negative when gap < 0
  rec_change  <-  share_revenue      * dPB   # gap<0 → receipts ↓
  prim_change <- -(1 - share_revenue) * dPB  # gap<0 → primary ↑
  list(
    receipts = rec_base + rec_change,
    primary  = prim_base + prim_change
  )
}

## ---------------- Baseline primitives ----------------
proj_years <- (last_hist_year + 1):(last_hist_year + proj_period)
g_base <- rep(0.05,  length(proj_years))   # nominal GDP growth
r_star <- rep(0.042, length(proj_years))   # baseline rate anchor

last_row      <- raw[year == last_hist_year]
receipts_last <- last_row$receipts / last_row$ngdp
primary_last  <- (last_row$payments - last_row$interest) / last_row$ngdp

bal_inc   <- pmin(seq_along(proj_years) / 2, 1) * 0.0085
rec_base  <- rep(receipts_last, length(proj_years)) + bal_inc
prim_base <- rep(primary_last,  length(proj_years))

# Rate shock path: +150bp for 3y, then 0 (your requirement)
r_add_prior <- c(rep(0.015, 3), rep(0.005, length(proj_years) - 3))

## ---------------- Canonical names ----------------
nm_baseline <- "Baseline"
nm_rateonly <- "Rate shock (+150bp, 3y) + debt-sensitive IR"
nm_gdp      <- "IR shock: +150bp + transient GDP effect (built on 2)"
nm_fiscal   <- "IR shock: +150bp + transient GDP & fiscal pass-through (built on 3)"

## ---------------- Build scenarios ----------------
# (1) Baseline (exogenous r, phi=0)
baseline <- make_scenario(
  years = proj_years, g_path = g_base, r_star_path = r_star,
  receipts_gdp = rec_base, primary_spend_gdp = prim_base,
  phi = 0, r_add = rep(0, length(proj_years)),
  sfa_rule = "hist_tail_avg", hist_sfa = hist$sfa_gdp
)[, scenario := nm_baseline]

# (2) Rates-only (φ ON, same r_add, PB and g unchanged)
rate_only <- make_scenario(
  years = proj_years, g_path = g_base, r_star_path = r_star,
  receipts_gdp = rec_base, primary_spend_gdp = prim_base,
  phi = phi, r_add = r_add_prior,
  sfa_rule = "hist_tail_avg", hist_sfa = hist$sfa_gdp
)[, scenario := nm_rateonly]

# (3) Transient GDP effect (built on 2)
g_dyn <- make_transient_growth_from_rshock(
  r_add = r_add_prior, g_base = g_base,
  target_cum_drop = 0.035, neg_weights = c(0.5, 0.3, 0.2)
)
# small numeric clean
g_dyn$gap[abs(g_dyn$gap) < 1e-12] <- 0

rate_gdp <- make_scenario(
  years = proj_years, g_path = g_dyn$g_path, r_star_path = r_star,
  receipts_gdp = rec_base, primary_spend_gdp = prim_base,
  phi = phi, r_add = r_add_prior,
  sfa_rule = "hist_tail_avg", hist_sfa = hist$sfa_gdp
)[, scenario := nm_gdp]

# (4) Transient fiscal pass-through (built on 3)
fp_dyn <- apply_fiscal_pass_through_transient(
  rec_base = rec_base, prim_base = prim_base, gap = g_dyn$gap,
  semi_pb_per_1pct = 0.003, share_revenue = 0.85
)

rate_gdp_fiscal <- make_scenario(
  years = proj_years, g_path = g_dyn$g_path, r_star_path = r_star,
  receipts_gdp = fp_dyn$receipts, primary_spend_gdp = fp_dyn$primary,
  phi = phi, r_add = r_add_prior,
  sfa_rule = "hist_tail_avg", hist_sfa = hist$sfa_gdp
)[, scenario := nm_fiscal]

# Guards
stopifnot(nrow(baseline)       == length(proj_years),
          nrow(rate_only)      == length(proj_years),
          nrow(rate_gdp)       == length(proj_years),
          nrow(rate_gdp_fiscal)== length(proj_years))

## ---------------- Bind & simulate ----------------
scens <- data.table::rbindlist(
  list(baseline, rate_only, rate_gdp, rate_gdp_fiscal),
  use.names = TRUE, fill = TRUE
)

cat("Scenarios in scens:\n"); print(sort(unique(scens$scenario)))

out <- scens[, simulate_debt(.SD, b0 = b0), by = scenario]

cat("Scenarios in out:\n"); print(sort(unique(out$scenario)))

## ---------------- Plots ----------------
# Scenario-only
p_debt <- ggplot(out, aes(year, b, colour = scenario)) +
  geom_line(size = 1) +
  labs(title = "Gross debt-to-GDP: scenarios", y = "Debt / GDP", x = NULL, colour = NULL) +
  theme_e61(legend = "bottom")
print(p_debt)

# History attached to every scenario
scenarios <- unique(out$scenario)

hist_expanded <- data.table::CJ(scenario = scenarios, year = hist[!is.na(b), year])
hist_expanded <- hist_expanded[hist[!is.na(b), .(year, b)], on = "year"]
hist_expanded[, segment := "History"]

proj_expanded <- out[, .(scenario, year, b)]
proj_expanded[, segment := "Projection"]

plot_df <- rbindlist(list(hist_expanded, proj_expanded), use.names = TRUE)
setorder(plot_df, scenario, year)

order_levels <- c(nm_rateonly, nm_gdp, nm_fiscal, nm_baseline)
plot_df[, scenario := factor(scenario, levels = order_levels)]

# Single geom_line for each scenario (history+projection continuous)
p_hist_proj_debt <- ggplot(plot_df, aes(year, b, colour = scenario, group = scenario)) +
  geom_line(size = 1) +
  geom_vline(xintercept = last_hist_year + 0.5, linetype = "dotted") +
  labs(
    title    = "Gross debt-to-GDP — history and projections",
    subtitle = "Baseline, rates-only (replica), then transient GDP & fiscal layered on top",
    x = NULL, y = "Debt / GDP", colour = NULL
  )
print(p_hist_proj_debt)


# Show only: Baseline, Rates-only, and Rates+GDP&Fiscal
scenarios_show <- c("Baseline", "Rate shock (+150bp, 3y) + debt-sensitive IR", "IR shock: +150bp + transient GDP & fiscal pass-through (built on 3)")
plot_df3 <- plot_df[scenario %in% scenarios_show]

p_hist_proj_debt <- ggplot(plot_df3, aes(year, b*100, colour = scenario, group = scenario)) +
  geom_line() +
  geom_vline(xintercept = last_hist_year + 0.5, linetype = "dotted") +
  geom_vline(xintercept = 2024, linetype = "dotted") +
  labs_e61(
    title    = "Gross debt-to-GDP — interest rate shock",
    x = NULL, y = "Gross Debt / GDP",
    sources  = c("e61","ABS","PBO"),
    footnotes = c("The full shock reflects an external debt crisis in the US that drives up domestic bond rates - generating an economic response. It includes an estimated GDP response, combined with automatic changes to government expenditure and revenue. No policy response is included.","Shock shown involves a three year 150bp increase in the 10-year bond rate in 2030, with a persistent 50bp change in later years.")
  ) +
  plab(c("150bp higher interest rates","External bond market shock","Base Scenario"),
       x = c(1980,1980,1980), y = c(45,55,35))

print(p_hist_proj_debt)

save_e61("Updated_Interest_rate_shock.png",res=2)

plot_df3[year %in% c(2025,2055)]

# Final dataset
out
