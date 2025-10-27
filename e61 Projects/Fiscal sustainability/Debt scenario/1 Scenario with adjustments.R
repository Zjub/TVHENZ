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

# Elasticity settings (AU Treasury WP 2010-04 style: small, positive)
phi_bps_per_ppt <- 3                   # 3 bps on r per +1ppt of debt/GDP
phi <- phi_bps_per_ppt / 100           # => 0.03 because 1ppt=0.01 in ratio

## ---------------- Import data ----------------
raw <- read_excel("Data_Federal.xlsx")
setDT(raw)

# Standardise column names we’ll use (be forgiving to case/spaces)
setnames(raw, tolower(names(raw)))
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

# Effective interest rate r_t (decimal): prefer interest_rate; else approximate
if (!("interest_rate" %in% names(raw))) {
  if ("interest" %in% names(raw)) raw[, interest_rate := interest / shift(gross_debt)]
  else raw[, interest_rate := NA_real_]
}

# Interest (gross) level; if missing but we have rate, back it out roughly
if (!("interest" %in% names(raw))) {
  if (!all(is.na(raw$interest_rate))) raw[, interest := interest_rate * shift(gross_debt)]
  else raw[, interest := NA_real_]
}

# Primary balance as % of GDP (surplus +): PB = (Receipts - (Payments - Interest)) / NGDP
raw[, pb_gdp := (receipts - (payments - interest)) / ngdp]

# SFA (stock–flow adjustment) – residual
if (!("sfa" %in% names(raw))) {
  raw[, sfa := (gross_debt - shift(gross_debt)) - (payments - receipts)]
}
raw[, sfa_gdp := sfa / ngdp]

# Convert to ratios/rates
raw[, `:=`(
  b       = gross_debt / ngdp,       # debt ratio (gross)
  int_gdp = interest / ngdp,         # interest as % of GDP
  g       = ngdp_growth,             # nominal GDP growth (decimal)
  r       = interest_rate            # effective rate (decimal)
)]

# Keep only columns we need downstream
hist <- raw[, .(year, g, r, b, pb_gdp, int_gdp, sfa_gdp, ngdp)]
last_hist_year <- max(hist$year, na.rm = TRUE)
b0    <- hist[year == last_hist_year, b][1]       # starting debt/GDP for projections
ngdp0 <- hist[year == last_hist_year, ngdp][1]

## ---------------- Core simulator (ratios) with debt-sensitive r ------------
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
  b_prev <- b0
  for (i in seq_len(n)) {
    r_eff <- x$r_star[i] + x$r_add[i] + x$phi[i] * (b_prev - b_star)
    r_eff <- pmax(r_eff, 0)    # guard against negatives if needed
    rt[i] <- r_eff
    it[i] <- r_eff * b_prev / (1 + x$g[i])
    bt[i] <- ((1 + r_eff)/(1 + x$g[i])) * b_prev - x$pb[i] + x$sfa[i]
    b_prev <- bt[i]
    print(c(bt[i],r_eff))
  }
  x[, `:=`(r = rt, b = bt, int_gdp = it, overall = pb - it)]
  x[]
}

## ---------------- Scenario builder -----------------------------------------
# Build scenarios in shares/rates. You pass baseline r in r_star, plus an
# optional additive shock r_add and a phi.
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
    pb = receipts_gdp - primary_spend_gdp,  # primary balance (% of GDP, + = surplus)
    sfa = sfa
  )
}

## ---------------- Shock helpers --------------------------------------------
## Below were keeping revenue persistently away from target
# Map a +150bp long-rate shock to nominal g via a 3-year hump to reach ~3.5% cum level gap.
make_g_shock_path <- function(g_base, cum_level_drop = 0.035, years_hump = 3,
                              weights = c(0.5, 0.3, 0.2)) {
  w <- weights / sum(weights)
  deltas <- -cum_level_drop * w                 # growth deltas over first 3 years
  g_out <- g_base
  n <- min(years_hump, length(g_base))
  g_out[seq_len(n)] <- g_base[seq_len(n)] + deltas[seq_len(n)]
  # Also return the (positive) level-gap path for pass-through (e.g., ~1.75%, 2.8%, 3.5%)
  list(g_path = g_out, level_gap = cumsum(-deltas)[seq_len(n)])
}

# Budget semi-elasticity: PB worsens ~0.3pp of GDP per 1% GDP gap.
# 85% via receipts (down), 15% via primary spending (up). Persist final gap after y3.
apply_fiscal_pass_through <- function(rec_base, prim_base, proj_years,
                                      level_gap_first3, semi_pb_per_1pct = 0.003,
                                      share_revenue = 0.85) {
  gap <- rep(0, length(proj_years))
  n <- min(3, length(proj_years))
  final_gap <- tail(level_gap_first3, 1)
  gap[seq_len(n)] <- level_gap_first3
  if (length(proj_years) > n) gap[(n+1):length(proj_years)] <- final_gap

  rec_change  <- -share_revenue       * semi_pb_per_1pct * (gap * 100)  # receipts ↓
  prim_change <- +(1 - share_revenue) * semi_pb_per_1pct * (gap * 100)  # primary ↑

  list(receipts = rec_base + rec_change,
       primary  = prim_base + prim_change)
}

# === Transient GDP & fiscal effects, tied to contemporaneous and lagged r_add ===

# Build a *finite-impulse* growth response to the rate shock:
# delta_g_t = sum_{k=0..3} beta_k * r_add_{t-k}, with sum(beta)=0,
# so level effects *revert to zero* after the shock + lags.
# Calibrate so that a +150bp shock sustained for 3y implies ~3.5% lower level at year 3,
# but then the gap closes back to zero as lags unwind.
make_transient_growth_from_rshock <- function(r_add, g_base,
                                              target_cum_drop = 0.035,
                                              neg_weights = c(0.5, 0.3, 0.2)) {
  # base shape: negative for 3 years, then positive rebound that closes the gap
  wneg <- neg_weights / sum(neg_weights)
  beta_base <- c(-wneg, sum(wneg))  # [-.5,-.3,-.2,+1.0], sum=0
  
  # calibrate scale (gamma) on a canonical 150bp-for-3y shock
  can <- c(rep(0.015, 3), rep(0, length(r_add) - 3))
  conv1 <- stats::filter(can, beta_base, sides = 1, method = "convolution")
  conv1[is.na(conv1)] <- 0
  s3_unit <- -sum(conv1[1:3])              # cum growth drop over first 3 years (unit scale)
  gamma <- if (s3_unit > 0) target_cum_drop / s3_unit else 0
  
  # apply to actual r_add
  conv <- stats::filter(r_add, beta_base, sides = 1, method = "convolution")
  conv[is.na(conv)] <- 0
  delta_g <- gamma * as.numeric(conv)      # temporary growth deviations
  g_path  <- g_base + delta_g
  gap     <- cumsum(delta_g)               # level gap: goes down, then reverses to 0
  
  list(g_path = g_path, gap = gap, delta_g = delta_g,
       beta = gamma * beta_base, gamma = gamma)
}

# Budget semi-elasticity on a *temporary* output gap (so PB returns to baseline).
# 1% lower GDP level → PB worsens by ~0.3pp of GDP; split 85% receipts, 15% primary.
apply_fiscal_pass_through_transient <- function(rec_base, prim_base, gap,
                                                semi_pb_per_1pct = 0.003,  # ≈0.3pp per 1% GDP gap
                                                share_revenue = 0.85) {
  # ΔPB (% of GDP) for a level gap (gap is in decimals, e.g. -0.02 = -2%)
  dPB <- semi_pb_per_1pct * (gap * 100)  # negative when gap < 0
  
  # Allocate: receipts take share_revenue of ΔPB; spending takes the rest with opposite sign
  rec_change  <- share_revenue * dPB           # gap<0 → receipts ↓
  prim_change <- -(1 - share_revenue) * dPB    # gap<0 → primary spend ↑
  
  list(
    receipts = rec_base + rec_change,
    primary  = prim_base + prim_change
  )
}


## ---------------- Baseline primitives --------------------------------------
proj_years <- (last_hist_year + 1):(last_hist_year + proj_period)

# Baseline macro paths
g_base <- rep(0.05,  length(proj_years))   # nominal GDP growth
r_star <- rep(0.042, length(proj_years))   # baseline effective rate (anchor)

# Last observed shares (from your data)
last_row      <- raw[year == last_hist_year]
receipts_last <- last_row$receipts / last_row$ngdp
primary_last  <- (last_row$payments - last_row$interest) / last_row$ngdp

# Your phased receipts lift
bal_inc   <- pmin(seq_along(proj_years) / 2, 1) * 0.0085
rec_base  <- rep(receipts_last, length(proj_years)) + bal_inc
prim_base <- rep(primary_last,  length(proj_years))

# IMPORTANT: exact r_add used in your prior script (replicate!)
# +150bp for 3 years, THEN +50bp thereafter (not zero)
r_add_prior <- c(rep(0.015, 3), rep(0.000, length(proj_years) - 3))

## ---------------- The four scenarios ---------------------------------------
# 1) Baseline (exogenous r, φ = 0) — matches your earlier baseline
baseline <- make_scenario(
  years = proj_years,
  g_path = g_base,
  r_star_path = r_star,
  receipts_gdp = rec_base,
  primary_spend_gdp = prim_base,
  phi = 0,                                 # baseline: keep rate exogenous
  r_add = rep(0, length(proj_years)),
  sfa_rule = "hist_tail_avg", hist_sfa = hist$sfa_gdp
)[, scenario := "Baseline"]

# 2) Rates-only: EXACT replica of your prior "Rate shock (+150bp, 3y) + debt-sensitive IR"
#    -> g unchanged, PB unchanged, φ ON, r_add = r_add_prior
rate_only <- make_scenario(
  years = proj_years,
  g_path = g_base,                          # unchanged GDP path
  r_star_path = r_star,
  receipts_gdp = rec_base,                  # unchanged PB components
  primary_spend_gdp = prim_base,
  phi = phi,                                # debt-sensitive response ON
  r_add = r_add_prior,                      # +150bp for 3y, then +50bp thereafter
  sfa_rule = "hist_tail_avg", hist_sfa = hist$sfa_gdp
)[, scenario := "Rate shock (+150bp, 3y) + debt-sensitive IR"]

# 3) Add GDP effect on top of (2): same r_add & φ, but GDP lowered (per MARTIN scaling)
g_cal <- make_g_shock_path(g_base, cum_level_drop = 0.035, years_hump = 3)
rate_gdp <- make_scenario(
  years = proj_years,
  g_path = g_cal$g_path,                    # lower g 3y → persistent level gap
  r_star_path = r_star,
  receipts_gdp = rec_base,
  primary_spend_gdp = prim_base,
  phi = phi,                                # keep φ ON
  r_add = r_add_prior,                      # same rate shock as (2)
  sfa_rule = "hist_tail_avg", hist_sfa = hist$sfa_gdp
)[, scenario := "Rate shock: +150bp + GDP effect (built on 2)"]

# 4) Add fiscal pass-through on top of (3): receipts ↓, primary ↑ persistently
fp <- apply_fiscal_pass_through(
  rec_base = rec_base, prim_base = prim_base, proj_years = proj_years,
  level_gap_first3 = g_cal$level_gap, semi_pb_per_1pct = 0.003, share_revenue = 0.85
)
rate_gdp_fiscal <- make_scenario(
  years = proj_years,
  g_path = g_cal$g_path,
  r_star_path = r_star,
  receipts_gdp = fp$receipts,               # apply stabilisers
  primary_spend_gdp = fp$primary,
  phi = phi,                                # keep φ ON
  r_add = r_add_prior,                      # same rate shock as (2)
  sfa_rule = "hist_tail_avg", hist_sfa = hist$sfa_gdp
)[, scenario := "Rate shock: +150bp + GDP & fiscal pass-through (built on 3)"]

## Testing new version of scenarios 3 and 4 with less persistent revenue shocks
# (3) Add GDP effect ON TOP of (2): same r_add & φ, *transient* GDP response
# g_dyn <- make_transient_growth_from_rshock(
#   r_add = r_add_prior,
#   g_base = g_base,
#   target_cum_drop = 0.035,       # 150bp → ~3.5% lower level at year 3 (RBA MARTIN scaling)
#   neg_weights = c(0.5, 0.3, 0.2) # shape of the 3-year hump (then rebound)
# )
# 
# rate_gdp <- make_scenario(
#   years = proj_years,
#   g_path = g_dyn$g_path,                  # transient growth deviations
#   r_star_path = r_star,
#   receipts_gdp = rec_base,                # PB components unchanged in (3)
#   primary_spend_gdp = prim_base,
#   phi = phi,                              # keep debt sensitivity ON, like (2)
#   r_add = r_add_prior,
#   sfa_rule = "hist_tail_avg", hist_sfa = hist$sfa_gdp
# )[, scenario := "IR shock: +150bp + transient GDP effect (built on 2)"]
# 
# # (4) Now add *transient* fiscal pass-through based on the temporary level gap
# fp_dyn <- apply_fiscal_pass_through_transient(
#   rec_base = rec_base,
#   prim_base = prim_base,
#   gap = g_dyn$gap,            # temporary level gap that *returns to zero*
#   semi_pb_per_1pct = 0.003,
#   share_revenue = 0.85
# )
# 
# rate_gdp_fiscal <- make_scenario(
#   years = proj_years,
#   g_path = g_dyn$g_path,
#   r_star_path = r_star,
#   receipts_gdp = fp_dyn$receipts,         # receipts ↓ while gap < 0, then revert
#   primary_spend_gdp = fp_dyn$primary,     # primary ↑ while gap < 0, then revert
#   phi = phi,                              # keep φ ON
#   r_add = r_add_prior,
#   sfa_rule = "hist_tail_avg", hist_sfa = hist$sfa_gdp
# )[, scenario := "IR shock: +150bp + transient GDP & fiscal pass-through (built on 3)"]


# Bind & simulate
scens <- rbindlist(list(baseline, rate_only, rate_gdp, rate_gdp_fiscal), use.names = TRUE)
out   <- scens[, simulate_debt(.SD, b0 = b0), by = scenario]

## ---------------- Plots ----------------
# Scenarios
p_debt <- ggplot(out, aes(year, b, colour = scenario)) +
  geom_line(size = 1) +
  labs(title = "Gross debt-to-GDP: scenarios", y = "Debt / GDP", x = NULL, colour = NULL) +
  theme_e61(legend = "bottom")

# History + projections (all scenarios)
hist_line <- hist[!is.na(b), .(scenario = "History", year, b)]
proj_line <- out[, .(scenario, year, b)]
p_hist_proj_debt <- ggplot() +
  geom_line(data = hist_line, aes(year, b), size = 1.1, colour = "black") +
  geom_line(data = proj_line[scenario != "Rate shock: +150bp + GDP effect (built on 2)"], aes(year, b, colour = scenario), size = 1) +
  geom_vline(xintercept = last_hist_year + 0.5, linetype = "dotted") +
  geom_vline(xintercept = 2024, linetype = "dotted") +
  labs(
    title = "Gross debt-to-GDP — history and projections",
    subtitle = "Baseline, rates-only (replica), then GDP & fiscal layered on top",
    x = NULL, y = "Debt / GDP", colour = NULL
  ) +
  theme_e61(legend = "bottom")

print(p_debt)
print(p_hist_proj_debt)

out

### Alternative graph format
# --- Build a single dataset with history attached to every scenario ---------
scenarios <- unique(out$scenario)

# History repeated for each scenario
hist_expanded <- data.table::CJ(scenario = scenarios,
                                year     = hist[!is.na(b), year])
hist_expanded <- hist_expanded[hist[!is.na(b), .(year, b)], on = "year"]
hist_expanded[, segment := "History"]

# Projections (already scenario-specific)
proj_expanded <- out[, .(scenario, year, b)]
proj_expanded[, segment := "Projection"]

# Combined, ordered
plot_df <- rbindlist(list(hist_expanded, proj_expanded), use.names = TRUE)
setorder(plot_df, scenario, year)

# (Optional) choose which scenarios to show
scenarios_show <- setdiff(
  unique(plot_df$scenario),
  "Rate shock: +150bp + GDP effect (built on 2)"   # drop any you don't want
)
plot_df <- plot_df[scenario %in% scenarios_show]

order_levels <- c(
  "Rate shock (+150bp, 3y) + debt-sensitive IR",
  "Rate shock: +150bp + GDP effect (built on 2)",
  "Rate shock: +150bp + GDP & fiscal pass-through (built on 3)",
  "Baseline"
)

# 2) Relevel the factor (works with data.table or dplyr)
plot_df[, scenario := factor(scenario, levels = order_levels)]

# --- Single-line plot for all scenarios (history + projections combined) ----
p_hist_proj_debt <- ggplot(plot_df, aes(year, b*100, colour = scenario, group = scenario)) +
  geom_line() +
  geom_vline(xintercept = last_hist_year + 0.5, linetype = "dotted") +
  geom_vline(xintercept = 2024, linetype = "dotted") +
  labs_e61(
    title    = "Bond market shock scenario",
    x = NULL, y = "Gross Debt / GDP",
    sources = c("e61","ABS","PBO"),
      footnotes = c("The full shock reflects an external debt crisis in the US that drives up domestic bond rates - generating an economic response. It includes an estimated GDP response, combined with automatic changes to government expenditure and revenue. No Policy response is included.","Initial shock increases interest rates by 150bp in 2030 for three years, they remain 50bp higher thereafter.")
  ) +
  plab(c("150bp higher interest rates","External bond market shock","Base Scenario"),x=c(1971,1971,1971),y=c(55,75,35))


print(p_hist_proj_debt)

save_e61("Interest_rate_shock.png",res=2)
save_e61("Interest_rate_shock.svg")



