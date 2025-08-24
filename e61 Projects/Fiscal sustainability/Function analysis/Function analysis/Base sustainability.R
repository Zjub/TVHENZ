## =========================
## 1) Setup
## =========================
library(data.table)
library(ggplot2)
suppressWarnings({ if (requireNamespace("ggrepel", quietly = TRUE)) library(ggrepel) })
if (requireNamespace("theme61", quietly = TRUE)) theme_set(theme61::theme_e61())

## Helper: nice %
p_pct <- function(x) scales::percent(x, accuracy = 0.1)

## =========================
## 2) Load / prepare data
## =========================
# Expect a CSV with columns: Year, NetDebt_GDP, Interest_GDP, Nominal_GDP_Growth, CAPB
# Example:
# aus <- fread("aus_budget_series.csv")

# --- Demo block (replace with real data when the code is working) ---------------------------
set.seed(42)
yrs  <- 2002:2028
g    <- c(runif(15, 0.045, 0.07), 0.05, 0.046, 0.043, 0.042, 0.045, 0.046, 0.044) # nominal GDP growth
b0   <- 0.18
pb   <- c(rep(0.0, 6), 0.01, 0.006, -0.005, 0.005, 0.01, 0.009, 0.008, 0.007, 0.006, 0.006, 0.005, 0.004, 0.004, 0.003)
r    <- c(rep(0.04, 10), 0.027, 0.029, 0.031, 0.033, 0.035, 0.036, 0.036)          # effective rate guess

# debt recursion to create a plausible history
b <- numeric(length(yrs)); b[1] <- b0
for (t in 1:(length(yrs)-1)) {
  b[t+1] <- ((1 + r[pmin(t,length(r))])/(1 + g[t])) * b[t] - pb[pmin(t,length(pb))]
}
# interest = r * avg debt (approx)
i_gdp <- c(NA, r[-length(r)] * zoo::na.locf0((b + data.table::shift(b, 1, type="lag"))/2, fromLast = TRUE))
aus <- data.table(
  Year = yrs,
  NetDebt_GDP = b,
  Interest_GDP = i_gdp,
  Nominal_GDP_Growth = g,
  CAPB = pb[1:length(yrs)]
)
# -----------------------------------------------------------------------------


# Minimal cleaning
setorder(aus, Year)
# If CAPB missing but Primary_Balance exists: aus[, CAPB := Primary_Balance]

# Effective interest rate (observed)
# If you have debt stock levels, prefer Interest / avg(Debt). Here use ratio approach:
aus[, eff_r := fifelse(shift(NetDebt_GDP) > 0,
                       Interest_GDP / shift(NetDebt_GDP), NA_real_)]

# r - g and stabilising primary balance (exact discrete-time formula)
aus[, `:=`(
  r_minus_g = eff_r - Nominal_GDP_Growth,
  pb_stab   = ((eff_r - Nominal_GDP_Growth) / (1 + Nominal_GDP_Growth)) * NetDebt_GDP
)]

## =========================
## 3) Charts (Australia only)
## =========================

# A) Net debt and interest burden
pA <- ggplot(aus, aes(Year)) +
  geom_line(aes(y = NetDebt_GDP, linetype = "Net debt (% GDP)")) +
  geom_line(aes(y = Interest_GDP, linetype = "Interest payments (% GDP)")) +
  scale_y_continuous(labels = p_pct) +
  scale_linetype_manual(values = c("Net debt (% GDP)" = "solid",
                                   "Interest payments (% GDP)" = "dashed"),
                        name = NULL) +
  labs(title = "Australia: debt and interest burden", y = "", x = NULL)
pA

# B) r – g (effective interest minus nominal GDP growth)
pB <- ggplot(aus, aes(Year, r_minus_g)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  scale_y_continuous(labels = p_pct) +
  labs(title = "r – g: financing conditions for debt dynamics", y = "", x = NULL)
pB

# C) Debt stabilisation requirement vs CAPB
pC <- ggplot(aus, aes(Year)) +
  geom_line(aes(y = pb_stab, linetype = "Required PB to stabilise debt")) +
  geom_line(aes(y = CAPB,   linetype = "Actual CAPB")) +
  scale_y_continuous(labels = p_pct) +
  scale_linetype_manual(values = c("Required PB to stabilise debt" = "dashed",
                                   "Actual CAPB" = "solid"),
                        name = NULL) +
  labs(title = "Headroom: stabilising PB vs actual CAPB", y = "", x = NULL)
pC

## =========================
## 4) Stress test (2-year macro + funding shock)
## =========================
# Debt dynamics: b_{t+1} = ((1+r_t)/(1+g_t)) * b_t - pb_t
simulate_debt <- function(b0, r_path, g_path, pb_path, years) {
  stopifnot(length(r_path) == length(g_path), length(pb_path) == length(g_path))
  b <- numeric(length(years)); b[1] <- b0
  for (t in 1:(length(years)-1)) {
    b[t+1] <- ((1 + r_path[t])/(1 + g_path[t])) * b[t] - pb_path[t]
  }
  data.table(Year = years, NetDebt_GDP = b)
}

# Build baseline paths from last observed year forward N years
h <- 6  # horizon
last <- tail(aus, 1)
years_fwd <- seq.int(last$Year, by = 1, length.out = h)

# Baseline assumptions: hold last eff_r & CAPB; growth at last obs
r_base <- rep(na.omit(last$eff_r)[1], h)
g_base <- rep(last$Nominal_GDP_Growth, h)
pb_base <- rep(last$CAPB, h)

base_path <- simulate_debt(b0 = last$NetDebt_GDP, r_base, g_base, pb_base, years_fwd)

# Stress: 2-year adverse macro & funding shock
stress_spec <- list(
  dg = -0.03,     # nominal GDP growth -3pp for 2 years
  dr =  0.01,     # effective interest +100bp for 2 years
  as =  0.5       # automatic stabilisers: 0.5% of GDP wider PB per 1% output loss
)
# Map growth shock to CAPB via automatic stabilisers (very stylised):
# If dg = -3pp nominal, approximate output gap change ~ -3pp ⇒ CAPB falls by as*0.03 = 0.015
pb_stress <- pb_base
pb_stress[1:2] <- pb_stress[1:2] - stress_spec$as * abs(stress_spec$dg)  # wider deficit
r_stress <- r_base; r_stress[1:2] <- r_stress[1:2] + stress_spec$dr
g_stress <- g_base; g_stress[1:2] <- g_stress[1:2] + stress_spec$dg

stress_path <- simulate_debt(last$NetDebt_GDP, r_stress, g_stress, pb_stress, years_fwd)

# Combine with history and plot
hist_df <- aus[, .(Year, NetDebt_GDP)]
sim_df  <- rbindlist(list(
  data.table(scenario = "Baseline", base_path),
  data.table(scenario = "Stress",   stress_path)
), use.names = TRUE)

pD <- ggplot() +
  geom_line(data = hist_df, aes(Year, NetDebt_GDP), linewidth = 0.6) +
  geom_line(data = sim_df,  aes(Year, NetDebt_GDP, colour = scenario), linewidth = 0.9) +
  scale_y_continuous(labels = p_pct) +
  labs(title = "Debt path: baseline vs two-year stress",
       y = "Net debt (% of GDP)", x = NULL, colour = NULL)
pD

# Interest burden paths (optional)
# Interest/GDP ≈ r_t * b_{t-1}
interest_paths <- function(path, r_path, last_hist_debt = NA_real_) {
  x <- copy(path)
  # Align r to the path length
  r_use <- r_path[seq_len(nrow(x))]
  
  # Option A (simple): first year is NA because we don't know b_{t-1} inside the path
  # x[, Interest_GDP := r_use * shift(NetDebt_GDP)]
  
  # Option B (preferred): use the last observed (historical) debt for the first simulated year
  # If last_hist_debt is NA, fall back to NA for the first year
  first_val <- if (is.na(last_hist_debt)) NA_real_ else r_use[1] * last_hist_debt
  x[, Interest_GDP := c(first_val, head(r_use, -1) * head(NetDebt_GDP, -1))]
  
  x
}

# Last observed debt ratio from your historical AUS table
last_hist_debt <- tail(aus$NetDebt_GDP, 1)

ip_base   <- interest_paths(base_path,  r_base,   last_hist_debt)[, scenario := "Baseline"]
ip_stress <- interest_paths(stress_path, r_stress, last_hist_debt)[, scenario := "Stress"]

ip_df <- rbind(ip_base, ip_stress)

ggplot(ip_df[!is.na(Interest_GDP)], aes(Year, Interest_GDP, colour = scenario)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "Interest burden under baseline vs stress", y = "", x = NULL, colour = NULL)

## =========================
## 5) Quick diagnostics
## =========================
# How big is the "required PB" today vs CAPB?
now_gap <- tail(aus[, .(Year, pb_stab, CAPB)], 1)
print(now_gap)

# Summary table under stress
summ <- merge(
  base_path[, .(Year, BaseDebt = NetDebt_GDP)],
  stress_path[, .(Year, StressDebt = NetDebt_GDP)],
  by = "Year"
)[, .(
  StartYear = min(Year),
  EndYear   = max(Year),
  StartDebt = first(BaseDebt),
  PeakDebt_Base   = max(BaseDebt),
  PeakDebt_Stress = max(StressDebt),
  PeakIncrease_pp = max(StressDebt - BaseDebt) * 100
)]
print(summ)
