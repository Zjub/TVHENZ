# OECD WP1581 (2019), equations 2-6; WP1440 (2017), equations 5 and 11-13.
# All *_ratio columns are decimal fractions of GDP, never percentage points.
json_series <- function(path) {
  x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  stopifnot(length(x$series$docs) > 0)
  if (x$series$num_found > length(x$series$docs)) stop("Truncated API response: ", path)
  data.table::rbindlist(lapply(x$series$docs, function(s) {
    y <- data.table::data.table(series_code = s$series_code,
      year = suppressWarnings(as.integer(unlist(s$period))),
      value = suppressWarnings(as.numeric(unlist(s$value))))
    for (n in names(s$dimensions)) y[, (n) := s$dimensions[[n]]]
    y
  }), fill = TRUE)
}

project_spending <- function(d, anchor, alpha = 0.7, ageing = 0.4, baumol = 0.5,
                             phi = 1, employment_rule = "aggregate_odds",
                             other_rule = "services", extra_health = 0) {
  d <- data.table::copy(d)
  data.table::setorder(d, year)
  b <- d[1]
  # This is a transparent approximation, not the unpublished OECD cohort model:
  # transfer the change in aggregate employment odds to two older age bands.
  shift <- if (employment_rule == "aggregate_odds") {
    qlogis(d$employment_rate) - qlogis(b$employment_rate)
  } else rep(0, nrow(d))
  d[, er55_64 := plogis(qlogis(b$er55_64) + shift)]
  d[, er65_74 := plogis(qlogis(b$er65_74) + shift)]
  d[, pensioner_proxy := pop * (share55_64 * (1 - er55_64) +
                                   share65_74 * (1 - er65_74) + share75p)]
  d[, `:=`(real_income_pc = real_gdp / pop, productivity = real_gdp / employment)]
  # Deflators cancel on expressing equation 6 as a share of GDP.
  d[, health_income_log := (alpha - 1) * log(real_income_pc / real_income_pc[1])]
  d[, health_ageing_log := ageing * log(share65p / share65p[1])]
  d[, health_baumol_log := baumol * log(productivity / productivity[1])]
  d[, health_extra_log := extra_health * (year - year[1])]
  d[, health_ratio := anchor$health_ratio * exp(health_income_log + health_ageing_log +
                                                health_baumol_log + health_extra_log)]
  d[, pension_demography_log := log((pensioner_proxy / employment) /
                                     (pensioner_proxy[1] / employment[1]))]
  d[, pension_indexation_log := -(1 - phi) * log(productivity / productivity[1])]
  d[, pension_ratio := anchor$pension_ratio * exp(pension_demography_log + pension_indexation_log)]
  d[, other_ratio := anchor$other_ratio * (pop / employment) / (pop[1] / employment[1])]
  if (other_rule == "gdp_share") d[, other_ratio := anchor$other_ratio]
  # WP1877 (September 2026), Table 3, applied as a rule-only sensitivity.
  # Keep the 2019 pension scope, capital and defence treatment unchanged here;
  # this is explicitly not a full EO119 replication.
  if (other_rule == "weighted_2026") {
    d[, education_log := log((pop * share5_24) / (pop[1] * share5_24[1])) +
          (2 / 3) * log(productivity / productivity[1]) - log(real_gdp / real_gdp[1])]
    d[, social_log := log(pop / pop[1]) + log(productivity / productivity[1]) -
          log(real_gdp / real_gdp[1])]
    d[, housing_log := log(pop / pop[1]) - log(real_gdp / real_gdp[1])]
    d[, other_ratio := anchor$other_ratio * exp(0.22 * education_log +
                                                0.31 * social_log + 0.02 * housing_log)]
  }
  d[, capital_ratio := anchor$capital_ratio]
  d[, primary_ratio := health_ratio + pension_ratio + other_ratio + capital_ratio]
  d[]
}

simulate_fiscal_debt <- function(d, anchor, closure = c("fixed_revenue", "oecd_rule"),
                                theta1 = 0.4, theta2 = 0.05, cap = 0.01) {
  closure <- match.arg(closure)
  d <- data.table::copy(d)
  n <- nrow(d)
  growth <- d$nominal_gdp / data.table::shift(d$nominal_gdp) - 1
  # WP1440 uses the mean of future annual nominal potential growth, 10 years.
  forward_growth <- vapply(seq_len(n), function(i) {
    z <- growth[pmin(i + 0:9, n)]
    mean(z[is.finite(z)])
  }, numeric(1))
  d[, `:=`(gross_liabilities_ratio = NA_real_, net_liabilities_ratio = NA_real_,
            interest_ratio = NA_real_, interest_receipts_ratio = NA_real_,
            primary_revenue_ratio = NA_real_, primary_revenue_target = NA_real_,
            total_outlays_ratio = NA_real_, net_lending_ratio = NA_real_)]
  debt <- anchor$gross_liabilities_ratio
  assets <- anchor$assets_ratio
  target <- debt
  revenue <- anchor$primary_revenue_ratio
  initial_r <- d$interest_rate[1]
  return_spread <- anchor$asset_return - initial_r
  d[1, `:=`(gross_liabilities_ratio = debt, net_liabilities_ratio = debt - assets,
             interest_ratio = anchor$interest_ratio,
             interest_receipts_ratio = anchor$interest_receipts_ratio,
             primary_revenue_ratio = revenue,
             total_outlays_ratio = primary_ratio + anchor$interest_ratio,
             net_lending_ratio = revenue + anchor$interest_receipts_ratio -
               primary_ratio - anchor$interest_ratio)]
  for (i in 2:n) {
    g <- growth[i]
    int <- d$interest_rate[i] * debt / (1 + g)
    # Assets held constant/GDP; borrowing finances their nominal accumulation.
    int_received <- (d$interest_rate[i] + return_spread) * assets / (1 + g)
    balance_target <- -forward_growth[i] / (1 + forward_growth[i]) * (target - assets)
    revenue_target <- balance_target + d$primary_ratio[i] + int - int_received
    if (closure == "oecd_rule") {
      change <- theta1 * (revenue_target - revenue) + theta2 * (debt - target)
      revenue <- revenue + min(change, cap)
    }
    balance <- revenue + int_received - d$primary_ratio[i] - int
    debt <- debt / (1 + g) - balance + assets * g / (1 + g)
    d[i, `:=`(gross_liabilities_ratio = debt, net_liabilities_ratio = debt - assets,
               interest_ratio = int, interest_receipts_ratio = int_received,
               primary_revenue_ratio = revenue, primary_revenue_target = revenue_target,
               total_outlays_ratio = primary_ratio + int, net_lending_ratio = balance)]
  }
  d[, closure := closure]
  d[]
}

# Exact additive allocation of a component's level change over log drivers.
# Integrating a multiplicative path along a straight line in log-driver space
# yields this allocation; contributions sum to the change, without ordering.
allocate_log_change <- function(base, logs) {
  total <- sum(logs)
  scale <- if (abs(total) < 1e-12) base else base * expm1(total) / total
  logs * scale
}
