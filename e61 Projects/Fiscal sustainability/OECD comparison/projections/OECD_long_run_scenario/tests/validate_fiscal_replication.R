# Financial and economic invariants; called by the runner before publishing the
# report. These checks do not require the imperfect benchmark match to pass.
checks <- data.table(check = character(), passed = logical(), maximum_error = numeric())
check <- function(name, error, tolerance = 1e-10) {
  value <- max(abs(error))
  checks <<- rbind(checks, data.table(check = name,
                 passed = is.finite(value) && value <= tolerance, maximum_error = value))
}
check("Unique and complete annual scenario/case paths", spending[, .N, by = .(scenario, case, year)]$N - 1)
check("All component paths finite", as.numeric(!all(is.finite(as.matrix(spending[, ..components])))))
check("Primary spending equals components", spending$primary_ratio -
        spending$health_ratio - spending$pension_ratio - spending$other_ratio - spending$capital_ratio)
check("All paths start at the same spending anchor", spending[year == base_year, primary_ratio] -
        sum(unlist(anchor[c("health_ratio", "pension_ratio", "other_ratio", "capital_ratio")])))
check("Health contributions reconcile", contrib[component == "Health", sum(contribution_pp), by = year]$V1 -
        central[year %in% c(2050, 2060, 2100), 100 * (health_ratio - anchor$health_ratio)])

raw_d <- copy(drivers[scenario == "BAU1"])
fixed <- project_spending(raw_d, anchor, employment_rule = "fixed_rates")
inflated <- copy(raw_d)
inflated[, nominal_gdp := nominal_gdp * exp(0.03 * (year - year[1]))]
inflated <- project_spending(inflated, anchor, employment_rule = "fixed_rates")
check("GDP-deflator inflation cancels in spending shares", inflated$primary_ratio - fixed$primary_ratio)
scaled <- copy(raw_d)
scaled[, `:=`(pop = pop * 2, employment = employment * 2, real_gdp = real_gdp * 2)]
scaled <- project_spending(scaled, anchor, employment_rule = "fixed_rates")
check("Population/economy scale invariance", scaled$primary_ratio - fixed$primary_ratio)
tech <- copy(raw_d)
tech[year > base_year, real_gdp := real_gdp * 1.1]
tech <- project_spending(tech, anchor, employment_rule = "fixed_rates")
check("Productivity affects health with net elasticity 0.2", tech[year > base_year, health_ratio] /
        fixed[year > base_year, health_ratio] - 1.1^0.2)
check("Productivity cancels from wage-linked other spending", tech$other_ratio - fixed$other_ratio)
check("Wage-indexed pensions do not fall with productivity", tech$pension_ratio - fixed$pension_ratio)

debt_check <- debt[, {
  g <- nominal_gdp / shift(nominal_gdp) - 1
  expected <- shift(gross_liabilities_ratio) / (1 + g) - net_lending_ratio +
    anchor$assets_ratio * g / (1 + g)
  .(error = gross_liabilities_ratio[-1] - expected[-1])
}, by = .(case, closure)]
check("Gross debt recursion includes financial asset acquisition", debt_check$error)
check("Net liabilities equal gross less financial assets", debt$net_liabilities_ratio -
        debt$gross_liabilities_ratio + anchor$assets_ratio)
check("Revenue minus outlays reconciles to net lending", debt$primary_revenue_ratio +
        debt$interest_receipts_ratio - debt$total_outlays_ratio - debt$net_lending_ratio)
check("Published chart components reconcile to total fiscal pressure",
      sum(benchmark_spending[component != "total_pressure", published_change_pp]) -
        benchmark_spending[component == "total_pressure", published_change_pp], 1e-8)
steady <- copy(central[rep(.N, 301)])
steady[, `:=`(year = 2024:2324, nominal_gdp = 100 * 1.04^(0:300), interest_rate = 0.045,
              primary_ratio = 0.35)]
steady <- simulate_fiscal_debt(steady, anchor, "oecd_rule")
check("Fiscal rule converges in a stationary economy", tail(steady$gross_liabilities_ratio, 1) -
        anchor$gross_liabilities_ratio, 1e-6)
check("Fiscal-rule annual revenue increase respects cap", debt[closure == "oecd_rule",
  max(pmax(diff(primary_revenue_ratio) - 0.01, 0)), by = case]$V1)
fwrite(checks, file.path(out, "validation_checks.csv"))
if (!all(checks$passed)) stop("Fiscal validation failed: ", paste(checks[passed == FALSE, check], collapse = "; "))
message(nrow(checks), " fiscal validation checks passed.")
