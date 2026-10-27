# Requires 04_prepare_fiscal_inputs.R; all inputs and assumptions are exported.
cases <- data.table(
  case = c("documented_equations", "fixed_older_employment", "price_indexed_pensions",
           "no_health_baumol", "other_spending_fixed_gdp", "higher_health_income_elasticity"),
  alpha = c(0.7, 0.7, 0.7, 0.7, 0.7, 1), ageing = 0.4,
  baumol = c(0.5, 0.5, 0.5, 0, 0.5, 0.5),
  phi = c(1, 1, 0, 1, 1, 1),
  employment_rule = c("aggregate_odds", "fixed_rates", rep("aggregate_odds", 4)),
  other_rule = c(rep("services", 4), "gdp_share", "services"))
cases <- rbind(cases, data.table(case = "september_2026_other_rule_only", alpha = 0.7,
  ageing = 0.4, baumol = 0.5, phi = 1, employment_rule = "aggregate_odds", other_rule = "weighted_2026"))
fwrite(cases, file.path(processed, "fiscal_model_parameters.csv"))
spending <- rbindlist(lapply(unique(drivers$scenario), function(s) {
  rbindlist(lapply(seq_len(nrow(cases)), function(j) {
    p <- cases[j]
    d <- project_spending(drivers[scenario == s], anchor, alpha = p$alpha,
      ageing = p$ageing, baumol = p$baumol, phi = p$phi,
      employment_rule = p$employment_rule, other_rule = p$other_rule)
    d[, case := p$case]
    d
  }), fill = TRUE)
}))
# Same 10% real-output gain, split into productivity versus employment sources.
# These are controlled illustrative shocks, not alternative OECD forecasts.
growth_counterfactual <- rbindlist(lapply(c("productivity_10pct", "employment_10pct"), function(channel) {
  d <- copy(drivers[scenario == "BAU1"])
  multiplier <- exp(log(1.1) * pmin((d$year - base_year) / (2060 - base_year), 1))
  d[, `:=`(real_gdp = real_gdp * multiplier, nominal_gdp = nominal_gdp * multiplier)]
  if (channel == "employment_10pct") d[, `:=`(
    employment = employment * multiplier, employment_rate = employment_rate * multiplier)]
  stopifnot(all(d$employment_rate < 1))
  p <- project_spending(d, anchor)
  p[, case := channel]
  p
}))
growth_components <- c("health_ratio", "pension_ratio", "other_ratio", "primary_ratio")
growth_effects <- merge(growth_counterfactual[, c("case", "year", growth_components), with = FALSE],
  spending[scenario == "BAU1" & case == "documented_equations", c("year", growth_components), with = FALSE],
  by = "year", suffixes = c("", "_baseline"))
for (v in growth_components) growth_effects[, (paste0(v, "_effect_pp")) := 100 * (get(v) - get(paste0(v, "_baseline")))]
fwrite(growth_effects, file.path(out, "growth_source_counterfactuals.csv"))
fwrite(spending, file.path(out, "spending_projection_paths.csv"))
components <- c("health_ratio", "pension_ratio", "other_ratio", "capital_ratio", "primary_ratio")
spending_long <- melt(spending, id.vars = c("scenario", "case", "year"),
                      measure.vars = components, variable.name = "component", value.name = "ratio")
spending_long[, change_pp := 100 * (ratio - ratio[year == base_year]), by = .(scenario, case, component)]
fwrite(spending_long, file.path(out, "spending_components.csv"))
fwrite(spending_long[year %in% c(2024, 2025, 2030, 2050, 2060, 2071, 2100)],
       file.path(out, "spending_endpoints.csv"))
benchmark_comparison <- merge(spending_long[scenario == "BAU1" & year == 2060 &
  component %in% c("health_ratio", "pension_ratio", "other_ratio"),
  .(case, component = as.character(component), model_change_pp = change_pp)],
  benchmark_spending, by = "component")
benchmark_comparison[, difference_pp := model_change_pp - published_change_pp]
fwrite(benchmark_comparison, file.path(out, "published_spending_comparison.csv"))

# Purely diagnostic inverse calculations: parameters that would close benchmark
# gaps. These NEVER feed the independent projections or the sensitivity cases.
central <- spending[scenario == "BAU1" & case == "documented_equations"]
last <- central[year == 2060]
bench_h <- benchmark_spending[component == "health_ratio", published_change_pp] / 100
bench_p <- benchmark_spending[component == "pension_ratio", published_change_pp] / 100
bench_o <- benchmark_spending[component == "other_ratio", published_change_pp] / 100
implied_baumol <- (log((anchor$health_ratio + bench_h) / anchor$health_ratio) -
   last$health_income_log - last$health_ageing_log) /
   log(last$productivity / central[year == base_year, productivity])
target_retired <- (anchor$pension_ratio + bench_p) / anchor$pension_ratio *
  central[year == base_year, pensioner_proxy / employment] * last$employment
implied_er5574 <- 1 - (target_retired / last$pop - last$share75p) /
                            (last$share55_64 + last$share65_74)
implied <- data.table(quantity = c("Health Baumol coefficient", "2060 employment rate age 55-74",
                                  "Other-spending baseline ratio required"),
  independent_value = c(0.5, (last$er55_64 * last$share55_64 + last$er65_74 * last$share65_74) /
    (last$share55_64 + last$share65_74), anchor$other_ratio),
  benchmark_implied_value = c(implied_baumol, implied_er5574,
    bench_o / ((last$pop / last$employment) / central[year == base_year, pop / employment] - 1)),
  status = "Diagnostic inverse calculation only; not an independently identified OECD parameter")
fwrite(implied, file.path(out, "benchmark_gap_diagnostics.csv"))

# Attribute the arithmetic shortfall to components; this identifies where the
# discrepancy is, without claiming that its underlying causes are identified.
gap_bridge <- benchmark_comparison[case == "documented_equations", .(
  component, model_change_pp, published_change_pp,
  shortfall_pp = published_change_pp - model_change_pp)]
gap_bridge[, share_of_total_shortfall_pct := 100 * shortfall_pp / sum(shortfall_pp)]
gap_bridge[, initial_ratio := unlist(anchor[component])]
gap_bridge[, projected_multiplier := 1 + model_change_pp / (100 * initial_ratio)]
gap_bridge[, implied_initial_share_pct := published_change_pp / (projected_multiplier - 1)]
gap_bridge[, implied_initial_share_feasible := implied_initial_share_pct > 0]
fwrite(gap_bridge, file.path(out, "replication_gap_bridge.csv"))
driver_gap <- data.table(
  quantity = c("Population per worker", "Health spending / initial health spending",
    "Pension spending / initial pension spending", "Employment rate aged 55-74"),
  initial = c(central[1, pop / employment], 1, 1,
    central[1, (er55_64 * share55_64 + er65_74 * share65_74) / (share55_64 + share65_74)]),
  calculated_2060 = c(last$pop / last$employment,
    last$health_ratio / anchor$health_ratio, last$pension_ratio / anchor$pension_ratio,
    implied[quantity == "2060 employment rate age 55-74", independent_value]),
  benchmark_implied_2060 = c(central[1, pop / employment] * (1 + bench_o / anchor$other_ratio),
    1 + bench_h / anchor$health_ratio, 1 + bench_p / anchor$pension_ratio,
    implied[quantity == "2060 employment rate age 55-74", benchmark_implied_value]))
driver_gap[, qualification := "Inverse calculation holding other inputs fixed; not an observed OECD driver"]
fwrite(driver_gap, file.path(out, "replication_driver_diagnostics.csv"))

debt <- rbindlist(lapply(c("documented_equations", "fixed_older_employment", "price_indexed_pensions"), function(ca) {
  rbindlist(lapply(c("fixed_revenue", "oecd_rule"), function(cl) {
    simulate_fiscal_debt(spending[scenario == "BAU1" & case == ca], anchor, closure = cl)
  }))
}))
fwrite(debt, file.path(out, "debt_and_revenue_paths.csv"))
fwrite(debt[year %in% c(2024, 2030, 2050, 2060, 2100),
 .(case, closure, year, primary_ratio, total_outlays_ratio, primary_revenue_ratio,
   gross_liabilities_ratio, net_liabilities_ratio, interest_ratio, net_lending_ratio)],
 file.path(out, "debt_and_revenue_endpoints.csv"))
debt_compare <- merge(debt[case == "documented_equations",
 .(year, closure, model_gross_liabilities_pct = gross_liabilities_ratio * 100)], bd, by = "year")
debt_compare[, published_gross_debt_pct := fifelse(closure == "fixed_revenue",
                                      published_no_adjustment, published_adjustment)]
debt_compare[, difference_pp := model_gross_liabilities_pct - published_gross_debt_pct]
debt_compare[, comparability := "Diagnostic only: initial fiscal levels, liability concepts, rule start and target differ"]
fwrite(debt_compare, file.path(out, "published_debt_comparison.csv"))

# Component contribution diagnostics, expressed in percentage points of GDP.
contrib <- rbindlist(lapply(c(2050L, 2060L, 2100L), function(y) {
  e <- central[year == y]
  h <- c(income = e$health_income_log, ageing = e$health_ageing_log, baumol = e$health_baumol_log)
  p <- c(demography_and_employment = e$pension_demography_log, indexation = e$pension_indexation_log)
  rbindlist(list(data.table(year = y, component = "Health", driver = names(h),
                          contribution_pp = 100 * allocate_log_change(anchor$health_ratio, h)),
    data.table(year = y, component = "Pensions", driver = names(p),
               contribution_pp = 100 * allocate_log_change(anchor$pension_ratio, p)),
    data.table(year = y, component = "Other", driver = "population_to_employment",
               contribution_pp = 100 * (e$other_ratio - anchor$other_ratio))))
}))
fwrite(contrib, file.path(out, "spending_driver_contributions.csv"))

# Comparisons with existing models use changes from 2030, a common post-forecast
# starting date, so differences in starting levels are not presented as forecasts.
parent_tables <- file.path(dirname(root), "2026 projections", "outputs", "tables")
existing <- list()
for (spec in list(c("bottom_up_total_projections.csv", "Bottom-up fixed exposure"),
                  c("bottom_up_data_enhanced_total_projections.csv", "Bottom-up data enhanced"))) {
  p <- file.path(parent_tables, spec[1])
  if (file.exists(p)) {
    e <- fread(p)[scenario == "central", .(year, method = spec[2], ratio = primary_expense_ratio)]
    existing[[length(existing) + 1L]] <- e
  }
}
stream <- file.path(dirname(root), "2026 projections", "streamlined_top_down", "outputs", "tables", "primary_spending_paths.csv")
if (file.exists(stream)) {
  e <- fread(stream)
  stopifnot(all(c("year", "model", "primary_fiscal_pp") %in% names(e)))
  existing[[length(existing) + 1L]] <- e[, .(year, method = paste0("Streamlined: ", model), ratio = primary_fiscal_pp / 100)]
}
existing[[length(existing) + 1L]] <- central[, .(year, method = "OECD equations with available inputs", ratio = primary_ratio)]
# Use the known PBO historical 2024 anchor for each Australian expenditure
# concept. Preserve the forecasts; do not manufacture a published OECD path.
all_existing <- rbindlist(existing)
endpoint_comparison <- all_existing[year == 2060, .(method, end_ratio = ratio)]
endpoint_comparison[, `:=`(start_year = base_year, end_year = 2060L,
  start_ratio = fifelse(grepl("^Bottom-up", method),
    a$expenses_ratio_gdp - a$public_debt_interest_ratio_gdp,
    a$expenses_ratio_gdp - a$public_debt_interest_ratio_gdp + a$net_capital_investment_ratio_gdp),
  source_type = "Existing Australian model",
  concept = fifelse(grepl("^Bottom-up", method), "Primary expenses",
                    "Primary fiscal expenditure including net capital investment"),
  baseline_source = "PBO FY2023-24 historical anchor for the same expenditure concept")]
endpoint_comparison[method == "OECD equations with available inputs", `:=`(
  start_ratio = central[year == base_year, primary_ratio],
  source_type = "Independent equation implementation",
  baseline_source = "Independent implementation's mixed-concept 2024 anchor")]
endpoint_comparison[, change_pp := 100 * (end_ratio - start_ratio)]
endpoint_comparison <- rbind(endpoint_comparison, data.table(
  method = "OECD published spending components", start_year = base_year, end_year = 2060L,
  start_ratio = NA_real_, end_ratio = NA_real_,
  change_pp = sum(benchmark_spending[component %in% c("health_ratio", "pension_ratio", "other_ratio"), published_change_pp]),
  source_type = "Published OECD benchmark",
  concept = "Published health + pensions + other primary spending; capital not separately identified",
  baseline_source = "OECD Australia 2026 Figure 1.14; endpoint changes only"), fill = TRUE)
endpoint_comparison[, limitation := "Same 2024-2060 period; differing spending concepts, macro inputs and data vintages"]
fwrite(endpoint_comparison, file.path(out, "existing_model_endpoint_comparison.csv"))
comparison <- rbindlist(existing)
comparison <- comparison[year >= 2030 & year <= 2071]
comparison[, change_since_2030_pp := 100 * (ratio - ratio[year == 2030]), by = method]
comparison[, limitation := "Different macro assumptions and expense/capital concepts; compare changes, not levels"]
fwrite(comparison, file.path(out, "existing_model_comparison.csv"))
message("Built spending sensitivities, benchmark reconciliation, driver contributions and debt illustrations.")
