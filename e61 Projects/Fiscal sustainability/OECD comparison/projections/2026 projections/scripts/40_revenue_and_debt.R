source(file.path("scripts", "00_config.R"))

official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))
bottom <- fread(file.path(table_dir, "bottom_up_total_projections.csv"))
top <- fread(file.path(table_dir, "top_down_projection_paths.csv"))
params <- fread(file.path(processed_dir, "scenario_parameters.csv"))

build_revenue_path <- function(scenario, indexed = FALSE) {
  scenario_name <- scenario
  p <- params[scenario == scenario_name]
  out <- data.table(year = 2025:projection_end)
  out <- merge(out, official[, .(year, official_revenue = revenue_ratio_gdp)], by = "year", all.x = TRUE)
  out[, revenue_ratio := official_revenue]
  anchor <- official[year == official_forecast_end, revenue_ratio_gdp]
  increase <- if (indexed) 0 else p$post_forecast_revenue_increase
  out[year > official_forecast_end, revenue_ratio := approx(
    c(official_forecast_end, 2037L), c(anchor, anchor + increase),
    xout = year, rule = 2
  )$y]
  out[, `:=`(
    scenario = if (indexed) "indexed_thresholds" else scenario,
    revenue_assumption = if (indexed) {
      "PBO forecast then revenue/GDP held constant (indexed-threshold proxy)"
    } else {
      paste0("PBO forecast then +", round(increase * 100, 1), " percentage points by 2037")
    },
    official_revenue = NULL
  )]
  out
}

revenue_paths <- rbindlist(list(
  build_revenue_path("central"),
  build_revenue_path("pressure"),
  build_revenue_path("restraint"),
  build_revenue_path("central", indexed = TRUE)
))

project_bottom_debt <- function(spending_scenario, revenue_scenario = spending_scenario) {
  spend <- bottom[scenario == spending_scenario]
  rev <- revenue_paths[scenario == revenue_scenario]
  p <- params[scenario == spending_scenario]
  dt <- merge(spend, rev[, .(year, revenue_ratio)], by = "year")
  dt <- merge(dt, official[, .(
    year,
    official_nci = net_capital_investment_ratio_gdp,
    official_net_debt = net_debt_ratio_gdp,
    official_interest = public_debt_interest_ratio_gdp
  )], by = "year", all.x = TRUE)
  dt[, nominal_gdp_growth := c(NA_real_, diff(log(nominal_gdp)))]
  dt[, net_capital_investment_ratio := fifelse(
    !is.na(official_nci), official_nci, p$net_capital_investment_gdp
  )]
  start_rate <- official[year == 2026, public_debt_interest_ratio_gdp] /
    official[year == 2025, net_debt_ratio_gdp]
  dt[, effective_interest_rate := approx(
    c(2026L, 2037L), c(start_rate, p$effective_interest_rate_long_run %||% 0.044),
    xout = year, rule = 2
  )$y]
  dt[, `:=`(net_debt_ratio = NA_real_, interest_ratio = NA_real_)]
  dt[year == 2025, net_debt_ratio := official[year == 2025, net_debt_ratio_gdp]]
  dt[year == 2025, interest_ratio := official[year == 2025, public_debt_interest_ratio_gdp]]
  for (i in 2:nrow(dt)) {
    g <- dt$nominal_gdp_growth[i]
    debt_open_current_gdp <- dt$net_debt_ratio[i - 1] / (1 + g)
    dt$interest_ratio[i] <- dt$effective_interest_rate[i] * debt_open_current_gdp
    dt$net_debt_ratio[i] <- debt_open_current_gdp +
      dt$primary_expense_ratio[i] + dt$interest_ratio[i] +
      dt$net_capital_investment_ratio[i] - dt$revenue_ratio[i] +
      p$stock_flow_adjustment
  }
  dt[, `:=`(
    spending_approach = "Bottom-up primary spending with endogenous interest",
    spending_model = spending_scenario,
    revenue_scenario = revenue_scenario,
    total_expense_ratio = primary_expense_ratio + interest_ratio,
    fiscal_deficit_ratio = primary_expense_ratio + interest_ratio + net_capital_investment_ratio - revenue_ratio
  )]
  dt
}

`%||%` <- function(x, y) if (length(x) && !is.na(x)) x else y

bottom_debt <- rbindlist(list(
  project_bottom_debt("central", "central"),
  project_bottom_debt("pressure", "pressure"),
  project_bottom_debt("restraint", "restraint"),
  project_bottom_debt("central", "indexed_thresholds")
), fill = TRUE)

central_macro <- bottom[scenario == "central", .(year, nominal_gdp)]
central_revenue <- revenue_paths[scenario == "central", .(year, revenue_ratio)]
top_join <- top[path_type == "Official forecast then model projection"]

top_debt <- top_join[, {
  dt <- data.table(year = year, modelled_expense_ratio = value)
  dt <- merge(dt, central_macro, by = "year")
  dt <- merge(dt, central_revenue, by = "year")
  dt <- merge(dt, official[, .(
    year,
    official_net_debt = net_debt_ratio_gdp,
    official_gross_debt = gross_debt_ratio_gdp,
    official_interest = public_debt_interest_ratio_gdp
  )], by = "year", all.x = TRUE)
  dt[, nominal_gdp_growth := c(NA_real_, diff(log(nominal_gdp)))]
  dt[, `:=`(
    net_debt_ratio = NA_real_, gross_debt_ratio = NA_real_, interest_ratio = NA_real_,
    primary_fiscal_expense_ratio = NA_real_, total_expense_ratio = NA_real_
  )]
  excludes_pdi <- topdown_interest_treatment != "include_interest"
  if (excludes_pdi) {
    dt[, primary_fiscal_expense_ratio := modelled_expense_ratio]
  } else {
    dt[, total_expense_ratio := modelled_expense_ratio]
  }

  # The top-down target already contains capital investment, matching the PBO
  # expenses-plus-net-capital anchor. Do not add capital investment again here.
  # Use published debt and PDI through the official period, then allow debt and
  # interest to evolve recursively from the modelled primary fiscal balance.
  opening <- official[year == 2025, net_debt_ratio_gdp]
  opening_gross <- official[year == 2025, gross_debt_ratio_gdp]
  gross_net_wedge <- official[year == official_forecast_end,
                              gross_debt_ratio_gdp - net_debt_ratio_gdp]
  implied_rate_2030 <- official[year == official_forecast_end,
    public_debt_interest_ratio_gdp] /
    (official[year == official_forecast_end - 1L, gross_debt_ratio_gdp] /
       (1 + dt[year == official_forecast_end, nominal_gdp_growth]))
  long_run_rate <- params[scenario == "central", effective_interest_rate_long_run]
  dt[, effective_interest_rate := approx(
    c(official_forecast_end, 2037L), c(implied_rate_2030, long_run_rate),
    xout = year, rule = 2
  )$y]
  for (i in seq_len(nrow(dt))) {
    g <- dt$nominal_gdp_growth[i]
    if (!is.finite(g)) g <- central_macro[year == dt$year[i], nominal_gdp] /
      (official[year == 2025, nominal_gdp_billion] * 1000) - 1
    if (dt$year[i] <= official_forecast_end) {
      dt$net_debt_ratio[i] <- dt$official_net_debt[i]
      dt$gross_debt_ratio[i] <- dt$official_gross_debt[i]
      dt$interest_ratio[i] <- dt$official_interest[i]
      if (excludes_pdi) {
        dt$total_expense_ratio[i] <-
          dt$primary_fiscal_expense_ratio[i] + dt$interest_ratio[i]
      }
      opening <- dt$net_debt_ratio[i]
      opening_gross <- dt$gross_debt_ratio[i]
      next
    }
    debt_open_current_gdp <- opening / (1 + g)
    if (excludes_pdi) {
      gross_debt_open_current_gdp <- opening_gross / (1 + g)
      dt$interest_ratio[i] <- dt$effective_interest_rate[i] *
        max(gross_debt_open_current_gdp, 0)
      dt$total_expense_ratio[i] <-
        dt$primary_fiscal_expense_ratio[i] + dt$interest_ratio[i]
    }
    dt$net_debt_ratio[i] <- debt_open_current_gdp + dt$total_expense_ratio[i] -
      dt$revenue_ratio[i] + params[scenario == "central", stock_flow_adjustment]
    # Hold the official-period financial-asset wedge constant as a share of GDP
    # so gross debt, rather than net debt, drives gross public debt interest.
    dt$gross_debt_ratio[i] <- max(dt$net_debt_ratio[i] + gross_net_wedge, 0)
    opening <- dt$net_debt_ratio[i]
    opening_gross <- dt$gross_debt_ratio[i]
  }
  dt
}, by = .(model, model_label)]
top_debt[, `:=`(
  spending_approach = if (topdown_interest_treatment == "include_interest") {
    "Top-down total fiscal expenditure (interest embedded)"
  } else {
    "Top-down primary fiscal expenditure with endogenous debt interest"
  },
  interest_treatment = topdown_interest_treatment,
  interest_treatment_label = topdown_interest_treatment_label(),
  revenue_scenario = "central"
)]

official_debt <- official[year >= 2025, .(
  year,
  net_debt_ratio = net_debt_ratio_gdp,
  gross_debt_ratio = gross_debt_ratio_gdp,
  series = "Official PBO forecast"
)]

fwrite(revenue_paths, file.path(table_dir, "revenue_scenario_paths.csv"))
fwrite(bottom_debt, file.path(table_dir, "debt_paths_bottom_up.csv"))
fwrite(top_debt, file.path(table_dir, "debt_paths_top_down.csv"))
fwrite(official_debt, file.path(table_dir, "debt_path_official_pbo.csv"))
message("Revenue and debt scenarios written. Bottom-up paths include endogenous debt interest.")
