source(file.path("scripts", "00_config.R"))

suppressPackageStartupMessages({
  library(officer)
  library(flextable)
})

official <- fread(file.path(table_dir, "official_forecast_anchor.csv"))
validation <- fread(file.path(table_dir, "data_validation.csv"))
manifest <- fread(file.path(raw_dir, "source_manifest.csv"))
category_assumptions <- fread(file.path(processed_dir, "bottom_up_category_assumptions.csv"))
scenario_parameters <- fread(file.path(processed_dir, "scenario_parameters.csv"))
bottom <- fread(file.path(table_dir, "bottom_up_total_projections.csv"))
bottom_debt <- fread(file.path(table_dir, "debt_paths_bottom_up.csv"))
top <- fread(file.path(table_dir, "top_down_projection_paths.csv"))
top_debt <- fread(file.path(table_dir, "debt_paths_top_down.csv"))
top_summary <- fread(file.path(table_dir, "top_down_model_summary.csv"))
ecm_long_run <- fread(file.path(table_dir, "top_down_ecm_long_run.csv"))
shapley <- fread(file.path(table_dir, "shapley_change_decomposition.csv"))
shapley_check <- fread(file.path(table_dir, "shapley_reconciliation.csv"))
rolling <- fread(file.path(table_dir, "forecast_check_rolling_summary.csv"))
official_check <- fread(file.path(table_dir, "forecast_check_against_official_summary.csv"))
fit_diagnostics <- fread(file.path(table_dir, "model_fit_diagnostics.csv"))
coefficient_se <- fread(file.path(table_dir, "model_coefficient_standard_errors.csv"))
forecast_intervals <- fread(file.path(table_dir, "model_conditional_forecast_intervals.csv"))
window_sensitivity <- fread(file.path(table_dir, "model_estimation_window_sensitivity.csv"))
driver_sensitivity <- fread(file.path(table_dir, "model_driver_sensitivity.csv"))
diagnostic_scope <- fread(file.path(table_dir, "model_diagnostic_scope.csv"))
model_selection <- fread(file.path(table_dir, "model_selection_assessment.csv"))
approach_recommendation <- fread(file.path(table_dir, "approach_recommendation.csv"))
ecm_validation <- fread(file.path(table_dir, "ecm_validation_summary.csv"))
ecm_bounds <- fread(file.path(table_dir, "ecm_bounds_tests.csv"))
ecm_integration <- fread(file.path(table_dir, "ecm_integration_assessment.csv"))
scale_income <- fread(file.path(table_dir, "top_down_scale_income_model_comparison.csv"))
model_collinearity <- fread(file.path(table_dir, "model_collinearity_diagnostics.csv"))

pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), 100 * x)
pp <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), x)
fy <- function(year) paste0(year - 1L, "-", substr(year, 3, 4))

add_heading <- function(doc, text, level = 1L) {
  body_add_par(doc, text, style = paste0("heading ", level))
}

add_text <- function(doc, text, style = "Normal") {
  body_add_par(doc, text, style = style)
}

add_bullets <- function(doc, items) {
  for (item in items) doc <- body_add_par(doc, paste0("- ", item), style = "Normal")
  doc
}

add_table <- function(doc, x, widths = NULL, font_size = 8.5) {
  x <- as.data.frame(x)
  ft <- flextable(x)
  ft <- theme_booktabs(ft)
  ft <- bg(ft, part = "header", bg = "#D9EAF7")
  ft <- bold(ft, part = "header")
  ft <- fontsize(ft, size = font_size, part = "all")
  ft <- valign(ft, valign = "top", part = "all")
  ft <- autofit(ft)
  ft <- fit_to_width(ft, max_width = 6.4)
  if (!is.null(widths)) {
    for (i in seq_along(widths)) ft <- width(ft, j = i, width = widths[[i]])
  }
  body_add_flextable(doc, ft)
}

add_figure <- function(doc, relative_path, caption, width = 6.4, height = 4.0) {
  path <- file.path(project_dir, relative_path)
  if (file.exists(path)) {
    doc <- body_add_img(doc, src = path, width = width, height = height)
    doc <- body_add_par(doc, caption, style = "Image Caption")
  }
  doc
}

script_table <- data.table(
  Script = c(
    "01_download_data.R", "02_clean_data.R", "03_assumptions.R",
    "04_official_forecast.R", "10_bottom_up_projection.R",
    "20_top_down_projection.R", "21_shapley_attribution.R", "30_forecast_checks.R",
    "31_model_diagnostics.R", "32_ecm_validation.R",
    "40_revenue_and_debt.R", "50_graphs.R", "60_word_report.R"
  ),
  Purpose = c(
    "Download/cache official inputs and write a source manifest.",
    "Clean annual data, construct fiscal concepts and run validation checks.",
    "Define macro, demographic, spending, revenue and financing scenarios.",
    "Extract the PBO consolidated forecast anchor.",
    "Project GFS purpose categories and aggregate primary expenses.",
    "Estimate seven aggregate time-series alternatives and anchor their changes.",
    "Average structural-factor contributions over every ordering for historical attribution.",
    "Run rolling historical tests and model-only checks over the official forecast.",
    "Test fit and residuals; calculate coefficient standard errors, conditional intervals and sensitivities.",
    "Test ECM integration order, the finite-sample bounds relationship, residual specification, stability and collinearity.",
    "Apply revenue assumptions and the debt accumulation identities.",
    "Generate PNG and SVG figures grouped by analytical task.",
    "Assemble this methodology and results document."
  )
)

source_table <- manifest[, .(
  Source = source,
  Vintage_or_file = basename(local_file),
  URL = url
)]

validation_table <- validation[, .(
  Check = check,
  Passed = fifelse(passed, "Yes", "No"),
  Detail = detail
)]

bottom_key <- merge(
  bottom[year %in% c(2029L, 2035L, 2050L, 2066L),
         .(year, scenario, primary_expense_ratio)],
  bottom_debt[year %in% c(2029L, 2035L, 2050L, 2066L),
              .(year, scenario = revenue_scenario, net_debt_ratio)],
  by = c("year", "scenario"), all.x = TRUE
)
bottom_key[, `:=`(
  `Financial year` = fy(year),
  Scenario = fifelse(scenario == "central", "Central",
                     fifelse(scenario == "pressure", "Pressure", "Restraint")),
  `Primary expenses / GDP` = pct(primary_expense_ratio),
  `Net debt / GDP` = pct(net_debt_ratio)
)]
bottom_key <- bottom_key[, .(`Financial year`, Scenario, `Primary expenses / GDP`, `Net debt / GDP`)]

indexed_endpoint <- bottom_debt[year == projection_end & revenue_scenario == "indexed_thresholds",
                                net_debt_ratio]

top_endpoint <- top[
  path_type == "Official forecast then model projection" & year == projection_end,
  .(Model = model_label, `Total expenses / GDP in 2065-66` = pct(value))
]
top_debt_endpoint <- top_debt[year == projection_end,
                              .(Model = model_label, `Net debt / GDP in 2065-66` = pct(net_debt_ratio))]
top_endpoint <- merge(top_endpoint, top_debt_endpoint, by = "Model", all.x = TRUE)

rolling_key <- rolling[horizon %in% c(1L, 5L), .(
  Model = model_label,
  Horizon = paste0(horizon, " year", fifelse(horizon == 1L, "", "s")),
  `MAE (percentage points)` = pp(mae_pp),
  `RMSE (percentage points)` = pp(rmse_pp)
)]

official_key <- official_check[, .(
  Model = model_label,
  Approach = approach,
  `Mean difference (pp)` = pp(mean_difference_pp),
  `MAE versus official (pp)` = pp(mae_difference_pp),
  `Maximum absolute difference (pp)` = pp(max_abs_difference_pp)
)]

model_explanation <- data.table(
  Model = c(
    "Structural OLS (levels)", "ARIMAX (levels)", "ARIMAX (differences)",
    "Dynamic differences", "Hybrid structural/macro",
    "ARDL error-correction model", "ARIMAX benchmark with real GDP per capita and COVID controls"
  ),
  Mechanism = c(
    "Spending/GDP is a level function of the 0-14 and 65+ population shares, terms of trade, relative government prices and unemployment, with separate FY2020-22 indicators.",
    "The same level relationship is combined with autocorrelated errors selected by AICc.",
    "Annual changes in spending/GDP respond to annual changes in the drivers; projected changes are accumulated from the last observation.",
    "Annual changes use variable-specific dynamics and lags, with separate FY2020-22 annual-change indicators.",
    "The selected age shares, relative government prices and FY2020-22 indicators determine a structural level component; macro changes explain movements around it.",
    "Annual spending growth responds to disequilibrium in lagged spending and structural levels, short-run macro changes and separate FY2020-22 indicators.",
    "A parsimonious ARIMAX relates spending/GDP to population, real GDP per capita and separate FY2020-22 indicators."
  ),
  `Why paths differ` = c(
    "Persistent changes in driver levels permanently alter the spending ratio and coefficient uncertainty can dominate far from the sample.",
    "Mean reversion in the ARIMA error pulls the path toward the fitted structural level.",
    "No deterministic drift is imposed, so stable future drivers make annual changes fade; early changes remain embedded in the level.",
    "Tailored transformations avoid treating every driver identically, but forecast errors still accumulate in the projected level.",
    "Slow structural pressures create a gradual trend while temporary macro impulses fade.",
    "The estimated speed of adjustment pulls spending toward an implied long-run relationship while retaining short-run dynamics.",
    "It responds to scale and income but deliberately omits the richer ageing, price and labour-market channels."
  )
)

assumption_table <- scenario_parameters[, .(
  Scenario = tools::toTitleCase(scenario),
  `Defence target / GDP` = pct(defence_target_gdp),
  `Target year` = fy(defence_target_year),
  `Long-run effective interest rate` = pct(effective_interest_rate_long_run),
  `Net capital investment / GDP` = pct(net_capital_investment_gdp),
  `Revenue increase after forecast` = paste0(pp(post_forecast_revenue_increase * 100, 1), " pp"),
  `Annual stock-flow adjustment` = paste0(pp(stock_flow_adjustment * 100, 2), " pp")
)]

category_table <- copy(category_assumptions)
setnames(category_table,
         c("category", "exposure", "central_excess_cost", "pressure_excess_cost", "restraint_excess_cost"),
         c("Category", "Exposure base", "Central", "Pressure", "Restraint"))
for (j in c("Central", "Pressure", "Restraint")) {
  category_table[, (j) := fifelse(is.na(get(j)), "Target rule", pct(get(j)))]
}

central_end <- bottom_debt[year == projection_end & revenue_scenario == "central"]
pressure_end <- bottom_debt[year == projection_end & revenue_scenario == "pressure"]
restraint_end <- bottom_debt[year == projection_end & revenue_scenario == "restraint"]

key_results <- data.table(
  metric = c(
    "Central bottom-up primary expenses / GDP in 2065-66",
    "Central bottom-up net debt / GDP in 2065-66",
    "Pressure bottom-up net debt / GDP in 2065-66",
    "Restraint bottom-up net debt / GDP in 2065-66",
    "Central spending with indexed-threshold revenue proxy: net debt / GDP in 2065-66"
  ),
  value = c(
    central_end$primary_expense_ratio, central_end$net_debt_ratio,
    pressure_end$net_debt_ratio, restraint_end$net_debt_ratio, indexed_endpoint
  )
)
key_results[, formatted_value := pct(value)]
fwrite(key_results, file.path(table_dir, "key_results_summary.csv"))

doc <- read_docx()
doc <- body_add_par(doc, "Australian fiscal projections: 2026 update", style = "heading 1")
doc <- body_add_par(doc, "Modular methodology, official-forecast bridge, projection scenarios and forecast checks", style = "centered")
doc <- body_add_par(doc, paste("Generated", format(Sys.Date(), "%d %B %Y")), style = "Normal")
doc <- body_add_par(doc, "All ratios are shares of nominal GDP unless otherwise stated. Financial years are labelled by their June-ending year.", style = "Normal")

doc <- add_heading(doc, "Executive summary")
doc <- add_text(doc, paste0(
  "The workflow deliberately separates the short-run official forecast from the long-run projection. ",
  "The PBO 2026-27 National Fiscal Outlook supplies the consolidated general-government anchor through ",
  fy(official_forecast_end), ". Bottom-up and top-down models are also run without that anchor over the same years, ",
  "so their difference from the official forecast is visible rather than hidden."
))
doc <- add_bullets(doc, c(
  paste0("In the central bottom-up scenario, primary expenses rise to ", pct(central_end$primary_expense_ratio),
         " of GDP and net debt reaches ", pct(central_end$net_debt_ratio), " by ", fy(projection_end), "."),
  paste0("The pressure and restraint debt endpoints are ", pct(pressure_end$net_debt_ratio), " and ",
         pct(restraint_end$net_debt_ratio), ", illustrating that these are conditional scenarios, not confidence bounds."),
  paste0("Holding revenue/GDP at the post-forecast level as a proxy for fully indexed personal-tax thresholds raises the central-spending debt endpoint to ",
         pct(indexed_endpoint), "."),
  "The seven top-down models behave differently because some model the spending ratio in levels, some model annual changes, and the ECM combines long-run levels with short-run changes. Real GDP per capita is included in every headline family; total population is excluded.",
  "The parsimonious ARIMAX benchmark performs best against the short official forecast in this vintage, but that is not evidence that its limited economic structure is the most informative long-run projection."
))
doc <- add_text(doc, "Recommended hierarchy: retain the published consolidated forecast for the official forecast period; use the bottom-up purpose model as the central long-run fiscal projection; use levels ARIMAX only as a conditional forecast comparison and dynamic differences as the least-collinear economic sensitivity; retain the parsimonious real-income ARIMAX as the statistical benchmark; do not trust the current ECM as a long-run model; and use the full scenario/model spread for uncertainty analysis.")
doc <- add_figure(doc, "outputs/figures/revenue_debt/02_bottom_up_debt.png",
                  "Figure 1. Bottom-up net debt paths under the spending, revenue and financing scenarios.")

doc <- body_add_break(doc)
doc <- add_heading(doc, "1. Workflow and reproducibility")
doc <- add_text(doc, "Run run_all.R from the 2026 projections folder. Each script reads stored inputs and writes explicit intermediate files, so individual stages can be inspected or rerun independently.")
doc <- add_table(doc, script_table, font_size = 8)
doc <- add_text(doc, "Raw downloads are cached. Set REFRESH_DATA=true before running to force a refresh. The source manifest records the URL, local file, file size and check time. PNG files support quick review; matching SVG files support publication-quality editing.")

doc <- add_heading(doc, "2. Data, vintages and accounting concepts")
doc <- add_text(doc, paste0(
  "Historical aggregate spending and macro variables come primarily from ABS national accounts. Purpose-level spending uses the ABS Government Finance Statistics annual general-government table for ",
  "all levels of government. Demographic projections use the Centre for Population 2025 Population Statement. The official forecast uses the PBO 2026-27 National Fiscal Outlook, which consolidates the 2026-27 Commonwealth, state and territory budgets."
))
doc <- add_table(doc, source_table, font_size = 7.5)
doc <- add_text(doc, "Concept bridge: the national-accounts top-down series is broad government final consumption plus public investment, whereas the PBO/GFS expense measure also reflects transfers and other operating expenses. The workflow does not equate these levels. It shows unanchored national-accounts model paths for diagnosis, then adds each model's post-forecast change to the PBO expense ratio at the join.")
doc <- add_figure(doc, "outputs/figures/data/01_historical_spending_concepts.png",
                  "Figure 2. Historical spending concepts and the official expense forecast.")

doc <- add_heading(doc, "3. Cleaning and validation")
doc <- add_text(doc, "Quarterly and monthly series are converted to financial years only where all periods are present. ABS revision-only duplicate series are excluded. Population is filtered to persons so male, female and persons series are not added together. GFS purpose categories are checked against total expenses, and historical and projected age shares must sum to one.")
doc <- add_table(doc, validation_table)
doc <- add_figure(doc, "outputs/figures/data/02_population_age_shares.png",
                  "Figure 3. Historical and projected age composition.")

doc <- body_add_break(doc)
doc <- add_heading(doc, "4. Official forecast anchor and projection join")
doc <- add_text(doc, paste0(
  "For ", fy(projection_start), " to ", fy(official_forecast_end),
  ", the published PBO consolidated expenses, revenue, net capital investment, public debt interest and net debt ratios are used. ",
  "From ", fy(official_forecast_end + 1L), ", the structural projection supplies changes. This separates an official forecast containing policy, near-term macro judgement and detailed budget information from a stylised long-run projection."
))
official_display <- official[, .(
  `Financial year` = fy(year),
  Status = status,
  `Expenses / GDP` = pct(expenses_ratio_gdp),
  `Revenue / GDP` = pct(revenue_ratio_gdp),
  `Public debt interest / GDP` = pct(public_debt_interest_ratio_gdp),
  `Net debt / GDP` = pct(net_debt_ratio_gdp)
)]
doc <- add_table(doc, official_display, font_size = 8)
doc <- add_figure(doc, "outputs/figures/data/03_official_revenue_expenses.png",
                  "Figure 4. Official consolidated revenue and expense ratios used as the short-run anchor.")

doc <- add_heading(doc, "5. Bottom-up spending projection")
doc <- add_text(doc, "The bottom-up model starts with ABS GFS expenses by purpose. Public debt interest is removed from general public services, leaving primary expenses. Each category is projected from a service-demand exposure base and a category-specific excess-cost assumption. Health exposure weights older age groups; education weights younger age groups; social protection combines age groups; most remaining categories use total population. Defence follows an explicit GDP target.")
doc <- add_text(doc, "For a non-defence category j, the core recursion is E[j,t] = E[j,t-1] * (X[j,t] / X[j,t-1]) * (1 + economy-wide productivity and inflation growth + category excess cost). The economy-wide component prevents all per-person services from mechanically shrinking relative to GDP. Excess costs represent price, wage, technology, intensity or policy pressures above that baseline.")
doc <- add_text(doc, paste0(
  "During the official forecast years the category values are proportionally scaled to PBO total expenses less PBO public debt interest. After ", fy(official_forecast_end),
  " the categories determine the aggregate. The raw, unscaled path is retained and used in the forecast-period checks."
))
doc <- add_table(doc, category_table, font_size = 7.5)
doc <- add_figure(doc, "outputs/figures/bottom_up/01_central_category_stack.png",
                  "Figure 5. Central bottom-up primary spending by GFS purpose.")
doc <- add_figure(doc, "outputs/figures/bottom_up/02_total_scenarios.png",
                  "Figure 6. Bottom-up primary spending scenarios.")

doc <- add_heading(doc, "6. Top-down aggregate projections")
doc <- add_text(doc, "The top-down models use the national-accounts broad spending ratio. Age shares, terms of trade, the relative government-consumption price, unemployment and the COVID period provide possible economic and demographic drivers. Standardisation is fixed using the pre-COVID 1980-2019 sample so later observations do not redefine the scale.")
doc <- add_table(doc, model_explanation, font_size = 7.5)
model_fit_display <- top_summary[, .(
  Model = unname(model_labels[model]),
  Specification = specification,
  `ARIMA order` = fifelse(is.na(arima_order), "n/a", arima_order),
  AIC = round(aic, 1),
  AICc = fifelse(is.na(aicc), "n/a", sprintf("%.1f", aicc)),
  BIC = round(bic, 1),
  Observations = observations
)]
doc <- add_table(doc, model_fit_display, font_size = 7.5)
doc <- add_text(doc, "Information criteria are reported within each specification. They should not be used to rank models with different dependent-variable transformations; rolling pseudo-out-of-sample errors provide the cross-model forecast comparison.")
ecm_long_run_display <- ecm_long_run[, .(
  Term = term,
  `Implied long-run estimate` = pp(estimate, 4),
  `Adjustment coefficient` = pp(adjustment, 4)
)]
doc <- add_text(doc, "The ECM adjustment coefficient is negative in this vintage, indicating dynamically convergent conditional forecasts. A negative coefficient is necessary but not sufficient evidence for a valid level relationship. The implied long-run coefficients below are diagnostics from the unrestricted ECM parameterisation, not a separate model.")
doc <- add_table(doc, ecm_long_run_display, font_size = 8)
doc <- add_heading(doc, "ECM validity checks", 2)
doc <- add_text(doc, "The ARDL bounds approach permits a mixture of I(0) and I(1) regressors but not I(2) variables. ADF and KPSS tests are therefore used to assess integration order, followed by finite-sample Case III bounds F- and t-tests. Breusch-Godfrey, Breusch-Pagan, RESET and recursive CUSUM tests examine the fitted equation, while dynamic roots, estimation-window adjustment estimates and collinearity diagnostics assess forecast stability and identification.")
ecm_validation_display <- ecm_validation[, .(
  Criterion = criterion,
  Result = result,
  Assessment = assessment
)]
doc <- add_table(doc, ecm_validation_display, font_size = 7.5)
current_bounds <- ecm_bounds[specification == "Projection ECM as fitted"]
doc <- add_text(doc, paste0(
  "For the projection ECM, the lagged-level F statistic is ",
  pp(current_bounds[grepl("F-test", test), statistic], 3),
  " against finite-sample 5 per cent bounds of ",
  pp(current_bounds[grepl("F-test", test), lower_bound_I0_5pct], 3), " and ",
  pp(current_bounds[grepl("F-test", test), upper_bound_I1_5pct], 3),
  "; the adjustment t statistic is ",
  pp(current_bounds[grepl("t-test", test), statistic], 3),
  ". Both lie in the region that does not support a level relationship for the fitted projection ECM. The separate canonical ARDL robustness statistics are inconclusive after population is removed. Integration tests do not cleanly rule out higher-order behaviour in the selected age shares, the long-run regressors remain severely collinear, and the CUSUM stability test fails. The current ECM is not a trusted long-run equation."
))
doc <- add_figure(doc, "outputs/figures/top_down/01_anchored_long_run_models.png",
                  "Figure 7. Official forecast followed by each top-down model's projected change.")
doc <- add_figure(doc, "outputs/figures/top_down/03_model_spread.png",
                  "Figure 8. Dispersion across the anchored top-down paths.")
doc <- add_heading(doc, "Shapley attribution diagnostic", 2)
doc <- add_text(doc, paste0(
  "The structural factors are also used to decompose the observed change from ",
  unique(shapley$base_year), " to ", unique(shapley$comparison_year), ". For each coalition of factor groups, ",
  "the regression is re-estimated and its fitted change is calculated. A factor's contribution is its average incremental fitted effect across all ",
  shapley_check$implied_orderings, " possible orderings. The residual is reported separately so the components reconcile to the observed change. ",
  "This preserves the earlier Shapley approach as an attribution exercise; it is not treated as a forecasting model or a causal decomposition."
))
shapley_display <- shapley[, .(
  Component = component,
  `Contribution (percentage points of GDP)` = pp(contribution_pp)
)]
doc <- add_table(doc, shapley_display, font_size = 8)
doc <- add_figure(doc, "outputs/figures/top_down/04_shapley_attribution.png",
                  "Figure 9. Shapley attribution of the historical change in the broad spending ratio.")

doc <- body_add_break(doc)
doc <- add_heading(doc, "7. Forecast-period checks")
doc <- add_text(doc, paste0("Two checks are reported. First, rolling origins from 2009-10 to 2019-20 estimate each top-down model using only data then available and project one to five years ahead. Future explanatory variables are set to their realised values, so this is a conditional model check rather than a real-time forecast exercise. Second, all models are estimated on the full historical sample and run without the PBO anchor over 2025-26 to ", fy(official_forecast_end), "; their paths are compared with the current official forecast."))
doc <- add_table(doc, rolling_key, font_size = 8)
doc <- add_table(doc, official_key, font_size = 8)
doc <- add_figure(doc, "outputs/figures/forecast_checks/01_rmse_by_horizon.png",
                  "Figure 10. Rolling historical RMSE by forecast horizon.")
doc <- add_figure(doc, "outputs/figures/forecast_checks/02_model_minus_official.png",
                  "Figure 11. Unanchored model paths minus the official expense forecast.")
doc <- add_text(doc, "Interpretation: a model may be useful as a long-run scenario even if it misses the official near-term path, because the latter includes policy decisions, budget measures and forecast judgement absent from parsimonious historical equations. Conversely, short-horizon fit does not validate a model's long-run causal interpretation. The top-down equations are predictive associations, not causal estimates.")

doc <- add_heading(doc, "8. Revenue and taxes")
doc <- add_text(doc, paste0(
  "Revenue is external to the spending models. PBO consolidated revenue is used through ", fy(official_forecast_end),
  ". The central and restraint paths then raise revenue by 1.2 percentage points of GDP by 2036-37, while the pressure path raises it by 0.6 percentage points. These are transparent policy/elasticity assumptions, not tax microsimulation results."
))
doc <- add_text(doc, "An indexed-threshold sensitivity holds revenue/GDP at its final official-forecast value. This is a simple proxy for removing fiscal drag from personal income-tax thresholds; it is not an estimate of the full tax system under indexation. A fuller tax module should separately project personal income tax, company tax, GST/excises and non-tax revenue using tax bases, elasticities, policy measures and threshold schedules.")
doc <- add_figure(doc, "outputs/figures/revenue_debt/01_revenue_scenarios.png",
                  "Figure 12. Consolidated revenue scenarios.")

doc <- add_heading(doc, "9. Debt accumulation")
doc <- add_text(doc, "The bottom-up debt model separates primary spending and interest. Let d be net debt/GDP, g nominal GDP growth, i the effective interest rate, p primary expenses/GDP, k net capital investment/GDP, r revenue/GDP and s a stock-flow adjustment. The recursion is: opening debt on current-year GDP = d[t-1] / (1 + g[t]); interest/GDP = i[t] * opening debt; d[t] = opening debt + p[t] + interest[t] + k[t] - r[t] + s[t].")
doc <- add_text(doc, "Under the preferred treatment, the top-down models project fiscal expenditure excluding conventional other interest. The debt module then adds public debt interest endogenously using the effective interest-rate assumptions and the preceding gross-debt stock. This keeps the spending regression separate from the debt-interest feedback. Effective interest rates transition from the official implied rate to the scenario long-run rate.")
doc <- add_table(doc, assumption_table, font_size = 7.5)
doc <- add_figure(doc, "outputs/figures/revenue_debt/03_top_down_debt_model_spread.png",
                  "Figure 13. Diagnostic debt paths implied by the top-down total-expense models.")

doc <- body_add_break(doc)
doc <- add_heading(doc, "10. Selected scenario results")
doc <- add_text(doc, "The following values are mechanical outputs of the stated assumptions. They are not point forecasts and should be read with the scenario definitions and limitations.")
doc <- add_table(doc, bottom_key, font_size = 8)
doc <- add_table(doc, top_endpoint, font_size = 8)

doc <- add_heading(doc, "11. Bottom-up/top-down comparison and statistical diagnostics")
doc <- add_text(doc, "The comparison places the central bottom-up total-expense path and the top-down total-expense-equivalent paths on one chart. Both approaches add endogenous public debt interest to spending excluding conventional other interest, although the bottom-up path is assembled from purpose categories while the top-down paths are estimated in aggregate.")
doc <- add_figure(doc, "outputs/figures/model_comparison/01_bottom_up_vs_top_down.png",
                  "Figure 14. Central bottom-up total expenses compared with all anchored top-down estimates.")
doc <- add_figure(doc, "outputs/figures/model_comparison/02_bottom_up_top_down_ranges.png",
                  "Figure 15. Bottom-up scenario range compared with top-down model spread.")

doc <- add_heading(doc, "Recommended approach", 2)
recommendation_display <- copy(approach_recommendation)
setnames(recommendation_display, c("horizon_or_use", "recommended_approach", "rationale"),
         c("Horizon or use", "Recommended approach", "Reason"))
doc <- add_table(doc, recommendation_display, font_size = 7.5)
doc <- add_text(doc, "The central recommendation is therefore a layered framework, not a mechanical choice of the equation with the highest in-sample R-squared. Official forecasts contain policy and budget information. Beyond that horizon, the bottom-up model is best suited to the fiscal-sustainability question because it identifies which services, demographic exposures, cost assumptions, revenue choices and interest feedbacks drive the debt path. Its weakness is that calibrated assumptions do not generate statistical standard errors, so those assumptions must be stress-tested and documented.")
doc <- add_text(doc, "With population removed and real GDP per capita retained, levels ARIMAX has the lowest five-year rolling RMSE, but severe collinearity among income and age shares prevents a structural interpretation. Dynamic differences has acceptable collinearity and reasonable rolling performance, but its accumulated long-run decline remains a sensitivity rather than a selected central path. The hybrid retains material collinearity in its structural block.")
doc <- add_text(doc, "The parsimonious real-income ARIMAX is a statistical benchmark. Its income coefficient is insignificant and unstable across estimation windows, and it omits richer ageing, relative-price, labour-market and policy channels.")

scale_display <- scale_income[model != "arimax_diff", .(
  Model = model_label,
  `Driver variant` = variant_label,
  `In-sample RMSE (pp)` = pp(in_sample_rmse_pp),
  `5-year rolling RMSE (pp)` = pp(rolling_rmse_5y_pp),
  `2065-66 endpoint (%)` = pp(anchored_2066 * 100)
)]
doc <- add_heading(doc, "Population and real-GDP-per-capita tests", 2)
doc <- add_text(doc, "Each model family was estimated with current controls only, population only, real GDP per capita only, and both together. The GDP-per-capita-only variant is the headline specification. Population is excluded because it has no clear scale interpretation in a spending-to-GDP equation and is almost perfectly correlated with age composition, income and time. The negative real-income coefficient materially lowers several long-run endpoints and should be tested for denominator, trend and regime effects before it is given a causal interpretation.")
doc <- add_table(doc, scale_display, font_size = 6.5)
collinearity_display <- model_collinearity[, .(
  Model = model_label, Component = component,
  `Maximum VIF` = pp(max(variance_inflation_factor), 2),
  `Condition number` = pp(first(standardised_design_condition_number), 2),
  Assessment = first(collinearity_assessment)
), by = .(model, component)][, c("model", "component") := NULL]
doc <- add_table(doc, collinearity_display, font_size = 6.5)
doc <- add_figure(doc, "outputs/figures/peer_review/07_scale_income_fit.png",
                  "Effect of adding population and real GDP per capita to each top-down family.", height = 4.2)

selection_display <- model_selection[, .(
  Model = model_label,
  `In-sample R2` = pp(r_squared, 3),
  `In-sample RMSE (pp)` = pp(in_sample_rmse_pp),
  `Rolling RMSE: 1 year (pp)` = pp(rolling_rmse_1y_pp),
  `Rolling RMSE: 5 years (pp)` = pp(rolling_rmse_5y_pp),
  `Official-period MAE (pp)` = pp(official_period_mae_pp),
  `Maximum sample-window shift (pp)` = pp(maximum_endpoint_window_shift_pp),
  `Conditional 95% width in 2065-66 (pp)` = pp(conditional_95_interval_width_pp)
)]
doc <- add_table(doc, selection_display, font_size = 7)
doc <- add_text(doc, "No single column determines the recommendation. In-sample fit rewards close historical tracking; rolling RMSE tests pseudo-out-of-sample stability; official-period MAE tests consistency with current forecast judgement; estimation-window shifts reveal specification instability; and conditional interval width shows how innovations accumulate within the model. These criteria answer different questions and should be read together.")

doc <- add_heading(doc, "Diagnostic coverage", 2)
scope_display <- diagnostic_scope
setnames(scope_display, c("approach", "model_fit", "sensitivity", "standard_errors"),
         c("Approach", "Model fit", "Sensitivity", "Standard errors"))
doc <- add_table(doc, scope_display, font_size = 7.5)
doc <- add_text(doc, "Conventional regression standard errors are not reported for the bottom-up categories because their excess-cost and exposure parameters are calibrated assumptions, not estimated coefficients. Bottom-up uncertainty is represented by transparent scenario sensitivity. Treating that scenario spread as a statistical confidence interval would be misleading.")

doc <- add_heading(doc, "In-sample fit and residual tests", 2)
fit_display <- fit_diagnostics[, .(
  Model = model_label,
  `R-squared` = pp(r_squared, 3),
  `MAE (pp)` = pp(mae_pp),
  `RMSE (pp)` = pp(rmse_pp),
  `Ljung-Box p-value` = pp(ljung_box_p_value, 3),
  `Shapiro-Wilk p-value` = pp(shapiro_wilk_p_value, 3)
)]
doc <- add_table(doc, fit_display, font_size = 8)
doc <- add_text(doc, "Levels ARIMAX has the lowest in-sample RMSE and highest in-sample R-squared in this vintage. The structural OLS Ljung-Box result indicates residual serial correlation, supporting models with explicit dynamics. Low Shapiro-Wilk p-values for some specifications warn against interpreting normal-theory intervals too literally. In-sample fit is not a model-selection rule: rolling forecast performance remains the more relevant short-horizon check.")
doc <- add_figure(doc, "outputs/figures/diagnostics/01_in_sample_fit.png",
                  "Figure 16. In-sample fit on the broad spending/GDP level.")

doc <- add_heading(doc, "Coefficient standard errors and forecast intervals", 2)
doc <- add_text(doc, "The output table model_coefficient_standard_errors.csv reports conventional standard errors for every estimated coefficient, HC1 heteroskedasticity-robust standard errors for the OLS equations, and maximum-likelihood covariance-matrix standard errors for ARIMA coefficients. These quantify sampling uncertainty conditional on each chosen equation; they do not cover model selection, data revisions or the assumed driver paths.")
structural_se_display <- coefficient_se[
  model == "structural_ols" & term != "(Intercept)",
  .(Term = term, Estimate = pp(estimate, 4), `Conventional SE` = pp(conventional_se, 4),
    `Robust HC1 SE` = pp(robust_hc1_se, 4), `Robust p-value` = pp(robust_hc1_p_value, 3))
]
doc <- add_table(doc, structural_se_display, font_size = 8)
interval_endpoint_display <- forecast_intervals[year == projection_end, .(
  Model = model_label,
  `Model-only mean` = pct(mean),
  `Forecast standard error` = paste0(pp(standard_error * 100), " pp"),
  `Lower 95%` = pct(lower_95),
  `Upper 95%` = pct(upper_95)
)]
doc <- add_table(doc, interval_endpoint_display, font_size = 8)
doc <- add_text(doc, "The forecast intervals are conditional model-only intervals. The economic and demographic paths are fixed. ARIMA intervals include simulated future innovations, the ECM recursively propagates simulated equation innovations, and all estimated coefficients are held fixed; structural OLS uses a conventional prediction interval. These ranges should not be combined mechanically with bottom-up scenario ranges.")
doc <- add_figure(doc, "outputs/figures/diagnostics/04_conditional_forecast_intervals.png",
                  "Figure 17. Conditional model-only 95% intervals for each top-down model.", height = 5.0)

doc <- add_heading(doc, "Sensitivity tests", 2)
window_display <- window_sensitivity[estimation_window != "Full sample", .(
  Model = model_label,
  `Estimation window` = estimation_window,
  `2065-66 change from full sample (pp)` = pp(endpoint_difference_from_full_sample_pp)
)]
doc <- add_table(doc, window_display, font_size = 8)
doc <- add_text(doc, "Estimation-window sensitivity is material: several structural endpoints move by multiple percentage points when estimation begins in 1990 or ends before COVID. Driver sensitivities now include a one per cent population level change and a one per cent real-GDP-per-capita change, as well as controlled ageing, unemployment, terms-of-trade and relative-price shocks. All models respond to population and income under the headline configuration.")
doc <- add_figure(doc, "outputs/figures/diagnostics/02_driver_sensitivity.png",
                  "Figure 18. Effect of controlled driver changes on each model's 2065-66 endpoint.")
doc <- add_figure(doc, "outputs/figures/diagnostics/03_estimation_window_sensitivity.png",
                  "Figure 19. Sensitivity of the 2065-66 endpoint to the estimation sample.")

doc <- add_heading(doc, "12. Limitations and next development priorities")
doc <- add_bullets(doc, c(
  "Only ten annual observations are available in the current GFS purpose table. The bottom-up category model is consequently calibrated, not econometrically estimated by category.",
  "The Centre for Population workbook publishes selected long-run years; intermediate annual age structures are interpolated. This is appropriate for smooth exposure paths but does not reproduce a full cohort-component model.",
  "The official anchor is the PBO 2026-27 consolidated outlook. Individual Commonwealth or state budget numbers do not by themselves replace this consolidated national forecast.",
  "The national-accounts and GFS/PBO spending concepts differ. Anchoring changes at the join is transparent but does not create a detailed accounting bridge.",
  "Excess-cost, defence, revenue, investment, interest and stock-flow assumptions are illustrative. Policy-costed scenarios should replace them where available.",
  "Conditional model-only forecast intervals hold coefficients and driver paths fixed. Model spread is specification uncertainty, while pressure/restraint paths are assumption sensitivities; neither is a statistical probability interval.",
  "The next valuable extension is a modular revenue block with tax-specific bases and elasticities, followed by stochastic macro shocks and parameter uncertainty."
))

doc <- add_heading(doc, "13. Reproduction and audit trail")
doc <- add_text(doc, "The complete run writes CSV tables, fitted-model RDS files, a source manifest, validation checks, figures and this report. Review data_validation.csv first, then the unanchored forecast comparison, before interpreting long-run debt endpoints. All important scenario levers are in scripts/03_assumptions.R.")

report_path <- file.path(documentation_dir, "2026_fiscal_projections_methodology.docx")
print(doc, target = report_path)

writeLines(capture.output(sessionInfo()), file.path(documentation_dir, "R_session_info.txt"))
message("Word methodology report written to: ", report_path)
