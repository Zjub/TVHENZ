source("config.R")
suppressPackageStartupMessages({
  library(data.table)
  library(officer)
  library(flextable)
})

fit <- fread(file.path(table_dir, "in_sample_fit.csv"))
rolling <- fread(file.path(table_dir, "rolling_fit_summary.csv"))
coefficients <- fread(file.path(table_dir, "coefficients.csv"))
collinearity <- fread(file.path(table_dir, "collinearity.csv"))
stability <- fread(file.path(table_dir, "residual_and_stability_tests.csv"))
residual_dynamics <- fread(file.path(
  table_dir, "residual_dynamics_sensitivity.csv"
))
endpoints <- fread(file.path(table_dir, "model_endpoints.csv"))
contributions <- fread(file.path(table_dir, "structural_driver_contributions.csv"))
actual_contributions <- fread(file.path(
  table_dir, "structural_actual_anchor_driver_contributions.csv"
))
debt <- fread(file.path(table_dir, "debt_endpoints.csv"))
debt_effects <- fread(file.path(table_dir, "structural_driver_debt_effects.csv"))
windows <- fread(file.path(table_dir, "structural_window_sensitivity.csv"))
covid <- fread(file.path(table_dir, "structural_covid_sensitivity.csv"))
income_assumptions <- fread(file.path(table_dir, "real_gdp_per_capita_scenarios.csv"))
historical_2019_paths <- fread(file.path(
  table_dir, "historical_2019_anchored_primary_spending_paths.csv"
))

fmt <- function(x, digits = 2L) formatC(x, format = "f", digits = digits)
pct <- function(x, digits = 2L) paste0(fmt(x, digits), "%")

add_table <- function(doc, table, font_size = 8) {
  ft <- flextable(table)
  ft <- theme_booktabs(ft)
  ft <- fontsize(ft, size = font_size, part = "all")
  ft <- bold(ft, part = "header")
  ft <- autofit(ft)
  body_add_flextable(doc, ft)
}

add_figure <- function(doc, filename, caption, width = 6.5, height = 4.1) {
  doc <- body_add_img(doc, src = file.path(figure_dir, filename),
                      width = width, height = height)
  body_add_par(doc, caption, style = "Image Caption")
}

fit_table <- merge(
  fit[, .(model, model_label, one_step_rmse_pp, residual_ljung_box_p)],
  rolling[, .(model, average_rmse_pp, rmse_1y_pp, rmse_5y_pp)],
  by = "model"
)[, .(
  Model = model_label,
  `One-step RMSE` = fmt(one_step_rmse_pp),
  `Rolling RMSE` = fmt(average_rmse_pp),
  `1-year RMSE` = fmt(rmse_1y_pp),
  `5-year RMSE` = fmt(rmse_5y_pp),
  `Residual LB p` = fmt(residual_ljung_box_p, 3)
)]

coefficient_labels <- c(
  d_age_0_14_pp = "Change in population aged 0-14 (percentage points)",
  d_age_65_74_pp = "Change in population aged 65-74 (percentage points)",
  d_age_75p_pp = "Change in population aged 75+ (percentage points)",
  d_log_relative_price = "Change in log relative government price",
  d_log_real_gdp_per_capita = "Change in log real GDP per capita"
)
structural_coefficients <- coefficients[
  model == "with_income" & group %in%
    c("Age composition", "Relative government prices", "Real GDP per capita")
][, .(
  Variable = unname(coefficient_labels[term]),
  Estimate = fmt(estimate, 3),
  `Standard error` = fmt(standard_error, 3),
  `p-value` = fmt(p_value, 3)
)]

debt_pbo <- debt[anchor_type == "pbo"]
debt_actual <- debt[anchor_type == "actual"]
case_label_map <- setNames(endpoints$model_label, endpoints$model)
high_growth_label <- unname(case_label_map["central_gdp"])
middle_growth_label <- unname(case_label_map["midpoint_gdp"])
low_growth_label <- unname(case_label_map["pressure_gdp"])
no_gdp_label <- unname(case_label_map["no_income"])

endpoint_table <- merge(
  endpoints[, .(model, model_label, anchored_primary_fiscal_2066_pp)],
  debt_pbo[, .(model, total_fiscal_expenditure_2066_pp,
               public_debt_interest_2066_pp, net_debt_2066_pp)],
  by = "model"
)[, .(
  Model = model_label,
  `Primary fiscal expenditure, 2065-66` = pct(anchored_primary_fiscal_2066_pp),
  `PDI, 2065-66` = pct(public_debt_interest_2066_pp),
  `Total fiscal expenditure, 2065-66` = pct(total_fiscal_expenditure_2066_pp),
  `Net debt, 2065-66` = pct(net_debt_2066_pp)
)]

contribution_table <- contributions[year == projection_end, .(
  Driver = group,
  `Spending contribution` = paste0(fmt(cumulative_contribution_pp), " pp")
)]
contribution_table <- merge(
  contribution_table,
  debt_effects[, .(
    Driver = contribution_group,
    `Net-debt contribution` = paste0(fmt(driver_effect_on_net_debt_2066_pp), " pp")
  )], by = "Driver", all.x = TRUE
)

robustness_table <- rbindlist(list(
  windows[, .(
    Test = window,
    `2065-66 primary expenditure` = pct(endpoint_2066_pp),
    `Difference from reference` = paste0(fmt(difference_from_full_sample_pp), " pp")
  )],
  covid[2L, .(
    Test = treatment,
    `2065-66 primary expenditure` = pct(endpoint_2066_pp),
    `Difference from reference` = paste0(fmt(difference_from_central_pp), " pp")
  )]
))

residual_dynamics_table <- residual_dynamics[, .(
  `Error process` = error_specification,
  BIC = fmt(bic, 1),
  `Ljung-Box p` = fmt(residual_ljung_box_p, 3),
  `65-74 coefficient` = fmt(age_65_74_coefficient, 2),
  `75+ coefficient` = fmt(age_75p_coefficient, 2),
  `2065-66 expenditure` = pct(pbo_anchored_primary_fiscal_2066_pp)
)]

income_summary_table <- income_assumptions[year >= official_forecast_end, .(
  Case = unique(income_case_label),
  `Average annual growth, 2030-66` = pct(
    100 * (exp(
      (log_real_gdp_per_capita[year == projection_end] -
         log_real_gdp_per_capita[year == official_forecast_end]) /
        (projection_end - official_forecast_end)
    ) - 1)
  ),
  `Growth in 2065-66` = pct(
    100 * real_gdp_per_capita_growth[year == projection_end]
  ),
  `2065-66 index (2029-30=100)` = fmt(
    100 * exp(
      log_real_gdp_per_capita[year == projection_end] -
        log_real_gdp_per_capita[year == official_forecast_end]
    ), 1
  )
), by = income_case][, income_case := NULL]
income_summary_table <- rbindlist(list(
  income_summary_table,
  data.table(
    Case = "No real GDP-per-capita term",
    `Average annual growth, 2030-66` = "Not used",
    `Growth in 2065-66` = "Not used",
    `2065-66 index (2029-30=100)` = "Not used"
  )
), use.names = TRUE)

literature_table <- data.table(
  Source = c(
    "Australian Treasury, 2023 IGR",
    "OECD long-term fiscal framework",
    "European Commission, 2024 Ageing Report",
    "OECD health fiscal sustainability model",
    "Wagner-law empirical literature",
    "Augmented ARDL methodology"
  ),
  Implication = c(
    "Anchor to official estimates; project major spending pressures from age profiles and non-demographic cost growth; abstract from COVID when estimating long-run trends.",
    "Separate pensions, health and long-term care from other primary expenditure; link other spending to population or GDP using explicit assumptions.",
    "Multiply age/sex cost profiles by population projections and allow unit costs to respond to GDP per capita and relative productivity.",
    "Treat income, demography, relative productivity and technology as distinct drivers; do not attribute all trend growth to ageing.",
    "GDP-income elasticities vary materially by estimator and sample. A long-run aggregate levels claim requires cointegration rather than a high in-sample R-squared.",
    "A valid ARDL long-run interpretation requires the bounds F test, lagged-dependent t test and independent-level F test to support cointegration. They do not in these Australian data."
  )
)

central_endpoint <- endpoints[model == "central_gdp"]
central_debt <- debt_pbo[model == "central_gdp"]
central_actual_debt <- debt_actual[model == "central_gdp"]
no_income_debt <- debt_pbo[model == "no_income"]
no_income_actual_debt <- debt_actual[model == "no_income"]
anchor_comparison_table <- debt[
  model %in% c("central_gdp", "midpoint_gdp", "pressure_gdp", "no_income"),
  .(
    model,
    Case = model_label,
    Anchor = ifelse(anchor_type == "pbo", "PBO", "Latest actual"),
    `Primary expenditure` = pct(primary_fiscal_expenditure_2066_pp),
    PDI = pct(public_debt_interest_2066_pp),
    `Total expenditure` = pct(total_fiscal_expenditure_2066_pp),
    `Net debt` = pct(net_debt_2066_pp)
  )
]
anchor_comparison_table[, model_order := match(
  model, c("central_gdp", "midpoint_gdp", "pressure_gdp", "no_income")
)]
anchor_comparison_table[, anchor_order := match(
  Anchor, c("PBO", "Latest actual")
)]
setorder(anchor_comparison_table, model_order, anchor_order)
anchor_comparison_table[, c("model", "model_order", "anchor_order") := NULL]
anchor_spending_table <- endpoints[, .(
  model,
  Case = model_label,
  `PBO anchor` = pct(anchored_primary_fiscal_2066_pp),
  `Latest-actual anchor` = pct(actual_anchored_primary_fiscal_2066_pp),
  `FY2018-19 anchor` = pct(historical_2019_anchored_primary_fiscal_2066_pp)
)]
anchor_spending_table[, case_order := match(
  model, c("central_gdp", "midpoint_gdp", "pressure_gdp", "no_income")
)]
setorder(anchor_spending_table, case_order)
anchor_spending_table[, c("model", "case_order") := NULL]
anchor_endpoint_gap <- central_endpoint$actual_anchored_primary_fiscal_2066_pp -
  central_endpoint$anchored_primary_fiscal_2066_pp
no_income_actual_endpoint <- endpoints[
  model == "no_income", actual_anchored_primary_fiscal_2066_pp
]
no_income_actual_gap <- no_income_actual_endpoint -
  central_endpoint$actual_anchored_primary_fiscal_2066_pp
central_2019_anchor <- historical_2019_paths[
  model == "central_gdp" & year == 2019L, primary_fiscal_pp
]
income_beta <- coefficients[model == "with_income" & term == "d_log_real_gdp_per_capita", estimate]
anchor_share <- 34.9
implied_income_elasticity <- 1 + income_beta / anchor_share
max_vif <- max(collinearity[model == "with_income", vif])
structural_stability <- stability[model == "with_income"]
selected_residual_fit <- residual_dynamics[is_bic_selected == TRUE][1L]
white_noise_residual_fits <- residual_dynamics[residual_ljung_box_p > 0.05]
residual_endpoint_range <- range(
  residual_dynamics$pbo_anchored_primary_fiscal_2066_pp,
  na.rm = TRUE
)
age_75_coefficient_range <- range(
  residual_dynamics$age_75p_coefficient,
  na.rm = TRUE
)

doc <- read_docx()
doc <- body_add_par(doc, "A streamlined structural top-down model of Australian government spending", style = "graphic title")
doc <- body_add_par(doc, paste0(
  "Model selection, 40-year structural decomposition and debt implications | ",
  format(Sys.Date(), "%d %B %Y")
), style = "centered")

doc <- body_add_par(doc, "Recommendation", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "Use the anchored structural-change bridge as an aggregate top-down lens and report the GDP assumptions as four explicit scenarios without selecting a single case as the model choice. ",
  "It estimates annual changes in non-interest fiscal expenditure using slow-moving demographic, relative-price and real-income factors, while unemployment, the terms of trade and COVID interventions condition the historical equation. ",
  "The main presentation uses the PBO level through FY2029-30. Alternative spending paths start from the latest National Accounts actual in FY2024-25 or the pre-COVID FY2018-19 actual. After each anchor, only the slow-moving factors are accumulated. Transitory macro effects and ARMA residuals are reset rather than allowed to alter spending permanently."
))
doc <- body_add_par(doc, paste0(
  "Four cases are reported. Three apply different real-GDP-per-capita paths to the same estimated structural equation and are labelled by their annual real-GDP-per-capita growth rates in 2060: ",
  high_growth_label, ", ", middle_growth_label, " and ", low_growth_label,
  ". The fourth, ", no_gdp_label,
  ", is a separately estimated equation without real GDP per capita. All other projected drivers and fiscal assumptions use common baseline settings, so their differences isolate the real-income channel. The model should be described as a semi-structural scenario model rather than a point forecast."
))

doc <- body_add_par(doc, "Why this model, and why not the old grid", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "The old levels OLS and levels ARIMAX equations fit the historical level closely, but their 2065-66 endpoints move by about 5-6 percentage points when COVID-era observations are removed. ",
  "A levels interpretation also requires a stable long-run relationship. The augmented ARDL tests reject that premise: the bounds F exact p-value is 0.438, the bounds t p-value is 0.675, and the independent-level F p-value is 0.225. The ECM is therefore not a defensible long-run model."
))
doc <- body_add_par(doc, paste0(
  "The former hybrid correctly recognised structural and cyclical blocks, but accumulated a forecast of residual annual changes. That gives a temporary disturbance a permanent effect on the spending level. ",
  "The streamlined bridge keeps the useful distinction but resets the cyclical gap at the PBO anchor. The simple differenced ARIMAX and the dynamic-differences model were substantively the same family; they are no longer reported separately."
))

doc <- body_add_par(doc, "Model specification", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "Let e[t] be fiscal expenditure excluding conventional other interest, measured in percentage points of GDP. Let S[t] contain the population shares aged 0-14, 65-74 and 75+, the log relative government price, and log real GDP per capita. Let C[t] contain unemployment and the log terms of trade. The estimated equation is:"
))
doc <- body_add_par(doc, paste0(
  "Change(e[t]) = beta_S * Change(S[t]) + gamma_0 * Change(C[t]) + gamma_1 * Change(C[t-1]) + delta * Change(Dcovid[t]) + u[t]."
))
doc <- body_add_par(doc, paste0(
  "The COVID variables are FY2020, FY2021 and FY2022 level indicators before differencing. Each therefore adds +1 in its own year and -1 in the following year, removing the exceptional level movement without imposing a ramp. A small ARMA search is allowed for u[t]; BIC selects ARMA(0,0) for the full-sample structural equation. No drift is estimated or extrapolated."
))
doc <- body_add_par(doc, paste0(
  "For the PBO-anchored cases and t after FY2029-30, the long-run path is e[t] = e[PBO] plus the cumulative beta_S * Change(S) terms. In the historical-anchor cases the same construction begins from e[2024-25] or e[2018-19]. The gamma, delta and residual terms are set to zero after the relevant anchor. This is the operational distinction between slow-moving pressures and transitory history."
))

doc <- body_add_par(doc, "Variable choices", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "Age composition is represented by the population shares aged 0-14, 65-74 and 75+, not total population. Separating the two older groups allows the model to distinguish the more intensive expenditure pressures associated with the oldest population. Total population is inappropriate in an expenditure/GDP ratio once demographic composition and GDP per capita are present; its influence is already embedded in the external population, age-share and GDP projections. A combined 65+ share is retained in the robustness table."
))
doc <- body_add_par(doc, paste0(
  "Real GDP per capita belongs in the structural block because the literature treats income and service demand as long-run spending drivers. The fitted coefficient implies an approximate real-spending-per-person income elasticity of ",
  fmt(implied_income_elasticity, 2), " at the PBO anchor. This is within the broad range found in some instrumental-variable work on government spending, but it is imprecise as an aggregate 40-year parameter and is therefore accompanied by a no-income sensitivity."
))
doc <- body_add_par(doc, paste0(
  "The relative government price captures Baumol-type cost pressure. Unemployment and the terms of trade are transitory conditioning variables: they help prevent cyclical history from contaminating the slow coefficients, but are not treated as permanent spending determinants. Interest is excluded because it is generated by debt and the effective interest rate, not by service demand."
))

doc <- body_add_par(doc, "Literature alignment", style = "heading 1")
doc <- add_table(doc, literature_table, 7.5)
doc <- body_add_par(doc, paste0(
  "The official literature is more supportive of component-based projections than of a single aggregate regression. This model should therefore be used as an independent aggregate lens and decomposition, alongside the bottom-up category model—not as a replacement for it."
))

doc <- body_add_par(doc, "Statistical assessment", style = "heading 1")
doc <- add_table(doc, fit_table, 7.5)
doc <- body_add_par(doc, paste0(
  "The structural equation with real GDP per capita has a one-step RMSE of ", fmt(fit[model == "with_income", one_step_rmse_pp]),
  " percentage points of GDP. Its maximum VIF is ", fmt(max_vif),
  " and its scaled design condition number is ", fmt(unique(collinearity[model == "with_income", condition_number])),
  ", so the differenced design does not show severe multicollinearity. Residuals reject a unit root and the recursive-CUSUM p-value is ",
  fmt(structural_stability$recursive_cusum_p, 3), "."
))
doc <- body_add_par(doc, "Residual-dynamics check", style = "heading 2")
doc <- add_table(doc, residual_dynamics_table, 7.5)
doc <- body_add_par(doc, paste0(
  "The ", selected_residual_fit$error_specification,
  " has a Ljung-Box p-value of ",
  fmt(selected_residual_fit$residual_ljung_box_p, 3),
  ". Fixed low-order ARMA alternatives are therefore used to check whether the residual warning changes the structural result. Specifications with a Ljung-Box p-value above 0.05 are: ",
  if (nrow(white_noise_residual_fits)) {
    paste(white_noise_residual_fits$error_specification, collapse = ", ")
  } else "none",
  ". Across all reported error processes, the 75+ coefficient ranges from ",
  fmt(age_75_coefficient_range[1L], 2), " to ",
  fmt(age_75_coefficient_range[2L], 2),
  " and the FY2065-66 PBO-anchored endpoint ranges from ",
  pct(residual_endpoint_range[1L]), " to ",
  pct(residual_endpoint_range[2L]), "."
))
doc <- body_add_par(doc, paste0(
  "The ARIMA benchmark has the lowest average rolling RMSE (",
  fmt(rolling[model == "arima_benchmark", average_rmse_pp]),
  ") versus ", fmt(rolling[model == "with_income", average_rmse_pp]),
  " for the structural bridge. That is evidence against presenting any structural path as a high-precision forecast. It is not a reason to use the ARIMA for a 40-year structural decomposition."
))
doc <- add_figure(doc, "03_rolling_rmse.png", "Figure 1. Conditional rolling forecast RMSE by horizon.")

doc <- body_add_par(doc, "Estimated slow-moving coefficients", style = "heading 1")
doc <- add_table(doc, structural_coefficients, 8)
doc <- body_add_par(doc, paste0(
  "The separate 65-74 and 75+ coefficients allow the historical equation to place different weights on the two older groups. Together with the young share, they produce the lifecycle contribution shown below. The combined 65+ specification is reported only as a sensitivity. Coefficient significance does not remove long-horizon parameter uncertainty."
))

doc <- body_add_par(doc, "Forty-year spending result", style = "heading 1")
doc <- add_figure(doc, "01_gdp_sensitivity_primary_spending.png", "Figure 2. PBO-anchored spending paths under the four real-GDP treatments.")
doc <- add_table(doc, income_summary_table, 7.5)
doc <- add_figure(doc, "05_structural_driver_paths.png", "Figure 3. Real GDP per capita paths used in the three income scenarios.")
doc <- body_add_par(doc, paste0(
  "The higher- and lower-growth paths are read directly from the existing macro assumptions. The middle path is halfway between their log real-GDP-per-capita levels in every year, which is equivalent to applying the midpoint of their cumulative log growth from the common starting point. Demography, relative prices, transitory variables, inflation, revenue and interest-rate assumptions use common baseline settings. Nominal GDP is reconstructed from each real-GDP path using the common inflation assumption so that the debt denominator remains internally consistent."
))
doc <- add_figure(doc, "02_structural_driver_contributions.png", paste0("Figure 4. Cumulative structural-driver contributions under ", high_growth_label, "."))
doc <- add_table(doc, contribution_table, 8)
doc <- body_add_par(doc, paste0(
  "From the FY2029-30 PBO anchor of 34.9% of GDP, age composition adds ",
  fmt(contributions[group == "Age composition" & year == projection_end, cumulative_contribution_pp]),
  " percentage points by FY2065-66. Rising real GDP per capita subtracts ",
  fmt(abs(contributions[group == "Real GDP per capita" & year == projection_end, cumulative_contribution_pp])),
  " points because estimated real spending per person grows less than one-for-one with income. The relative-price contribution is only ",
  fmt(contributions[group == "Relative government prices" & year == projection_end, cumulative_contribution_pp]),
  " points because the supplied baseline relative-price projection is essentially flat after the early 2030s."
))
doc <- body_add_par(doc, paste0(
  "The ", high_growth_label, " primary-fiscal-expenditure endpoint is ",
  pct(central_endpoint$anchored_primary_fiscal_2066_pp),
  " of GDP. The coefficient-only 80% interval is ",
  pct(central_endpoint$central_lower_80_pp), " to ",
  pct(central_endpoint$central_upper_80_pp),
  "; the 95% interval is ", pct(central_endpoint$central_lower_95_pp),
  " to ", pct(central_endpoint$central_upper_95_pp),
  ". These ranges exclude uncertainty in future population, GDP, prices, policy and the PBO anchor."
))

doc <- body_add_par(doc, "Alternative latest-actual anchor", style = "heading 2")
doc <- add_figure(doc, "07_gdp_pc_model_anchor_comparison.png", paste0("Figure 5. ", high_growth_label, " under the PBO, latest-actual and FY2018-19 anchors."))
doc <- add_table(doc, anchor_spending_table, 7.5)
doc <- body_add_par(doc, paste0(
  "The alternative does not use the PBO expenditure projection. It starts from the FY2024-25 National Accounts actual and applies the same estimated structural changes from FY2025-26 onward. It reaches ",
  pct(central_endpoint$actual_anchored_primary_fiscal_2066_pp),
  " of GDP in FY2065-66, compared with ",
  pct(central_endpoint$anchored_primary_fiscal_2066_pp),
  " under the PBO anchor; a difference of ", fmt(anchor_endpoint_gap),
  " percentage points. The difference reflects both the higher latest-actual starting level and model-implied changes over FY2025-26 to FY2029-30 that replace the PBO near-term consolidation path."
))
doc <- body_add_par(doc, paste0(
  "The latest-actual coefficient-only 80% interval is ",
  pct(central_endpoint$actual_anchor_lower_80_pp), " to ",
  pct(central_endpoint$actual_anchor_upper_80_pp),
  "; the 95% interval is ", pct(central_endpoint$actual_anchor_lower_95_pp),
  " to ", pct(central_endpoint$actual_anchor_upper_95_pp),
  ". Comparing the anchors is therefore a transparent level and near-term-policy sensitivity, not a re-estimation of the model."
))
doc <- add_figure(doc, "09_latest_actual_gdp_sensitivity.png", "Figure 6. Latest-actual spending paths under the four real-GDP treatments.")
doc <- body_add_par(doc, paste0(
  "The separately re-estimated model that omits real GDP per capita reaches ",
  pct(no_income_actual_endpoint), " of GDP under the latest-actual anchor, ",
  fmt(no_income_actual_gap), " percentage points above the ", high_growth_label,
  " latest-actual result. GDP per capita is removed from the historical estimation as well as the projection, so the age, relative-price and transitory coefficients are re-estimated; this is not the structural income equation with its GDP contribution mechanically set to zero."
))

doc <- body_add_par(doc, "Pre-COVID FY2018-19 anchor", style = "heading 2")
doc <- add_figure(doc, "10_historical_2019_gdp_sensitivity.png", "Figure 7. FY2018-19-anchored spending paths under the four real-GDP treatments.")
doc <- body_add_par(doc, paste0(
  "These paths begin from observed FY2018-19 non-interest fiscal expenditure of ",
  pct(central_2019_anchor),
  " of GDP and accumulate only the estimated slow-moving demographic, relative-price and real-income contributions from FY2019-20 onward. The FY2020-22 COVID interventions, transitory macro terms and residual innovations are not accumulated. Actual slow-driver movements from 2020 to 2025 are used, followed by the relevant projected GDP path."
))
doc <- body_add_par(doc, paste0(
  "The FY2018-19-anchored endpoints range from ",
  pct(endpoints[model == "central_gdp", historical_2019_anchored_primary_fiscal_2066_pp]),
  " under ", high_growth_label, " to ",
  pct(endpoints[model == "pressure_gdp", historical_2019_anchored_primary_fiscal_2066_pp]),
  " under ", low_growth_label, "; the ", no_gdp_label, " equation reaches ",
  pct(endpoints[model == "no_income", historical_2019_anchored_primary_fiscal_2066_pp]),
  ". This is an anchor sensitivity using the full-sample coefficients, not an equation estimated only on data available in 2019. A debt path is not attached to this case because the streamlined debt module begins from the official FY2024-25 debt stock."
))

doc <- body_add_par(doc, "Debt implications", style = "heading 1")
doc <- add_table(doc, endpoint_table, 7.5)
doc <- add_table(doc, anchor_comparison_table, 7.5)
doc <- add_figure(doc, "04_gdp_sensitivity_spending_and_debt.png", "Figure 8. PBO-anchored spending and debt under the four real-GDP treatments.", height = 6.6)
doc <- add_figure(doc, "08_gdp_pc_model_anchor_spending_and_debt.png", paste0("Figure 9. ", high_growth_label, " total-spending and net-debt paths under the PBO and latest-actual anchors."), height = 6.6)
doc <- add_figure(doc, "06_structural_driver_effects_on_debt.png", paste0("Figure 10. Mechanical contribution of each structural driver to net debt in FY2065-66 under ", high_growth_label, "."))
doc <- body_add_par(doc, paste0(
  "Under the common baseline revenue path, the ", high_growth_label, " case produces net debt of ",
  pct(central_debt$net_debt_2066_pp), " in FY2065-66 and PDI of ",
  pct(central_debt$public_debt_interest_2066_pp), ". Omitting GDP per capita instead produces net debt of ",
  pct(no_income_debt$net_debt_2066_pp), " and PDI of ",
  pct(no_income_debt$public_debt_interest_2066_pp), ". The debt result is therefore much more sensitive than the spending endpoint: persistent annual spending differences cumulate and then feed back through interest."
))
doc <- body_add_par(doc, paste0(
  "Negative net debt is an accounting scenario outcome, not a forecast that governments will accumulate assets indefinitely. In practice, governments would probably adjust taxes or spending. The debt module is best read as the fiscal adjustment implied if all other assumptions are mechanically held fixed."
))
doc <- body_add_par(doc, paste0(
  "For the latest-actual case, debt is recursively simulated from the official FY2024-25 stock rather than retaining the PBO debt path through FY2029-30. Under the same revenue, GDP-growth and interest-rate assumptions, that case reaches total expenditure of ",
  pct(central_actual_debt$total_fiscal_expenditure_2066_pp),
  ", PDI of ", pct(central_actual_debt$public_debt_interest_2066_pp),
  " and net debt of ", pct(central_actual_debt$net_debt_2066_pp),
  " in FY2065-66. Under the latest-actual no-GDP-per-capita model, total expenditure is ",
  pct(no_income_actual_debt$total_fiscal_expenditure_2066_pp),
  " and net debt is ", pct(no_income_actual_debt$net_debt_2066_pp),
  ". The higher primary-spending paths cumulate into debt and then feed back through interest, so these large divergences should be read as anchor sensitivities under no fiscal policy response, not as point debt forecasts."
))

doc <- body_add_par(doc, "Robustness and limitations", style = "heading 1")
doc <- add_table(doc, robustness_table, 8)
doc <- body_add_par(doc, paste0(
  "The ", high_growth_label, " endpoint is not fully robust to the estimation period. Starting in 1990 changes it by less than one percentage point, but estimating only through 2019 lowers it by about 2.6 points; excluding the FY2020-23 annual changes lowers it by about 2.8 points. The COVID level interventions prevent the pandemic observations from entering as unexplained permanent spending changes, but they do not eliminate the leverage those years exert on relationships among the other variables."
))
doc <- body_add_par(doc, paste0(
  "The decisive uncertainty is the aggregate income elasticity and the future real-income path. The structural model estimates the elasticity from Australian history; the no-income case is equivalent to assuming spending per person keeps pace one-for-one with real GDP per capita. Category-level age-cost and excess-cost-growth models remain more defensible for health, aged care, education and transfers."
))

doc <- body_add_par(doc, "Sources", style = "heading 1")
sources <- c(
  "Australian Treasury, 2023 Intergenerational Report, methodology and assumptions: https://treasury.gov.au/sites/default/files/2023-08/p2023-435150.pdf",
  "Guillemette and Turner (2017), The fiscal projection framework in long-term scenarios, OECD Working Paper 1440: https://doi.org/10.1787/8eddfa18-en",
  "European Commission (2023), 2024 Ageing Report: Underlying assumptions and projection methodologies: https://economy-finance.ec.europa.eu/system/files/2023-11/ip257_en_1.pdf",
  "OECD (2024), Fiscal Sustainability of Health Systems, long-term projection model: https://www.oecd.org/en/publications/fiscal-sustainability-of-health-systems_880f3195-en/full-report/component-6.html",
  "Lamartina and Zaghini (2011), Increasing Public Expenditure: Wagner's Law in OECD Countries: https://doi.org/10.1111/j.1468-0475.2010.00517.x",
  "Brückner, Chong and Gradstein (2012), Estimating the permanent income elasticity of government expenditures: https://doi.org/10.1016/j.jpubeco.2012.07.002",
  "Sam, McNown and Goh (2019), An augmented ARDL bounds test for cointegration: https://doi.org/10.1016/j.econmod.2018.11.001",
  "Parliamentary Budget Office, medium-term fiscal outlook and debt framework: https://www.pbo.gov.au/publications-and-data/publications/2025-26-Medium-term-budget-outlook"
)
for (source_text in sources) doc <- body_add_par(doc, source_text)

output_path <- file.path(documentation_dir, "streamlined_structural_top_down_model.docx")
print(doc, target = output_path)
message("Streamlined model-selection note written to ", output_path)
