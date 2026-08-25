source(file.path("scripts", "00_config.R"))
check_packages(c("officer", "flextable", "data.table"))
suppressPackageStartupMessages({library(officer); library(flextable); library(data.table)})

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
comparison <- fread(file.path(table_dir, "interest_treatment_model_comparison.csv"))
coefficients <- fread(file.path(table_dir, "interest_treatment_coefficients.csv"))
debt <- fread(file.path(table_dir, "debt_paths_top_down.csv"))
bounds <- fread(file.path(table_dir, "ecm_bounds_tests.csv"))
adjustment <- fread(file.path(table_dir, "ecm_adjustment_sensitivity.csv"))

fmt <- function(x, d = 2) sprintf(paste0("%.", d, "f"), x)
add_table <- function(doc, x, size = 8) {
  ft <- flextable(as.data.frame(x)) |> theme_booktabs() |>
    bg(part = "header", bg = "#D9EAF7") |> bold(part = "header") |>
    fontsize(size = size, part = "all") |> valign(valign = "top", part = "all") |>
    autofit() |> fit_to_width(max_width = 6.5)
  body_add_flextable(doc, ft)
}
add_figure <- function(doc, filename, caption, height = 4.5) {
  path <- file.path(documentation_dir, "figures", filename)
  if (file.exists(path)) {
    doc <- body_add_img(doc, src = path, width = 6.5, height = height)
    doc <- body_add_par(doc, caption, style = "Image Caption")
  }
  doc
}

treatment_table <- data.table(
  `Configuration value` = c(
    "exclude_other_interest", "exclude_total_interest", "include_interest"
  ),
  `Modelled National Accounts aggregate` = c(
    "GFCE + GFCF + total income payable - conventional other interest",
    "GFCE + GFCF + total income payable - total interest payable",
    "GFCE + GFCF + total income payable"
  ),
  Interpretation = c(
    "Preferred PBO-aligned non-PDI concept; retains imputed unfunded-superannuation interest",
    "Strict non-interest concept; requires a separate superannuation-interest treatment for a complete total",
    "Previous treatment; debt interest remains embedded in the regression"
  )
)

history_display <- historical[year %in% c(1980L, 1990L, 2000L, 2010L, 2019L, 2025L), .(
  Year = year,
  `Including interest` = fmt(100 * broad_expenditure_including_interest_gdp),
  `Other interest` = fmt(100 * interest_payable_other_gdp),
  `Unfunded-super interest` = fmt(100 * interest_payable_unfunded_super_gdp),
  `Excluding other interest` = fmt(100 * broad_expenditure_excluding_other_interest_gdp),
  `Excluding total interest` = fmt(100 * broad_expenditure_excluding_total_interest_gdp)
)]

fit_wide <- dcast(
  comparison[interest_treatment %in% c("include_interest", "exclude_other_interest")],
  model + model_label ~ interest_treatment,
  value.var = c("in_sample_rmse_pp", "average_rolling_rmse_pp", "rolling_rmse_5y_pp")
)
fit_display <- fit_wide[, .(
  Model = model_label,
  `In-sample RMSE: included` = fmt(in_sample_rmse_pp_include_interest),
  `In-sample RMSE: excluded` = fmt(in_sample_rmse_pp_exclude_other_interest),
  `Average rolling: included` = fmt(average_rolling_rmse_pp_include_interest),
  `Average rolling: excluded` = fmt(average_rolling_rmse_pp_exclude_other_interest),
  `5-year rolling: included` = fmt(rolling_rmse_5y_pp_include_interest),
  `5-year rolling: excluded` = fmt(rolling_rmse_5y_pp_exclude_other_interest)
)]
setorder(fit_display, Model)

old_endpoint <- comparison[interest_treatment == "include_interest", .(
  model, previous_total_endpoint = `Matching PBO anchor`
)]
primary_endpoint <- comparison[interest_treatment == "exclude_other_interest", .(
  model, model_label, primary_endpoint = `Matching PBO anchor`
)]
total_endpoint <- debt[year == projection_end, .(
  model, interest_endpoint = interest_ratio * 100,
  revised_total_endpoint = total_expense_ratio * 100,
  net_debt_endpoint = net_debt_ratio * 100,
  gross_debt_endpoint = gross_debt_ratio * 100
)]
projection_display <- Reduce(
  function(x, y) merge(x, y, by = "model"),
  list(primary_endpoint, old_endpoint, total_endpoint)
)[, .(
  Model = model_label,
  `Previous total` = fmt(previous_total_endpoint),
  `Revised primary fiscal spending` = fmt(primary_endpoint),
  `Revised PDI` = fmt(interest_endpoint),
  `Revised total` = fmt(revised_total_endpoint),
  `Change in total` = fmt(revised_total_endpoint - previous_total_endpoint)
)]
setorder(projection_display, Model)

age_coefficients <- dcast(
  coefficients[
    interest_treatment %in% c("include_interest", "exclude_other_interest") &
      term %in% c("age_0_14", "age_65p", "d_age_0_14", "d_age_65p")
  ],
  component + term ~ interest_treatment,
  value.var = "estimate"
)
age_display <- age_coefficients[, .(
  Model = fifelse(component == "hybrid_structural_ols", "Hybrid structural block",
                  unname(model_labels[component])),
  Term = term,
  `Interest included` = fmt(include_interest, 3),
  `Other interest excluded` = fmt(exclude_other_interest, 3)
)]

f_exact <- bounds[test == "Bounds F: all lagged levels", exact_p_value]
t_exact <- bounds[test == "Bounds t: lagged dependent level", exact_p_value]

doc <- read_docx()
doc <- body_add_par(doc, "Interest treatment in the top-down spending models", style = "heading 1")
doc <- body_add_par(doc, "ABS extraction, model switch and re-estimation results", style = "centered")
doc <- body_add_par(doc, "Internal methods note | 25 August 2026")

doc <- body_add_par(doc, "Conclusion", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "The preferred top-down dependent variable now excludes conventional debt interest. ",
  "This prevents the spending regressions from trying to explain movements generated by debt stocks and borrowing costs. ",
  "The configuration is set to `exclude_other_interest`, while strict total-interest exclusion and the previous include-interest treatment remain available."
))
doc <- body_add_par(doc, paste0(
  "Removing conventional interest improves every model's in-sample fit and improves five-year rolling RMSE for all five models. ",
  "Average rolling RMSE improves for the structural OLS, levels ARIMAX and hybrid, is almost unchanged for the ECM, and deteriorates slightly for the differenced ARIMAX. ",
  "Among models that do not require cointegration, the differenced ARIMAX still has the lowest average rolling RMSE. The model-selection conclusion therefore does not change."
))

doc <- body_add_par(doc, "ABS data and construction", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "The workflow already downloads ABS catalogue 5206.0 using `readabs`. Table 17, General Government Income Account, reports total income payable and three separate interest lines: conventional other interest, interest on unfunded superannuation liabilities, and total interest. ",
  "The quarterly series start in September 1959 and cover the complete annual estimation sample. They are converted to financial-year totals using the same seasonally adjusted treatment as the existing total-income-payable series."
))
doc <- body_add_par(doc, paste0(
  "The downloaded components satisfy: total interest = other interest + unfunded-superannuation interest, subject only to ABS rounding. ",
  "All three expenditure outcomes and all three interest components are retained in the processed National Accounts and top-down model datasets, so changing the configuration does not require another download."
))
doc <- add_table(doc, history_display)
doc <- add_figure(doc, "interest_treatment_historical_comparison.png",
                  "Historical National Accounts spending under the three available interest treatments.", 3.9)

doc <- body_add_par(doc, "The configuration switch", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "Set `topdown_interest_treatment` near the start of scripts/00_config.R. The selected outcome is applied consistently to estimation, rolling validation, diagnostics, Shapley attribution, historical graphs and PBO anchoring."
))
doc <- add_table(doc, treatment_table, 7.4)
doc <- body_add_par(doc, paste0(
  "The preferred option subtracts `Property income payable - Interest - Other interest`. This is the closest National Accounts analogue to PBO public debt interest. ",
  "It leaves the separately measured imputed interest on unfunded superannuation liabilities in the residual spending concept because that item is not generated by ordinary public debt and is not included in PBO net debt."
))

doc <- body_add_par(doc, "PBO anchor and debt arithmetic", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "Under the preferred treatment, the near-term anchor is PBO expenses minus public debt interest plus net capital investment. ",
  "This is a primary fiscal-expenditure concept: it includes capital investment but excludes conventional debt interest. After FY2029-30, modelled PDI is added once to obtain total fiscal expenditure."
))
doc <- body_add_par(doc, paste0(
  "Published PBO net debt, gross debt and PDI are used through FY2029-30. Thereafter, PDI is calculated from the preceding gross-debt stock and a gradually converging effective interest rate. ",
  "Net debt evolves with the fiscal balance. The official FY2029-30 gap between gross and net debt is held constant as a share of GDP as a transparent financial-asset assumption. ",
  "This also removes the previous risk of adding net capital investment twice: it is already contained in the top-down fiscal-expenditure target."
))

doc <- body_add_par(doc, "Effect on model fit", style = "heading 1")
doc <- add_table(doc, fit_display, 7.1)
doc <- add_figure(doc, "interest_treatment_rolling_fit.png",
                  "Rolling conditional forecast RMSE under all three interest treatments.", 4.8)
doc <- body_add_par(doc, paste0(
  "The fall in in-sample RMSE is not by itself decisive because the non-interest outcome is less volatile. The more informative result is the rolling comparison. ",
  "For the differenced ARIMAX, average rolling RMSE moves from 3.49 to 3.58 percentage points, although its five-year RMSE is essentially unchanged at 4.97 rather than 5.00. ",
  "The levels ARIMAX, hybrid and structural OLS improve at five years by approximately 0.76, 0.53 and 0.75 percentage points respectively."
))

doc <- body_add_par(doc, "Effect on demographic coefficients", style = "heading 1")
doc <- add_table(doc, age_display)
doc <- body_add_par(doc, paste0(
  "Removing conventional interest substantially reduces the magnitude of the age coefficients. The older-age coefficient remains positive in the structural OLS, levels ARIMAX and hybrid structural block. ",
  "In the differenced ARIMAX it remains negative, but moves from -0.687 to -0.471. Interest contamination was therefore part of the instability, but it does not fully resolve the weak annual-change relationship between ageing and aggregate spending."
))

doc <- body_add_par(doc, "Effect on projections", style = "heading 1")
doc <- add_table(doc, projection_display, 7.2)
doc <- body_add_par(doc, paste0(
  "The primary endpoints are the direct outputs of the re-estimated models. Revised total expenditure adds the model-specific debt-interest path. ",
  "For models that eventually eliminate gross debt under the common revenue assumptions, PDI reaches zero; this is a result of the fiscal/debt scenario rather than an assumption that interest rates become zero."
))
doc <- add_figure(doc, "interest_treatment_projection_changes.png",
                  "Post-anchor model dynamics under each treatment; expressing changes from FY2029-30 removes the concept-level gap.", 4.8)
doc <- add_figure(doc, "interest_treatment_total_expenditure_comparison.png",
                  "Previous interest-embedded total paths compared with primary spending plus endogenous PDI.", 4.8)

doc <- body_add_par(doc, "ECM assessment", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "The ECM adjustment coefficient becomes more negative after conventional interest is removed: ",
  fmt(adjustment$estimate, 3), " with p=", fmt(adjustment$p_value, 3), ". This is an improvement over the former estimate, but it remains statistically insignificant. ",
  "The exact bounds F-test has p=", fmt(f_exact, 3), " and the bounds t-test has p=", fmt(t_exact, 3), ". Cointegration is still not established, so the ECM remains a diagnostic sensitivity rather than a valid central long-run equation."
))

doc <- body_add_par(doc, "Interpretation", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "The change is economically important but does not overturn the forecast ranking. It removes a misspecified component from every regression, strengthens the level-model fits and makes the debt feedback explicit. ",
  "The differenced ARIMAX remains the best rolling forecaster among specifications not dependent on an unsupported cointegrating relationship, while the levels models remain useful sensitivities."
))
doc <- body_add_par(doc, paste0(
  "Projection differences should be interpreted carefully. An endpoint from the preferred regression is primary fiscal expenditure, whereas the former endpoint included interest. Only the revised-total column is directly comparable with the former total-spending projection."
))

doc <- body_add_par(doc, "Sources", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "Australian Bureau of Statistics, Australian National Accounts: National Income, Expenditure and Product, Table 17, General Government Income Account: ",
  "https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-national-income-expenditure-and-product/latest-release"
))
doc <- body_add_par(doc, paste0(
  "ABS, Australian System of National Accounts - property income and unfunded-superannuation interest: ",
  "https://www.abs.gov.au/statistics/detailed-methodology-information/concepts-sources-methods/australian-system-national-accounts-concepts-sources-and-methods/edition-8/chapter-13-income-account/property-income"
))
doc <- body_add_par(doc, paste0(
  "Parliamentary Budget Office, 2026-27 National Fiscal Outlook and Data Explorer: ",
  "https://www.pbo.gov.au/publications-and-data/publications/2026-27-National-Fiscal-Outlook"
))

output_path <- file.path(documentation_dir, "top_down_interest_treatment_explainer.docx")
print(doc, target = output_path)
message("Top-down interest-treatment explainer written to ", output_path)
