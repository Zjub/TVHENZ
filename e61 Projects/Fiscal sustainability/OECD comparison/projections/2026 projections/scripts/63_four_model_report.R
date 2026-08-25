source(file.path("scripts", "00_config.R"))
suppressPackageStartupMessages({library(officer); library(flextable); library(data.table)})

assessment <- fread(file.path(table_dir, "model_selection_assessment.csv"))
bounds <- fread(file.path(table_dir, "ecm_bounds_tests.csv"))
adjustment <- fread(file.path(table_dir, "ecm_adjustment_sensitivity.csv"))
spec_tests <- fread(file.path(table_dir, "ecm_specification_tests.csv"))
integration <- fread(file.path(table_dir, "ecm_integration_assessment.csv"))
breaks <- fread(file.path(table_dir, "ecm_break_diagnostic.csv"))
bottom <- fread(file.path(table_dir, "bottom_up_method_comparison.csv"))

fmt <- function(x, d = 2) sprintf(paste0("%.", d, "f"), x)
assessment_display <- assessment[, .(
  Model = model_label,
  `In-sample RMSE (pp)` = fmt(in_sample_rmse_pp),
  `Average rolling RMSE (pp)` = fmt(average_rolling_rmse_pp),
  `1-year RMSE (pp)` = fmt(rolling_rmse_1y_pp),
  `5-year RMSE (pp)` = fmt(rolling_rmse_5y_pp),
  `2065-66 PBO-anchored (% GDP)` = fmt(pbo_anchored_endpoint),
  `Max window shift (pp)` = fmt(maximum_window_shift_pp)
)]
bounds_display <- bounds[, .(
  Test = test, Statistic = fmt(statistic, 3),
  `5% lower` = fmt(lower_bound_5pct, 3), `5% upper` = fmt(upper_bound_5pct, 3),
  `p-value` = fmt(fifelse(is.finite(exact_p_value), exact_p_value, standard_p_value), 3),
  Conclusion = conclusion
)]
bottom_endpoint <- bottom[year == projection_end & scenario == "central", data.table(
  Approach = c("Fixed demographic-rate baseline", "Data-enhanced sensitivity"),
  `Primary spending (% GDP)` = fmt(c(baseline_ratio, data_enhanced_ratio) * 100)
)]

add_table <- function(doc, x) {
  ft <- flextable(as.data.frame(x)) |> theme_booktabs() |> bg(part = "header", bg = "#D9EAF7") |>
    bold(part = "header") |> fontsize(size = 8, part = "all") |> autofit() |> fit_to_width(max_width = 6.5)
  body_add_flextable(doc, ft)
}
add_figure <- function(doc, filename, caption) {
  path <- file.path(documentation_dir, "figures", filename)
  if (file.exists(path)) {
    doc <- body_add_img(doc, src = path, width = 6.5, height = 4.0)
    doc <- body_add_par(doc, caption, style = "Image Caption")
  }
  doc
}

fstat <- bounds[test == "Bounds F: all lagged levels", statistic]
fp <- bounds[test == "Bounds F: all lagged levels", exact_p_value]
tstat <- bounds[test == "Bounds t: lagged dependent level", statistic]
tp <- bounds[test == "Bounds t: lagged dependent level", exact_p_value]
indf <- bounds[test == "Augmented ARDL F: lagged independent levels", statistic]
indp <- bounds[test == "Augmented ARDL F: lagged independent levels", standard_p_value]

doc <- read_docx()
doc <- body_add_par(doc, "Australian government spending projections", style = "heading 1")
doc <- body_add_par(doc, "Top-down model selection and bottom-up comparison", style = "centered")
doc <- body_add_par(doc, "Internal peer-review draft | 24 August 2026")
doc <- body_add_par(doc, "Conclusion", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "The preferred top-down specification is the differenced ARIMAX. It is the best-supported compromise between economic content, rolling forecast performance and stability to the estimation window. ",
  "The ECM tracks history closely, but the required cointegrating relationship is not established: the exact bounds F statistic is ", fmt(fstat, 2), " (p=", fmt(fp, 3), ") and the bounds t statistic is ", fmt(tstat, 2), " (p=", fmt(tp, 3), "). Its adjustment coefficient is negative but insignificant. The ECM is therefore retained as a diagnostic sensitivity, not a valid central long-run model."
))

doc <- body_add_par(doc, "Data and common treatment", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "All top-down equations use annual Australian financial-year data from 1979-80 to 2024-25 (46 observations before lags). The dependent variable is primary fiscal expenditure: government final consumption, gross fixed capital formation and income payable, less conventional other interest, divided by nominal GDP. Imputed interest on unfunded superannuation liabilities remains in the aggregate. The age block is limited to the shares aged 0-14 and 65+. Aggregate population is excluded. Separate FY2020, FY2021 and FY2022 indicators absorb the exceptional pandemic observations in every specification."
))
doc <- body_add_par(doc, "The five specifications", style = "heading 1")
doc <- body_add_par(doc, "1. Structural OLS: a transparent old-Shapley-style levels regression using the two age shares, terms of trade, relative government prices, unemployment and COVID indicators.")
doc <- body_add_par(doc, "2. Levels ARIMAX: the same levels relationship with a stationary ARMA error process.")
doc <- body_add_par(doc, "3. Differenced ARIMAX: annual spending-share changes on changes in the two age shares and relative prices, current and lagged changes in unemployment and the terms of trade, plus COVID indicators and stationary ARMA errors.")
doc <- body_add_par(doc, "4. Hybrid: a slow structural level using age shares and relative prices, with unexplained annual changes modelled using unemployment and terms-of-trade changes.")
doc <- body_add_par(doc, "5. ECM: a parsimonious augmented-Mann long-run candidate in log spending/GDP, log real GDP per capita and the log relative government price. Unemployment and terms of trade enter only as short-run changes. BIC selects between one- and two-lag UECMs.")

doc <- body_add_par(doc, "Relative fit and projections", style = "heading 1")
doc <- add_table(doc, assessment_display)
doc <- body_add_par(doc, paste0(
  "The differenced ARIMAX has an average rolling RMSE of ",
  fmt(assessment[model == "arimax_diff", average_rolling_rmse_pp]), " percentage points and its pre-COVID endpoint differs from the full-sample estimate by only ",
  fmt(abs(fread(file.path(table_dir, "model_estimation_window_sensitivity.csv"))[model == "arimax_diff" & estimation_window == "Estimated through 2019", endpoint_difference_from_full_sample_pp])), " points. The levels ARIMAX, hybrid and structural OLS perform materially worse at five years and are much more sensitive to excluding the pandemic-era observations."
))
doc <- add_figure(doc, "five_model_rolling_rmse.png", "Rolling conditional forecast RMSE by horizon.")
doc <- add_figure(doc, "five_model_paths_model_only.png", "Model-only projections from the latest National Accounts actual.")
doc <- add_figure(doc, "five_model_paths_pbo_anchor.png", "PBO expenses less public debt interest plus net capital investment through 2029-30, followed by model changes.")

doc <- body_add_par(doc, "Cointegration assessment", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "The ECM follows the ARDL bounds-testing literature. All three long-run variables pass the combined first-difference ADF/KPSS screen with no I(2) warning. The exact finite-sample Case III bounds tests do not reject no cointegration. The augmented-ARDL independent-level F statistic is ", fmt(indf, 2), " (p=", fmt(indp, 3), "), so a degenerate-regressor case is not ruled out. The adjustment estimate is ", fmt(adjustment$estimate, 3), " (p=", fmt(adjustment$p_value, 3), "). Residual, RESET and CUSUM tests do not reject at 5%, but these conditional diagnostics cannot substitute for cointegration. A one-break levels diagnostic selects ", breaks$break_year, ", reinforcing caution about treating the full sample as one stable equilibrium."
))
doc <- add_table(doc, bounds_display)

doc <- body_add_par(doc, "Literature background", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "Time-series studies of Wagner's law commonly use either a Peacock-Wiseman formulation in log real spending and log real output or a Mann/Musgrave formulation in the log spending share and log real income per person. Akitoby et al. (2006) show why short-run dynamics and a long-run fiscal-output relation should be separated. Arpaia and Turrini (2008) similarly estimate government expenditure dynamics with error-correction methods. For a spending-share projection, the Mann form is the cleaner match to the dependent variable. Relative government prices are included because public services are labour intensive and their price can move differently from the GDP deflator."
))
doc <- body_add_par(doc, paste0(
  "Pesaran, Shin and Smith (2001) allow a mixture of I(0) and I(1) regressors but no I(2) variables. Sam, McNown and Goh's augmented ARDL procedure adds tests of the lagged dependent and independent levels to guard against degenerate cases. Australian evidence is mixed and regime-sensitive: published studies find weak or period-dependent support for Wagner-type relationships. That makes formal cointegration and stability evidence a prerequisite for using the ECM, rather than assuming a long-run relationship from a good in-sample fit."
))
doc <- body_add_par(doc, "References", style = "heading 2")
refs <- c(
  "Akitoby et al. (2006), Public Spending, Voracity, and Wagner's Law in Developing Countries: https://www.imf.org/external/pubs/ft/wp/2004/wp04202.pdf",
  "Arpaia and Turrini (2008), Government expenditure and economic growth in the EU: https://www.bancaditalia.it/pubblicazioni/altri-atti-convegni/2007-fiscal-policy/Arpaia_Turrini.pdf?language_id=1",
  "Pesaran, Shin and Smith (2001), Bounds testing approaches: https://onlinelibrary.wiley.com/doi/pdf/10.1002/jae.616",
  "Sam, McNown and Goh (2019), augmented ARDL bounds test: https://www.sciencedirect.com/science/article/pii/S0264999318307843",
  "Australian Wagner-law evidence: https://www.tandfonline.com/doi/full/10.1080/00036846.2016.1203063",
  "Relative prices and government spending: https://journals.sagepub.com/doi/10.1177/1091142103031003002"
)
for (ref in refs) doc <- body_add_par(doc, paste0("- ", ref))

doc <- body_add_par(doc, "Bottom-up comparison", style = "heading 1")
doc <- body_add_par(doc, "The fixed demographic-rate model remains the baseline. Defence is exogenous; health, education and social protection use relevant demographic exposure, and health also includes a transparent excess-cost-growth assumption. The data-enhanced sensitivity adds public relative-price evidence and a shrunk historical intensity residual. The enhanced version is a sensitivity rather than a replacement because category histories are short and affected by classification and pandemic disruptions.")
if (nrow(bottom_endpoint)) doc <- add_table(doc, bottom_endpoint)
doc <- body_add_par(doc, "The five top-down specifications and two bottom-up paths should not be averaged mechanically. The top-down equations now project primary fiscal expenditure; debt interest is added separately in the debt module. The preferred differenced ARIMAX supplies the empirical top-down path; OLS supports attribution; levels ARIMAX and the hybrid show sensitivity to level dynamics; the ECM tests, but currently fails to validate, a long-run income relationship; and the bottom-up models explain functional spending pressures.")

print(doc, target = file.path(documentation_dir, "model_assessment_and_bottom_up_development.docx"))
message("Five-model Word assessment written.")
