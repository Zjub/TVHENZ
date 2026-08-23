source(file.path("scripts", "00_config.R"))

suppressPackageStartupMessages({
  library(officer)
  library(flextable)
})

model_selection <- fread(file.path(table_dir, "model_selection_assessment.csv"))
ecm_validation <- fread(file.path(table_dir, "ecm_validation_summary.csv"))
category_assumptions <- fread(file.path(processed_dir, "bottom_up_category_assumptions.csv"))
gfs_purpose <- fread(file.path(processed_dir, "historical_gfs_expenses_by_purpose.csv"))
bottom_categories <- fread(file.path(table_dir, "bottom_up_category_projections.csv"))
bottom_totals <- fread(file.path(table_dir, "bottom_up_total_projections.csv"))

pp <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), x)
pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), 100 * x)
fy <- function(year) paste0(year - 1L, "-", substr(year, 3, 4))

add_heading <- function(doc, text, level = 1L) {
  body_add_par(doc, text, style = paste0("heading ", level))
}

add_text <- function(doc, text, style = "Normal") {
  tryCatch(
    body_add_par(doc, text, style = style),
    error = function(e) {
      stop("Unable to add paragraph beginning: ", substr(text, 1L, 120L), "\n", conditionMessage(e), call. = FALSE)
    }
  )
}

add_bullets <- function(doc, items) {
  for (item in items) doc <- body_add_par(doc, paste0("- ", item), style = "Normal")
  doc
}

add_numbered <- function(doc, items) {
  for (i in seq_along(items)) {
    doc <- body_add_par(doc, paste0(i, ". ", items[[i]]), style = "Normal")
  }
  doc
}

add_table <- function(doc, x, font_size = 8, max_width = 6.4) {
  ft <- flextable(as.data.frame(x))
  ft <- theme_booktabs(ft)
  ft <- bg(ft, part = "header", bg = "#D9EAF7")
  ft <- bold(ft, part = "header")
  ft <- fontsize(ft, size = font_size, part = "all")
  ft <- valign(ft, valign = "top", part = "all")
  ft <- autofit(ft)
  ft <- fit_to_width(ft, max_width = max_width)
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

add_source <- function(doc, number, title, url, use) {
  body_add_fpar(
    doc,
    fpar(
      ftext(paste0(number, ". "), prop = fp_text(bold = TRUE)),
      hyperlink_ftext(title, href = url),
      ftext(paste0(" - ", use))
    )
  )
}

model_role <- function(model) {
  fcase(
    model == "dynamic_diff", "Preferred driver-based top-down cross-check",
    model == "univariate_arima", "Preferred near-term statistical benchmark",
    model == "ardl_ecm", "Experimental sensitivity only",
    model == "arimax_diff", "Difference-model robustness check",
    model == "arimax_level", "Levels/dynamics robustness check",
    model == "structural_ols", "Transparent attribution benchmark",
    model == "hybrid", "Structural/macro robustness check",
    default = "Comparator"
  )
}

model_order <- c(
  "dynamic_diff", "univariate_arima", "arimax_diff", "arimax_level",
  "structural_ols", "hybrid", "ardl_ecm"
)
topdown_table <- copy(model_selection)
topdown_table[, role := model_role(model)]
topdown_table[, order := match(model, model_order)]
setorder(topdown_table, order)
topdown_display <- topdown_table[, .(
  Model = model_label,
  `1-year rolling RMSE (pp)` = pp(rolling_rmse_1y_pp),
  `5-year rolling RMSE (pp)` = pp(rolling_rmse_5y_pp),
  `Official-period MAE (pp)` = pp(official_period_mae_pp),
  `Maximum endpoint window shift (pp)` = pp(maximum_endpoint_window_shift_pp),
  Role = role
)]

ecm_display <- ecm_validation[, .(
  Criterion = criterion,
  Result = result,
  Assessment = assessment
)]

current_category_methods <- data.table(
  category = c(
    "General public services excl interest", "Defence", "Public order and safety",
    "Economic affairs", "Environmental protection", "Housing and community amenities",
    "Health", "Recreation, culture and religion", "Education", "Social protection",
    "Transport"
  ),
  current_method = c(
    "Total population; economy-wide unit-cost growth; no central excess cost",
    "Exogenous linear transition to a scenario target share of GDP",
    "Total population; economy-wide unit-cost growth; 0.25% central excess cost",
    "Total population; economy-wide unit-cost growth; no central excess cost",
    "Total population; economy-wide unit-cost growth; no central excess cost",
    "Total population; economy-wide unit-cost growth; no central excess cost",
    "Fixed age weights; economy-wide unit-cost growth; 0.75% central excess cost",
    "Total population; economy-wide unit-cost growth; no central excess cost",
    "Fixed age weights; economy-wide unit-cost growth; 0.25% central excess cost",
    "Fixed age weights; economy-wide unit-cost growth; 0.25% central excess cost",
    "Total population; economy-wide unit-cost growth; no central excess cost"
  ),
  estimated_time_series = "No"
)

recommended_methods <- data.table(
  category = c(
    "Defence",
    "Health",
    "Education",
    "Social protection",
    "General public services",
    "Public order and safety",
    "Transport and economic affairs",
    "Housing and environmental protection",
    "Recreation and culture",
    "Interest"
  ),
  recommended_treatment = c(
    "Exogenous policy scenario",
    "Estimated demographic-volume-price model",
    "Enrolment-volume-price model with policy overlays",
    "Program/rules-based model split by payment or service",
    "Parsimonious population/activity model",
    "Workload-volume-price model where data permit",
    "Exogenous project pipeline plus mean-reverting recurrent component",
    "Scenario/program model; avoid forcing smooth demographic elasticities",
    "Simple population-volume-price model or stable share benchmark",
    "Endogenous debt recursion"
  ),
  principal_drivers = c(
    "Published policy commitments and target share of GDP; scenario timing",
    "Age/service utilisation, population, health input prices, non-demographic intensity",
    "School-age population, enrolments/EFTSL, participation, education labour costs",
    "Eligibility, recipient counts, legislated indexation, unemployment and ageing",
    "Population, public-administration wages and government consumption prices",
    "Population, police/corrections/court workloads and public-sector wages",
    "Committed capital plans, maintenance, population and construction/input prices",
    "Housing caseloads, environmental programs, grants and explicit policy scenarios",
    "Population and relevant public-sector/service prices",
    "Opening debt, nominal GDP growth and effective interest rate"
  )
)

base_category <- bottom_categories[year == 2025 & scenario == "central", .(
  base_share_gdp = sum(share_gdp)
), by = category]
current_category_methods <- merge(
  current_category_methods,
  base_category,
  by = "category",
  all.x = TRUE
)
current_category_display <- current_category_methods[, .(
  Category = category,
  `2024-25 calibrated share of GDP` = pct(base_share_gdp),
  `Current projection treatment` = current_method,
  `Historically estimated?` = estimated_time_series
)]

central_endpoint <- bottom_totals[scenario == "central" & year == projection_end]
gfs_start <- min(gfs_purpose$year)
gfs_end <- max(gfs_purpose$year)
gfs_years <- uniqueN(gfs_purpose$year)

source_items <- list(
  list(
    "ABS Government Finance Statistics, Annual",
    "https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/latest-release",
    "COFOG-A spending levels, economic-type expenses and historical vintages."
  ),
  list(
    "ABS Government Finance Statistics archive",
    "https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual",
    "Earlier releases back to the 1990s, subject to classification-break reconciliation."
  ),
  list(
    "ABS Wage Price Index",
    "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/wage-price-index-australia/latest-release",
    "Public-sector and industry wage-price series for labour-intensive services."
  ),
  list(
    "ABS Australian System of National Accounts",
    "https://www.abs.gov.au/statistics/economy/national-accounts/australian-system-national-accounts/latest-release",
    "Government consumption prices, GDP deflators and government consumption by purpose."
  ),
  list(
    "AIHW Health Expenditure Database",
    "https://www.aihw.gov.au/about-our-data/our-data-collections/health-expenditure-database",
    "Long-run health expenditure by funder and area, with health price measures."
  ),
  list(
    "Productivity Commission Report on Government Services",
    "https://www.pc.gov.au/ongoing/report-on-government-services/",
    "Service expenditure, activity, utilisation and workload data for health, education, justice, aged care and other services."
  ),
  list(
    "Department of Education student data",
    "https://www.education.gov.au/higher-education-statistics/student-data",
    "Annual enrolments, equivalent full-time student load and completions."
  ),
  list(
    "Treasury 2023 Intergenerational Report",
    "https://treasury.gov.au/sites/default/files/2023-08/p2023-435150.pdf",
    "Established Australian long-run projection methods and policy assumptions."
  )
)

doc <- read_docx()
doc <- body_add_par(doc, "Government spending projection model assessment", style = "heading 1")
doc <- body_add_par(
  doc,
  "Preferred top-down model and a development path for the bottom-up framework",
  style = "centered"
)
doc <- body_add_par(doc, paste("Prepared", format(Sys.Date(), "%d %B %Y")), style = "Normal")
doc <- body_add_par(
  doc,
  "Australian general-government spending; financial years are labelled by their June-ending year.",
  style = "Normal"
)

doc <- add_heading(doc, "Executive conclusion")
doc <- add_bullets(doc, c(
  "Use the published consolidated PBO forecast during the official forecast period.",
  "Use the bottom-up purpose model as the central long-run fiscal-sustainability framework, but treat its current calibrated category paths as scenarios rather than estimated forecasts.",
  "Use the dynamic-difference model as the preferred driver-based top-down cross-check. It has the best five-year rolling performance among models with economic or demographic drivers and the lowest tested endpoint sensitivity to the estimation window.",
  "Use the ARIMA with COVID controls as the near-term statistical benchmark only. Its forecast errors are smallest, but it cannot respond to ageing, relative prices, policy or service demand.",
  "Do not select the current ECM as the preferred top-down model. Its finite-sample bounds tests do not support the fitted long-run relationship.",
  "Defence should remain exogenous. Health, education and much of social spending should be rebuilt as service-specific exposure-volume-price models, with historical estimation of residual intensity where sufficient data exist."
))

doc <- add_heading(doc, "1. Preferred top-down model")
doc <- add_text(doc, paste0(
  "No one equation is best for every use. The ARIMA with COVID controls has the lowest rolling RMSE, but it is deliberately driver-free outside the pandemic interventions. For long-horizon policy assessment, the preferred top-down equation is therefore the dynamic-difference specification: it retains demographic, relative-price, unemployment and terms-of-trade dynamics with separate FY2020-22 indicators, without imposing an unsupported long-run cointegrating relationship. It should be interpreted as a cross-check around the central bottom-up projection, not as a forty-year point forecast."
))
doc <- add_table(doc, topdown_display, font_size = 7)
doc <- add_figure(
  doc,
  "outputs/figures/forecast_checks/01_rmse_by_horizon.png",
  "Figure 1. Rolling pseudo-out-of-sample RMSE by horizon."
)
doc <- add_text(doc, paste0(
  "The dynamic-difference model's rolling RMSE is ",
  pp(model_selection[model == "dynamic_diff", rolling_rmse_1y_pp]),
  " percentage points at one year and ",
  pp(model_selection[model == "dynamic_diff", rolling_rmse_5y_pp]),
  " percentage points at five years. Its maximum long-run endpoint shift across the tested estimation windows is ",
  pp(model_selection[model == "dynamic_diff", maximum_endpoint_window_shift_pp]),
  " percentage points. Difference-model innovation uncertainty nevertheless accumulates over long horizons, so scenario and model spread remain essential."
))

doc <- add_heading(doc, "2. Why the ECM is not preferred")
doc <- add_text(doc, "A negative error-correction coefficient is not by itself evidence of cointegration. The model must establish an admissible integration order and a statistically supported lagged-level relationship, while also passing residual, functional-form and stability checks.")
doc <- add_table(doc, ecm_display, font_size = 7)
doc <- add_text(doc, "The fitted ECM is dynamically stable and does not fail the recursive CUSUM test. However, both fitted-equation bounds statistics fall in the region that does not support a level relationship; the canonical ARDL robustness test is inconclusive; residual serial correlation and RESET tests reject at 5 per cent; and the long-run age-share regressors are highly collinear. The ECM should remain in the output set as an experimental sensitivity, not as the principal projection model.")

doc <- body_add_break(doc)
doc <- add_heading(doc, "3. What the bottom-up model currently does")
doc <- add_text(doc, paste0(
  "The current bottom-up model begins with each COFOG-A purpose category's ",
  fy(2025), " ABS GFS expense level. General public services is reduced by interest, because interest is projected separately. The stored GFS purpose panel covers ",
  fy(gfs_start), " to ", fy(gfs_end), " (", gfs_years,
  " annual observations), but those historical observations are not used to estimate category equations: only the latest level is used as the starting point."
))
doc <- add_text(doc, "For every category other than defence, nominal spending evolves mechanically as:")
doc <- add_text(doc, "Spending[c,t] = Spending[c,t-1] * exposure growth[c,t] * unit-cost growth[c,t].")
doc <- add_text(doc, "Exposure is either total population or a fixed weighted combination of age groups. Unit-cost growth equals inflation plus an economy-wide productivity proxy plus a calibrated category excess-cost assumption. Defence instead transitions to a scenario share of GDP. The category total is scaled to the official primary-expense forecast through 2029-30, after which the final common scale factor is held fixed and category growth determines the path.")
doc <- add_table(doc, current_category_display, font_size = 6.8)
doc <- add_figure(
  doc,
  "outputs/figures/bottom_up/03_category_contributions.png",
  "Figure 2. Contribution of purpose categories to the change in central bottom-up spending."
)
doc <- add_text(doc, paste0(
  "Under these assumptions, central primary spending reaches ",
  pct(central_endpoint$primary_expense_ratio), " of GDP in ", fy(projection_end),
  ". This is a conditional scenario result. It is not presently the output of category-level time-series estimation."
))

doc <- add_heading(doc, "4. What should be exogenous and what should be estimated")
doc <- add_text(doc, "The appropriate split depends on whether spending is mainly governed by explicit policy, service demand, statutory eligibility, input costs or a lumpy capital program. A single time-series treatment across all purposes would be less defensible than a modular approach.")
doc <- add_table(doc, recommended_methods, font_size = 6.8)
doc <- add_text(doc, "Defence is correctly handled as an exogenous policy path. Transport, housing, environmental and economic-affairs spending also contain large discretionary or project-based components, so published capital programs and explicit scenarios should dominate rather than a smooth demographic regression. Health, education and service components of social protection are the strongest candidates for empirical exposure-volume-price models. Cash transfers should instead follow recipient counts, eligibility rules and legislated indexation.")

doc <- add_heading(doc, "5. Recommended category model architecture")
doc <- add_text(doc, "For a service category c, decompose nominal spending into exposure, service intensity and input prices:")
doc <- add_text(doc, "Spending[c,t] = Exposure[c,t] * Service intensity[c,t] * Input price[c,t].")
doc <- add_text(doc, "Estimate historical non-demographic intensity only after removing observed exposure and price growth:")
doc <- add_text(doc, "Residual intensity growth[c,t] = change in log(Spending[c,t] / Input price[c,t]) - change in log(Exposure[c,t]).")
doc <- add_text(doc, "A parsimonious time-series equation can then model residual intensity growth using an intercept or damped trend, one autoregressive term, relevant policy or labour-market variables, and intervention dummies for COVID or classification breaks. Forecasts reconstruct spending from projected exposure, projected sector prices and the estimated residual-intensity path. This makes the role of demographics and prices explicit and prevents their effects from being hidden in an arbitrary excess-cost parameter.")

doc <- add_heading(doc, "6. Health example")
doc <- add_numbered(doc, c(
  "Split health where possible into hospitals, primary/community care, pharmaceuticals and other health. Their utilisation and price drivers differ.",
  "Construct exposure from projected population by age multiplied by observed age-specific utilisation or expenditure weights. Replace the current fixed illustrative weights with weights estimated from AIHW, Medicare/PBS or Report on Government Services data.",
  "Construct a health input-price index. Prefer the AIHW health price index and a transparent labour/non-labour blend using the Health Care and Social Assistance Wage Price Index. Use CPI health only as a sensitivity because it principally measures consumer-facing prices, not the full cost of government production.",
  "Deflate nominal health spending, divide by the needs-weighted population, and estimate the residual non-demographic intensity trend. Use shrinkage or a simple damped trend because structural changes and a short sample make large ARDL systems unreliable.",
  "Project exposure and input prices separately, then apply central, high and low residual-intensity assumptions informed by the estimation interval and policy judgement."
))

doc <- add_heading(doc, "7. Education and social protection")
doc <- add_text(doc, "Education should use actual or projected enrolments rather than age shares alone. School enrolments, vocational students and higher-education equivalent full-time student load should be projected separately, with participation-rate scenarios. Labour costs can use Education and Training WPI, supplemented by the government-consumption deflator. Funding reforms, indexation rules and capped grants should enter as explicit policy overlays.")
doc <- add_text(doc, "Social protection should be disaggregated before modelling. Age pension, unemployment payments, family payments, disability/NDIS, aged-care services and other transfers have different eligibility populations and indexation rules. Recipient counts and legislated CPI/wage indexation are generally more defensible than a single age-weighted time series. A residual take-up or cost-per-recipient equation can be estimated within each sufficiently long program series.")

doc <- add_heading(doc, "8. Data limitation and estimation strategy")
doc <- add_text(doc, paste0(
  "The immediate constraint is that the current consistent GFS purpose file contains only ",
  gfs_years, " annual observations. That is too short for separate multi-regressor time-series models across eleven categories. The ABS archive provides older releases, but classification and accounting changes must be reconciled before joining them. Service-specific sources often provide longer or richer panels and should be preferred for health, education, justice and community services."
))
doc <- add_bullets(doc, c(
  "Start with identities and transparent policy rules; estimate only the residual component that is not already explained mechanically.",
  "Use one or two regressors and at most one autoregressive term per category unless a materially longer series is assembled.",
  "Where category histories remain short, use pooled or hierarchical estimation across related services, or calibrate the central residual trend and report wide scenarios rather than presenting unstable coefficients as evidence.",
  "Use rolling-origin tests, pre-COVID estimation, classification-break dummies and leave-one-period-out sensitivity for every estimated category.",
  "Keep the official forecast anchor, but preserve unscaled category-only paths so that model error is visible rather than absorbed by the common scale factor."
))

doc <- add_heading(doc, "9. Practical implementation sequence")
doc <- add_numbered(doc, c(
  "Create a category-driver registry specifying whether each category is policy-driven, volume-driven, price-driven or mixed, together with its official data source and refresh date.",
  "Extend and reconcile historical purpose spending using archived GFS releases; record every classification bridge and retain break flags.",
  "Build annual public input-price series: government consumption deflator, public-sector WPI, health/education industry WPI and the AIHW health price index. Do not substitute household CPI subgroups without documenting the conceptual mismatch.",
  "Build observed service-volume series: health utilisation, enrolments/EFTSL, aged-care places, NDIS participants, payment recipients and justice workloads.",
  "Estimate parsimonious residual-intensity equations and compare them with a no-residual-growth benchmark, damped historical trend and explicit high/low scenarios.",
  "Replace the current fixed excess-cost parameter only where an estimated and validated residual-intensity model is credible. Retain calibrated scenarios elsewhere.",
  "Aggregate categories, anchor to the official forecast, rerun debt arithmetic and publish contribution, sensitivity and rolling-validation tables."
))

doc <- add_heading(doc, "10. Public data sources")
for (i in seq_along(source_items)) {
  item <- source_items[[i]]
  doc <- add_source(doc, i, item[[1]], item[[2]], item[[3]])
}

doc <- add_heading(doc, "11. Recommended decision")
doc <- add_text(doc, "For the current model vintage, retain the bottom-up purpose framework as the central long-run assessment because its assumptions and fiscal mechanisms are auditable. Label the category projections as calibrated scenarios. Use dynamic differences as the preferred top-down structural cross-check and the ARIMA with COVID controls as the near-term statistical benchmark. Do not promote the current ECM to preferred status.")
doc <- add_text(doc, "The highest-value next development is a health module that combines needs-weighted population, AIHW/RoGS service data and a health input-price index, followed by enrolment-based education and program-based social-protection modules. This improves the largest and most demographically exposed categories before investing effort in small or inherently discretionary purposes.")

note_path <- file.path(documentation_dir, "model_assessment_and_bottom_up_development.docx")
print(doc, target = note_path)
message("Model assessment and bottom-up development note written to: ", note_path)
