source(file.path("scripts", "00_config.R"))
check_packages(c("officer", "flextable"))

suppressPackageStartupMessages({
  library(officer)
  library(flextable)
})

comparison <- fread(file.path(table_dir, "bottom_up_method_comparison.csv"))
category_comparison <- fread(file.path(table_dir, "bottom_up_method_category_comparison.csv"))
calibration <- fread(file.path(table_dir, "bottom_up_data_enhanced_calibration.csv"))
wpi <- fread(file.path(processed_dir, "bottom_up_external_price_drivers.csv"))

fy <- function(year) paste0(year - 1L, "-", substr(year, 3, 4))
pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), 100 * x)
pp <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), x)

add_text <- function(doc, text, style = "Normal") body_add_par(doc, text, style = style)
add_heading <- function(doc, text, level = 1L) body_add_par(doc, text, style = paste0("heading ", level))
add_bullets <- function(doc, items) {
  for (item in items) doc <- body_add_par(doc, paste0("- ", item), style = "Normal")
  doc
}
add_table <- function(doc, x, font_size = 8, width = 6.4) {
  ft <- flextable(as.data.frame(x))
  ft <- theme_booktabs(ft)
  ft <- bg(ft, part = "header", bg = "#D9EAF7")
  ft <- bold(ft, part = "header")
  ft <- fontsize(ft, size = font_size, part = "all")
  ft <- valign(ft, valign = "top", part = "all")
  ft <- autofit(ft)
  ft <- fit_to_width(ft, max_width = width)
  body_add_flextable(doc, ft)
}
add_figure <- function(doc, filename, caption, height = 4.0) {
  path <- file.path(figure_dir, "bottom_up_comparison", filename)
  if (file.exists(path)) {
    doc <- body_add_img(doc, src = path, width = 6.4, height = height)
    doc <- body_add_par(doc, caption, style = "Image Caption")
  }
  doc
}
add_source <- function(doc, number, title, url, description) {
  body_add_fpar(doc, fpar(
    ftext(paste0(number, ". "), prop = fp_text(bold = TRUE)),
    hyperlink_ftext(title, href = url), ftext(paste0(" - ", description))
  ))
}

years_show <- c(2029L, 2040L, 2050L, projection_end)
central <- comparison[scenario == "central" & year %in% years_show]
central_display <- central[, .(
  `Financial year` = fy(year),
  `Baseline (% GDP)` = pct(baseline_ratio),
  `Data-enhanced (% GDP)` = pct(data_enhanced_ratio),
  `Difference (pp)` = pp(difference_pp)
)]

endpoint <- category_comparison[year == projection_end & scenario == "central"]
setorder(endpoint, -difference_pp)
endpoint_display <- endpoint[, .(
  Category = category,
  `Baseline (% GDP)` = pct(baseline_share_gdp),
  `Data-enhanced (% GDP)` = pct(data_enhanced_share_gdp),
  `Difference (pp)` = pp(difference_pp)
)]

calibration_display <- calibration[, .(
  Category = category,
  `Price series` = gsub("public_|_wpi", "", price_driver),
  `Non-COVID changes` = observations,
  `Historical price premium (%)` = pp(100 * historical_relative_price_premium),
  `Historical residual intensity (%)` = pp(100 * historical_residual_intensity_growth),
  `Baseline excess (%)` = pp(100 * baseline_central_excess),
  `Enhanced excess (%)` = pp(100 * enhanced_central_excess)
)]

end <- comparison[scenario == "central" & year == projection_end]
wpi_start <- min(wpi$year)
wpi_end <- max(wpi$year)

doc <- read_docx()
doc <- body_add_par(doc, "Bottom-up government spending projections", style = "heading 1")
doc <- body_add_par(doc, "Baseline and data-enhanced estimates", style = "centered")
doc <- body_add_par(doc, paste("Prepared", format(Sys.Date(), "%d %B %Y")), style = "Normal")

doc <- add_heading(doc, "Executive summary")
doc <- add_bullets(doc, c(
  "Retain the existing fixed-demographic-rate model as the baseline. It is transparent, stable and appropriately modest given the available category history.",
  "Use the new data-enhanced model as a sensitivity, not yet as the central forecast. It adds official public-sector wage-price evidence and estimates the remaining non-demographic spending intensity from the short GFS panel.",
  paste0("In the central scenario, the data-enhanced estimate is ", pct(end$data_enhanced_ratio), " of GDP in ", fy(projection_end), ", compared with ", pct(end$baseline_ratio), " in the baseline: a difference of ", pp(end$difference_pp), " percentage points."),
  "The small aggregate difference conceals material offsets across purposes. Health and education are higher in the enhanced model, while social protection and economic affairs are lower.",
  "Defence remains exogenous in both versions and transitions to the scenario target share of GDP. This is preferable to extrapolating a short historical time series for a policy-determined category."
))

doc <- add_heading(doc, "1. What has been added")
doc <- add_text(doc, "The baseline is unchanged. It starts from 2024-25 ABS Government Finance Statistics purpose spending, uses total population or fixed age-weighted exposure rates, applies economy-wide inflation and productivity growth plus a calibrated category excess-cost rate, and anchors aggregate primary spending to the PBO forecast through 2029-30.")
doc <- add_text(doc, paste0(
  "The second script downloads ABS Wage Price Index financial-year series for the public sector. The available online series cover ", fy(wpi_start), " to ", fy(wpi_end),
  " and distinguish health care and social assistance, education and training, public administration and safety, and all public industries. Each spending category is mapped to the closest available public-sector price series."
))
doc <- add_text(doc, "For each non-defence category, historical nominal spending growth is decomposed into demographic exposure growth, relative public-sector wage-price growth, and residual service intensity. The residual captures utilisation, policy, scope, productivity measurement, non-labour prices and any remaining classification effects; it should not be read as a pure causal demand parameter.")

doc <- add_heading(doc, "2. What excess-cost growth means")
doc <- add_text(doc, "Excess-cost growth is the additional annual growth in nominal spending per unit of demographic exposure, over and above the common economy-wide unit-cost allowance. It is not the total growth rate of a category and it is not simply inflation.")
doc <- add_text(doc, "In the baseline model, the calculation for a non-defence category is:")
doc <- add_text(doc, "Spending growth = demographic-exposure growth + inflation + economy-wide real income/productivity growth + category excess-cost growth (with the exact calculation applied multiplicatively).")
doc <- add_text(doc, "For example, the baseline health assumption of 0.75 per cent means that, after allowing for growth in the needs-weighted population and the common inflation/productivity rate, health spending per exposure unit grows by a further 0.75 per cent each year. Because it compounds, this assumption affects long-run spending materially even though the annual number is small.")
doc <- add_text(doc, "Conceptually, excess-cost growth can capture sector wages rising faster than general costs, changes in service intensity or utilisation, new treatments and technology, quality improvements, policy expansions, and any residual measurement or classification effects. It should therefore be interpreted as a composite projection parameter, not as a directly observed price index or a causal coefficient.")
doc <- add_text(doc, "The baseline central values are modelling judgements recorded in scripts/03_assumptions.R: 0.75 per cent for health; 0.25 per cent for education, public order and safety, and social protection; and zero for the other non-defence categories. These particular numbers are not estimates published by ABS, Treasury, AIHW or PBO. They are deliberately simple scenario calibrations. Pressure and restraint values are likewise scenario assumptions. Defence has no excess-cost parameter because it follows an explicit target share of GDP.")
doc <- add_text(doc, "The conceptual approach is nevertheless consistent with Australian long-run fiscal projection practice. Treasury's intergenerational-report methodology separates demographic effects from growth in real spending per person and fits trends to the remaining component. AIHW similarly distinguishes current-price health spending, volume changes and health-sector prices. Those references support the decomposition, but they should not be presented as the source of the baseline numerical rates.")

doc <- add_heading(doc, "3. Where the enhanced numbers come from")
doc <- add_text(doc, "For each category and usable year, the enhanced model calculates:")
doc <- add_text(doc, "Observed excess over common cost = nominal category-spending growth - demographic-exposure growth - all-public-sector WPI growth.")
doc <- add_text(doc, "It also decomposes that result into (a) the relevant industry WPI growth relative to all-public-sector WPI and (b) residual service-intensity growth after removing demographic exposure and the industry WPI. The category mappings use ABS series A2705248W for public health care and social assistance, A2705238T for public education and training, A2705258A for public administration and safety, and A2705270T for all public industries.")
doc <- add_text(doc, "The spending numerator is the ABS GFS purpose series for 2015-16 to 2024-25. Exposure is total population or the baseline's fixed age-weighted population measure. The reported historical estimate is the median of six annual changes after excluding changes ending in 2019-20, 2020-21 and 2021-22. The median is capped at plus or minus 2 percentage points, receives a 35 per cent weight, and is combined with a 65 per cent weight on the baseline value. Half of the resulting departure from baseline is then damped away by 2039-40. The 35 per cent weight, cap and damping rate are conservative e61 modelling choices rather than externally estimated parameters.")

doc <- add_heading(doc, "4. Conservative calibration and resulting rates")
doc <- add_text(doc, "Only ten consistent annual GFS purpose observations are currently stored. The calibration therefore excludes the 2019-20 to 2021-22 changes, uses the median of the remaining six annual changes, caps the empirical excess-cost signal at plus or minus 2 percentage points, and places only 35 per cent weight on it. The remaining 65 per cent stays on the baseline assumption. Half of the resulting adjustment is damped away by 2039-40.")
doc <- add_table(doc, calibration_display, font_size = 6.5)
doc <- add_figure(doc, "04_excess_cost_calibration.png", "Figure 1. Baseline and evidence-weighted annual excess-cost assumptions.", 4.2)

doc <- add_heading(doc, "5. Comparison of aggregate estimates")
doc <- add_table(doc, central_display, font_size = 8)
doc <- add_figure(doc, "01_central_paths.png", "Figure 2. Central baseline and data-enhanced primary-spending projections.", 4.0)
doc <- add_figure(doc, "02_all_scenarios.png", "Figure 3. Baseline and data-enhanced projections under all three scenarios.", 5.7)

doc <- add_heading(doc, "6. Which categories explain the difference")
doc <- add_table(doc, endpoint_display, font_size = 7)
doc <- add_figure(doc, "03_endpoint_category_differences.png", paste0("Figure 4. Category differences in ", fy(projection_end), "."), 4.4)
doc <- add_text(doc, "The enhanced result is not uniformly higher. The historical decomposition raises the long-run excess-cost calibration for health, education, public order, transport, environmental protection and recreation. It lowers the calibration for social protection, economic affairs, housing and general public services. These estimates are sensitive to the short period used and may partly reflect temporary programs or classification changes.")

doc <- add_heading(doc, "7. Interpretation and preferred use")
doc <- add_text(doc, "The baseline remains the preferred central bottom-up model for this vintage. Fixed demographic rates are a credible first-order way to incorporate ageing when detailed service-use data are unavailable, and their transparency is an advantage. The enhanced model provides a disciplined test of how observed sector wage prices and recent category spending trends would change the result, but the sample is too short to justify treating its empirical residuals as structural long-run forecasts.")
doc <- add_text(doc, "For fiscal-risk analysis, report the baseline central path alongside both enhanced and baseline pressure/restraint scenarios. A difference between the two methods should be described as model uncertainty, not a confidence interval.")

doc <- add_heading(doc, "8. Next improvements")
doc <- add_bullets(doc, c(
  "Health: replace illustrative age weights with age-specific utilisation or expenditure weights; combine AIHW/RoGS activity with the health WPI and a non-labour health price component.",
  "Education: replace age shares with projected school enrolments, VET students and higher-education EFTSL; retain education WPI and explicit funding-policy overlays.",
  "Social protection: split cash transfers, aged care, disability/NDIS and other services; project recipient counts and legislated indexation separately.",
  "Public order: add police, court, prison and emergency-service workload measures from Report on Government Services.",
  "Capital-heavy purposes: use published project pipelines and construction prices rather than treating all spending as a smooth recurrent service.",
  "Historical depth: reconcile archived GFS releases and flag classification breaks before increasing the statistical weight on estimated residual trends."
))

doc <- add_heading(doc, "9. References and data provenance")
doc <- add_source(doc, 1, "ABS Wage Price Index, Australia", "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/wage-price-index-australia/latest-release", "online public-sector and industry wage-price series used by the enhanced script.")
doc <- add_source(doc, 2, "ABS Government Finance Statistics, Annual", "https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/latest-release", "purpose spending levels used for the baseline and historical decomposition.")
doc <- add_source(doc, 3, "Centre for Population 2025 Population Statement", "https://population.gov.au/publications/statements/2025-population-statement", "population and age-structure projections used in both versions.")
doc <- add_source(doc, 4, "Treasury 2007 Intergenerational Report - detailed methodology and assumptions", "https://treasury.gov.au/publication/intergenerational-report-2007-2/appendix-c-detailed-methodology-and-assumptions", "Australian projection precedent for separating population/age effects from trends in real spending per person.")
doc <- add_source(doc, 5, "Treasury 2023 Intergenerational Report", "https://treasury.gov.au/sites/default/files/2023-08/p2023-435150.pdf", "current Australian long-run fiscal projection context and discussion of demographic and non-demographic spending pressures.")
doc <- add_source(doc, 6, "AIHW Health Expenditure Database", "https://www.aihw.gov.au/about-our-data/our-data-collections/health-expenditure-database", "health expenditure and price evidence; recommended for replacing the provisional health residual with a richer health-specific module.")
doc <- add_source(doc, 7, "Productivity Commission Report on Government Services", "https://www.pc.gov.au/ongoing/report-on-government-services/", "recommended service-volume and workload data for the next model stage.")
doc <- add_text(doc, "Reproducibility note: the baseline parameters are stored in data/processed/bottom_up_category_assumptions.csv; downloaded WPI observations and ABS series identifiers are stored in data/processed/bottom_up_external_price_drivers.csv; annual decomposition observations are stored in outputs/tables/bottom_up_data_enhanced_historical_decomposition.csv; and final calibration calculations are stored in outputs/tables/bottom_up_data_enhanced_calibration.csv.")

report_path <- file.path(documentation_dir, "bottom_up_baseline_vs_data_enhanced.docx")
print(doc, target = report_path)
message("Bottom-up comparison report written to: ", report_path)
