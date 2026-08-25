source(file.path("scripts", "00_config.R"))
check_packages(c("officer", "flextable", "data.table"))
suppressPackageStartupMessages({
  library(officer)
  library(flextable)
  library(data.table)
})

category_long <- fread(file.path(table_dir, "age_profile_category_weights.csv"))
effective <- fread(file.path(table_dir, "age_profile_effective_age_weights.csv"))
profile_index <- fread(file.path(table_dir, "age_expenditure_profile_index.csv"))
comparison <- fread(file.path(table_dir, "age_profile_model_comparison.csv"))
coefficients <- fread(file.path(table_dir, "age_profile_model_coefficients.csv"))

fmt <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), x)
age_labels <- c(
  "0_14" = "0-14", "15_34" = "15-34", "35_54" = "35-54",
  "55_64" = "55-64", "65p" = "65+"
)

add_table <- function(doc, x, font_size = 8) {
  ft <- flextable(as.data.frame(x)) |>
    theme_booktabs() |>
    bg(part = "header", bg = "#D9EAF7") |>
    bold(part = "header") |>
    fontsize(size = font_size, part = "all") |>
    autofit() |>
    fit_to_width(max_width = 6.5)
  body_add_flextable(doc, ft)
}

add_figure <- function(doc, filename, caption, height = 4.0) {
  path <- file.path(documentation_dir, "figures", filename)
  if (file.exists(path)) {
    doc <- body_add_img(doc, src = path, width = 6.5, height = height)
    doc <- body_add_par(doc, caption, style = "Image Caption")
  }
  doc
}

add_source <- function(doc, number, title, url, use) {
  doc <- body_add_par(doc, paste0(number, ". ", title), style = "heading 3")
  doc <- body_add_par(doc, use)
  body_add_par(doc, url)
}

category_weights <- unique(category_long[, .(
  Category = category,
  `FY2024-25 expenditure ($m)` = round(base_expenditure),
  `Index weight (%)` = fmt(100 * category_expenditure_weight, 1)
)])
setorder(category_weights, -`FY2024-25 expenditure ($m)`)

age_profiles <- dcast(
  category_long,
  category ~ age_group,
  value.var = "relative_weight"
)
setnames(age_profiles, "category", "Category")
for (column in names(age_labels)) {
  if (column %in% names(age_profiles)) setnames(age_profiles, column, age_labels[[column]])
}
setorder(age_profiles, Category)

effective_display <- effective[, .(
  `Age group` = age_labels[age_group],
  `Effective relative weight` = fmt(effective_relative_weight, 2),
  `2025 population share (%)` = fmt(100 * base_population_share, 1),
  `2025 index contribution (%)` = fmt(100 * base_index_contribution, 1)
)]

index_display <- profile_index[year %in% c(1980L, 2000L, 2025L, 2030L, 2040L, 2050L, 2066L), .(
  Year = year,
  Period = period,
  `Profile index (2025=1)` = fmt(index, 3),
  `Change from 2025 (%)` = fmt(100 * (index - 1), 1)
)]

comparison_display <- comparison[model != "ardl_ecm", .(
  Model = model_label,
  Demographics = demographic_label,
  `Average rolling RMSE (pp)` = fmt(average_rolling_rmse_pp, 2),
  `5-year RMSE (pp)` = fmt(rolling_rmse_5y_pp, 2),
  `2065-66, PBO anchored (% GDP)` = fmt(`Common PBO anchor`, 2)
)]
setorder(comparison_display, Model, Demographics)

profile_coefficients <- coefficients[demographic_specification == "expenditure_profile", .(
  Model = fifelse(component == "hybrid_structural", "Hybrid structural component",
                  model_labels[component]),
  Term = term,
  Estimate = fmt(estimate, 3)
)]

audit <- data.table(
  Input = c(
    "Category expenditure shares", "Population age shares", "Projected age shares",
    "Within-category age relativities", "Flat profile for other categories",
    "Fixed 2025 category mix"
  ),
  `Current basis` = c(
    "ABS GFS expenditure by purpose, FY2024-25, excluding interest from general public services",
    "Historical population data used by the existing model",
    "Centre for Population 2025 Population Statement",
    "Grouped proxy assumptions already used in the bottom-up model",
    "Assumption that defence and remaining categories have no systematic lifecycle gradient",
    "Laspeyres-type modelling choice"
  ),
  Assessment = c(
    "Observed evidence", "Observed evidence", "Official projection",
    "Judgemental; direction is literature-consistent but magnitudes are not administrative estimates",
    "Judgemental", "Transparent simplification"
  )
)

doc <- read_docx()
doc <- body_add_par(doc, "Age-expenditure profile option", style = "heading 1")
doc <- body_add_par(doc, "Construction, evidence audit and top-down sensitivity", style = "centered")
doc <- body_add_par(doc, "Internal methods note | 25 August 2026")

doc <- body_add_par(doc, "Recommendation", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "Keep the separate shares aged 0-14 and 65+ as the baseline demographic treatment in the top-down time-series models. ",
  "Use the age-expenditure profile index as a documented sensitivity and as a bridge to the bottom-up model. ",
  "This is closest to normal research practice: aggregate time-series regressions generally use age shares or dependency ratios, while age-cost profiles are widely used in direct fiscal projection exercises. ",
  "The profile option should not become the preferred top-down treatment until its current proxy relativities are replaced with Australian administrative age-cost profiles."
))

doc <- body_add_par(doc, "What the index measures", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "The index compresses the full age distribution into one measure of demographic spending pressure. ",
  "For category k and age group a, let omega_k be the category's FY2024-25 expenditure share, r_ka its relative per-person age cost, and s_at the population share. ",
  "The implemented index is A_t = sum_k omega_k [sum_a r_ka s_at / sum_a r_ka s_a,2025]. ",
  "It is normalised to one in 2025, holds the category mix fixed, and changes only when population composition changes. ",
  "It therefore represents an age-composition pressure, not total spending growth: wages, technology, policy, eligibility and excess cost growth remain elsewhere in the model."
))
doc <- body_add_par(doc, paste0(
  "The expenditure base is general-government spending classified by primary purpose. This is an approximate match to the broader top-down dependent variable, which also contains investment and income payable. ",
  "The index excludes interest from the category weights because interest does not have a meaningful beneficiary age profile. Defence is retained with a flat age profile, so it affects the aggregate expenditure weights but not demographic change."
))

doc <- body_add_par(doc, "Where the weights come from", style = "heading 1")
doc <- body_add_par(doc, "Category weights", style = "heading 2")
doc <- body_add_par(doc, "The outer weights are observed FY2024-25 ABS Government Finance Statistics expenditure shares. They identify how much each purpose contributes to the aggregate index.")
doc <- add_table(doc, category_weights)
doc <- body_add_par(doc, "Within-category lifecycle weights", style = "heading 2")
doc <- body_add_par(doc, paste0(
  "The inner weights describe relative exposure by age. Health rises strongly at older ages; education is concentrated among children and young adults; social protection is U-shaped but largest for older people; all other purposes are flat. ",
  "These numerical relativities are transparent grouped proxies, not estimates recovered from Australian administrative microdata. Multiplying them by category expenditure shares produces the effective aggregate weights shown below."
))
doc <- add_table(doc, age_profiles)
doc <- add_table(doc, effective_display)
doc <- add_figure(doc, "age_expenditure_effective_weights.png", "Effective lifecycle weights after combining purpose shares and within-purpose age profiles.", 3.8)

doc <- body_add_par(doc, "Evidence versus judgement", style = "heading 1")
doc <- add_table(doc, audit, 7.5)
doc <- body_add_par(doc, paste0(
  "The construction is therefore partly evidence-based and partly conjectural. The expenditure shares and population paths are observed or official. ",
  "The most influential uncertainty is the assumed within-category age-cost curve. The resulting effective weights - 1.20 for ages 0-14, 0.83 for 15-34, 0.66 for 35-54, 0.85 for 55-64 and 1.68 for 65+ - should be read as a transparent calibration. ",
  "They should not be described as measured Australian incidence weights."
))

doc <- body_add_par(doc, "Implied demographic pressure", style = "heading 1")
doc <- add_table(doc, index_display)
doc <- body_add_par(doc, paste0(
  "On the current population projections, the index rises from 1.000 in 2025 to ",
  fmt(profile_index[year == 2066L, index], 3), " in 2066. Holding everything else fixed, the calibration therefore implies a ",
  fmt(100 * (profile_index[year == 2066L, index] - 1), 1), "% rise in age-weighted primary-spending exposure per person over the projection period."
))
doc <- add_figure(doc, "age_expenditure_profile_index.png", "Historical and projected age-expenditure profile index, normalised to 2025.", 3.8)

doc <- body_add_par(doc, "Effect on the top-down estimates", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "Each model was re-estimated with the profile index replacing the separate youth and older-age shares; all non-demographic variables, COVID indicators, samples and forecast assumptions were held fixed. ",
  "The ECM is unchanged because its validated candidate equation deliberately contains no demographic term."
))
doc <- add_table(doc, comparison_display, 7.5)
doc <- add_figure(doc, "age_profile_projection_comparison.png", "Matched PBO-anchored projections using separate age shares or the profile index.", 4.8)
doc <- add_figure(doc, "age_profile_rolling_comparison.png", "Rolling conditional forecast errors under the two demographic treatments.", 4.4)
doc <- body_add_par(doc, paste0(
  "The profile materially improves rolling fit for the levels ARIMAX and structural OLS and produces a smaller improvement for the hybrid and differenced ARIMAX. ",
  "For the differenced ARIMAX, average rolling RMSE falls from ",
  fmt(comparison[model == "arimax_diff" & demographic_specification == "age_shares", average_rolling_rmse_pp]), " to ",
  fmt(comparison[model == "arimax_diff" & demographic_specification == "expenditure_profile", average_rolling_rmse_pp]), " percentage points, and the PBO-anchored 2065-66 endpoint rises from ",
  fmt(comparison[model == "arimax_diff" & demographic_specification == "age_shares", `Common PBO anchor`]), "% to ",
  fmt(comparison[model == "arimax_diff" & demographic_specification == "expenditure_profile", `Common PBO anchor`]), "% of GDP."
))
doc <- body_add_par(doc, paste0(
  "The coefficient evidence remains mixed. The profile coefficient is positive in the structural OLS, levels ARIMAX and hybrid structural block, but negative in the differenced ARIMAX. ",
  "That negative change coefficient means the profile option does not resolve the weak identification of ageing effects in annual aggregate changes. Better fit alone is not enough to prefer a constructed index whose internal weights are partly imposed."
))
doc <- add_table(doc, profile_coefficients)

doc <- body_add_par(doc, "How this relates to the literature", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "There are genuine empirical applications of age-expenditure profiles. Australian Treasury Intergenerational Reports multiply age-specific spending rates by projected populations for health, education and payments; health profiles draw on administrative program data and may distinguish decedents from non-decedents. ",
  "The NSW Intergenerational Report uses static and dynamic age-cost indices. The Productivity Commission has embedded age-gender health and education expenditure profiles in a computable general equilibrium model. The European Commission's Ageing Report applies age- and sex-specific per-capita expenditure profiles to population projections."
))
doc <- body_add_par(doc, paste0(
  "Those applications normally use profiles as direct accounting inputs or within structural simulation models. In aggregate single-country time-series equations, researchers more commonly include elderly and youth shares, dependency ratios or the working-age share. ",
  "The two methods answer different questions: shares allow the historical data to estimate separate aggregate associations, whereas a profile index imposes the lifecycle shape before estimation. ",
  "For this project, age shares are therefore the more defensible headline top-down specification; the profile is a useful sensitivity and the natural framework for the bottom-up exercise."
))

doc <- body_add_par(doc, "What would make the profile evidence-based", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "Replace the proxy r_ka values with Australian per-person profiles derived from administrative or survey sources: Medicare and AIHW health spending by age and sex; Department of Education enrolments and expenditure per student; and DSS payment-recipient and outlay data by age and payment type. ",
  "Map those profiles to the GFS purpose categories, document the reference year and population denominator, and test static versus gradually shifting health profiles. ",
  "Until that work is complete, retain the option as a sensitivity and report the full weight audit alongside any result."
))

doc <- body_add_par(doc, "Sources", style = "heading 1")
doc <- add_source(doc, 1, "ABS Government Finance Statistics, Annual, 2024-25",
                  "https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/2024-25",
                  "Observed expenditure-by-purpose weights used in the index.")
doc <- add_source(doc, 2, "Centre for Population, 2025 Population Statement",
                  "https://population.gov.au/publications/statements/2025-population-statement",
                  "Official projected age structure used by the model.")
doc <- add_source(doc, 3, "Australian Treasury, 2007 Intergenerational Report - detailed methodology",
                  "https://treasury.gov.au/publication/intergenerational-report-2007-2/appendix-c-detailed-methodology-and-assumptions",
                  "Australian application of age-specific spending rates to projected populations, including health and education methods.")
doc <- add_source(doc, 4, "Australian Treasury, 2023 Intergenerational Report",
                  "https://treasury.gov.au/sites/default/files/2023-08/p2023-435150.pdf",
                  "Recent Australian application and source discussion for health age-cost profiles.")
doc <- add_source(doc, 5, "NSW Treasury, 2021 Intergenerational Report technical note",
                  "https://www.treasury.nsw.gov.au/sites/default/files/2021-06/2021%20NSW%20IGR%20-%20Technical%20Note%20and%20Sensitivity%20Tables.pdf",
                  "Application of static and dynamic age-cost indices.")
doc <- add_source(doc, 6, "Productivity Commission, Migrant Intake into Australia - technical supplement",
                  "https://assets.pc.gov.au/inquiries/completed/migrant-intake/draft/migrant-intake-draft-supplementa.pdf",
                  "Age-gender expenditure profiles embedded in an Australian economy-wide model.")
doc <- add_source(doc, 7, "European Commission, 2024 Ageing Report methodology",
                  "https://economy-finance.ec.europa.eu/system/files/2023-11/ip257_en_1.pdf",
                  "Official projection method combining age-sex spending profiles with population projections.")
doc <- add_source(doc, 8, "New Zealand Treasury Working Paper 03/15",
                  "https://www.treasury.govt.nz/publications/wp/modelling-effect-population-ageing-government-social-expenditures-wp-03-15",
                  "Discussion of age-specific expenditure projections and age-share treatments in aggregate models.")

output_path <- file.path(documentation_dir, "age_expenditure_profile_option.docx")
print(doc, target = output_path)
message("Age-expenditure profile note written to ", output_path)
