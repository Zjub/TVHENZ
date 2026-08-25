source(file.path("R", "helpers.R"))

project_dir <- find_project_root()
setwd(project_dir)

raw_dir <- file.path(project_dir, "data", "raw")
processed_dir <- file.path(project_dir, "data", "processed")
table_dir <- file.path(project_dir, "outputs", "tables")
model_dir <- file.path(project_dir, "outputs", "models")
figure_dir <- file.path(project_dir, "outputs", "figures")
documentation_dir <- file.path(project_dir, "documentation")

make_dirs(c(
  raw_dir, processed_dir, table_dir, model_dir, documentation_dir,
  file.path(figure_dir, "data"), file.path(figure_dir, "bottom_up"),
  file.path(figure_dir, "top_down"), file.path(figure_dir, "forecast_checks"),
  file.path(figure_dir, "revenue_debt"), file.path(figure_dir, "model_comparison"),
  file.path(figure_dir, "diagnostics"), file.path(figure_dir, "bottom_up_comparison")
))

projection_start <- 2026L
projection_end <- 2066L
official_forecast_end <- 2030L
estimation_start <- 1980L
forecast_check_start <- 2010L
forecast_check_end <- 2020L
forecast_check_horizon <- 5L

# Top-down specification options. The limited demographic block avoids asking
# a short annual sample to identify four strongly collinear population shares.
topdown_age_specification <- "limited"
topdown_age_groups <- switch(
  topdown_age_specification,
  limited = c("0_14", "65p"),
  full = c("0_14", "15_34", "55_64", "65p"),
  stop("Unknown topdown_age_specification: ", topdown_age_specification)
)
topdown_covid_years <- 2020:2022

# The reported baseline estimates separate youth and older-population shares.
# `expenditure_profile` is an optional sensitivity that collapses the full age
# distribution using category expenditure shares and lifecycle cost weights.
topdown_demographic_specification <- "age_shares"
topdown_demographic_options <- c("age_shares", "expenditure_profile")

# Interest treatment for the aggregate top-down dependent variable. The
# baseline removes conventional debt interest but retains the separately
# identified imputed interest on unfunded superannuation liabilities. All
# three variants remain available for matched sensitivity testing.
topdown_interest_treatment <- "exclude_other_interest"
topdown_interest_treatment_options <- c(
  "exclude_total_interest", "exclude_other_interest", "include_interest"
)

# Aggregate population is excluded from the spending-share equations. Real GDP
# per capita enters only the dedicated cointegration/ECM specification.
topdown_scale_drivers <- character()

refresh_data <- tolower(Sys.getenv("REFRESH_DATA", "false")) %in% c("true", "1", "yes")

source_urls <- list(
  gfs_annual = "https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/2024-25/55120DO021_202425.xlsx",
  population_projection = "https://population.gov.au/sites/population.gov.au/files/2026-01/2025-population-statement-age-sex-structure.xlsx",
  pbo_nfo = "https://www.pbo.gov.au/sites/default/files/2026-08/PBO%202026-27%20National%20Fiscal%20Outlook%20-%20Data.xlsx",
  budget_nominal_gdp = "https://budget.gov.au/content/bp1/download/bp1_s2-data-nominal-GDP.xlsx",
  budget_receipts_cash = "https://budget.gov.au/content/bp1/download/bp1_s5-online_t1.csv",
  budget_receipts_gdp = "https://budget.gov.au/content/bp1/download/bp1_s5-online_t2.csv",
  labour_force_table1 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/may-2026/62020001.xlsx"
)

model_labels <- c(
  structural_ols = "Structural OLS (levels)",
  arimax_level = "ARIMAX (levels)",
  arimax_diff = "Differenced ARIMAX",
  hybrid = "Hybrid structural/macro",
  ardl_ecm = "ARDL error-correction model"
)

palette_models <- c(
  "Structural OLS (levels)" = "#0072B2",
  "ARIMAX (levels)" = "#D55E00",
  "Differenced ARIMAX" = "#009E73",
  "Hybrid structural/macro" = "#CC79A7",
  "ARDL error-correction model" = "#332288",
  "Official PBO forecast" = "#000000",
  "PBO expenses plus net capital investment" = "#000000"
)

reported_topdown_models <- names(model_labels)

palette_scenarios <- c(
  central = "#0072B2",
  pressure = "#D55E00",
  restraint = "#009E73",
  indexed_thresholds = "#CC79A7"
)
