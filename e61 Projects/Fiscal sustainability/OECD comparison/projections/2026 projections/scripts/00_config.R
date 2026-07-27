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
  file.path(figure_dir, "diagnostics")
))

projection_start <- 2026L
projection_end <- 2066L
official_forecast_end <- 2029L
estimation_start <- 1980L
forecast_check_start <- 2010L
forecast_check_end <- 2020L
forecast_check_horizon <- 5L

refresh_data <- tolower(Sys.getenv("REFRESH_DATA", "false")) %in% c("true", "1", "yes")

source_urls <- list(
  gfs_annual = "https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/2024-25/55120DO021_202425.xlsx",
  population_projection = "https://population.gov.au/sites/population.gov.au/files/2026-01/2025-population-statement-age-sex-structure.xlsx",
  pbo_nfo = "https://www.pbo.gov.au/sites/default/files/2025-12/PBO%202025-26%20National%20Fiscal%20Outlook.xlsx",
  budget_nominal_gdp = "https://budget.gov.au/content/bp1/download/bp1_s2-data-nominal-GDP.xlsx",
  budget_receipts_cash = "https://budget.gov.au/content/bp1/download/bp1_s5-online_t1.csv",
  budget_receipts_gdp = "https://budget.gov.au/content/bp1/download/bp1_s5-online_t2.csv",
  labour_force_table1 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/may-2026/62020001.xlsx"
)

model_labels <- c(
  structural_ols = "Structural OLS (levels)",
  arimax_level = "ARIMAX (levels)",
  arimax_diff = "ARIMAX (differences)",
  hybrid = "Hybrid demographics/macro",
  univariate_arima = "Univariate ARIMA"
)

palette_models <- c(
  "Structural OLS (levels)" = "#0072B2",
  "ARIMAX (levels)" = "#D55E00",
  "ARIMAX (differences)" = "#009E73",
  "Hybrid demographics/macro" = "#CC79A7",
  "Univariate ARIMA" = "#E69F00",
  "Official PBO forecast" = "#000000"
)

palette_scenarios <- c(
  central = "#0072B2",
  pressure = "#D55E00",
  restraint = "#009E73",
  indexed_thresholds = "#CC79A7"
)
