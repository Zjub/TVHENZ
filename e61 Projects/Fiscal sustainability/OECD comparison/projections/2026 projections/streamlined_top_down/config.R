suppressPackageStartupMessages({
  library(data.table)
})

find_streamlined_root <- function(start = getwd()) {
  here <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(here, "config.R")) &&
        dir.exists(file.path(here, "scripts"))) return(here)
    parent <- dirname(here)
    if (identical(parent, here)) stop("Could not find streamlined_top_down root.")
    here <- parent
  }
}

streamlined_dir <- find_streamlined_root()
project_dir <- normalizePath(dirname(streamlined_dir), winslash = "/")
input_dir <- file.path(project_dir, "data", "processed")
workflow_mode <- Sys.getenv("STREAMLINED_WORKFLOW_MODE", unset = "full_sample")
if (!workflow_mode %in% c("full_sample", "pre_2020")) {
  stop("Unknown STREAMLINED_WORKFLOW_MODE: ", workflow_mode)
}
workflow_is_pre_2020 <- identical(workflow_mode, "pre_2020")
output_root_dir <- file.path(streamlined_dir, "outputs")
output_dir <- if (workflow_is_pre_2020) {
  file.path(output_root_dir, "pre_2020_estimation")
} else output_root_dir
table_dir <- file.path(output_dir, "tables")
figure_dir <- file.path(output_dir, "figures")
model_dir <- file.path(output_dir, "models")
documentation_dir <- file.path(streamlined_dir, "documentation")

invisible(lapply(
  c(output_dir, table_dir, figure_dir, model_dir, documentation_dir),
  dir.create, recursive = TRUE, showWarnings = FALSE
))

projection_start <- 2026L
official_forecast_end <- 2030L
projection_end <- 2066L
estimation_start <- 1980L
estimation_end <- if (workflow_is_pre_2020) 2019L else 2025L
rolling_start <- 2005L
rolling_end <- if (workflow_is_pre_2020) 2014L else 2020L
rolling_horizon <- 5L

# This workflow deliberately fixes the fiscal concept. Conventional public-debt
# interest is removed from the regression and added back through the debt block.
interest_treatment <- "exclude_other_interest"

model_labels <- c(
  with_income = "Structural-change model with real GDP per capita",
  with_income_65p = "Sensitivity: combined population share aged 65+",
  no_income = "Sensitivity: omit real GDP per capita",
  structural_only = "Sensitivity: omit transitory macro factors",
  arima_benchmark = "Statistical ARIMA benchmark"
)

reported_variants <- c(
  "with_income", "no_income", "structural_only", "arima_benchmark"
)

model_colours <- c(
  with_income = "#0072B2",
  no_income = "#D55E00",
  structural_only = "#009E73",
  arima_benchmark = "#777777"
)
