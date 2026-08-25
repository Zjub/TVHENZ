# Run the complete 2026 fiscal projection workflow.

rm(list = ls())

script_order <- c(
  "scripts/01_download_data.R",
  "scripts/02_clean_data.R",
  "scripts/04_official_forecast.R",
  "scripts/03_assumptions.R",
  "scripts/05_download_bottom_up_drivers.R",
  "scripts/10_bottom_up_projection.R",
  "scripts/11_bottom_up_data_enhanced.R",
  "scripts/20_top_down_projection.R",
  "scripts/21_shapley_attribution.R",
  "scripts/30_forecast_checks.R",
  "scripts/31_model_diagnostics.R",
  "scripts/32_ecm_validation.R",
  "scripts/34_four_model_assessment.R",
  "scripts/40_revenue_and_debt.R",
  "scripts/50_graphs.R",
  "scripts/51_bottom_up_comparison_graphs.R",
  "scripts/53_four_model_figures.R",
  "scripts/54_precovid_structural_comparison.R",
  "scripts/55_age_profile_sensitivity.R",
  "scripts/56_interest_treatment_sensitivity.R",
  "scripts/63_four_model_report.R",
  "scripts/64_age_profile_note.R",
  "scripts/65_top_down_literature_review.R",
  "scripts/66_interest_treatment_note.R",
  "scripts/62_bottom_up_comparison_report.R"
)

args <- commandArgs(trailingOnly = FALSE)
hit <- args[startsWith(args, "--file=")]
project_dir <- if (length(hit)) {
  dirname(normalizePath(sub("--file=", "", hit[[1]])))
} else {
  normalizePath(getwd())
}
setwd(project_dir)

started <- Sys.time()
for (script in script_order) {
  message("\n", paste(rep("=", 72), collapse = ""))
  message("Running ", script)
  message(paste(rep("=", 72), collapse = ""))
  status <- system2(
    file.path(R.home("bin"), "Rscript.exe"),
    c("--vanilla", shQuote(script)),
    stdout = "",
    stderr = ""
  )
  if (!identical(status, 0L)) stop("Pipeline stopped in ", script, call. = FALSE)
}

message("\nWorkflow completed in ", round(difftime(Sys.time(), started, units = "mins"), 1), " minutes.")
message("See outputs/figures, outputs/tables and documentation.")
