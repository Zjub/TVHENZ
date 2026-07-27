# Run the complete 2026 fiscal projection workflow.

rm(list = ls())

script_order <- c(
  "scripts/01_download_data.R",
  "scripts/02_clean_data.R",
  "scripts/03_assumptions.R",
  "scripts/04_official_forecast.R",
  "scripts/10_bottom_up_projection.R",
  "scripts/20_top_down_projection.R",
  "scripts/21_shapley_attribution.R",
  "scripts/30_forecast_checks.R",
  "scripts/31_model_diagnostics.R",
  "scripts/40_revenue_and_debt.R",
  "scripts/50_graphs.R",
  "scripts/60_word_report.R"
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
