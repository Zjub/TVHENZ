rm(list = ls())

args <- commandArgs(trailingOnly = FALSE)
hit <- args[startsWith(args, "--file=")]
streamlined_dir <- if (length(hit)) {
  dirname(normalizePath(sub("--file=", "", hit[[1]]), winslash = "/"))
} else normalizePath(getwd(), winslash = "/")
setwd(streamlined_dir)

scripts <- c(
  "scripts/01_estimate_validate.R",
  "scripts/02_project_debt.R",
  "scripts/03_figures.R",
  "scripts/04_write_note.R"
)

pre_2020_scripts <- c(
  "scripts/01_estimate_validate.R",
  "scripts/02_project_debt.R",
  "scripts/03_figures.R"
)

started <- Sys.time()
for (script in scripts) {
  message("\n", paste(rep("=", 70), collapse = ""))
  message("Running ", script)
  status <- system2(
    file.path(R.home("bin"), "Rscript.exe"),
    c("--vanilla", shQuote(script)), stdout = "", stderr = ""
  )
  if (!identical(status, 0L)) stop("Streamlined workflow stopped in ", script, call. = FALSE)
}
Sys.setenv(STREAMLINED_WORKFLOW_MODE = "pre_2020")
for (script in pre_2020_scripts) {
  message("\n", paste(rep("=", 70), collapse = ""))
  message("Running pre-COVID estimation suite: ", script)
  status <- system2(
    file.path(R.home("bin"), "Rscript.exe"),
    c("--vanilla", shQuote(script)),
    stdout = "", stderr = ""
  )
  if (!identical(status, 0L)) {
    stop("Pre-COVID estimation suite stopped in ", script, call. = FALSE)
  }
}
Sys.unsetenv("STREAMLINED_WORKFLOW_MODE")
message("\nStreamlined workflow completed in ",
        round(difftime(Sys.time(), started, units = "mins"), 1), " minutes.")
