# Offline by default. Use --download to acquire/refresh missing public sources.
args <- commandArgs(FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)][1])
root <- normalizePath(dirname(script), mustWork = TRUE)
Sys.setenv(OECD_LTS_ROOT = root)
if ("--download" %in% commandArgs(TRUE)) {
  source(file.path(root, "scripts", "03_acquire_fiscal_sources.R"))
  source(file.path(root, "scripts", "03b_download_imf_sources.R"))
}
source(file.path(root, "scripts", "04_prepare_fiscal_inputs.R"))
source(file.path(root, "scripts", "05_replicate_fiscal_model.R"))
source(file.path(root, "scripts", "05b_compare_imf_projections.R"))
source(file.path(root, "tests", "validate_fiscal_replication.R"))
source(file.path(root, "scripts", "06_fiscal_report.R"))
message("Fiscal replication completed: ", file.path(root, "outputs", "fiscal"))
