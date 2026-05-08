# Run the Australia workflow for the OECD Economic Outlook 117 long-run
# scenarios. The script can be launched from the repository root, the
# projections project root, or this folder.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]

if (length(file_arg)) {
  workflow_root <- dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE))
} else {
  workflow_root <- normalizePath(getwd(), mustWork = TRUE)
  if (!file.exists(file.path(workflow_root, "scripts", "00_config.R")) &&
      dir.exists(file.path(workflow_root, "projections", "OECD_long_run_scenario"))) {
    workflow_root <- file.path(workflow_root, "projections", "OECD_long_run_scenario")
  }
}

Sys.setenv(OECD_LTS_ROOT = workflow_root)

# Set to FALSE to rebuild tables and plots from the last downloaded CSV in
# data/processed without refreshing the OECD/DBnomics data.
refresh_download <- TRUE

source(file.path(workflow_root, "scripts", "00_config.R"))
source(file.path(workflow_root, "scripts", "01_download_oecd_lts_data.R"))
source(file.path(workflow_root, "scripts", "02_build_australia_projection_outputs.R"))

download_oecd_lts_australia(force = refresh_download)
build_oecd_lts_australia_outputs()

message("OECD long-run scenario outputs written to: ", normalizePath(oecd_lts_config$output_dir))
