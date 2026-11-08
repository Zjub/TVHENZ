# One-command entry point: pull public data, check it and render all charts.
# --cached reuses source files. --plots-only renders existing processed data.
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(arg)) stop("Run this file with Rscript.")
root <- dirname(normalizePath(sub("^--file=", "", arg[1])))
flags <- commandArgs(TRUE)
if (any(!flags %in% c("--cached", "--plots-only"))) stop("Supported flags: --cached, --plots-only")
if ("--cached" %in% flags) Sys.setenv(NZ_DATA_REFRESH = "false")
executable <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
scripts <- if ("--plots-only" %in% flags) "04_make_topic_plots.R" else c("03_pull_topic_data.R", "06_pull_benefit_history.R", "04_make_topic_plots.R")
for (script in scripts) {
  status <- system2(executable, shQuote(file.path(root, script)))
  if (status != 0L) stop(script, " failed (exit ", status, ").")
}
message("Open: ", file.path(root, "figures", "op_ed_topics", "index.html"))
