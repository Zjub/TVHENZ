# Election extension runner. Existing pack 05 must have been run once.
arg <- grep("^--file=",commandArgs(FALSE),value=TRUE)
if (!length(arg)) stop("Run with Rscript")
root <- dirname(normalizePath(sub("^--file=","",arg[1])))
flags <- commandArgs(TRUE)
if (any(!flags %in% c("--cached","--plots-only"))) stop("Use --cached or --plots-only")
exe <- file.path(R.home("bin"),if(.Platform$OS.type=="windows")"Rscript.exe" else "Rscript")
if (!"--plots-only" %in% flags) {
  args <- c(shQuote(file.path(root,"08_pull_election_data.R")),if("--cached" %in% flags)"--cached")
  if (system2(exe,args)!=0L) stop("Election data pull failed")
}
if (system2(exe,shQuote(file.path(root,"09_make_election_plots.R")))!=0L) stop("Election plotting failed")
message("Open ",file.path(root,"figures","election_2026","index.html"))
