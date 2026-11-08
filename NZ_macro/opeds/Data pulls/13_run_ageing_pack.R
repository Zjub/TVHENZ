# Ageing extension; existing topic and election processed files are required.
arg <- grep("^--file=",commandArgs(FALSE),value=TRUE)
if(!length(arg))stop("Run with Rscript")
root <- dirname(normalizePath(sub("^--file=","",arg[1])));flags <- commandArgs(TRUE)
if(any(!flags %in% c("--cached","--plots-only")))stop("Use --cached or --plots-only")
exe <- file.path(R.home("bin"),if(.Platform$OS.type=="windows")"Rscript.exe" else "Rscript")
if(!"--plots-only" %in% flags) {
  if(system2(exe,c(shQuote(file.path(root,"11_pull_ageing_data.R")),if("--cached" %in% flags)"--cached"))!=0L)stop("Ageing pull failed")
}
if(system2(exe,shQuote(file.path(root,"12_make_ageing_plots.R")))!=0L)stop("Ageing charts failed")
message("Open ",file.path(root,"figures","ageing","index.html"))
