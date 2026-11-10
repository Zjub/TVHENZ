# Run this script from any working directory. --cached: saved snapshots only.
# --plots-only: reuse processed source tables, rebuild models and all figures.
args <- commandArgs(FALSE)
script <- sub('^--file=', '', grep('^--file=', args, value = TRUE)[1])
root <- if (!is.na(script)) dirname(normalizePath(script)) else normalizePath('Australia_CGT')
rscript <- file.path(R.home('bin'), 'Rscript.exe')
if (!file.exists(rscript)) rscript <- file.path(R.home('bin'), 'Rscript')
for (file in c(if (!'--plots-only' %in% commandArgs(TRUE)) '01_pull_data.R', '02_models.R', '03_make_plots.R')) {
  message('Running ', file)
  status <- system2(rscript, c(shQuote(file.path(root,file)), commandArgs(TRUE)))
  if (status != 0L) stop(file, ' failed')
}
python <- Sys.getenv('AU_CGT_PYTHON','')
if (!nzchar(python)) python <- if(file.exists('C:/Users/OEM/anaconda3/python.exe')) 'C:/Users/OEM/anaconda3/python.exe' else Sys.which('python')
status <- system2(python,shQuote(file.path(root,'helpers','validate_outputs.py')))
if(status!=0L) stop('Output validation failed')
