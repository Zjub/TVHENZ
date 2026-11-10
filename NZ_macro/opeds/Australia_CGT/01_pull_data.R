# Independent Australia CGT pack. Existing NZ scripts/outputs are not required.
args <- commandArgs(FALSE)
script <- sub('^--file=', '', grep('^--file=', args, value = TRUE)[1])
root <- if (!is.na(script)) dirname(normalizePath(script)) else normalizePath('Australia_CGT')
python <- Sys.getenv('AU_CGT_PYTHON', '')
if (!nzchar(python)) {
  local_python <- 'C:/Users/OEM/anaconda3/python.exe'
  python <- if (file.exists(local_python)) local_python else Sys.which('python')
}
if (!nzchar(python)) stop('Set AU_CGT_PYTHON to a Python executable; see README.md.')
run_python <- function(file, args = character()) {
  status <- system2(python, c(shQuote(file.path(root, 'helpers', file)), args))
  if (status != 0L) stop('Python helper failed: ', file)
}
options <- intersect(commandArgs(TRUE), c('--cached', '--missing'))
run_python('pull_sources.py', options)
run_python('normalise_sources.py')
