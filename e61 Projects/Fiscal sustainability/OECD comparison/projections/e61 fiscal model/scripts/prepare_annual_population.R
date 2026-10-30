args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/prepare_annual_population.R');prepare_annual_population()
cat('Prepared official annual population by single-year age and sex, 2026-2066, with 2025 bridge.\n')
