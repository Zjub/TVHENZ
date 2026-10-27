# Explicit, cached primary-source snapshot for model update steps 1-4.
args <- commandArgs(FALSE)
script <- sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
dir.create('archive/v1',recursive=TRUE,showWarnings=FALSE)
for (f in c('README.md','README.pdf','app.R','R','data','tests')) {
  dest <- file.path('archive/v1',basename(f))
  if(!file.exists(dest)) file.copy(f,'archive/v1',recursive=dir.exists(f))
}
dir.create('data/raw/v2',recursive=TRUE,showWarnings=FALSE)
sources <- data.frame(file=c('pbo_nfo_2026_27.xlsx','population_statement_2025.xlsx',
  'abs_gfs_2024_25.xlsx','abs_tax_2024_25.xlsx','abs_lf_age_mar2026.xlsx',
  'abs_hours_age_mar2026.xlsx','igr_2026.pdf','igr_2026_charts.zip',
  'ato_individuals_2023_24_table16.xlsx','ndis_q4_2024_25.pdf','ndia_annual_2024_25.pdf'),
  url=c('https://www.pbo.gov.au/sites/default/files/2026-08/PBO%202026-27%20National%20Fiscal%20Outlook%20-%20Data.xlsx',
    'https://population.gov.au/sites/population.gov.au/files/2026-01/2025-population-statement-age-sex-structure.xlsx',
    'https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/2024-25/55120DO021_202425.xlsx',
    'https://www.abs.gov.au/statistics/economy/government/taxation-revenue-australia/2024-25/55060DO001_202425.xlsx',
    'https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/mar-2026/6291001.xlsx',
    'https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/mar-2026/EM1a.xlsx',
    'https://treasury.gov.au/sites/default/files/2026-09/p2026-797917.pdf',
    'https://treasury.gov.au/sites/default/files/2026-09/p2026-797917-chart.zip',
    'https://data.gov.au/data/dataset/faea4485-f407-457d-97f8-3f0822ccd654/resource/abb69db8-b053-4cd2-84d1-64ac75d02cb1/download/ts24individual16percentiledistributionontaxableincomebysexstate.xlsx',
    'https://www.ndis.gov.au/media/7891/download','https://www.ndis.gov.au/media/8108/download?attachment='),stringsAsFactors=FALSE)
cached <- c('pbo_national_fiscal_outlook_2026_27.xlsx','population_statement_2025_age_sex.xlsx',
            'abs_gfs_annual_2024_25.xlsx',rep(NA_character_,8))
sources$status <- NA_character_
for(i in seq_len(nrow(sources))) {
  dest <- file.path('data/raw/v2',sources$file[i])
  if(!file.exists(dest) && !is.na(cached[i])) {
    src <- file.path('..','2026 projections','data/raw',cached[i])
    if(file.exists(src)) file.copy(src,dest)
  }
  if(file.exists(dest)) {sources$status[i]<-'cached';next}
  sources$status[i] <- tryCatch({curl::curl_download(sources$url[i],dest,quiet=TRUE,
    handle=curl::new_handle(timeout=120));'downloaded'},error=function(e) conditionMessage(e))
  message(sources$file[i],': ',sources$status[i])
}
sources$snapshot_date <- '2026-10-25'
sources$bytes <- file.info(file.path('data/raw/v2',sources$file))$size
sources$md5 <- unname(tools::md5sum(file.path('data/raw/v2',sources$file)))
write.csv(sources,'data/raw/v2/download_manifest.csv',row.names=FALSE)
if(file.exists('data/raw/v2/igr_2026_charts.zip')) unzip('data/raw/v2/igr_2026_charts.zip',exdir='data/raw/v2/igr_charts')
for(f in c('igr_2026.pdf','ndis_q4_2024_25.pdf','ndia_annual_2024_25.pdf')) {
  if(file.exists(file.path('data/raw/v2',f)))writeLines(pdftools::pdf_text(file.path('data/raw/v2',f)),
    file.path('data/raw/v2',sub('[.]pdf$','.txt',f)))
}
cat('Saved primary-source manifest.\n')
