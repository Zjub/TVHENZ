# Run from this directory, normally through run_estimation.R.
# No fiscal-model inputs or Shiny files are modified.
dir.create('data/raw',recursive=TRUE,showWarnings=FALSE)
dir.create('outputs',showWarnings=FALSE)
sources <- data.frame(file=c('population.xlsx','cpi.xlsx','health_tables.xlsx','health_cube.xlsx'),url=c(
 'https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population/mar-2026/3101059.xlsx',
 'https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/jun-quarter-2025/640101.xlsx',
 'https://www.aihw.gov.au/getmedia/44735a41-5372-414e-9a18-d25f7c13fe4c/AIHW-HWE-110-HEA-2024-25-data-tables.xlsx',
 'https://www.aihw.gov.au/getmedia/eea92c98-edd3-4ffe-a855-3c945610f9c4/AIHW-HWE-110-HEA-2024-25-datacube.xlsx'))
for(i in seq_len(nrow(sources))) {
 target<-file.path('data/raw',sources$file[i])
 if(!file.exists(target))curl::curl_download(sources$url[i],target,quiet=FALSE)
}
# Snapshot the exact existing official-data vintages used by this exercise.
cached<-data.frame(file=c('gfs_purpose.csv','education.xlsx','wpi.rds','national_accounts.xlsx',
 'age_profiles.csv','subfunctions.csv','service_parameters.csv','igr_2026.txt'),
 path=c('../data/historical_gfs_expenses_by_purpose.csv','../data/raw/bottom_up/abs_education_2025.xlsx',
 '../../2026 projections/data/raw/abs_wpi_public_industry.rds',
 '../../2026 projections/data/raw/abs_cache/5206.0/5206034_key_aggregates_and_analytical_series_annual_data.xlsx',
 '../data/processed/subfunction_age_profiles.csv','../data/spending_subfunctions.csv',
 '../data/service_cost_parameters.csv','../data/raw/v2/igr_2026.txt'))
for(i in seq_len(nrow(cached))) {
 target<-file.path('data/raw',cached$file[i])
 if(!file.exists(target))stopifnot(file.copy(cached$path[i],target,overwrite=FALSE))
}
manifest<-rbind(data.frame(file=sources$file,source=sources$url),data.frame(file=cached$file,source=cached$path))
manifest$bytes<-file.info(file.path('data/raw',manifest$file))$size
manifest$md5<-unname(tools::md5sum(file.path('data/raw',manifest$file)))
manifest$snapshot_date<-as.character(Sys.Date())
if(file.exists('data/source_manifest.csv')){
 old<-read.csv('data/source_manifest.csv');j<-match(manifest$file,old$file)
 same<-!is.na(j) & manifest$md5==old$md5[j]
 manifest$snapshot_date[same]<-old$snapshot_date[j[same]]
}
write.csv(manifest,'data/source_manifest.csv',row.names=FALSE)
cat('Acquisition complete; cached files are never silently refreshed.\n')
