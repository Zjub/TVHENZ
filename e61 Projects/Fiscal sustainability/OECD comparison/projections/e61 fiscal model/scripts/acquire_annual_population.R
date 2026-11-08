# Cache the official annual national IGR population projection; no refresh by default.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
url<-'https://population.gov.au/sites/population.gov.au/files/2026-10/igr-2026-age-and-sex-national_0.xlsx'
dest<-'data/raw/v2/igr_2026_population_age_sex.xlsx'
if(!file.exists(dest))curl::curl_download(url,dest,quiet=TRUE)
write.csv(data.frame(file=dest,url=url,access_date=as.character(Sys.Date()),
 bytes=file.info(dest)$size,md5=unname(tools::md5sum(dest)),
 source='Centre for Population, 2026 IGR annual national age/sex projections; updated 27 October 2026'),
 'data/raw/v2/annual_population_manifest.csv',row.names=FALSE)
