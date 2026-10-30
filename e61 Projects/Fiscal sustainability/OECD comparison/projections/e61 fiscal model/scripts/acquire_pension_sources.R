# Cache public sources once. No credentials, survey microdata or HILDA access.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
p<-'data/raw/pension';dir.create(p,recursive=TRUE,showWarnings=FALSE)
fetch<-function(url,file){dest<-file.path(p,file);if(!file.exists(dest))curl::curl_download(url,dest,quiet=TRUE);dest}
resources<-function(id){f<-fetch(paste0('https://data.gov.au/data/api/3/action/package_show?id=',id),paste0(id,'_metadata.json'));jsonlite::fromJSON(f)$result$resources}
d<-resources('dss-payment-demographic-data');a<-resources('taxation-statistics-2023-24')
jobs<-data.frame(file=c('dss_june_2026.xlsx','ato_snapshot_2024.xlsx','abs_age_2020.xlsx','abs_super_2020.xlsx'),
 url=c(d$url[grepl('June 2026',d$name)],a$url[grepl('Snapshot.*Table 5',a$name)],
 'https://www.abs.gov.au/statistics/economy/finance/household-income-and-wealth-australia/2019-20/10.%20Age%20of%20reference%20person.xlsx',
 'https://www.abs.gov.au/statistics/economy/finance/household-income-and-wealth-australia/2019-20/12.%20Superannuation%20of%20persons.xlsx'))
stopifnot(nrow(jobs)==4,all(startsWith(jobs$url,'https://')))
for(i in seq_len(nrow(jobs)))fetch(jobs$url[i],jobs$file[i])
jobs$md5<-unname(tools::md5sum(file.path(p,jobs$file)));write.csv(jobs,file.path(p,'source_downloads.csv'),row.names=FALSE)
cat('Public pension sources cached; existing snapshots were not refreshed.\n')
