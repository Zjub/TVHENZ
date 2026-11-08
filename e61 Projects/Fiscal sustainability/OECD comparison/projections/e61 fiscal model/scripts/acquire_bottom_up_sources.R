# Cache official additional sources; no changes to the existing source vintage.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
dir.create('data/raw/bottom_up',recursive=TRUE,showWarnings=FALSE)
sources<-data.frame(file=c('abs_education_2025.xlsx','aihw_commonwealth_2025.html','aihw_states_2025.html','gen_spending_2025.html'),
 url=c('https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/2024-25/55120DO095_202425.xlsx',
 'https://www.aihw.gov.au/reports/health-welfare-expenditure/health-expenditure-australia-2024-25/contents/spending-trends-by-source/australian-government-spending',
 'https://www.aihw.gov.au/reports/health-welfare-expenditure/health-expenditure-australia-2024-25/contents/spending-trends-by-source/state-and-territory-government-spending',
 'https://www.gen-agedcaredata.gov.au/topics/spending-on-aged-care'))
for(i in seq_len(nrow(sources))){p<-file.path('data/raw/bottom_up',sources$file[i]);if(!file.exists(p))curl::curl_download(sources$url[i],p,quiet=TRUE)}
sources$snapshot_date<-'2026-10-27';p<-file.path('data/raw/bottom_up',sources$file)
sources$bytes<-file.info(p)$size;sources$md5<-unname(tools::md5sum(p))
write.csv(sources,'data/raw/bottom_up/source_manifest.csv',row.names=FALSE)
cat('Cached',nrow(sources),'official bottom-up source files.\n')
