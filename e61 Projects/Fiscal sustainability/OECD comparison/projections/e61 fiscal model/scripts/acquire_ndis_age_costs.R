# Public NDIA age-cost source snapshot. June 2025 matches the participation vintage.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
raw<-'data/raw/ndis_age_costs';dir.create(raw,recursive=TRUE,showWarnings=FALSE)
s<-data.frame(file=c('participant_budgets_june_2025.csv','utilisation_june_2025.csv',
 'participant_budget_rules.docx','utilisation_rules.docx'),
 url=paste0('https://dataresearch.ndis.gov.au/media/',c(4366,4367,4260,4270),'/download?attachment='),
 vintage=c('30 June 2025','30 June 2025','June 2026 definitions; includes December 2024 age-band change',
 'June 2026 definitions; utilisation uses a lagged six-month window'))
for(i in seq_len(nrow(s)))if(!file.exists(file.path(raw,s$file[i])))
 curl::curl_download(s$url[i],file.path(raw,s$file[i]),quiet=TRUE)
s$bytes<-file.info(file.path(raw,s$file))$size;s$md5<-unname(tools::md5sum(file.path(raw,s$file)))
s$retrieved<-'2026-11-03';write.csv(s,file.path(raw,'manifest.csv'),row.names=FALSE)
