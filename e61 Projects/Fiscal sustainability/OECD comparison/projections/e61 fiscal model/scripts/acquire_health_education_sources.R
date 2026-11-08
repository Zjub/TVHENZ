# Cache official evidence; runtime projections never use the network.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
dir.create('data/raw/health_education',recursive=TRUE,showWarnings=FALSE)
urls<-c(
 hospital_costs.xlsm='https://www.ihacpa.gov.au/sites/default/files/2026-05/nhcdc_public_appendix_tables_2023-24.xlsm',
 hospital_report.pdf='https://www.ihacpa.gov.au/sites/default/files/2026-05/nhcdc_public_sector_report_2023-24.pdf',
 school_funding.pdf='https://dataandreporting.blob.core.windows.net/anrdataportal/ANR-Documents/ANR2024/nationalreportonschoolinginaustralia_2024_Chapter9.pdf',
 vet_tables.xlsx='https://assets.pc.gov.au/2026-06/rogs-202606-partb-section5-vet-data-tables.xlsx?VersionId=lfmB4_0fFeCzi8A8nWsGVUrriixy7_GK',
 university_finance.pdf='https://www.education.gov.au/download/19832/finance-2024-financial-reports-higher-education-providers/43005/2024-university-finance-summary-information/pdf',
 skills_agreement.pdf='https://federalfinancialrelations.gov.au/sites/federalfinancialrelations.gov.au/files/2023-12/national-skills-agreement.pdf')
manifest<-do.call(rbind,lapply(names(urls),function(n){
 p<-file.path('data/raw/health_education',n)
 if(!file.exists(p))curl::curl_download(urls[[n]],p,quiet=TRUE)
 stopifnot(file.info(p)$size>1000)
 data.frame(file=n,url=urls[[n]],accessed='2026-10-27',bytes=file.info(p)$size,md5=unname(tools::md5sum(p)))
}))
write.csv(manifest,'data/raw/health_education/source_manifest.csv',row.names=FALSE)
cat('Cached',nrow(manifest),'official source files.\n')
