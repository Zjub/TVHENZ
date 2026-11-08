# Offline extraction from the cached official 2026 IGR chart workbook.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
path<-'data/raw/v2/igr_charts/IGR-2026-Chapter-12.xlsx'
extract<-function(programme,sheet,column=2L,label=programme){
 d<-suppressMessages(readxl::read_excel(path,sheet=sheet,col_names=FALSE))
 year<-suppressWarnings(as.integer(substr(as.character(d[[1]]),1,4))+1L)
 value<-suppressWarnings(as.numeric(d[[column]]));keep<-!is.na(year)&!is.na(value)&year>=2030
 data.frame(programme,year=year[keep],igr_pct=value[keep],igr_label=label,
   locator=paste('2026 IGR chart',sheet),
   source_url='https://treasury.gov.au/publication/2026-intergenerational-report')
}
d<-do.call(rbind,list(extract('Health','C12.9'),extract('Education','C12.21'),
 extract('Aged care','C12.15'),extract('Age Pension','C12.20',4L,'Age and Service Pensions'),
 extract('Other social protection','C12.23',2L,'Other income support and family assistance'),
 extract('NDIS','C12.17',2L,'Total NDIS Scheme')))
# Published budget-accounting defence series: PDF Table A2.3, printed page 299.
# Transcribed published years only; do not interpolate or use NATO chart C12.26.
d<-rbind(d,data.frame(programme='Defence',year=c(2036L,2046L,2056L,2066L),
 igr_pct=c(2.4,2.5,2.5,2.5),igr_label='Defence (budget accounting)',
 locator='2026 IGR Table A2.3, printed page 299; C12.5 verifies 2066 endpoint',
 source_url='https://treasury.gov.au/publication/2026-intergenerational-report'))
stopifnot(!anyDuplicated(d[c('programme','year')]),all(is.finite(d$igr_pct)),all(d$igr_pct>=0))
write.csv(d,'data/processed/igr_spending_benchmarks.csv',row.names=FALSE)
cat('Extracted',nrow(d),'published IGR observations; no interpolation.\n')
