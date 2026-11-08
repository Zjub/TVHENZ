# Reproduce the NDIS benchmark check and private-return sensitivity offline.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions()
b<-project_model(x,a,cal)
# C12.17 is TOTAL Scheme spending. C12.5 is Commonwealth net spending and
# must not be compared with our total-Scheme module as if coverage were equal.
p<-'data/raw/v2/igr_charts/IGR-2026-Chapter-12.xlsx'
d<-as.data.frame(suppressMessages(readxl::read_excel(p,sheet='C12.17',col_names=FALSE)))
keep<-grepl('^[0-9]{4}',d[[1]]) & !is.na(d[[1]])
official<-data.frame(year=as.integer(substr(d[[1]][keep],1,4))+1L,
  igr_2026_pct=as.numeric(d[[2]][keep]),igr_2023_pct=as.numeric(d[[3]][keep]),
  coverage='Total NDIS Scheme; own GDP denominator; rounded published shares',
  source='2026 IGR chart C12.17',url='https://treasury.gov.au/publication/2026-intergenerational-report')
stopifnot(!anyDuplicated(official$year),all(c(2030,2035,2050,2066)%in%official$year),
  official$igr_2026_pct[official$year==2035]==1.6,
  official$igr_2026_pct[official$year==2066]==2)
write.csv(official,'data/processed/ndis_igr_benchmark.csv',row.names=FALSE)
ndis<-subset(b$components,programme=='NDIS')
ndis$nominal_growth_pct<-c(NA,100*(ndis$spending_bn[-1]/head(ndis$spending_bn,-1)-1))
comparison<-merge(ndis,official,by='year',all.x=TRUE)
comparison$gap_2026_igr_pp<-comparison$share_gdp-comparison$igr_2026_pct
write.csv(comparison,'outputs/ndis_benchmark_check.csv',row.names=FALSE)
# Manually transcribed accrual expenses, AFSR 2024-25 table 1, page 13.
# Older policy vintage; not an instruction to fit the current baseline to it.
older<-data.frame(year=c(2026,2027,2028,2029,2035),expenses_bn=c(50.705,53.958,58.126,62.461,95.755),
  source='NDIA AFSR 2024-25 table 1; June 2025 projection; total Scheme accrual expenses',
  url='https://ndis.gov.au/media/8183/download?attachment=')
older$model_bn<-ndis$spending_bn[match(older$year,ndis$year)]
older$gap_bn<-older$model_bn-older$expenses_bn
write.csv(older,'outputs/ndis_2025_vintage_check.csv',row.names=FALSE)
sensitivity<-do.call(rbind,lapply(c(.01,.02,.03,.04,.05,.06,.065,.07),function(rr){
  z<-project_model(x,modifyList(a,list(super_real_return=rr)),cal)
  data.frame(real_return_pct=100*rr,pension_2066_pct=tail(subset(z$components,programme=='Age Pension')$share_gdp,1),
    net_debt_2066_pct=tail(z$path$net_debt_pct,1),pension_coverage_2066=tail(z$pension$coverage,1))
}))
write.csv(sensitivity,'outputs/super_return_sensitivity.csv',row.names=FALSE)
print(comparison[comparison$year%in%c(2030,2035,2050,2066),c('year','spending_bn','share_gdp','nominal_growth_pct','igr_2026_pct')])
print(sensitivity)
