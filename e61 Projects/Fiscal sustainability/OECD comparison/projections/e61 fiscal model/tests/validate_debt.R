# Debt and interest accounting under the per-capita structure.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions();a$macro_mode<-'ngdp_per_capita'
cases<-list(Reference=a,'Productivity 1.7 percent'=modifyList(a,list(productivity=.017)),
  'Population growth 1.6 percent'=modifyList(a,list(population_growth=.016)))
paths<-lapply(cases,function(z)debt_diagnostics(project_model(x,z,cal)))
checks<-data.frame(check=character(),passed=logical())
check<-function(label,z){checks[nrow(checks)+1,]<<-list(label,isTRUE(z));if(!isTRUE(z))stop('Failed: ',label)}
near<-function(x,y)max(abs(x-y))<1e-7
for(n in names(paths)){
  d<-paths[[n]]
  check(paste(n,'gross minus assets equals net debt'),near(d$gross_debt_bn-d$financial_assets_bn,d$net_debt_bn))
  check(paste(n,'interest on opening gross debt'),near(d$opening_gross_debt_bn[-1]*d$effective_interest_rate[-1],d$interest_bn[-1]))
  check(paste(n,'receipts on opening assets'),near(d$opening_financial_assets_bn[-1]*d$asset_yield_pct[-1]/100,d$interest_receipts_bn[-1]))
  check(paste(n,'interest GDP identity'),near(d$interest_pct,100*d$interest_bn/d$nominal_gdp))
  check(paste(n,'net interest reconciles'),near(d$net_interest_pct,d$interest_pct-d$interest_receipts_pct))
}
check('Productivity changes debt without changing GDP',identical(paths[[1]]$nominal_gdp,paths[[2]]$nominal_gdp)&&tail(paths[[2]]$net_debt_pct,1)<tail(paths[[1]]$net_debt_pct,1))
report<-do.call(rbind,lapply(names(paths),function(n)transform(paths[[n]],case=n)))
write.csv(report,'outputs/debt_comparison.csv',row.names=FALSE)
write.csv(checks,'outputs/debt_validation_checks.csv',row.names=FALSE)
cat(nrow(checks),'debt checks passed.\n')
