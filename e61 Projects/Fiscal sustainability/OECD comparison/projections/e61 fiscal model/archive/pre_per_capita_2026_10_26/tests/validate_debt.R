# Reproduce the productivity/NGDP comparison and audit debt-interest accounting.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions()
cases<-list(Reference=a,'1.7% productivity, linked NGDP'=modifyList(a,list(productivity=.017)),
  '1.7% productivity, 4.4% NGDP'=modifyList(a,list(productivity=.017,ngdp_override=TRUE)))
paths<-lapply(cases,function(z)debt_diagnostics(project_model(x,z,cal)))
checks<-data.frame(check=character(),passed=logical())
check<-function(label,z){checks[nrow(checks)+1,]<<-list(label,isTRUE(z));if(!isTRUE(z))stop('Failed: ',label)}
near<-function(x,y)max(abs(x-y))<1e-7
for(n in names(paths)){
  d<-paths[[n]]
  check(paste(n,'closing gross minus assets equals net debt'),near(d$gross_debt_bn-d$financial_assets_bn,d$net_debt_bn))
  check(paste(n,'interest on opening gross debt'),near(d$opening_gross_debt_bn[-1]*d$effective_interest_rate[-1],d$interest_bn[-1]))
  check(paste(n,'receipts on opening financial assets'),near(d$opening_financial_assets_bn[-1]*d$asset_yield_pct[-1]/100,d$interest_receipts_bn[-1]))
  check(paste(n,'interest to GDP identity'),near(d$interest_pct,100*d$interest_bn/d$nominal_gdp))
  check(paste(n,'net interest reconciles'),near(d$net_interest_pct,d$interest_pct-d$interest_receipts_pct))
}
linked<-tail(paths[[2]],1);fixed<-tail(paths[[3]],1);ref<-tail(paths[[1]],1)
check('Ticking fixed NGDP lowers dollar GDP and dollar interest',fixed$nominal_gdp<linked$nominal_gdp&&fixed$interest_bn<linked$interest_bn)
check('Ticking fixed NGDP raises interest GDP share despite lower dollar interest',fixed$interest_pct>linked$interest_pct)
check('Ticking fixed NGDP lowers dollar gross debt',fixed$gross_debt_bn<linked$gross_debt_bn)
check('Assets fall more than gross debt so dollar net debt increases',
  linked$financial_assets_bn-fixed$financial_assets_bn>linked$gross_debt_bn-fixed$gross_debt_bn&&fixed$net_debt_bn>linked$net_debt_bn)
check('Borrowing rate unchanged by NGDP toggle',near(paths[[2]]$effective_interest_rate,paths[[3]]$effective_interest_rate))
check('Both scenarios improve debt and interest shares versus fixed reference',
  all(c(linked$net_debt_pct,fixed$net_debt_pct)<ref$net_debt_pct)&&all(c(linked$interest_pct,fixed$interest_pct)<ref$interest_pct))
report<-do.call(rbind,lapply(names(paths),function(n)transform(paths[[n]],case=n)))
write.csv(report,'outputs/productivity_ngdp_debt_comparison.csv',row.names=FALSE)
write.csv(checks,'outputs/debt_validation_checks.csv',row.names=FALSE)
cat(nrow(checks),'debt diagnostics checks passed.\n')
print(subset(report,year==2066,c(case,nominal_gdp,gross_debt_bn,financial_assets_bn,net_debt_bn,interest_bn,interest_pct)),row.names=FALSE)
