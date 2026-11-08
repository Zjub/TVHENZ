# Economic restrictions: inflation neutrality, explicit bases and no regression use.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions()
checks<-data.frame(check=character(),passed=logical())
check<-function(label,z){checks[nrow(checks)+1,]<<-list(label,isTRUE(z));if(!isTRUE(z))stop('Failed: ',label)}
near<-function(x,y,tol=1e-8)all(is.finite(c(x,y)))&&max(abs(x-y))<tol
b<-project_model(x,a,cal)
high<-project_model(x,modifyList(a,list(ngdp_per_capita_growth=.08)),cal)
share<-function(z,n)100*z$revenue[[n]]/z$path$nominal_gdp
heads<-setdiff(names(b$revenue),c('year','interest_receipts'))
for(n in heads)check(paste('Pure inflation leaves',n,'GDP share unchanged'),near(share(b,n),share(high,n)))
for(n in c('gst','property','stamp_duties','other_taxes'))
  check(paste(n,'fixed coverage benchmark has constant GDP share'),near(share(b,n),rep(share(b,n)[1],37)))
for(n in c('superannuation','medicare_fbt','payroll'))
  check(paste(n,'tracks the nominal wage bill'),near(b$revenue[[n]]/b$revenue[[n]][1],b$path$nominal_earnings_index*b$path$employed_index))
check('Excise follows population times indexed unit rate',near(b$revenue$excises_customs/b$revenue$excises_customs[1],b$path$population_index*b$path$domestic_price_index))
check('Excise GDP share falls as real output per resident increases',tail(share(b,'excises_customs'),1)<share(b,'excises_customs')[1])
low<-project_model(x,modifyList(a,list(productivity=0,ngdp_per_capita_growth=.032)),cal)
stress<-project_model(x,modifyList(a,list(productivity=0,ngdp_per_capita_growth=.08)),cal)
for(n in heads)check(paste('Zero-productivity high-inflation scenario leaves',n,'GDP share unchanged'),near(share(low,n),share(stress,n)))
check('Zero real earnings growth produces no PIT bracket creep',near(stress$revenue_diagnostics$bracket_creep_revenue_bn,rep(0,37)))
check('Wage-indexed default PIT share is invariant to productivity',near(share(low,'personal_income'),share(b,'personal_income')))
# Perturb the historical evidence radically; projection output must be identical.
altered<-x;altered$tax_elasticities$applied_elasticity<-rep(99,nrow(x$tax_elasticities))
check('No historical regression coefficient affects any output',identical(project_model(altered,stress$assumptions,cal)$path,stress$path))
altered$tax_elasticities<-NULL
check('Projection works without historical elasticity evidence',identical(project_model(altered,stress$assumptions,cal)$revenue,stress$revenue))
check('All heads retain the common 2030 calibration',near(as.numeric(stress$revenue[1,-1]),as.numeric(b$revenue[1,-1])))
check('Nominal GDP remains independent of productivity',identical(high$path$nominal_gdp,stress$path$nominal_gdp))
fixed<-project_model(x,modifyList(a,list(tax_policy='fixed',ngdp_per_capita_growth=.08)),cal)
check('Fixed nominal PIT thresholds explicitly break inflation neutrality',tail(share(fixed,'personal_income'),1)>tail(share(high,'personal_income'),1))
tt<-project_model(x,modifyList(a,list(tot_change=.2)),cal)
check('GST proxy has no implicit additional ToT response',near(share(tt,'gst'),share(b,'gst')))
check('Royalty share response includes domestic price adjustment',near(share(tt,'royalties')/share(b,'royalties'),tt$path$tot_index^(a$resource_elasticity-a$tot_income_elasticity)))
reg<-read.csv('data/assumption_register.csv')
check('Every spending module appears in evidence register',all(cal$programmes$programme%in%reg$item))
check('All register entries describe classification, rule, source and limitation',all(nzchar(reg$classification)&nzchar(reg$value_rule)&nzchar(reg$source)&nzchar(reg$limitation)))
check('Published source parameters retain individual provenance',all(subset(x$policy,!parameter%in%c('super_real_return','retirement_drawdown','ndis_plan_expenses_2025','aged_care_expenses_2025','age_pension_expenses_2025'))$parameter%in%reg$item))
write.csv(checks,'outputs/structural_revenue_checks.csv',row.names=FALSE)
report<-do.call(rbind,lapply(list(Reference=b,'Zero productivity, default nominal growth'=low,'Zero productivity, 8% nominal growth'=stress),function(z){
 d<-z$revenue;d$nominal_gdp<-z$path$nominal_gdp
 for(n in setdiff(names(z$revenue),'year'))d[[paste0(n,'_pct')]]<-share(z,n)
 d
}))
report$scenario<-rep(c('Reference','Zero productivity, default nominal growth','Zero productivity, 8% nominal growth'),each=37)
write.csv(report,'outputs/structural_revenue_scenarios.csv',row.names=FALSE)
cat(nrow(checks),'structural revenue checks passed.\n')
print(subset(report,year%in%c(2030,2066),c('scenario','year','personal_income_pct','gst_pct','excises_customs_pct','payroll_pct')),row.names=FALSE)
