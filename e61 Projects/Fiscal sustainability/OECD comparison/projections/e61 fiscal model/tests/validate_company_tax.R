# Structural company tax: GDP-share invariance and level exposure to ToT.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions()
checks<-data.frame(check=character(),passed=logical())
check<-function(label,z){checks[nrow(checks)+1,]<<-list(label,isTRUE(z));if(!isTRUE(z))stop('Failed: ',label)}
near<-function(x,y,tol=1e-8)all(is.finite(c(x,y)))&&max(abs(x-y))<tol
base_share<-100*cal$revenue_anchor['company']/cal$gdp0
cases<-list(Reference=a,
  'NGDP per capita 1 percent'=modifyList(a,list(ngdp_per_capita_growth=.01)),
  'NGDP per capita 8 percent'=modifyList(a,list(ngdp_per_capita_growth=.08)),
  'Screenshot: zero productivity, 8 percent nominal per capita'=modifyList(a,list(productivity=0,ngdp_per_capita_growth=.08)),
  'Productivity 1.7 percent'=modifyList(a,list(productivity=.017)),
  'Population growth 1.6 percent'=modifyList(a,list(population_growth=.016)),
  'Average hours growth 0.2 percent'=modifyList(a,list(hours_growth=.002)),
  'ToT up 10 percent'=modifyList(a,list(tot_change=.10)),
  'ToT up 20 percent'=modifyList(a,list(tot_change=.20)),
  'ToT down 20 percent'=modifyList(a,list(tot_change=-.20)),
  'High nominal growth and ToT up 20 percent'=modifyList(a,list(ngdp_per_capita_growth=.08,tot_change=.20)),
  'ToT sensitivity zero'=modifyList(a,list(tot_change=.20,company_tot_elasticity=0)),
  'ToT sensitivity two'=modifyList(a,list(tot_change=.20,company_tot_elasticity=2)))
projections<-lapply(cases,function(z)project_model(x,z,cal))
for(n in names(cases)){
  z<-projections[[n]];m<-z$path;share<-100*z$revenue$company/m$nominal_gdp
  check(paste(n,'matches structural rule'),near(share,base_share*m$tot_index^cases[[n]]$company_tot_elasticity))
  check(paste(n,'preserves common 2030 revenue anchor'),near(m$revenue_bn[1],projections[[1]]$path$revenue_bn[1]))
}
for(n in names(cases)[1:7]){
  z<-projections[[n]]
  check(paste(n,'constant company tax GDP share in all years'),near(100*z$revenue$company/z$path$nominal_gdp,rep(base_share,37)))
}
get_share<-function(n){z<-projections[[n]];100*z$revenue$company/z$path$nominal_gdp}
up<-get_share('ToT up 20 percent');down<-get_share('ToT down 20 percent')
check('Higher ToT raises and lower ToT reduces the structural share',all(up[-1]>base_share)&&all(down[-1]<base_share))
check('A permanent ToT level shock stops changing the share after 2040',near(up[11:37],rep(1.2*base_share,27)))
check('Changing NGDP does not change the share even with a ToT shock',near(up,get_share('High nominal growth and ToT up 20 percent')))
check('Sensitivity zero disables the ToT share response',near(get_share('ToT sensitivity zero'),rep(base_share,37)))
check('Sensitivity two has the stated endpoint response',near(tail(get_share('ToT sensitivity two'),1),base_share*1.2^2))
report<-do.call(rbind,lapply(names(projections),function(n){
  z<-projections[[n]];data.frame(case=n,year=z$path$year,nominal_gdp=z$path$nominal_gdp,
    tot_index=z$path$tot_index,company_tax_bn=z$revenue$company,
    company_tax_pct=100*z$revenue$company/z$path$nominal_gdp,
    company_tot_elasticity=cases[[n]]$company_tot_elasticity)
}))
write.csv(checks,'outputs/company_tax_validation_checks.csv',row.names=FALSE)
write.csv(report,'outputs/structural_company_tax_scenarios.csv',row.names=FALSE)
cat(nrow(checks),'structural company tax checks passed.\n')
print(subset(report,year==2066,c(case,company_tax_pct,company_tax_bn)),row.names=FALSE)
