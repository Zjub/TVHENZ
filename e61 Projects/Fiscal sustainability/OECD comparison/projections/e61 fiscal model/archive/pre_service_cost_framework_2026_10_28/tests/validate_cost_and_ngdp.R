# Structural restrictions for scenario cost controls and nominal price accounting.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions()
checks<-data.frame(check=character(),passed=logical())
check<-function(label,z){checks[nrow(checks)+1,]<<-list(label,isTRUE(z));if(!isTRUE(z))stop('Failed: ',label)}
near<-function(x,y,tol=1e-7)all(is.finite(c(x,y)))&&max(abs(x-y))<tol
b<-project_model(x,a,cal);keys<-cost_pass_through_parameters()
for(n in names(keys)) {
 scaling<-n%in%c('Health','Education')
 low<-a;low[[keys[[n]]]]<-0;high<-a;high[[keys[[n]]]]<-if(scaling).01 else 1
 lo<-project_model(x,low,cal);hi<-project_model(x,high,cal)
 ld<-subset(lo$components,programme==n);hd<-subset(hi$components,programme==n)
 check(paste(n,'cost response matches selected control'),near(hd$spending_bn/ld$spending_bn,if(scaling)1.01^(b$path$year-2030) else b$path$productivity_index))
 check(paste(n,'common anchor preserved'),near(subset(hi$components,year==2030)$spending_bn,subset(b$components,year==2030)$spending_bn))
 check(paste(n,'other primary programmes unchanged'),near(subset(hi$components,programme!=n)$spending_bn,subset(b$components,programme!=n)$spending_bn))
 check(paste(n,'does not change GDP or tax receipts'),identical(hi$path$nominal_gdp,b$path$nominal_gdp)&&near(hi$revenue$personal_income,b$revenue$personal_income))
}
bad<-a;bad$health_cost_growth_adjustment<-.03
check('Out-of-range pass-through rejected',inherits(try(project_model(x,bad,cal),silent=TRUE),'try-error'))
std<-modifyList(a,list(policy='standards',social_pass_through=0))
std_hi<-modifyList(std,list(social_pass_through=1.5))
check('Relative standards explicitly overrides social protection coefficient',near(project_model(x,std,cal)$components$spending_bn,project_model(x,std_hi,cal)$components$spending_bn))
tt<-modifyList(a,list(tot_change=.2));tn<-modifyList(tt,list(ngdp_per_capita_growth=.04))
t0<-project_model(x,tt,cal);t1<-project_model(x,tn,cal)
factor<-(1.04/1.032)^(t0$path$year-2030)
check('NGDP control raises annual GDP at fixed ToT and real assumptions',near(t1$path$nominal_gdp/t0$path$nominal_gdp,factor))
check('NGDP control raises domestic prices by the same factor',near(t1$path$domestic_price_index/t0$path$domestic_price_index,factor))
for(n in unique(b$components$programme)) {
 d0<-subset(t0$components,programme==n);d1<-subset(t1$components,programme==n)
 check(paste(n,'nominal dollars increase with NGDP even after ToT shock'),near(d1$spending_bn/d0$spending_bn,factor))
 check(paste(n,'GDP share is neutral to that pure nominal change'),near(d1$share_gdp,d0$share_gdp))
}
check('ToT preserves nominal GDP path',identical(t0$path$nominal_gdp,b$path$nominal_gdp))
check('ToT lowers service spending shares through the real-income wedge',near(subset(t0$components,programme=='Health')$share_gdp/subset(b$components,programme=='Health')$share_gdp,t0$path$tot_index^(-a$tot_income_elasticity)))
change<-a;change$health_cost_growth_adjustment<-.01;at<-scenario_attribution(x,change,cal)
check('Pass-through attribution reconciles the debt change',near(sum(at$net_debt_change_pp),tail(project_model(x,change,cal)$path$net_debt_pct-b$path$net_debt_pct,1)))
check('Pass-through changes allocated to their own group',all(abs(at$net_debt_change_pp[at$driver!='Cost pass-through'])<1e-9))
write.csv(checks,'outputs/cost_and_ngdp_checks.csv',row.names=FALSE)
out<-do.call(rbind,lapply(list(Reference=b,'ToT +20%'=t0,'ToT +20%, NGDP per person 4%'=t1),function(z){
 d<-subset(z$components,programme=='Health');data.frame(year=z$path$year,gdp_bn=z$path$nominal_gdp,
  domestic_price_index=z$path$domestic_price_index,health_bn=d$spending_bn,health_pct=d$share_gdp)
}))
out$scenario<-rep(c('Reference','ToT +20%','ToT +20%, NGDP per person 4%'),each=37)
write.csv(out,'outputs/cost_and_ngdp_scenarios.csv',row.names=FALSE)
cat(nrow(checks),'cost pass-through and NGDP checks passed.\n');print(subset(out,year==2066),row.names=FALSE)
