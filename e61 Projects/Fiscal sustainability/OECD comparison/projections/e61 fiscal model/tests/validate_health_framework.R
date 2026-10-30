args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1]);setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions();a$macro_mode<-'ngdp_per_capita';b<-project_model(x,a,cal)
n<-0;check<-function(ok,label){if(!isTRUE(ok))stop(label);n<<-n+1};near<-function(x,y,tol=1e-8)max(abs(x-y))<tol
f<-setdiff(names(spending_factor_labels()),'gdp_factor')
check(all(abs(unlist(subset(b$subfunctions,year==2030)[f])-1)<1e-12),'All anchor factors equal one')
check(all(subset(b$subfunctions,programme=='Health')$intensity_factor==1),'Zero remaining pressures replace previous allowance')
check(a$health_income_elasticity==.8 && a$health_service_productivity_share==.25,'New reference defaults')
zero<-project_model(x,modifyList(a,list(health_income_elasticity=0)),cal)
full<-project_model(x,modifyList(a,list(health_service_productivity_share=1)),cal)
price<-project_model(x,modifyList(a,list(health_cpi_wedge=.005)),cal)
wage<-project_model(x,modifyList(a,list(health_wage_premium=.005)),cal)
high<-project_model(x,modifyList(a,list(productivity=.017)),cal)
legacy<-project_model(x,modifyList(a,list(health_specification='legacy')),cal)
check(all(subset(legacy$subfunctions,programme=='Health')$income_factor==1),'Legacy health specification excludes income demand')
for(p in list(zero,full,price,wage)){
 check(identical(p$path$nominal_gdp,b$path$nominal_gdp),'Health controls leave nominal GDP unchanged')
 check(identical(p$path$nominal_hourly_wage_index,b$path$nominal_hourly_wage_index),'Economy-wide nominal wages unchanged')
 for(name in c('NDIS','Other social protection','Age Pension','Education','Aged care','Other public services','Defence')){
   k<-b$components$programme==name;check(identical(p$components$spending_bn[k],b$components$spending_bn[k]),paste(name,'unchanged by health controls'))
 }
 check(near(p$subfunctions$spending_bn,p$subfunctions$anchor_2030_bn*apply(p$subfunctions[f],1,prod)),'Spending factors reconstruct amounts')
}
hb<-subset(b$subfunctions,programme=='Health');hz<-subset(zero$subfunctions,programme=='Health')
check(all(hz$income_factor==1),'Zero elasticity removes income channel')
for(key in setdiff(f,'income_factor'))check(identical(hb[[key]],hz[[key]]),paste('Zero elasticity preserves',key))
for(id in hb$id[!duplicated(hb$id)]){
 d<-full$subfunctions[full$subfunctions$id==id,];check(near(d$real_unit_factor,rep(1,nrow(d))),'Equal wage and service productivity removes real input-cost growth at zero CPI wedge')
}
med<-subset(price$subfunctions,id=='h_medicines');check(all(med$efficiency_factor==1)&all(med$wage_factor==1),'Medicines no labour or efficiency component')
hospital<-subset(price$subfunctions,id=='h_hospital');L<-x$service_cost_parameters$wage_weight[x$service_cost_parameters$id=='h_hospital'];t<-price$path$year-2030
Q<-(1+.25*a$productivity)^t
expected<-(1-L)*hospital$price_factor+L*price$path$nominal_hourly_wage_index/Q
check(near(hospital$price_factor*hospital$real_unit_factor,expected),'CPI and real wages reconcile to nominal input costs')
check(near(hb$income_factor,(hb$real_gdp_per_capita_index)^.8),'Income uses real GDP per person')
check(identical(b$path$nominal_gdp,high$path$nominal_gdp),'Productivity does not change fixed nominal GDP')
hours<-project_model(x,modifyList(a,list(hours_growth=.005)),cal)
check(!near(subset(hours$subfunctions,id=='h_hospital')$income_factor,subset(b$subfunctions,id=='h_hospital')$income_factor),'Hours affect real income demand')
for(source in c('subfunctions','aggregate_long')){
 z<-a;z$health_pressure_source<-source;z$health_remaining_growth<-health_pressure_centre(x,z,cal);p<-project_model(x,z,cal)
 r<-health_pressure_base(x,z);d<-subset(p$subfunctions,programme=='Health')
 check(near(d$additional_service_growth,unname(r[d$id])),'Historical residuals use new equation and fixed baseline')
 z$productivity<-.017;pp<-project_model(x,z,cal)
 check(identical(d$additional_service_growth,subset(pp$subfunctions,programme=='Health')$additional_service_growth),'Residual not refitted as productivity changes')
}
targets<-a$spending_gdp_targets;targets['Health']<-0
z<-project_model(x,modifyList(a,list(spending_gdp_targets=targets)),cal)
for(p in list(b,price,z))for(unit in c('pct','bn','real_bn')){
 d<-spending_contributions(p,measure=unit,include_financing=TRUE);ends<-d[!duplicated(d$id),]
 check(abs(sum(d$contribution)-sum(ends$change))<1e-6,'Shapley interactions reconcile including income and zero targets')
}
bc<-service_cost_comparison(b,x);pc<-service_cost_comparison(price,x)
check(identical(bc$reference_combined_non_demographic_growth,pc$reference_combined_non_demographic_growth),'Reference stays fixed')
for(change in list(list(health_income_elasticity=2),list(health_remaining_growth=.03),list(health_cpi_wedge=.02),list(health_wage_premium=NA_real_)))
 check(inherits(try(project_model(x,modifyList(a,change),cal),silent=TRUE),'try-error'),'Invalid controls rejected')
z<-x$service_cost_parameters[1,];z$programme<-'Health';m<-b$path;m$domestic_price_index<-c(1,rep(.0001,36))
check(inherits(try(health_cost_factors(z,m,modifyList(a,list(health_cpi_wedge=-.01))),silent=TRUE),'try-error'),'Nonpositive gross CPI factor rejected')
history<-read.csv('estimation/outputs/extended/gfce_history_1963_2025.csv');check(all(table(history$programme)==63),'63 observed years for each aggregate function')
fits<-read.csv('estimation/outputs/extended/residual_estimates.csv');check(any(fits$start==1999 & fits$n==27)&any(fits$start==1987 & fits$n==39),'Long consistent and wage-proxy sensitivity windows')
cat('PASS:',n,'health income/cost framework checks.\n')
