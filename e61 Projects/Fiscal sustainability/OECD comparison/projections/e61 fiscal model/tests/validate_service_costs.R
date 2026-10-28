args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');source('R/sector_review.R')
x<-load_inputs();cal<-calibrate(x);a<-default_assumptions();a$ndis_cost_rule<-'input_mix';b<-project_model(x,a,cal)
checks<-data.frame(check=character(),passed=logical())
check<-function(label,v){checks[nrow(checks)+1L,]<<-list(label,isTRUE(v));if(!isTRUE(v))stop(label)}
near<-function(x,y)length(x)==length(y)&&all(is.finite(c(x,y)))&&max(abs(x-y))<1e-8
theta<-modifyList(a,list(health_service_productivity_share=1,education_service_productivity_share=.5))
c<-project_model(x,theta,cal)
high<-project_model(x,modifyList(theta,list(productivity=.017)),cal)
extra<-project_model(x,modifyList(theta,list(health_service_growth_adjustment=.01)),cal)
zero<-project_model(x,modifyList(theta,list(productivity=0)),cal)
for(id in c('h_hospital','h_primary','h_medicines','e_school','e_higher','e_vet','n_ndis')){
 d<-c$subfunctions[c$subfunctions$id==id,];z<-x$service_cost_parameters[x$service_cost_parameters$id==id,]
 W<-c$path$productivity_index;t<-c$path$year-2030
 response<-z$service_productivity_share+theta[[service_productivity_parameters()[[d$programme[1]]]]]
 Q<-(1+response*theta$productivity)^t
 expected<-switch(z$cost_rule,input_mix=(1-z$wage_weight)+z$wage_weight*W/Q,
   cpi_proxy=rep(1,length(t)),funding_proxy=(1+z$wage_weight*theta$productivity)^t)
 check(paste(id,'labour-only efficiency formula'),near(d$real_unit_factor,expected))
 check(paste(id,'wages and efficiency reconstruct unit cost'),near(d$wage_factor*d$efficiency_factor,d$real_unit_factor))
 check(paste(id,'additional growth independent of macro productivity'),near(d$intensity_factor,
   high$subfunctions$intensity_factor[high$subfunctions$id==id]))
}
check('Full service productivity offset eliminates hospital real input-cost growth',near(subset(c$subfunctions,id=='h_hospital')$real_unit_factor,rep(1,37)))
check('Zero productivity eliminates wage-productivity effects',near(zero$subfunctions$real_unit_factor[zero$subfunctions$id=='h_hospital'],rep(1,37)))
check('Efficiency improves hospital spending outcome',tail(subset(c$components,programme=='Health')$spending_bn,1)<tail(subset(b$components,programme=='Health')$spending_bn,1))
check('Additional growth increases spending with efficiency fixed',tail(subset(extra$components,programme=='Health')$spending_bn,1)>tail(subset(c$components,programme=='Health')$spending_bn,1))
check('Productivity does not change nominal GDP path',near(c$path$nominal_gdp,high$path$nominal_gdp))
check('Controls do not change 2030 allocations',near(subset(b$subfunctions,year==2030)$spending_bn,subset(extra$subfunctions,year==2030)$spending_bn))
f<-setdiff(names(spending_factor_labels()),'gdp_factor')
for(z in list(b,c,high,extra,zero)){
 check(paste('Factor product reconstructs all spending',nrow(checks)),near(z$subfunctions$spending_bn,z$subfunctions$anchor_2030_bn*apply(z$subfunctions[,f],1,prod)))
 for(unit in c('pct','bn','real_bn')){
  k<-spending_contributions(z,2030,2066,unit,include_financing=TRUE)
  endpoints<-k[!duplicated(k$id),]
  check(paste('Shapley all-spending reconciliation',unit,nrow(checks)),near(sum(k$contribution),sum(endpoints$change)))
 }
}
baseline_bench<-service_cost_comparison(b,x);moved_bench<-service_cost_comparison(high,x)
check('Reference comparators do not refit with scenario',identical(baseline_bench$reference_benchmark_growth,moved_bench$reference_benchmark_growth))
check('Reference implied growth remains fixed',identical(baseline_bench$reference_combined_non_demographic_growth,moved_bench$reference_combined_non_demographic_growth))
check('No unpublished health coefficient',all(is.na(subset(baseline_bench,benchmark_rule=='health_trend')$reference_benchmark_growth)))
check('School funding comparator equals 75% real wage growth',near(subset(baseline_bench,id=='e_school')$reference_benchmark_growth,.009))
check('NDIS comparator equals reference real earnings growth',near(subset(baseline_bench,id=='n_ndis')$reference_benchmark_growth,.012))
check('Service-control attribution reconciles without overlapping groups',near(sum(scenario_attribution(x,theta,cal)$net_debt_change_pp),tail(c$path$net_debt_pct-project_model(x,default_assumptions(),cal)$path$net_debt_pct,1)))
write.csv(checks,'outputs/service_cost_validation_checks.csv',row.names=FALSE)
write.csv(baseline_bench,'outputs/service_cost_baseline_and_benchmarks.csv',row.names=FALSE)
cat(nrow(checks),'service-cost framework checks passed.\n')
