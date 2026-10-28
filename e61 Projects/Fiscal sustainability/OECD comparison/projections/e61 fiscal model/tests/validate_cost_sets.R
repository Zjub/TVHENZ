args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions()
n<-0;check<-function(ok,label){if(!isTRUE(ok))stop(label);n<<-n+1}
b<-project_model(x,a,cal)
check(a$health_service_productivity_share==.25 && a$education_service_productivity_share==.25,'Health/education non-zero efficiency defaults')
check(all(subset(b$subfunctions,programme%in%c('Health','Education'))$additional_service_growth==.005),'Health/education non-zero cost defaults')
for(g in c(.0,.012,.017)){
 z<-project_model(x,modifyList(a,list(productivity=g)),cal);d<-subset(z$subfunctions,id=='n_ndis');m<-z$path
 expected<-d$anchor_2030_bn[1]*d$exposure_index*m$domestic_price_index*(.15+.85*m$productivity_index)*1.005^(m$year-2030)
 check(max(abs(d$spending_bn-expected))<1e-8,'NDIS matches original participant x plan-cost x additional-use formula')
 check(all(d$efficiency_factor==1) && all(d$service_productivity_share==0),'No NDIS efficiency deduction')
 s<-subset(z$subfunctions,id=='s_other')
 expected<-s$anchor_2030_bn[1]*s$exposure_index*m$domestic_price_index*m$productivity_index^.65
 check(max(abs(s$spending_bn-expected))<1e-8,'Original cash-transfer rule preserved')
}
eff<-project_model(x,modifyList(a,list(health_service_productivity_share=.75,education_service_productivity_share=.75)),cal)
for(name in c('NDIS','Other social protection','Age Pension')){
 k<-b$components$programme==name
 check(identical(eff$components$spending_bn[k],b$components$spending_bn[k]),paste(name,'unaffected by sector efficiency'))
}
h<-project_model(x,modifyList(a,list(service_cost_set='historical')),cal)
adj<-project_model(x,modifyList(a,list(service_cost_set='historical',health_service_growth_adjustment=.002)),cal)
check(identical(b$path$nominal_gdp,h$path$nominal_gdp),'Cost set leaves GDP unchanged')
check(identical(subset(b$subfunctions,year==2030)$spending_bn,subset(h$subfunctions,year==2030)$spending_bn),'Common anchor')
for(id in x$historical_service_cost_parameters$id){
 d<-h$subfunctions[h$subfunctions$id==id,];r<-x$historical_service_cost_parameters$additional_service_growth[match(id,x$historical_service_cost_parameters$id)]
 check(max(abs(d$additional_service_growth-r))<1e-12,paste(id,'historical rate'))
 if(grepl('^h_',id))check(max(abs(adj$subfunctions$additional_service_growth[adj$subfunctions$id==id]-r-.002))<1e-12,paste(id,'preserves relative adjustment'))
}
# Exact restoration of the reference before the disputed efficiency/indexation changes.
oldx<-x;oldx$service_cost_parameters$additional_service_growth[grepl('^e_',oldx$service_cost_parameters$id)]<-0
old<-project_model(oldx,modifyList(a,list(health_service_productivity_share=0,education_service_productivity_share=0)),cal)
check(abs(tail(old$path$net_debt_pct,1)-56.90783)<.0001,'Earlier complete reference can be reconstructed')
for(change in list(list(ndis_service_productivity_share=.25),list(social_service_productivity_share=.25),list(ndis_cost_rule='earnings_index'),list(transfer_cost_rule='cpi_earnings_mix')))
 check(inherits(try(project_model(x,modifyList(a,change),cal),silent=TRUE),'try-error'),'Transfer efficiency/new indexation rejected')
for(unit in c('pct','bn','real_bn')){
 d<-spending_contributions(h,measure=unit);s<-aggregate(contribution~id,d,sum);t<-d[!duplicated(d$id),]
 check(max(abs(s$contribution-t$change[match(s$id,t$id)]))<1e-6,'Factor decomposition reconciles')
}
ref<-service_cost_comparison(b,x);hc<-service_cost_comparison(h,x)
check(max(abs(ref$reference_combined_non_demographic_growth-hc$reference_combined_non_demographic_growth))<1e-12,'Reference cost comparisons fixed')
d<-scenario_attribution(x,modifyList(a,list(service_cost_set='historical')),cal)
check(abs(sum(d$net_debt_change_pp)-tail(h$path$net_debt_pct-b$path$net_debt_pct,1))<1e-8,'Debt attribution reconciles')
cat('PASS:',n,'sector defaults and transfer-restoration checks.\n')
