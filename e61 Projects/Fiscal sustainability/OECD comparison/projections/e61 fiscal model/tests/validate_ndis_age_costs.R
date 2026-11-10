args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();a<-default_assumptions();cal<-calibrate(x)
n<-0;check<-function(v,label){if(!isTRUE(v))stop(label);n<<-n+1L}
near<-function(x,y,tol=1e-8)max(abs(x-y))<tol
w<-x$ndis_age_costs
check(nrow(w)==9 && sum(w$observed_participants)==739414,'Nine national bands reconcile to published ALL count')
check(near(w$annual_cost_proxy,w$annual_plan_budget*w$utilisation),'Budget and utilisation proxy reconciles')
check(near(weighted.mean(w$relative_cost_weight,w$observed_participants),1),'Cost weights normalised using observed participants')
check(w$annual_cost_proxy[w$age_min==55]>w$annual_cost_proxy[w$age_min==0],'Source distinguishes child and adult costs')
check(all(w$relative_cost_weight>0 & is.finite(w$relative_cost_weight)),'Finite positive weights')
flat<-x;flat$ndis_age_costs$relative_cost_weight<-1;flat$ndis_age_costs$annual_cost_proxy<-1
flat$ndis<-ndis_projection(flat)
check(identical(x$ndis$participants,flat$ndis$participants),'Cost weights do not alter participant counts')
check(identical(x$ndis$participants_65plus,flat$ndis$participants_65plus),'Older participant retention unchanged')
check(all(flat$ndis$age_cost_mix_index==1),'Flat costs remove all age-cost mix effects')
check(isTRUE(all.equal(calibrate(flat),cal)),'2030 allocations and all programme anchors unchanged')
p<-project_model(x,a,cal);pf<-project_model(flat,a,cal)
d<-subset(p$subfunctions,programme=='NDIS');f<-subset(pf$subfunctions,programme=='NDIS')
check(near(d$spending_bn/f$spending_bn,d$ndis_age_cost_mix_factor),'Spending change isolates age cost mix exactly')
check(identical(p$path$nominal_gdp,pf$path$nominal_gdp),'NDIS does not alter nominal GDP')
check(near(subset(p$components,programme!='NDIS')$spending_bn,subset(pf$components,programme!='NDIS')$spending_bn),'Other primary spending unchanged')
check(d$spending_bn[1]==f$spending_bn[1],'2030 spending unchanged')
check(tail(d$spending_bn,1)>tail(f$spending_bn,1),'More expensive future mix raises spending')
check(near(d$exposure_index,d$ndis_participant_factor*d$ndis_age_cost_mix_factor),'Participant and age-cost factors reconcile')
detail<-attr(x$ndis,'age_profiles')
for(y in 2025:2066){
 z<-subset(detail,year==y);s<-subset(x$ndis,year==y)
 check(near(sum(z$participants),s$participants,1e-6),'Age counts reconcile')
 check(near(sum(z$cost_weighted_participants),s$cost_weighted_participants,1e-6),'Age weighted counts reconcile')
 check(near(weighted.mean(z$annual_cost_proxy,z$participants),s$average_age_cost_proxy),'Age average cost reconciles')
}
check(length(unique(detail$relative_cost_weight[detail$age>=65]))==1,'One pooled older cost weight; no unsupported extrapolation')
scaled<-economic_inputs(x,modifyList(a,list(population_growth=.02)));recomputed<-ndis_projection(scaled)
check(near(scaled$ndis$cost_weighted_participants,recomputed$cost_weighted_participants,1e-6),'Neutral scaling equals full weighted cohort recomputation')
check(near(scaled$ndis$age_cost_mix_index,x$ndis$age_cost_mix_index),'Neutral growth preserves age cost mix')
profiles<-ndis_age_projection_data(x,a)
check(all(abs(tapply(profiles$participant_share,profiles$year,sum)-1)<1e-12),'Annual participant shares sum to one')
check(all(abs(tapply(profiles$cost_proxy_share,profiles$year,sum)-1)<1e-12),'Annual spending proxy shares sum to one')
# A change in age mix must affect costs even when total participant numbers do not.
counts<-matrix(0,101,2,dimnames=list(0:100,c(2030,2031)))
counts[9,1]<-100;counts[36,2]<-100
toy_pop<-expand.grid(age=0:100,year=2030:2031);toy_pop$population<-1000
toy<-ndis_cost_summary(counts,toy_pop,x,2030:2031)
check(toy$participants[1]==toy$participants[2] && toy$age_cost_mix_index[2]>1,'Fixed count counterfactual isolates age cost mechanism')
con<-subset(spending_contributions(p,2030,2066,'pct'),programme=='NDIS')
check(near(sum(con$contribution),tail(d$share_gdp,1)-d$share_gdp[1]),'Shapley contributions reconcile with age costs')
override<-project_model(x,modifyList(a,list(spending_gdp_targets=setNames(2,'NDIS'))),cal)
check(all(abs(subset(override$components,programme=='NDIS' & year>2030)$share_gdp-2)<1e-10),'Fixed-share policy still overrides cost projection')
impact<-merge(subset(p$components,programme=='NDIS')[c('year','spending_bn','share_gdp')],
 subset(pf$components,programme=='NDIS')[c('year','spending_bn','share_gdp')],by='year',suffixes=c('_age_costs','_flat_costs'))
impact$age_cost_mix_index<-d$ndis_age_cost_mix_factor
write.csv(impact,'outputs/ndis_age_cost_impact.csv',row.names=FALSE)
cat('PASS:',n,'NDIS age-cost source, cohort, accounting and scenario checks.\n')
