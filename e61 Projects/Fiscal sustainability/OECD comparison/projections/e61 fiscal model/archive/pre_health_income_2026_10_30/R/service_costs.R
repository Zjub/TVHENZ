# Constant-input basket: labour requirements fall with service productivity.
# Indices equal one at the common 2030 anchor.
effective_service_parameters <- function(x,a) {
 p<-x$service_cost_parameters
 p$growth_evidence<-'Arbitrary modelling default; not an empirical estimate'
 if(a$service_cost_set=='historical'){
   h<-x$historical_service_cost_parameters
   stopifnot(!anyDuplicated(h$id),setequal(h$id,p$id[grepl('^[he]_',p$id)]),all(is.finite(h$additional_service_growth)))
   k<-match(h$id,p$id);p$additional_service_growth[k]<-h$additional_service_growth
   p$growth_evidence[k]<-paste('Historical net residual, 2016-2025, conditional on arbitrary 25% service-productivity assumption;',h$evidence_class)
 }
 p$cost_rule[p$id=='n_ndis']<-'ndis_transfer'
 p
}
service_cost_factors <- function(z,m,a) {
 t<-m$year-a$anchor_year;programme<-z$programme
 theta<-z$service_productivity_share+a[[service_productivity_parameters()[[programme]]]]
 r<-z$additional_service_growth+a[[cost_pass_through_parameters()[[programme]]]]
 stopifnot(is.finite(theta),theta>=0,theta<=1.5,is.finite(r),r> -1)
 L<-z$wage_weight;W<-m$productivity_index;Q<-(1+theta*a$productivity)^t
 if(z$cost_rule=='input_mix') {
   wage<-(1-L)+L*W
   efficiency<-((1-L)+L*W/Q)/wage
 } else if(z$cost_rule=='funding_proxy') {
   wage<-(1+L*a$productivity)^t;efficiency<-rep(1,length(t))
 } else if(z$cost_rule=='ndis_transfer') {
   # Original plan-cost index, with no service-efficiency deduction.
   wage<-(1-L)+L*W;efficiency<-rep(1,length(t))
 } else if(z$cost_rule=='indexation_proxy') {
   weight<-if(a$policy=='standards')1 else L
   wage<-W^weight;efficiency<-rep(1,length(t))
 } else if(z$cost_rule=='cpi_proxy') {
   wage<-efficiency<-rep(1,length(t))
 } else stop('Unknown service cost rule')
 data.frame(wage_factor=wage,efficiency_factor=efficiency,
   real_unit_factor=wage*efficiency,intensity_factor=(1+r)^t,
   configured_service_productivity_share=theta,
   service_productivity_share=if(z$cost_rule=='input_mix')theta else 0,
   service_productivity_growth=if(z$cost_rule=='input_mix')theta*a$productivity else 0,
   additional_service_growth=r,wage_weight=L,cost_rule=z$cost_rule)
}

project_primary_spending <- function(x,m,a,cal,pp,pension) {
 spec<-cal$subfunctions;hp<-effective_service_parameters(x,a)
 expected<-spec$id[!spec$programme%in%c('Age Pension','Defence')]
 stopifnot(!anyDuplicated(hp$id),setequal(hp$id,expected),
   all(is.finite(hp$wage_weight)),all(hp$wage_weight>=0&hp$wage_weight<=1),
   all(is.finite(hp$service_productivity_share)),all(is.finite(hp$additional_service_growth)))
 detail<-do.call(rbind,lapply(seq_len(nrow(spec)),function(j){
   s<-spec[j,];q<-pp[match(s$programme,pp$programme),]
   exposure<-subfunction_demand(x,s$profile,m$year);exposure<-exposure/exposure[1]
   pop<-m$population_index;age<-exposure/pop;price<-m$domestic_price_index
   coverage<-wage<-efficiency<-intensity<-target<-rep(1,nrow(m))
   L<-theta<-service_growth<-r<-NA_real_;rule<-'Own policy rule'
   if(s$id%in%hp$id){
     z<-hp[match(s$id,hp$id),];z$programme<-s$programme;f<-service_cost_factors(z,m,a)
     wage<-f$wage_factor;efficiency<-f$efficiency_factor;intensity<-f$intensity_factor
     coverage<-(1+z$coverage_growth)^(m$year-a$anchor_year)
     L<-z$wage_weight;theta<-f$service_productivity_share[1]
     service_growth<-f$service_productivity_growth[1];r<-f$additional_service_growth[1];rule<-z$cost_rule
   }
   if(s$programme=='Age Pension'){
     coverage<-pension$coverage/pension$coverage[1]
     wage<-pension$payment_index/(exposure*coverage)/price
   }
   if(q$kind=='target'){
     pop[]<-age[]<-price[]<-1
     target<-m$nominal_gdp*approx(c(2030,2035),c(q$anchor_2030_bn/cal$gdp0,.025),xout=m$year,rule=2)$y/s$anchor_2030_bn
   }
   value<-s$anchor_2030_bn*pop*age*coverage*price*wage*efficiency*intensity*target
   data.frame(year=m$year,programme=s$programme,id=s$id,subfunction=s$subfunction,
     anchor_2030_bn=s$anchor_2030_bn,spending_bn=value,share_gdp=100*value/m$nominal_gdp,
     exposure_index=exposure,coverage_index=coverage,cost_pass_through=NA_real_,
     wage_weight=L,service_productivity_share=theta,service_productivity_growth=service_growth,
     additional_service_growth=r,cost_rule=rule,population_factor=pop,age_factor=age,
     coverage_factor=coverage,price_factor=price,wage_factor=wage,efficiency_factor=efficiency,
     real_unit_factor=wage*efficiency,intensity_factor=intensity,policy_factor=target,
     relative_price_factor=1,aggregate_cost_factor=1)
 }))
 detail$unconstrained_spending_bn<-detail$spending_bn
 detail$gdp_target_active<-FALSE;detail$gdp_target_pct<-NA_real_
 targets<-a$spending_gdp_targets
 if(a$other_spending_rule=='gdp_share' && is.na(targets['Other public services']))
   targets['Other public services']<-100*sum(detail$spending_bn[detail$programme=='Other public services' & detail$year==a$anchor_year])/m$nominal_gdp[1]
 for(programme in names(targets)){
   target<-targets[[programme]]
   if(is.na(target))next
   for(y in m$year[m$year>a$anchor_year]){
     k<-detail$programme==programme & detail$year==y
     total<-sum(detail$spending_bn[k]);desired<-target/100*m$nominal_gdp[m$year==y]
     if(total>0){
       multiplier<-desired/total
       detail$spending_bn[k]<-detail$spending_bn[k]*multiplier
       detail$policy_factor[k]<-detail$policy_factor[k]*multiplier
     } else {
       # A positive policy target can fund a function whose modelled eligibility
       # has fallen to zero. Allocate by anchor shares and record the policy rule.
       for(f in c('population_factor','age_factor','coverage_factor','price_factor',
                   'wage_factor','efficiency_factor','intensity_factor'))detail[k,f]<-1
       detail$real_unit_factor[k]<-1
       detail$spending_bn[k]<-desired*detail$anchor_2030_bn[k]/sum(detail$anchor_2030_bn[k])
       detail$policy_factor[k]<-detail$spending_bn[k]/detail$anchor_2030_bn[k]
     }
     detail$share_gdp[k]<-100*detail$spending_bn[k]/m$nominal_gdp[m$year==y]
     detail$gdp_target_active[k]<-TRUE;detail$gdp_target_pct[k]<-target
   }
 }
 components<-do.call(rbind,lapply(seq_len(nrow(pp)),function(j){
   q<-pp[j,];d<-detail[detail$programme==q$programme,]
   spend<-as.numeric(tapply(d$spending_bn,d$year,sum))
   exp<-as.numeric(tapply(d$anchor_2030_bn*d$exposure_index,d$year,sum))/q$anchor_2030_bn
   cov<-if(q$programme=='Age Pension')pension$coverage/pension$coverage[1] else rep(1,nrow(m))
   # Aggregate unit cost is descriptive; subfunction factors drive accounting.
   data.frame(year=m$year,programme=q$programme,spending_bn=spend,cost_pass_through=NA_real_,
     exposure_index=exp,unit_cost_index=spend/(q$anchor_2030_bn*exp*cov),
     coverage_index=cov,intensity_index=1,share_gdp=100*spend/m$nominal_gdp)
 }))
 list(components=components,subfunctions=detail)
}

spending_factor_labels <- function() c(population_factor='Population size',age_factor='Age / cohort demand',
 coverage_factor='Coverage / eligibility',price_factor='General prices',
 wage_factor='Real wages / funding indexation',efficiency_factor='Service productivity',
 intensity_factor='Additional service growth',policy_factor='Policy / financing rule',gdp_factor='GDP denominator')

service_cost_comparison <- function(projection,inputs) {
 s<-projection$calibration$subfunctions;p<-effective_service_parameters(inputs,projection$assumptions)
 p<-merge(s[,c('id','programme','subfunction')],p,by='id',sort=FALSE)
 m<-projection$path;a<-projection$assumptions;t<-tail(m$year,1)-a$anchor_year
 # Benchmarks are evaluated once at reference macro assumptions, independent
 # of the selected scenario. No residual is refitted when a control moves.
 g<-default_assumptions()$productivity
 refm<-macro_path(inputs,default_assumptions())
 rows<-lapply(seq_len(nrow(p)),function(i){z<-p[i,];f<-service_cost_factors(z,m,a)
   input_growth<-tail(f$real_unit_factor,1)^(1/t)-1
   combined<-tail(f$real_unit_factor*f$intensity_factor,1)^(1/t)-1
   refp<-effective_service_parameters(inputs,default_assumptions())
   zref<-refp[match(z$id,refp$id),];zref$programme<-z$programme
   fref<-service_cost_factors(zref,refm,default_assumptions())
   reference_growth<-tail(fref$real_unit_factor*fref$intensity_factor,1)^(1/t)-1
   benchmark<-switch(z$benchmark_rule,earnings=g,school_index=.75*g,cpi_grants=0,vet_index=.6975*g,NA_real_)
   data.frame(z,selected_service_productivity_share=f$service_productivity_share[1],
     selected_service_productivity_growth=f$service_productivity_growth[1],
     selected_additional_service_growth=f$additional_service_growth[1],
     baseline_additional_growth=z$additional_service_growth,
     additional_growth_adjustment=a[[cost_pass_through_parameters()[[z$programme]]]],
     real_unit_growth=input_growth,combined_non_demographic_growth=combined,
     reference_combined_non_demographic_growth=reference_growth,
     reference_benchmark_growth=benchmark,
     spending_gdp_target_pct={k<-projection$subfunctions$programme==z$programme & projection$subfunctions$gdp_target_active
       if(any(k))projection$subfunctions$gdp_target_pct[which(k)[1]] else NA_real_},
     benchmark_gap=if(is.finite(benchmark))combined-benchmark else NA_real_,
     benchmark_status=if(z$benchmark_rule=='health_trend')'Method published; coefficient unavailable' else
       if(z$benchmark_rule=='none')'No matching comparator' else 'Rule / scope proxy, not a fitted consolidated target')
 })
 do.call(rbind,rows)
}

format_service_cost_comparison <- function(d) data.frame(Subfunction=d$subfunction,
 Rule=d$cost_rule,'Labour/index weight (%)'=round(100*d$wage_weight,2),
 'Service productivity / economy (%)'=round(100*d$selected_service_productivity_share,1),
 'Baseline additional growth (%/yr)'=round(100*d$baseline_additional_growth,2),
 'Aggregate adjustment (pp/yr)'=round(100*d$additional_growth_adjustment,2),
 'Additional growth (%/yr)'=round(100*d$selected_additional_service_growth,2),
 'Reference combined growth (%/yr)'=round(100*d$reference_combined_non_demographic_growth,2),
 'Scenario combined growth (%/yr)'=round(100*d$combined_non_demographic_growth,2),
 'Comparator at reference macro (%/yr)'=round(100*d$reference_benchmark_growth,2),
 'Fixed spending target (% GDP)'=ifelse(is.na(d$spending_gdp_target_pct),'Projection rule',formatC(d$spending_gdp_target_pct,digits=2,format='f')),
 'Growth evidence'=d$growth_evidence,
 'Comparator scope / status'=paste(d$benchmark_status,d$benchmark_scope,sep='; '),check.names=FALSE)
