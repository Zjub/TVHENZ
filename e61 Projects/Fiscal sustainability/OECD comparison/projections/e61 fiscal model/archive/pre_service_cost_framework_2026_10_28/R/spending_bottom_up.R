# Subfunction construction and exact factor accounting. Monetary values AUD bn.
subfunction_demand <- function(x,profile,years) {
 if(profile%in%c('hospital','medical','pharma'))
   return(profile_exposure(x,x$subfunction_age_profiles[x$subfunction_age_profiles$profile==profile,],years))
 if(profile=='aged_care')return(profile_exposure(x,x$care_profile,years))
 if(profile=='ndis')return(x$ndis$participants[match(years,x$ndis$year)])
 p<-x$population
 weight<-switch(profile,school_age=as.numeric(p$age>=5 & p$age<=17),
   tertiary_age=as.numeric(p$age>=18 & p$age<=24),pension=as.numeric(p$age>=67),
   vet_age=as.numeric(p$age>=15 & p$age<=64),
   working_age=as.numeric(p$age>=15 & p$age<67),population=rep(1,nrow(p)),target=rep(1,nrow(p)),
   stop('Unknown demand profile: ',profile))
 totals<-tapply(p$population*weight,p$year,sum)
 as.numeric(totals[as.character(years)])
}

calibrate_subfunctions <- function(x,cal) {
 d<-x$subfunctions;pp<-cal$programmes
 stopifnot(!anyDuplicated(d$id),all(d$base_2025_bn>0),setequal(unique(d$programme),pp$programme))
 d$source_2025_bn<-d$base_2025_bn
 d$parent_control_2025_bn<-pp$base_2025_bn[match(d$programme,pp$programme)]
 d$parent_reconciliation_factor<-d$parent_control_2025_bn/as.numeric(tapply(d$base_2025_bn,d$programme,sum)[d$programme])
 d$base_2025_bn<-d$base_2025_bn*d$parent_reconciliation_factor
 d$demand_2025<-d$demand_2030<-numeric(nrow(d))
 for(i in seq_len(nrow(d))){v<-subfunction_demand(x,d$profile[i],c(2025,2030));d$demand_2025[i]<-v[1];d$demand_2030[i]<-v[2]}
 q<-pp[match(d$programme,pp$programme),]
 d$raw_2030_bn<-d$base_2025_bn*(d$demand_2030/d$demand_2025)*1.025^5*1.012^(5*q$wage_pass_through)*
   1.003^ifelse(q$kind%in%c('service','mixed'),5,0)*(1+q$excess_cost_growth)^5
 d$anchor_2030_bn<-d$raw_2030_bn/as.numeric(tapply(d$raw_2030_bn,d$programme,sum)[d$programme])*q$anchor_2030_bn
 d$anchor_reconciliation_factor<-d$anchor_2030_bn/d$raw_2030_bn
 d$parent_share_2030<-d$anchor_2030_bn/q$anchor_2030_bn
 d
}

project_primary_spending <- function(x,m,a,cal,pp,pension) {
 spec<-cal$subfunctions;t<-m$year-a$anchor_year;keys<-cost_pass_through_parameters()
 hp<-x$health_education_parameters
 stopifnot(!anyDuplicated(hp$id),setequal(hp$id,spec$id[spec$programme%in%c('Health','Education')]),
   all(hp$cost_rule%in%c('input_mix','cpi_proxy','funding_proxy')),
   all(is.finite(as.matrix(hp[,c('wage_weight','service_productivity_growth','coverage_growth','intensity_growth','relative_price_loading')]))),
   all(hp$wage_weight>=0&hp$wage_weight<=1),all(hp$service_productivity_growth> -1),
   all(hp$coverage_growth> -1),all(hp$intensity_growth> -1),all(hp$relative_price_loading>=0))
 rows<-lapply(seq_len(nrow(spec)),function(j){
   s<-spec[j,];q<-pp[match(s$programme,pp$programme),]
   exposure<-subfunction_demand(x,s$profile,m$year);exposure<-exposure/exposure[1]
   pass<-if(a$policy=='standards' && q$kind=='mixed')1 else q$wage_pass_through
   coverage<-rep(1,nrow(m));real_unit<-m$productivity_index^pass
   intensity<-(1+if(s$programme=='NDIS')a$ndis_intensity_growth else q$excess_cost_growth)^t
   rel<-if(q$kind%in%c('service','mixed'))(1+a$relative_price)^t else rep(1,nrow(m))
   pop<-m$population_index;age<-exposure/pop;price<-m$domestic_price_index
   target<-aggregate_cost<-rep(1,nrow(m));wage_weight<-service_productivity<-NA_real_
   if(s$id%in%hp$id){
     z<-hp[match(s$id,hp$id),];wage_weight<-z$wage_weight;service_productivity<-z$service_productivity_growth
     # Input quantities held fixed per service: a base-period input-cost basket.
     # Funding proxy instead compounds the stated wage/CPI weighting each year.
     real_unit<-if(z$cost_rule=='funding_proxy')(1+wage_weight*a$productivity)^t else
       (1-wage_weight)+wage_weight*m$productivity_index
     real_unit<-real_unit/(1+service_productivity)^t
     coverage<-(1+z$coverage_growth)^t
     intensity<-(1+z$intensity_growth)^t
     rel<-(1+a$relative_price)^(t*z$relative_price_loading)
     key<-keys[[s$programme]];aggregate_cost<-(1+a[[key]])^t
     pass<-NA_real_
   }
   if(s$programme=='Age Pension'){
     coverage<-pension$coverage/pension$coverage[1]
     mean_payment<-ifelse(exposure*coverage>0,pension$payment_index/(exposure*coverage),1)
     real_unit<-mean_payment/price;rel[]<-1;intensity[]<-1
   }
   if(q$kind=='target'){
     pop[]<-age[]<-price[]<-real_unit[]<-rel[]<-intensity[]<-1
     target<-m$nominal_gdp*approx(c(2030,2035),c(q$anchor_2030_bn/cal$gdp0,.025),xout=m$year,rule=2)$y/s$anchor_2030_bn
   }
   value<-s$anchor_2030_bn*pop*age*coverage*price*real_unit*rel*intensity*target*aggregate_cost
   data.frame(year=m$year,programme=s$programme,id=s$id,subfunction=s$subfunction,
     anchor_2030_bn=s$anchor_2030_bn,spending_bn=value,share_gdp=100*value/m$nominal_gdp,
     exposure_index=exposure,coverage_index=coverage,
     cost_pass_through=if(s$programme%in%names(keys))pass else NA_real_,
     wage_weight=wage_weight,service_productivity_growth=service_productivity,
     population_factor=pop,age_factor=age,coverage_factor=coverage,price_factor=price,
     real_unit_factor=real_unit,relative_price_factor=rel,intensity_factor=intensity,policy_factor=target,
     aggregate_cost_factor=aggregate_cost)
 })
 detail<-do.call(rbind,rows)
 # Effective aggregates retain the old driver-audit API; detailed factors below
 # are the authoritative decomposition and sum across separately modelled units.
 aggregate_rows<-lapply(seq_len(nrow(pp)),function(j){
   q<-pp[j,];d<-detail[detail$programme==q$programme,]
   spend<-as.numeric(tapply(d$spending_bn,d$year,sum))
   exp<-as.numeric(tapply(d$anchor_2030_bn*d$exposure_index,d$year,sum))/q$anchor_2030_bn
   cov<-if(q$programme=='Age Pension')pension$coverage/pension$coverage[1] else rep(1,nrow(m))
   intensity<-(1+if(q$programme=='NDIS')a$ndis_intensity_growth else q$excess_cost_growth)^t
   cost<-ifelse(exp*cov*intensity>0,spend/(q$anchor_2030_bn*exp*cov*intensity),1)
   pass<-if(q$programme%in%c('Health','Education'))NA_real_ else if(a$policy=='standards' && q$kind=='mixed')1 else q$wage_pass_through
   data.frame(year=m$year,programme=q$programme,spending_bn=spend,
     cost_pass_through=if(q$programme%in%names(keys))pass else NA_real_,exposure_index=exp,
     unit_cost_index=cost,coverage_index=cov,intensity_index=intensity,share_gdp=100*spend/m$nominal_gdp)
 })
 list(components=do.call(rbind,aggregate_rows),subfunctions=detail)
}

spending_factor_labels <- function() c(population_factor='Population size',age_factor='Age / cohort demand',
 coverage_factor='Coverage / eligibility',price_factor='General prices',real_unit_factor='Real unit cost / payment',
 relative_price_factor='Relative service prices',intensity_factor='Service intensity',
 policy_factor='Policy / financing rule',aggregate_cost_factor='Aggregate cost scaling',gdp_factor='GDP denominator')

product_shapley <- function(initial,final,anchor) {
 # Symmetric allocation of interactions, including zero-valued factors.
 result<-rep(0,length(initial));active<-which(initial!=final);n<-length(active)
 if(!n)return(result)
 for(j in seq_len(n)) {
   others<-setdiff(seq_len(n),j);contribution<-0
   for(mask in 0:(2^length(others)-1L)){
     switched<-if(length(others))others[as.logical(as.integer(intToBits(mask))[seq_along(others)])] else integer()
     v<-initial;if(length(switched))v[active[switched]]<-final[active[switched]]
     before<-prod(v);v[active[j]]<-final[active[j]];after<-prod(v)
     k<-length(switched);contribution<-contribution+factorial(k)*factorial(n-k-1)/factorial(n)*(after-before)
   }
   result[active[j]]<-anchor*contribution
 }
 result
}

spending_contributions <- function(projection,start_year=2030L,end_year=2066L,measure='pct',include_financing=FALSE) {
 stopifnot(start_year<end_year,measure%in%c('pct','bn','real_bn'))
 m<-projection$path;detail<-projection$subfunctions
 if(!all(c(start_year,end_year)%in%m$year))stop('Contribution years outside projection')
 if(include_financing){
   for(n in c('Interest','Net capital investment')){
     d<-detail[detail$id==detail$id[1],];d$programme<-d$subfunction<-n;d$id<-n
     for(f in setdiff(names(spending_factor_labels()),'gdp_factor'))d[[f]]<-1
     amount<-if(n=='Interest')m$interest_bn else m$net_capital_investment_bn
     d$anchor_2030_bn<-amount[1];d$spending_bn<-amount;d$share_gdp<-100*amount/m$nominal_gdp
     if(n=='Interest')d$policy_factor<-amount/amount[1] else {
       d$population_factor<-m$population_index;d$price_factor<-m$domestic_price_index
       d$real_unit_factor<-m$productivity_index^.5
     }
     detail<-rbind(detail,d)
   }
 }
 factors<-names(spending_factor_labels());detail$gdp_factor<-1
 if(measure=='pct')detail$gdp_factor<-projection$calibration$gdp0/m$nominal_gdp[match(detail$year,m$year)]
 if(measure=='real_bn')detail$price_factor<-detail$price_factor/m$domestic_price_index[match(detail$year,m$year)]
 rows<-lapply(unique(detail$id),function(id){
   d<-detail[detail$id==id & detail$year%in%c(start_year,end_year),];d<-d[order(d$year),]
   f0<-as.numeric(d[1,factors]);f1<-as.numeric(d[2,factors])
   anchor<-d$anchor_2030_bn[1]*if(measure=='pct')100/projection$calibration$gdp0 else 1
   values<-anchor*c(prod(f0),prod(f1));v<-product_shapley(f0,f1,anchor)
   data.frame(programme=d$programme[1],id,subfunction=d$subfunction[1],factor=unname(spending_factor_labels()),
     contribution=v,factor_start=f0,factor_end=f1,start_value=values[1],end_value=values[2],change=values[2]-values[1],
     start_year,end_year,measure)
 })
 do.call(rbind,rows)
}
