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
       d$wage_factor<-m$productivity_index^.5;d$real_unit_factor<-d$wage_factor
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
