# Model update 1-4. Monetary amounts AUD bn; years are financial-year ending years.
load_inputs <- function(path='data') {
  read <- function(f,processed=TRUE) read.csv(file.path(path,if(processed)'processed' else '',f),
    check.names=FALSE,stringsAsFactors=FALSE)
  x<-list(official=read('official_pbo_nfo_wide.csv'),population=read('population_age_sex.csv'),
    rates=read('observed_labour_rates.csv'),hours=read('observed_hours_by_age.csv'),
    participation=read('participation_targets.csv'),health_profile=read('health_age_cost_profile.csv'),
    care_profile=read('aged_care_age_cost_profile.csv'),education_profile=read('education_age_cost_proxy.csv'),
    pension_benchmark=read('igr_pension_status_benchmark.csv'),tax_history=read('tax_head_history.csv'),
    tax_elasticities=read('tax_elasticity_estimates.csv'),income_distribution=read('ato_income_distribution.csv'),
    policy=read('policy_parameters.csv',FALSE),ndis_incidence=read('ndis_incidence_2025.csv',FALSE),
    purpose=read('historical_gfs_expenses_by_purpose.csv',FALSE),
    operating=read('historical_gfs_operating_statement.csv',FALSE),
    subfunctions=read('spending_subfunctions.csv',FALSE),subfunction_age_profiles=read('subfunction_age_profiles.csv'),
    health_education_parameters=read('health_education_parameters.csv',FALSE))
  x$official<-subset(x$official,level=='national')
  x$labour<-labour_projection(x)
  x$ndis<-ndis_projection(x)
  x
}
default_assumptions <- function() list(productivity=.012,ngdp_per_capita_growth=.032,
  population_growth=.012,hours_growth=0,
  relative_price=.003,tot_change=0,policy='indexed',tax_policy='wage_indexed',interest_rate=.044,
  tot_income_elasticity=.20,resource_elasticity=2,company_tot_elasticity=1,
  super_real_return=.04,ndis_intensity_growth=.005,
  health_cost_growth_adjustment=0,education_cost_growth_adjustment=0,aged_care_pass_through=.8,
  ndis_pass_through=.85,social_pass_through=.65,other_services_pass_through=.7,
  anchor_year=2030L,end_year=2066L)
cost_pass_through_parameters <- function() c(Health='health_cost_growth_adjustment',Education='education_cost_growth_adjustment',
  `Aged care`='aged_care_pass_through',NDIS='ndis_pass_through',
  `Other social protection`='social_pass_through',`Other public services`='other_services_pass_through')
policy_value <- function(x,n) x$policy$value[match(n,x$policy$parameter)]

labour_projection <- function(x) {
  # Observed five-year age/sex rates, projected to published IGR sex-specific
  # structural participation targets by a common odds shift. This is explicitly
  # a bridge to Treasury projections, not a reproduction of its cohort model.
  rows<-lapply(2025:2066,function(y) {
    p<-subset(x$population,year==y & age>=15)
    p$participation<-p$unemployment<-p$hours<-NA_real_
    for(sx in c('Male','Female')) {
      k<-which(p$sex==sx);rr<-subset(x$rates,sex==sx);hh<-subset(x$hours,sex==sx)
      idx<-findInterval(p$age[k],rr$age_min)
      idxh<-findInterval(p$age[k],hh$age_min)
      rates<-rr$participation_rate[idx]
      target<-approx(x$participation$year,x$participation[[sx]],xout=y,rule=2)$y
      shift<-uniroot(function(d)weighted.mean(plogis(qlogis(rates)+d),p$population[k])-target,c(-10,10))$root
      p$participation[k]<-plogis(qlogis(rates)+shift)
      p$unemployment[k]<-rr$unemployment_rate[idx]
      p$hours[k]<-hh$hours_per_worker[idxh]
    }
    p$employed<-p$population*p$participation*(1-p$unemployment)
    data.frame(year=y,labour_force=sum(p$population*p$participation),
      employed=sum(p$employed),raw_hours=sum(p$employed*p$hours),
      participation=sum(p$population*p$participation)/sum(p$population),
      population=sum(subset(x$population,year==y)$population),population_15plus=sum(p$population))
  })
  d<-do.call(rbind,rows)
  # C7.2 gives aggregate average hours trend. Adjust observed age/sex hour
  # composition to that aggregate target, avoiding a second composition effect.
  avg0<-d$raw_hours[d$year==2026]/d$employed[d$year==2026]
  d$average_hours<-avg0*.999^(d$year-2026)
  d$total_hours<-d$employed*d$average_hours*52
  d
}

ndis_projection <- function(x) {
  # Source D.13 incidence applies below 65. Above 65 existing participants age
  # forward with population survival; there are no new entrants aged 65+.
  pop<-aggregate(population~year+age,x$population,sum)
  years<-2025:2066;counts<-matrix(0,101,length(years),dimnames=list(0:100,years))
  p0<-pop$population[match(paste(2025,0:100),paste(pop$year,pop$age))]
  incidence<-x$ndis_incidence$participation_rate[findInterval(0:100,x$ndis_incidence$age_min)]
  counts[,1]<-p0*incidence
  for(i in 2:length(years)) {
    current<-pop$population[match(paste(years[i],0:100),paste(pop$year,pop$age))]
    previous<-pop$population[match(paste(years[i-1],0:100),paste(pop$year,pop$age))]
    counts[1:65,i]<-current[1:65]*incidence[1:65]
    counts[66:100,i]<-counts[65:99,i-1]*current[66:100]/previous[65:99]
    counts[101,i]<-sum(counts[100:101,i-1])*current[101]/sum(previous[100:101])
  }
  data.frame(year=years,participants=colSums(counts),
    participants_65plus=colSums(counts[66:101,,drop=FALSE]))
}

economic_inputs <- function(inputs,assumptions=default_assumptions()) {
  a<-modifyList(default_assumptions(),assumptions);x<-inputs
  if(any(c('ngdp_override','ngdp_growth')%in%names(a)))
    stop('The linked/override NGDP controls were removed. Use ngdp_per_capita_growth, population_growth and hours_growth.')
  # Scale all age/sex cells equally within each year. The sourced age shares
  # still evolve; this is not a migration/fertility/healthy-ageing scenario.
  d<-x$labour;t<-pmax(d$year-a$anchor_year,0)
  base<-d$population[d$year==a$anchor_year]
  target<-base*(1+a$population_growth)^t
  scale<-ifelse(d$year>=a$anchor_year,target/d$population,1)
  x$population$population<-x$population$population*scale[match(x$population$year,d$year)]
  for(n in c('population','population_15plus','labour_force','employed','raw_hours','total_hours'))d[[n]]<-d[[n]]*scale
  h0<-d$average_hours[d$year==a$anchor_year]
  future<-d$year>=a$anchor_year
  d$average_hours[future]<-h0*(1+a$hours_growth)^t[future]
  d$total_hours<-d$employed*d$average_hours*52
  x$labour<-d
  # Uniform within-year population scaling commutes with the NDIS cohort
  # retention recursion, including the 100+ group. Preserve that exact scaling.
  for(n in c('participants','participants_65plus'))
    x$ndis[[n]]<-x$ndis[[n]]*scale[match(x$ndis$year,d$year)]
  x
}

macro_path <- function(inputs,assumptions=default_assumptions(),prepared=FALSE) {
  a<-modifyList(default_assumptions(),assumptions)
  if(any(c(a$productivity,a$ngdp_per_capita_growth,a$population_growth,a$hours_growth,a$relative_price,a$tot_change)<=-1))stop('Growth rates must exceed -100%.')
  if(!prepared)inputs<-economic_inputs(inputs,a)
  d<-subset(inputs$labour,year>=a$anchor_year & year<=a$end_year)
  t<-d$year-a$anchor_year
  A<-(1+a$productivity)^t; Q<-d$total_hours/d$total_hours[1]*A
  T<-(1+a$tot_change)^pmin(t/10,1);income<-T^a$tot_income_elasticity
  g0<-inputs$official$nominal_gdp_billion[inputs$official$year==2030]
  # GDP is determined only by population and nominal GDP per capita. Neither
  # productivity nor hours can change it when these two inputs are fixed.
  per_capita<-(1+a$ngdp_per_capita_growth)^t
  gdp<-g0*(d$population/d$population[1])*per_capita
  P<-(gdp/g0)/(Q*income)
  data.frame(year=d$year,population=d$population,population_index=d$population/d$population[1],
    employed=d$employed,employed_index=d$employed/d$employed[1],participation_rate=d$participation,
    average_hours=d$average_hours,average_hours_index=d$average_hours/d$average_hours[1],
    labour_index=d$total_hours/d$total_hours[1],productivity_index=A,real_output_index=Q,
    nominal_gdp_per_capita=gdp*1e9/d$population,nominal_gdp_per_capita_index=per_capita,
    hours_per_resident=d$total_hours/d$population,
    domestic_price_index=P,tot_index=T,income_effect=income,nominal_gdp=gdp,
    nominal_gdp_growth=c(NA,diff(gdp)/head(gdp,-1)),real_growth=c(NA,diff(Q)/head(Q,-1)),
    domestic_inflation=c(NA,diff(P)/head(P,-1)),gdp_deflator_index=P*income,
    real_income_index=Q*income,nominal_hourly_wage_index=P*A,
    nominal_earnings_index=P*A*d$average_hours/d$average_hours[1],
    government_unit_price_index=P*A^a$other_services_pass_through*(1+a$relative_price)^t)
}

profile_exposure <- function(x,profile,years) {
  p<-x$population
  w<-profile$weight[findInterval(p$age,profile$age_min)]
  totals<-tapply(p$population*w,p$year,sum)
  as.numeric(totals[as.character(years)])
}
programme_exposures <- function(x,years) {
  eligible<-tapply(ifelse(x$population$age>=67,x$population$population,0),x$population$year,sum)
  working<-tapply(ifelse(x$population$age>=15 & x$population$age<67,x$population$population,0),x$population$year,sum)
  population<-tapply(x$population$population,x$population$year,sum)
  data.frame(year=years,Health=profile_exposure(x,x$health_profile,years),
    Education=profile_exposure(x,x$education_profile,years),
    `Age Pension`=as.numeric(eligible[as.character(years)]),
    `Aged care`=profile_exposure(x,x$care_profile,years),
    NDIS=x$ndis$participants[match(years,x$ndis$year)],
    `Other social protection`=as.numeric(working[as.character(years)]),
    `Other public services`=as.numeric(population[as.character(years)]),
    Defence=as.numeric(population[as.character(years)]),check.names=FALSE)
}

income_tax <- function(income,threshold_index=1,year=2030) {
  # Resident basic schedule and LITO; Medicare is a separate revenue block. WATO is
  # excluded because Table 16 does not identify wage-only eligibility. The 2030
  # aggregate calibration absorbs its starting level, not a behavioural estimate.
  lower<-c(18200,45000,135000,190000)*threshold_index
  rates<-c(if(year<=2026).16 else if(year==2027).15 else .14,.30,.37,.45)
  out<-pmax(income-lower[1],0)*rates[1]
  for(j in 2:4)out<-out+pmax(income-lower[j],0)*(rates[j]-rates[j-1])
  # LITO dollar values index with the selected threshold convention.
  lito<-pmax(0,700*threshold_index-.05*pmax(income-37500*threshold_index,0))
  lito[income>45000*threshold_index]<-pmax(0,325*threshold_index-.015*(income[income>45000*threshold_index]-45000*threshold_index))
  pmax(out-lito,0)
}

pension_entitlement <- function(x,financial_assets,other_assets,other_income_annual=0,
    couple=FALSE,homeowner=TRUE,price_index=1,wage_index=1) {
  n<-ifelse(couple,2,1)
  max_fort<-ifelse(couple,2*policy_value(x,'pension_couple_each_fortnight'),policy_value(x,'pension_single_fortnight'))*wage_index
  name<-paste0('asset_free_',ifelse(couple,'couple','single'),'_',ifelse(homeowner,'home','renter'))
  free<-x$policy$value[match(name,x$policy$parameter)]*price_index
  income_free<-ifelse(couple,policy_value(x,'income_free_couple'),policy_value(x,'income_free_single'))*price_index
  deem_free<-ifelse(couple,policy_value(x,'deeming_threshold_couple'),policy_value(x,'deeming_threshold_single'))*price_index
  deemed<-pmin(financial_assets,deem_free)*policy_value(x,'deeming_lower')+
    pmax(financial_assets-deem_free,0)*policy_value(x,'deeming_upper')
  asset_cut<-pmax(financial_assets+other_assets-free,0)*policy_value(x,'asset_taper_fortnight')
  income_taper<-ifelse(couple,2*policy_value(x,'income_taper_couple_each'),policy_value(x,'income_taper_single'))
  income_cut<-pmax((deemed+other_income_annual)/26-income_free,0)*income_taper
  # Combined household income taper is .50 for couples (.25 per person).
  pmax(0,max_fort-pmax(asset_cut,income_cut))*26/n
}

pension_path <- function(x,m,a) {
  # Representative cohort model: actual rules, age eligibility and SG accumulation;
  # distribution is synthetic, calibrated only to the 2026 IGR starting status mix.
  # It is not household microsimulation or a fitted annual coverage decline.
  shares<-subset(x$pension_benchmark,year==2026)[,c('full','assets_tested','income_tested','no_pension')]
  shares<-as.numeric(shares);shares<-shares/sum(shares)
  d<-expand.grid(age=67:100,status=1:4,couple=c(FALSE,TRUE),homeowner=c(FALSE,TRUE))
  d$weight<-shares[d$status]*ifelse(d$couple,.65,.35)*ifelse(d$homeowner,.75,.25)
  free<-x$policy$value[match(paste0('asset_free_',ifelse(d$couple,'couple','single'),'_',ifelse(d$homeowner,'home','renter')),x$policy$parameter)]
  max_fort<-ifelse(d$couple,2*policy_value(x,'pension_couple_each_fortnight'),policy_value(x,'pension_single_fortnight'))
  taper<-policy_value(x,'asset_taper_fortnight')
  assets<-ifelse(d$status==1,.3*free,ifelse(d$status==2,free+.5*max_fort/taper,
    ifelse(d$status==3,.3*free,1.15*(free+max_fort/taper))))
  # Solve the income-tested archetype for a half-rate pension at calibration.
  deem_free<-ifelse(d$couple,policy_value(x,'deeming_threshold_couple'),policy_value(x,'deeming_threshold_single'))
  deemed<-pmin(.8*assets,deem_free)*policy_value(x,'deeming_lower')+pmax(.8*assets-deem_free,0)*policy_value(x,'deeming_upper')
  taper_income<-ifelse(d$couple,2*policy_value(x,'income_taper_couple_each'),policy_value(x,'income_taper_single'))
  other_income<-ifelse(d$status==3,pmax(0,(ifelse(d$couple,policy_value(x,'income_free_couple'),policy_value(x,'income_free_single'))+.5*max_fort/taper_income)*26-deemed),0)
  # Policy parameters are September 2026; uprate price thresholds and pensions to 2030.
  P0<-1.025^4;W0<-(1.025*1.012)^4
  taxable_mean<-weighted.mean(x$income_distribution$mean_income,x$income_distribution$count)*(1.025*1.012)^6
  values<-coverage<-numeric(nrow(m))
  for(i in seq_len(nrow(m))) {
    t<-m$year[i]-2030;retire_year<-m$year[i]-d$age+67
    extra_years<-pmax(0,retire_year-2030)
    rr<-a$super_real_return
    annuity<-if(abs(rr)<1e-9)extra_years else ((1+rr)^extra_years-1)/rr
    saving<-taxable_mean*policy_value(x,'super_guarantee')*(1-policy_value(x,'super_contributions_tax'))*
      annuity*(1+a$productivity)^(extra_years/2)*ifelse(d$couple,2,1)
    retired_years<-pmin(t,pmax(d$age-67,0))
    draw<-(1+rr-policy_value(x,'retirement_drawdown'))^retired_years
    P<-P0*m$domestic_price_index[i]
    W<-W0*max(m$domestic_price_index[1:i],m$nominal_earnings_index[1:i])
    if(a$policy=='standards') {
      per_person<-pension_entitlement(x,.8*assets*P0,.2*assets*P0,other_income*P0,d$couple,d$homeowner,P0,W0)*W/W0
    } else {
      per_person<-pension_entitlement(x,(.8*assets*P0+saving)*draw*m$domestic_price_index[i],
        .2*assets*P0*m$domestic_price_index[i],other_income*P,
        d$couple,d$homeowner,P,W)
    }
    pa<-aggregate(population~age,subset(x$population,year==m$year[i] & age>=67),sum)
    people<-pa$population[match(d$age,pa$age)]*d$weight
    values[i]<-sum(people*per_person)
    coverage[i]<-sum(people[per_person>0])/sum(people)
  }
  data.frame(year=m$year,payment_index=values/values[1],coverage=coverage,
    eligible_population=sapply(m$year,function(y)sum(subset(x$population,year==y & age>=67)$population)))
}

calibrate <- function(inputs) {
  x<-inputs;a<-default_assumptions();b<-subset(x$purpose,year==2025)
  val<-function(n)b$value[b$item==n]/1000
  interest<-subset(x$operating,year==2025 & item=='Interest expenses n.e.c.')$value/1000
  pension<-policy_value(x,'age_pension_expenses_2025');care<-policy_value(x,'aged_care_expenses_2025');ndis<-policy_value(x,'ndis_plan_expenses_2025')
  pp<-data.frame(programme=c('Health','Education','Age Pension','Aged care','NDIS','Other social protection','Other public services','Defence'),
    kind=c('service','service','transfer','service','service','mixed','service','target'),
    wage_pass_through=c(.7,.8,1,.8,.85,.65,.7,0),excess_cost_growth=c(.005,0,0,.003,.005,0,0,0),
    base_2025_bn=c(val('Health'),val('Education'),pension,care,ndis,val('Social Protection')-pension-care-ndis,
      sum(b$value)/1000-interest-val('Health')-val('Education')-val('Social Protection')-val('Defence'),val('Defence')))
  ex<-programme_exposures(x,c(2025,2030))
  growth<-as.numeric(ex[2,-1]/ex[1,-1])
  raw<-pp$base_2025_bn*growth*(1.025)^5*(1.012)^(5*pp$wage_pass_through)*
    (1.003)^ifelse(pp$kind%in%c('service','mixed'),5,0)*(1+pp$excess_cost_growth)^5
  off<-subset(x$official,year==2030);g0<-off$nominal_gdp_billion
  primary<-off$expenses_billion_dollars-off$public_debt_interest_billion_dollars
  pp$anchor_2030_bn<-raw*primary/sum(raw);pp$calibration_factor<-primary/sum(raw)
  # Observed tax and non-tax revenue levels, projected to 2030 at default
  # economic bases and reconciled once to PBO aggregate revenue.
  tax<-subset(x$tax_history,year==2025)[,-1]
  operating<-subset(x$operating,year==2025)
  ov<-function(n)operating$value[operating$item==n]/1000
  revenue0<-c(as.numeric(tax),royalties=ov('Royalty income'),fees_other=ov('Total GFS revenue')-
    sum(as.numeric(tax))-ov('Royalty income')-ov('Interest income'),interest_receipts=ov('Interest income'))
  names(revenue0)[seq_len(ncol(tax))]<-names(tax)
  # Allocation projection uses nominal GDP growth for all non-PIT heads;
  # income tax instead uses ATO distribution and the legislated schedule change.
  projected<-revenue0*(1.025*1.012)^5
  dist<-x$income_distribution
  pit_growth<-sum(dist$count*income_tax(dist$mean_income*(1.025*1.012)^6,1,2030))/
    sum(dist$count*income_tax(dist$mean_income*(1.025*1.012),1,2025))
  projected['personal_income']<-revenue0['personal_income']*pit_growth
  projected<-projected*off$revenue_billion_dollars/sum(projected)
  assets0<-off$gross_debt_billion_dollars-off$net_debt_billion_dollars
  prior<-subset(x$official,year==2029)
  result<-list(programmes=pp,official=off,gdp0=g0,gross_debt0=off$gross_debt_billion_dollars,
    assets0=assets0,interest_receipts0=unname(projected['interest_receipts']),
    asset_yield=unname(projected['interest_receipts'])/assets0,revenue_anchor=projected,
    revenue_observed_2025=revenue0,nci0=off$net_capital_investment_billion_dollars,
    implied_interest_rate=off$public_debt_interest_billion_dollars/prior$gross_debt_billion_dollars)
  result$subfunctions<-calibrate_subfunctions(x,result)
  result
}

revenue_projection <- function(x,m,a,c) {
  # Structural benchmark: constant effective yields and taxable coverage.
  # Historical GDP regressions are descriptive evidence only; none are applied.
  # Consumption and property values have constant GDP ratios (explicit proxies).
  # Excise uses CPI-indexed unit rates and constant taxable units per resident.
  bases<-function(z)list(superannuation=z$nominal_earnings_index*z$employed_index,
    medicare_fbt=z$nominal_earnings_index*z$employed_index,gst=z$nominal_gdp/c$gdp0,
    excises_customs=z$domestic_price_index*z$population_index,payroll=z$nominal_earnings_index*z$employed_index,
    property=z$nominal_gdp/c$gdp0,stamp_duties=z$nominal_gdp/c$gdp0,other_taxes=z$nominal_gdp/c$gdp0,
    royalties=z$domestic_price_index*z$real_output_index*z$tot_index^a$resource_elasticity,
    fees_other=z$domestic_price_index*z$real_output_index)
  bs<-bases(m);rev<-data.frame(year=m$year)
  dist<-x$income_distribution;income0<-dist$mean_income*(1.025*1.012)^6
  threshold<-switch(a$tax_policy,wage_indexed=m$nominal_earnings_index,cpi=m$domestic_price_index,
                    fixed=rep(1,nrow(m)),creep=rep(1,nrow(m)),stop('Unknown tax threshold rule'))
  tax0<-sum(dist$count*income_tax(income0,1,2030))
  pit<-sapply(seq_len(nrow(m)),function(i)sum(dist$count*income_tax(income0*m$nominal_earnings_index[i],threshold[i],m$year[i]))/tax0)
  rev$personal_income<-c$revenue_anchor['personal_income']*m$employed_index*pit
  # Structural company tax: nominal scaling is exactly proportional to GDP.
  # ToT changes the level of the GDP share, not its annual growth rate. The
  # sensitivity is an explicit scenario assumption, not the historical GDP fit.
  if(!is.finite(a$company_tot_elasticity)||a$company_tot_elasticity<0)
    stop('Company tax ToT elasticity must be finite and non-negative.')
  rev$company<-unname(c$revenue_anchor['company'])*(m$nominal_gdp/c$gdp0)*
    m$tot_index^a$company_tot_elasticity
  for(n in names(bs)) {
    rev[[n]]<-unname(c$revenue_anchor[n])*bs[[n]]
  }
  # Distinguish the effect of threshold policy from a wage-indexed counterfactual.
  wage_pit<-sapply(seq_len(nrow(m)),function(i)sum(dist$count*
    income_tax(income0*m$nominal_earnings_index[i],m$nominal_earnings_index[i],m$year[i]))/tax0)
  diagnostics<-data.frame(year=m$year,threshold_index=threshold,
    mean_taxable_income=weighted.mean(income0,dist$count)*m$nominal_earnings_index,
    effective_income_tax_rate=pit*tax0/(sum(dist$count*income0)*m$nominal_earnings_index),
    bracket_creep_revenue_bn=rev$personal_income-c$revenue_anchor['personal_income']*m$employed_index*wage_pit,
    wage_bill_index=m$nominal_earnings_index*m$employed_index,
    consumption_proxy_index=bs$gst,property_value_proxy_index=bs$property,
    excise_quantity_index=m$population_index,excise_rate_index=m$domestic_price_index)
  list(revenue=rev,diagnostics=diagnostics)
}

project_model <- function(inputs,assumptions=default_assumptions(),calibration=calibrate(inputs)) {
  if(any(c('health_pass_through','education_pass_through')%in%names(assumptions)))
    stop('Health/education use annual cost-growth adjustments now; replace obsolete pass-through fields with health_cost_growth_adjustment and education_cost_growth_adjustment (default zero).')
  c<-calibration;a<-modifyList(default_assumptions(),assumptions)
  x<-economic_inputs(inputs,a)
  stopifnot(a$anchor_year==2030,a$end_year==2066,a$policy%in%c('indexed','standards'))
  m<-macro_path(x,a,prepared=TRUE);pp<-c$programmes;t<-m$year-2030
  # Change future cost responses only; never rebuild the common 2030 anchor.
  pass_keys<-cost_pass_through_parameters()
  for(n in names(pass_keys)) {
    value<-a[[pass_keys[[n]]]]
    scaling<-n%in%c('Health','Education')
    lower<-if(scaling)-.02 else 0;upper<-if(scaling).02 else 1.5
    if(length(value)!=1L||!is.finite(value)||value<lower||value>upper)
      stop('Cost control out of range: ',n)
    if(!scaling)pp$wage_pass_through[pp$programme==n]<-value
  }
  pension<-pension_path(x,m,a)
  spending<-project_primary_spending(x,m,a,c,pp,pension)
  parts<-spending$components
  m$primary_expense_bn<-as.numeric(tapply(parts$spending_bn,parts$year,sum))
  rr<-revenue_projection(x,m,a,c);rev<-rr$revenue
  m$non_interest_revenue_bn<-rowSums(rev[,-1])
  m$financial_assets_bn<-c$assets0*m$population_index*m$domestic_price_index
  m$interest_receipts_bn<-c(c$interest_receipts0,head(m$financial_assets_bn,-1)*c$asset_yield)
  m$revenue_bn<-m$non_interest_revenue_bn+m$interest_receipts_bn
  m$net_capital_investment_bn<-c$nci0*m$population_index*m$domestic_price_index*m$productivity_index^.5
  m$net_financial_asset_acquisition_bn<-c(0,diff(m$financial_assets_bn))
  m$effective_interest_rate<-approx(c(2030,2040),c(c$implied_interest_rate,a$interest_rate),xout=m$year,rule=2)$y
  m$interest_bn<-m$gross_debt_bn<-m$net_debt_bn<-rep(NA_real_,nrow(m))
  m$interest_bn[1]<-c$official$public_debt_interest_billion_dollars
  m$gross_debt_bn[1]<-c$gross_debt0;m$net_debt_bn[1]<-c$gross_debt0-c$assets0
  m$stock_flow_adjustment_bn<-rep(0,nrow(m))
  for(i in 2:nrow(m)) {
    m$interest_bn[i]<-m$effective_interest_rate[i]*m$gross_debt_bn[i-1]
    borrowing<-m$primary_expense_bn[i]+m$interest_bn[i]+m$net_capital_investment_bn[i]-m$revenue_bn[i]
    gross<-m$gross_debt_bn[i-1]+borrowing+m$net_financial_asset_acquisition_bn[i]
    if(gross<0) {
      extra<- -gross;m$financial_assets_bn[i:nrow(m)]<-m$financial_assets_bn[i:nrow(m)]+extra
      m$net_financial_asset_acquisition_bn[i]<-m$net_financial_asset_acquisition_bn[i]+extra
      if(i<nrow(m)) {
        m$interest_receipts_bn[(i+1):nrow(m)]<-m$interest_receipts_bn[(i+1):nrow(m)]+extra*c$asset_yield
        m$revenue_bn[(i+1):nrow(m)]<-m$revenue_bn[(i+1):nrow(m)]+extra*c$asset_yield
      }
    }
    m$gross_debt_bn[i]<-max(0,gross);m$net_debt_bn[i]<-m$gross_debt_bn[i]-m$financial_assets_bn[i]
  }
  m$total_spending_bn<-m$primary_expense_bn+m$interest_bn+m$net_capital_investment_bn
  m$fiscal_balance_bn<-m$revenue_bn-m$total_spending_bn
  m$primary_balance_bn<-m$non_interest_revenue_bn-m$primary_expense_bn-m$net_capital_investment_bn
  for(n in c('primary_expense','interest','revenue','total_spending','fiscal_balance','primary_balance','gross_debt','net_debt','net_capital_investment'))
    m[[paste0(n,'_pct')]]<-100*m[[paste0(n,'_bn')]]/m$nominal_gdp
  m$real_primary_per_person<-m$primary_expense_bn*1e9/m$domestic_price_index/m$population
  m$pension_coverage<-pension$coverage
  m$ndis_participants<-x$ndis$participants[match(m$year,x$ndis$year)]
  rev$interest_receipts<-m$interest_receipts_bn
  list(path=m,components=parts,subfunctions=spending$subfunctions,revenue=rev,revenue_diagnostics=rr$diagnostics,
    pension=pension,assumptions=a,calibration=c)
}

debt_diagnostics <- function(projection) {
  d<-projection$path
  d$financial_assets_pct<-100*d$financial_assets_bn/d$nominal_gdp
  d$opening_gross_debt_bn<-c(NA_real_,head(d$gross_debt_bn,-1))
  d$opening_financial_assets_bn<-c(NA_real_,head(d$financial_assets_bn,-1))
  d$interest_receipts_pct<-100*d$interest_receipts_bn/d$nominal_gdp
  d$net_interest_bn<-d$interest_bn-d$interest_receipts_bn
  d$net_interest_pct<-100*d$net_interest_bn/d$nominal_gdp
  d$asset_yield_pct<-100*projection$calibration$asset_yield
  d$real_borrowing_rate_pct<-100*((1+d$effective_interest_rate)/(1+d$domestic_inflation)-1)
  d
}

spending_driver_audit <- function(projection) {
  # Expose the multiplicative spending identity without changing projections.
  d<-projection$components;m<-projection$path
  k<-match(d$year,m$year)
  start<-d$year==min(d$year)
  anchor<-setNames(d$spending_bn[start],d$programme[start])
  initial_share<-setNames(d$share_gdp[start],d$programme[start])
  d$real_unit_cost_index<-d$unit_cost_index/m$domestic_price_index[k]
  d$intensity_coverage_index<-d$intensity_index*d$coverage_index
  d$gdp_after_domestic_prices_index<-(m$nominal_gdp[k]/m$nominal_gdp[1])/m$domestic_price_index[k]
  d$implied_share_gdp<-unname(initial_share[d$programme])*d$exposure_index*
    d$real_unit_cost_index*d$intensity_coverage_index/d$gdp_after_domestic_prices_index
  d$implied_spending_bn<-unname(anchor[d$programme])*d$exposure_index*
    d$unit_cost_index*d$intensity_coverage_index
  d$rule<-ifelse(d$programme=='Defence','GDP-share target',
    ifelse(d$programme=='Age Pension','Eligible population, coverage and mean payment','Number, unit cost and intensity'))
  # Defence is explicitly targeted at a GDP share, not generated by these factors.
  target<-d$programme=='Defence'
  d[target,c('real_unit_cost_index','intensity_coverage_index','implied_share_gdp','implied_spending_bn')]<-NA_real_
  d
}

scenario_attribution <- function(inputs,assumptions,calibration=calibrate(inputs)) {
  base<-default_assumptions()
  assumptions<-modifyList(base,assumptions)
  groups<-list('Productivity'='productivity','NGDP per capita'='ngdp_per_capita_growth',
    'Population'='population_growth','Average hours'='hours_growth',
    'Government service prices'='relative_price','Cost pass-through'=unname(cost_pass_through_parameters()),
    'Terms of trade'=c('tot_change','company_tot_elasticity'),
    'Policy and financing'=c('policy','tax_policy','interest_rate','super_real_return','ndis_intensity_growth'))
  # Evaluate only groups that differ from the fixed reference. The Shapley dummy
  # property makes this identical to allocating all groups, with zero for defaults.
  active<-which(vapply(groups,function(keys)!isTRUE(all.equal(base[keys],assumptions[keys])),logical(1)))
  result<-setNames(rep(0,length(groups)),names(groups))
  n<-length(active)
  if(!n)return(data.frame(driver=names(result),net_debt_change_pp=unname(result)))
  masks<-0:(2^n-1L);values<-numeric(length(masks))
  for(mask in masks) {
    z<-base
    for(j in seq_len(n))if(bitwAnd(mask,bitwShiftL(1,j-1))!=0)z[groups[[active[j]]]]<-assumptions[groups[[active[j]]]]
    values[mask+1]<-tail(project_model(inputs,z,calibration)$path$net_debt_pct,1)
  }
  result[active]<-sapply(seq_len(n),function(j) {
    bit<-bitwShiftL(1,j-1);without<-masks[bitwAnd(masks,bit)==0]
    sum(sapply(without,function(mask){k<-sum(as.integer(intToBits(mask))[seq_len(n)]);
      factorial(k)*factorial(n-1-k)/factorial(n)*(values[mask+bit+1]-values[mask+1])}))
  })
  data.frame(driver=names(result),net_debt_change_pp=unname(result))
}
