# Independent historical estimation. Rates are decimals per year, years are FY-ending.
# Observed spending identifies a NET residual conditional on the cost assumptions.
dir.create('outputs',showWarnings=FALSE)
rx<-function(file,sheet='Data1')as.data.frame(suppressMessages(readxl::read_excel(file,sheet,col_names=FALSE)))
num<-function(x)suppressWarnings(as.numeric(x))
abs_series<-function(file,sheets='Data1')do.call(rbind,lapply(sheets,function(sheet){
 d<-rx(file,sheet);dates<-as.Date(num(d[-(1:10),1]),origin='1899-12-30')
 do.call(rbind,lapply(2:ncol(d),function(j)data.frame(date=dates,series=as.character(d[1,j]),
  series_id=as.character(d[10,j]),value=num(d[-(1:10),j]))))
}))
pick<-function(d,label){z<-d[d$series==label & is.finite(d$value),];stopifnot(nrow(z)>0,!anyDuplicated(z$date));z}
fy<-function(date)as.integer(format(date,'%Y'))+as.integer(format(date,'%m')>'06')

pop<-abs_series('data/raw/population.xlsx',c('Data1','Data2'))
pop<-pop[grepl('Population ;  Persons ;  [0-9]+( and over)? ;$',pop$series),]
pop$age<-as.integer(sub('.*Persons ;  ([0-9]+).*','\\1',pop$series))
pop$year<-as.integer(format(pop$date,'%Y'))
stopifnot(!anyDuplicated(pop[,c('year','age')]))
# Require single years 0--99 and 100+, never add overlapping age groups/totals.
pop<-pop[pop$age<=100,];stopifnot(all(table(pop$year)==101))
pop<-pop[pop$year%in%2014:2025,c('year','age','value')]
mid<-merge(transform(pop,year=year+1),pop,by=c('year','age'),suffixes=c('_start','_end'))
mid$population<-(mid$value_start+mid$value_end)/2
write.csv(mid,'outputs/population_fy_age.csv',row.names=FALSE)
profiles<-read.csv('data/raw/age_profiles.csv');spec<-read.csv('data/raw/subfunctions.csv')
params<-read.csv('data/raw/service_parameters.csv')
spec<-merge(spec,params[,c('id','cost_rule','wage_weight')],by='id')
exposure<-function(profile){
 weight<-switch(profile,school_age=as.numeric(mid$age>=5 & mid$age<=17),
  tertiary_age=as.numeric(mid$age>=18 & mid$age<=24),vet_age=as.numeric(mid$age>=15 & mid$age<=64),
  population=rep(1,nrow(mid)),{
   p<-profiles[profiles$profile==profile,];stopifnot(nrow(p)>0)
   vapply(mid$age,function(age){k<-which(age>=p$age_min & age<=p$age_max);stopifnot(length(k)==1);p$weight[k]},numeric(1))
  })
 z<-tapply(mid$population*weight,mid$year,sum);z/z['2025']
}
gfs<-read.csv('data/raw/gfs_purpose.csv');ed<-rx('data/raw/education.xlsx','Table_1')
years<-as.integer(sub('.*-','20',as.character(ed[6,-1])))
stopifnot(identical(years,2016:2025))
spending<-rbind(data.frame(group='Health',year=gfs$year[gfs$item=='Health'],
  spending_bn=gfs$value[gfs$item=='Health']/1000),
 data.frame(group='Education',year=years,spending_bn=num(ed[12,-1])/1000),
 data.frame(group='Schools',year=years,spending_bn=num(ed[9,-1])/1000),
 data.frame(group='Tertiary',year=years,spending_bn=num(ed[10,-1])/1000),
 data.frame(group='Other education',year=years,spending_bn=num(ed[11,-1])/1000))
# Government funding only: exclude non-government funders and never add state
# spending to Commonwealth grants. AIHW's source-of-funds accounts do the netting.
hc<-rx('data/raw/health_cube.xlsx','Datacube')
hc<-hc[grepl('^[0-9]{4}-[0-9]{2}$',hc[[1]]),]
names(hc)<-c('fy','jurisdiction','sector','area','broad','source','current','constant','perperson','realperperson')
hc$current<-num(hc$current);hc$year<-as.integer(sub('.*-','20',hc$fy))
hc<-hc[hc$jurisdiction=='AUS' & hc$broad%in%c('Australian Government','State and local government'),]
stopifnot(all(is.finite(hc$current)),!anyDuplicated(hc[,c('year','sector','area','broad','source')]))
hc$group<-ifelse(hc$sector=='Hospitals','Hospitals',
 ifelse(hc$area=='Benefit-paid pharmaceuticals','Medicines',
 ifelse(hc$sector=='Primary health care','Primary/community care',
 ifelse(hc$sector=='Referred medical services','Referred medical services',NA_character_))))
hs<-aggregate(current~group+year,hc[!is.na(hc$group),],sum);names(hs)[3]<-'spending_bn';hs$spending_bn<-hs$spending_bn/1e9
hs<-hs[hs$year%in%years,]
other<-spending[spending$group=='Health',];named<-aggregate(spending_bn~year,hs,sum)
other$spending_bn<-other$spending_bn-named$spending_bn[match(other$year,named$year)]
other$group<-'Other health / reconciliation';stopifnot(all(other$spending_bn>0))
spending<-rbind(spending,hs,other)
write.csv(hc,'outputs/aihw_government_source_rows.csv',row.names=FALSE)
write.csv(spending,'outputs/observed_spending.csv',row.names=FALSE)
health_map<-c(h_hospital='Hospitals',h_primary='Primary/community care',h_referred='Referred medical services',
 h_medicines='Medicines',h_other='Other health / reconciliation')
for(id in names(health_map))spec$base_2025_bn[spec$id==id]<-spending$spending_bn[spending$year==2025 & spending$group==health_map[id]]
cpi<-pick(abs_series('data/raw/cpi.xlsx'),'Index Numbers ;  All groups CPI ;  Australia ;')
cpi$year<-fy(cpi$date);cpi<-cpi[cpi$year%in%2015:2025,];stopifnot(all(table(cpi$year)==4))
cpi<-aggregate(value~year,cpi,mean);names(cpi)[2]<-'cpi'
wpi<-readRDS('data/raw/wpi.rds')
getw<-function(industry){label<-paste0('Financial Year Index ;  Total hourly rates of pay excluding bonuses ;  Australia ;  Private and Public ;  ',industry,' ;')
 z<-pick(wpi,label);data.frame(year=as.integer(format(z$date,'%Y')),value=z$value)}
wa<-getw('All industries');wh<-getw('Health care and social assistance');we<-getw('Education and training')
na<-abs_series('data/raw/national_accounts.xlsx')
prod<-pick(na,'GDP per hour worked: Index ;');prod$year<-as.integer(format(prod$date,'%Y'))
coe<-pick(na,'Compensation of employees per hour: Current prices ;');coe$year<-as.integer(format(coe$date,'%Y'))
groups<-list(Health=spec$id[spec$programme=='Health'],Education=spec$id[spec$programme=='Education'],
 Schools='e_school',Tertiary=c('e_higher','e_vet'),'Other education'='e_other')
for(id in names(health_map))groups[[health_map[id]]]<-id

annual<-do.call(rbind,lapply(names(groups),function(group){
 s<-spec[match(groups[[group]],spec$id),];weights<-s$base_2025_bn/sum(s$base_2025_bn)
 do.call(rbind,lapply(c('all_industry_wpi','sector_wpi','compensation_per_hour'),function(wage_basis){
  do.call(rbind,lapply(c(0,.25,.5),function(theta){
   z<-spending[spending$group==group,];z<-z[order(z$year),];y<-z$year
   P<-cpi$cpi[match(y,cpi$year)];P<-P/P[y==2016]
   wage<-switch(wage_basis,all_industry_wpi=wa$value[match(y,wa$year)],
    sector_wpi={w<-if(group=='Health' || group%in%unname(health_map))wh else we;w$value[match(y,w$year)]},
    compensation_per_hour=coe$value[match(y,coe$year)])
   W<-wage/wage[y==2025]/(P/P[y==2025])
   A<-prod$value[match(y,prod$year)];A<-A/A[y==2025];Q<-A^theta
   E<-B<-rep(0,length(y))
   for(i in seq_len(nrow(s))){
    D<-exposure(s$profile[i])[as.character(y)]
    # Funding proxies use a compounded annual funding index, as in the app.
    basket<-switch(s$cost_rule[i],input_mix=(1-s$wage_weight[i])+s$wage_weight[i]*W/Q,
      cpi_proxy=rep(1,length(y)),funding_proxy={v<-cumprod(c(1,1+s$wage_weight[i]*(W[-1]/head(W,-1)-1)));v/tail(v,1)},
      stop('Unexpected cost rule'))
    E<-E+weights[i]*D;B<-B+weights[i]*D*basket
   }
   # Sum subfunction spending before taking logs; demographic weights vary with age demand.
   unit<-B/E;E<-E/E[1];unit<-unit/unit[1]
   residual<-z$spending_bn/(E*P*unit)
   data.frame(group,year=y,wage_basis,service_productivity_share=theta,
    spending_bn=z$spending_bn,demographic_index=E,price_index=P,wage_cost_index=unit,
    real_wage_index=W,aggregate_productivity_index=A,residual_index=residual/residual[1],
    log_residual=log(residual),nominal_spending_source=if(group=='Health')'ABS GFS Table 4 Health' else
     if(group%in%unname(health_map))'AIHW government-funded current expenditure; other = GFS reconciliation' else 'ABS GFS Education Table 1')
  }))
 }))
}))
stopifnot(all(is.finite(annual$log_residual)),!anyDuplicated(annual[,c('group','year','wage_basis','service_productivity_share')]))
windows<-list(full_2016_2025=2016:2025,pre_covid_2016_2019=2016:2019,
 excluding_2020_2022=c(2016:2019,2023:2025),recent_2019_2025=2019:2025)
# Newey-West covariance, one annual lag, finite-sample correction.
hac_se<-function(fit){X<-model.matrix(fit);e<-residuals(fit);n<-nrow(X);k<-ncol(X)
 U<-X*as.numeric(e);S<-crossprod(U)
 lagged<-crossprod(U[-1,,drop=FALSE],U[-n,,drop=FALSE]);S<-S+.5*(lagged+t(lagged))
 bread<-solve(crossprod(X));sqrt(diag(bread%*%S%*%bread*n/(n-k)))[2]
}
keys<-unique(annual[,c('group','wage_basis','service_productivity_share')])
estimates<-do.call(rbind,lapply(seq_len(nrow(keys)),function(i){key<-keys[i,]
 d<-annual[annual$group==key$group & annual$wage_basis==key$wage_basis & annual$service_productivity_share==key$service_productivity_share,]
 do.call(rbind,lapply(names(windows),function(window){z<-d[d$year%in%windows[[window]],];z$t<-z$year-2016
  fit<-lm(log_residual~t,z);beta<-unname(coef(fit)[2]);se<-hac_se(fit);crit<-qt(.975,nrow(z)-2)
  data.frame(key,window,n=nrow(z),start_year=min(z$year),end_year=max(z$year),
   log_trend=beta,additional_growth=expm1(beta),hac_se_log_trend=se,
   descriptive_ci_lower=expm1(beta-crit*se),descriptive_ci_upper=expm1(beta+crit*se),
   residual_sd=summary(fit)$sigma,r_squared=summary(fit)$r.squared)
 }))
}))
main<-estimates[estimates$window=='full_2016_2025' & estimates$wage_basis=='all_industry_wpi' & estimates$service_productivity_share==0,]
# Exact annual log accounting, conditional on the chosen historical basket.
decomp<-annual[annual$wage_basis=='all_industry_wpi' & annual$service_productivity_share==0,]
decomp<-do.call(rbind,lapply(split(decomp,decomp$group),function(d){d<-d[order(d$year),]
 for(pair in list(c('spending_bn','observed'),c('demographic_index','demography'),c('price_index','prices'),
   c('wage_cost_index','wage_costs'),c('residual_index','net_residual')))
  d[[paste0(pair[2],'_log_change')]]<-c(NA,diff(log(d[[pair[1]]])))
 d
}))
stopifnot(max(abs(with(decomp,observed_log_change-demography_log_change-prices_log_change-wage_costs_log_change-net_residual_log_change)),na.rm=TRUE)<1e-10)
write.csv(annual,'outputs/annual_estimation_data.csv',row.names=FALSE)
write.csv(estimates,'outputs/window_and_structure_sensitivity.csv',row.names=FALSE)
write.csv(main,'outputs/central_estimates.csv',row.names=FALSE)
write.csv(decomp,'outputs/annual_growth_decomposition.csv',row.names=FALSE)

# Candidates have the app's field names, but are deliberately NOT installed by this script.
candidates<-params[params$id%in%spec$id[spec$programme%in%c('Health','Education')],]
candidates$estimation_group<-ifelse(grepl('^h_',candidates$id),unname(health_map[candidates$id]),
 ifelse(candidates$id=='e_school','Schools',ifelse(candidates$id%in%c('e_higher','e_vet'),'Tertiary','Other education')))
candidates$additional_service_growth<-main$additional_growth[match(candidates$estimation_group,main$group)]
candidates$service_productivity_share<-0
candidates$adoption_status<-'Candidate for expert review; not applied to fiscal model'
candidates$evidence_class<-ifelse(candidates$estimation_group=='Tertiary',
 'Aggregate tertiary estimated net residual shared across higher education and VET; allocation assumption',
 ifelse(candidates$id=='h_other','Reconciliation residual trend; not an independently observed service',
 'Estimated net residual conditional on demand and input-cost proxies'))
candidates$estimate_source<-'AIHW government funding / ABS GFS 2016-2025; ABS ERP, CPI, WPI; current fixed demographic/input profiles'
candidates$estimate_limitation<-'Ten observations; policy/coverage/quality/mix and measurement remain in residual; no separate causal efficiency estimate'
candidates$limitation[candidates$id=='h_medicines']<-'CPI proxy omits separate medicine price disclosure and confidential rebates; the estimated net residual bundles price, policy and mix changes'
write.csv(candidates,'outputs/candidate_service_cost_parameters.csv',row.names=FALSE)

igr_url<-'https://treasury.gov.au/sites/default/files/2026-09/p2026-797917.pdf'
igr<-data.frame(function_name=c('NDIS','Residential aged care','Home care','Age Pension','DSP, Parenting Payment Single, Carer Payment',
 'JobSeeker, Youth Allowance, Austudy, Parenting Payment Partnered, FTB','Parental Leave Pay','Child Care Subsidy','Other spending','GST transfers'),
 published_rule=c('After medium term, age/disability-specific average costs transition over five years to average weekly earnings',
 'CPI plus historical real age-adjusted residential-care cost trend from 2037-38',
 'Separate age/sex utilisation and cost model; no common numerical extra-growth coefficient stated in reviewed appendix',
 'MARIA microsimulation of superannuation, assets, income and pension entitlements',
 'Higher of average weekly earnings and CPI; programme-specific recipient coverage',
 'CPI; programme-specific recipient coverage/unemployment/demography',
 'Ten-year historical weekly minimum-wage growth; births and take-up',
 'Prices trend to CPI and hours growth to zero by end of medium term; age-specific coverage',
 'Residual Commonwealth payments indexed to nominal GDP',
 'Equals GST receipts plus pool boost; intergovernmental transfers consolidate out'),
 numeric_extra_growth=NA_real_,page=c(309,308,308,309,311,311,311,311,312,312),
 provisional_use=c('Use earnings-indexed unit costs as a transparent long-run scenario; current 85% labour basket plus 0.5% extra is a different assumption',
 'No published scalar to copy; derive an explicitly labelled implied residual or estimate history separately',
 'No numerical IGR default available to copy directly',
 'Retain explicit pension module; IGR spending/GDP path is a benchmark, not a unit-cost parameter',
 'Published unit-cost rule can be adopted after transfer block is split by programme',
 'Published CPI rule can be adopted after transfer block is split by programme',
 'Requires historical minimum-wage data; not a blanket wage index',
 'CPI unit-cost rule is a possible long-run simplification with separate age/coverage assumptions',
 'Constant GDP-share option is available; applying it to consolidated residual is a scope assumption',
 'Do not add as consolidated final expenditure'),source_url=igr_url)
write.csv(igr,'outputs/igr_provisional_rules.csv',row.names=FALSE)

# A simple annual implied NDIS extra rate that matches the IGR's earnings index
# at the endpoint, CONDITIONAL on this app's input basket and a fixed macro baseline.
# It is a derived calibration, not an official coefficient or a transition model.
L<-params$wage_weight[params$id=='n_ndis'];g<-.012;t<-36
implied<-data.frame(programme='NDIS',aggregate_productivity=g,wage_weight=L,horizon_years=t,
 target_real_unit_cagr=g,implied_extra_growth=((1+g)^t/((1-L)+L*(1+g)^t))^(1/t)-1,
 method='Match earnings-index endpoint with zero service productivity savings; not an IGR published scalar; no medium-term transition',
 current_extra_growth=params$additional_service_growth[params$id=='n_ndis'])
write.csv(implied,'outputs/ndis_earnings_rule_translation.csv',row.names=FALSE)
cat('Estimated',nrow(estimates),'specifications. Candidate rates (% annually):\n')
print(data.frame(group=main$group,additional_growth_pct=round(main$additional_growth*100,3)),row.names=FALSE)
