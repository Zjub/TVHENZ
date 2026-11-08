args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(dirname(script),mustWork=TRUE))
n<-0L
check<-function(ok,label){if(!isTRUE(ok))stop(label);n<<-n+1L}
d<-read.csv('outputs/annual_estimation_data.csv');s<-read.csv('outputs/observed_spending.csv')
e<-read.csv('outputs/window_and_structure_sensitivity.csv');p<-read.csv('outputs/candidate_service_cost_parameters.csv')
check(nrow(e)==360,'All 360 specifications must be estimated')
check(!anyDuplicated(d[,c('group','year','wage_basis','service_productivity_share')]),'Unique annual keys')
check(all(is.finite(d$log_residual)),'Finite residuals')
check(setequal(unique(d$year),2016:2025),'Financial-year sample')
check(all(e$n[e$window=='pre_covid_2016_2019']==4),'Short pre-COVID sample is exposed')
check(all(e$n[e$window=='excluding_2020_2022']==7),'Omitted observations counted correctly')
for(group in unique(d$group)){
 z<-d[d$group==group & d$wage_basis=='all_industry_wpi' & d$service_productivity_share==0,]
 rebuilt<-z$spending_bn[1]*z$demographic_index*z$price_index/z$price_index[1]*z$wage_cost_index*z$residual_index
 check(max(abs(rebuilt-z$spending_bn))<1e-8,paste(group,'spending identity'))
 z<-z[order(z$year),];r<-expm1(unname(coef(lm(log_residual~year,z))[2]))
 main<-e[e$group==group & e$window=='full_2016_2025' & e$wage_basis=='all_industry_wpi' & e$service_productivity_share==0,]
 check(abs(main$additional_growth-r)<1e-12,paste(group,'calendar-year trend'))
}
health_names<-c('Hospitals','Primary/community care','Referred medical services','Medicines','Other health / reconciliation')
for(y in 2016:2025){
 check(abs(sum(s$spending_bn[s$year==y & s$group%in%health_names])-s$spending_bn[s$year==y & s$group=='Health'])<1e-8,paste(y,'health reconciliation'))
 check(abs(sum(s$spending_bn[s$year==y & s$group%in%c('Schools','Tertiary','Other education')])-s$spending_bn[s$year==y & s$group=='Education'])<.002,paste(y,'education reconciliation'))
}
check(nrow(p)==9 && !anyDuplicated(p$id),'Nine candidate subfunction rows')
check(p$additional_service_growth[p$id=='e_higher']==p$additional_service_growth[p$id=='e_vet'],'Shared tertiary rate labelled and preserved')
check(all(p$service_productivity_share==0),'Efficiency not falsely estimated')
check(all(grepl('not applied',p$adoption_status)),'Candidates explicitly unadopted')
# Check the offline source snapshot against the recorded bytes, not just filenames.
m<-read.csv('data/source_manifest.csv')
check(identical(unname(tools::md5sum(file.path('data/raw',m$file))),m$md5),'Input snapshot hashes')
live<-read.csv('../data/service_cost_parameters.csv');snapshot<-read.csv('data/raw/service_parameters.csv')
check(isTRUE(all.equal(live[,c('id','wage_weight')],snapshot[,c('id','wage_weight')],check.attributes=FALSE)),'Historical input-share snapshot remains compatible with independently selected runtime defaults')
pop<-read.csv('outputs/population_fy_age.csv')
check(all(table(pop$year)==101),'Single ages exhaust the population once')
check(max(abs(pop$population-(pop$value_start+pop$value_end)/2))<1e-8,'Financial-year midpoint population')
# Known-trend fixture with independently specified, changing non-linear drivers.
y<-c(2016:2019,2023:2025);t<-y-2016;D<-exp(.015*t+.001*t^2);P<-1.025^t
cost<-.3+.7*(1.035/1.025)^t;truth<-.0123
spend<-80*D*P*cost*(1+truth)^t
recovered<-expm1(unname(coef(lm(log(spend/D/P/cost)~y))[2]))
check(abs(recovered-truth)<1e-12,'Known residual recovered despite nonlinear drivers and missing years')
med<-d[d$group=='Medicines' & d$wage_basis=='all_industry_wpi',]
check(max(abs(med$wage_cost_index-1))<1e-12,'CPI-only medicines have no wage or efficiency deduction')
wh<-readRDS('data/raw/wpi.rds');label<-'Financial Year Index ;  Total hourly rates of pay excluding bonuses ;  Australia ;  Private and Public ;  Health care and social assistance ;'
wh<-wh[wh$series==label,];wh$year<-as.integer(format(wh$date,'%Y'))
for(group in c('Health',health_names)){
 z<-d[d$group==group & d$wage_basis=='sector_wpi' & d$service_productivity_share==0,]
 w<-wh$value[match(z$year,wh$year)];expected<-w/w[z$year==2025]/(z$price_index/z$price_index[z$year==2025])
 check(max(abs(expected-z$real_wage_index))<1e-10,paste(group,'uses health sector WPI'))
}
ig<-read.csv('outputs/igr_provisional_rules.csv')
check(all(is.na(ig$numeric_extra_growth)),'No invented IGR coefficients')
nd<-read.csv('outputs/ndis_earnings_rule_translation.csv');t<-nd$horizon_years
basket<-(1-nd$wage_weight)+nd$wage_weight*(1+nd$aggregate_productivity)^t
check(abs(basket*(1+nd$implied_extra_growth)^t-(1+nd$aggregate_productivity)^t)<1e-10,'Derived NDIS rule translation matches endpoint')
cat('PASS:',n,'estimation checks.\n')
