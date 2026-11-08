# Offline construction from cached public tables. No HILDA parameters are estimated.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');source('R/pension_cohorts.R');x<-load_inputs(include_pension=FALSE)
raw<-'data/raw/pension';out<-'outputs/pension_cohort_preparation';dir.create(out,recursive=TRUE,showWarnings=FALSE)
read<-function(f,s)as.data.frame(suppressMessages(readxl::read_excel(file.path(raw,f),s,col_names=FALSE)))
num<-function(v)suppressWarnings(as.numeric(v))
wa<-read('abs_age_2020.xlsx','Table 10.2');hc<-read('abs_age_2020.xlsx','Table 10.3')
sc<-read('abs_super_2020.xlsx','Table 12.1');sb<-read('abs_super_2020.xlsx','Table 12.3');ato<-read('ato_snapshot_2024.xlsx','12')
stopifnot(grepl('2023',ato[1,1]),grepl('2026',read('dss_june_2026.xlsx','Age Pension - Age')[1,1]),
 as.character(sb[11,1])=='Mean',as.character(sb[12,1])=='Median',grepl('Total financial assets',wa[23,1]))
parameters<-data.frame(parameter=c('retirement_consumption_rate','liquid_real_return','property_income_yield','open_age_survival'),
 value=c(.05,.01,.03,.65),classification='Judgement; not estimated from HILDA',
 interpretation=c('Annual consumption from opening super plus liquid wealth after retirement; other income consumed separately',
 'Real return on non-super liquid assets; separate from the super return slider','Annual income from net investment property/business proxy',
 'Annual relative survival allocation within the sourced 100+ population; not additional population mortality'))
x$pension_cohort_parameters<-parameters;write.csv(parameters,'data/pension_cohort_parameters.csv',row.names=FALSE)
bands<-data.frame(age_min=c(15,25,35,45,55,65,75),age_max=c(24,34,44,54,64,74,100))
wealth<-do.call(rbind,lapply(c('Male','Female'),function(sex){
 col<-if(sex=='Male')23 else 24;rows<-c(11,14,17,20,23,27,30)
 sigma<-sqrt(pmax(.04,2*log(num(sb[rows,col])/num(sb[rows+1,col]))))
 cov<-num(sc[c(23:27,29,30),if(sex=='Male')22 else 23])/100
 qhouse<-(num(hc[45,3:9])+num(hc[47,3:9]))/100
 household_adults<-1+qhouse
 # Positive-balance median from 2024 ATO; SIH 2020 supplies dispersion and a zero-balance proxy.
 atage<-c(18,25,30,35,40,45,50,55,60,65,70,75)
 med<-num(ato[6:17,if(sex=='Male')2 else 3])
 data.frame(sex,bands,super_coverage=cov,super_sigma=sigma,
  liquid_mean_2020=pmax(0,num(wa[23,2:8])-num(wa[22,2:8])-num(wa[20,2:8])-num(wa[21,2:8]))*1000/household_adults,
  property_mean_2020=pmax(0,num(wa[28,2:8])-num(wa[39,2:8])+num(wa[20,2:8])+num(wa[21,2:8]))*1000/household_adults,
  personal_assets_mean_2020=(num(wa[30,2:8])+num(wa[31,2:8]))*1000/household_adults,
  asset_sigma=sqrt(pmax(.1,2*log(num(wa[9,2:8])/num(wa[10,2:8])))),
  homeowner_share=pmin(1,(num(hc[33,3:9])+num(hc[34,3:9]))/100),
  partnered_person_proxy=2*qhouse/(1+qhouse))
}))
write.csv(wealth,'data/pension_wealth_inputs.csv',row.names=FALSE)
atoflat<-do.call(rbind,lapply(c('Male','Female'),function(sex)data.frame(sex,age_min=c(18,25,30,35,40,45,50,55,60,65,70,75),
 median_2024=num(ato[6:17,if(sex=='Male')2 else 3]))))
write.csv(atoflat,file.path(out,'ato_positive_super_medians.csv'),row.names=FALSE)
age<-read('dss_june_2026.xlsx','Age Pension - Age');rate<-read('dss_june_2026.xlsx','Payment by Rate')
k<-which(as.character(rate[[1]])=='Age Pension');active<-num(rate[k,2])+num(rate[k,5])
active_fraction<-active/num(rate[k,9]);full_fraction<-num(rate[k,2])/active
counts<-colSums(sapply(age[4:11,2:7],num)) # exclude geographic Unknown (includes overseas)
target<-data.frame(age_min=c(67,70,75,80,85,90),age_max=c(69,74,79,84,89,100),resident_recorded_recipients=counts)
target$population_denominator<-vapply(seq_len(nrow(target)),function(i)sum(subset(x$population,year==2026 & age>=target$age_min[i]&age<=target$age_max[i])$population),numeric(1))
target$coverage<-target$resident_recorded_recipients*active_fraction/target$population_denominator
target$full_share_of_recipients<-full_fraction
target$definition<-'June 2026 DSS identified-state recipients, national active-payment adjustment; denominator cached 2026 age population; no age-by-payment-rate joint table'
stopifnot(all(target$coverage>0 & target$coverage<1));write.csv(target,'data/pension_public_age.csv',row.names=FALSE)
seed<-expand.grid(age=20:100,sex=c('Male','Female'),couple=c(FALSE,TRUE),homeowner=c(FALSE,TRUE),
 quantile=1:20,retirement_age=c(60,65,75),stringsAsFactors=FALSE)
seed$birth_year<-2026-seed$age;seed$source_age<-seed$age
seed$weight<-seed$super<-seed$liquid<-seed$other<-seed$property<-seed$earnings<-0
earnmean<-weighted.mean(x$income_distribution$mean_income,x$income_distribution$count)*(1.025*1.012)^2
for(sex in c('Male','Female')){
 k<-which(seed$sex==sex);d<-seed[k,];w<-wealth[wealth$sex==sex,];j<-findInterval(d$age,w$age_min)
 r<-x$rates[x$rates$sex==sex,];e<-r$participation_rate*(1-r$unemployment_rate);peak<-max(e[r$age_min>=30&r$age_min<=54])
 e55<-e[match(55,r$age_min)]/peak;e60<-e[match(60,r$age_min)]/peak;e65<-e[match(65,r$age_min)]/peak
 retprob<-c(1-e60,e60-e65,e65)
 q<-(d$quantile-.5)/20;cov<-w$super_coverage[j];positive<-q>1-cov
 qp<-pmin(.999,pmax(.001,(q-(1-cov))/cov))
 at<-atoflat[atoflat$sex==sex,];median<-at$median_2024[findInterval(d$age,at$age_min)]*1.025^2
 seed$super[k]<-ifelse(positive,median*exp(w$super_sigma[j]*qnorm(qp)),0)*(1+as.numeric(d$couple))
 rank<-exp(w$asset_sigma[j]*qnorm(q)-w$asset_sigma[j]^2/2)
 seed$liquid[k]<-w$liquid_mean_2020[j]*1.025^6*rank*(1+as.numeric(d$couple))
 seed$property[k]<-w$property_mean_2020[j]*1.025^6*rank*(1+as.numeric(d$couple))
 seed$other[k]<-seed$property[k]+w$personal_assets_mean_2020[j]*1.025^6*sqrt(rank)*(1+as.numeric(d$couple))
 seed$earnings[k]<-earnmean*exp(.5*qnorm(q)-.5*.5^2)*(1+as.numeric(d$couple))
 seed$weight[k]<-1/20*retprob[match(d$retirement_age,c(60,65,75))]*
  ifelse(d$couple,w$partnered_person_proxy[j],1-w$partnered_person_proxy[j])*
  ifelse(d$homeowner,w$homeowner_share[j],1-w$homeowner_share[j])
}
# Match observed age coverage and the national full/part marginal once, before the bridge.
seed$prior_weight<-seed$weight
payment<-pension_cell_payment(seed,x);maximum<-ifelse(seed$couple,policy_value(x,'pension_couple_each_fortnight'),policy_value(x,'pension_single_fortnight'))*26
status<-ifelse(payment<=.01,0,ifelse(payment>=maximum-.01,2,1));seed$initial_payment<-payment
for(a in 67:100)for(sex in c('Male','Female')){
 k<-which(seed$age==a & seed$sex==sex);t<-target[findInterval(a,target$age_min),]
 desired<-c(1-t$coverage,t$coverage*(1-full_fraction),t$coverage*full_fraction)
 for(st in 0:2){idx<-k[status[k]==st];stopifnot(length(idx)>0,sum(seed$weight[idx])>0);seed$weight[idx]<-seed$weight[idx]/sum(seed$weight[idx])*desired[st+1]}
}
key<-paste(seed$age,seed$sex);seed$weight<-seed$weight/as.numeric(tapply(seed$weight,key,sum)[key])
seed$source_population<-x$population$population[match(paste(2026,seed$source_age,seed$sex),paste(x$population$year,x$population$age,x$population$sex))]
audit<-do.call(rbind,lapply(67:100,function(age){k<-seed$age==age;w<-seed$weight[k]*seed$source_population[k];data.frame(age,
 coverage=sum(w*(payment[k]>.01))/sum(w),full_share=sum(w*(status[k]==2))/sum(w*(payment[k]>.01)),
 prior_mean_super_2026=weighted.mean(seed$super[k]/(1+as.numeric(seed$couple[k])),seed$prior_weight[k]*seed$source_population[k]),
 prior_mean_assessable_assets_2026=weighted.mean((seed$super[k]+seed$liquid[k]+seed$other[k])/(1+as.numeric(seed$couple[k])),seed$prior_weight[k]*seed$source_population[k]),
 mean_super_2026=weighted.mean(seed$super[k]/(1+as.numeric(seed$couple[k])),w),
 mean_assessable_assets_2026=weighted.mean((seed$super[k]+seed$liquid[k]+seed$other[k])/(1+as.numeric(seed$couple[k])),w))}))
write.csv(audit,file.path(out,'initial_age_calibration.csv'),row.names=FALSE)
write.csv(audit,'data/pension_initial_calibration.csv',row.names=FALSE)
seed$initial_payment<-seed$prior_weight<-NULL
for(i in 1:4)seed<-pension_advance(seed,x,.04,1.012) # fixed 2026-2030 bridge, independent of scenario
seed$earnings<-seed$earnings # already in 2026-price units with real earnings growth
write.csv(seed,'data/pension_cohort_seed_2030.csv',row.names=FALSE)
src<-data.frame(file=c('dss_june_2026.xlsx','ato_snapshot_2024.xlsx','abs_age_2020.xlsx','abs_super_2020.xlsx'),
 url=c('https://data.gov.au/data/dataset/dss-payment-demographic-data','https://data.gov.au/data/dataset/taxation-statistics-2023-24',
 rep('https://www.abs.gov.au/statistics/economy/finance/household-income-and-wealth-australia/2019-20',2)),
 locator=c('Age Pension - Age; Payment by Rate','Snapshot Table 5, sheet 12','Tables 10.2 and 10.3','Tables 12.1 and 12.3'),
 classification=c('Sourced counts; calibrated age rates','Sourced positive-balance medians','Sourced marginal means; synthetic household allocation','Sourced dispersion/coverage proxies; not a joint wealth distribution'))
src$md5<-unname(tools::md5sum(file.path(raw,src$file)));write.csv(src,'data/pension_source_register.csv',row.names=FALSE)
cat('Prepared',nrow(seed),'weighted public-data cohort cells.\n');print(target[,c('age_min','age_max','coverage','full_share_of_recipients')],row.names=FALSE)
