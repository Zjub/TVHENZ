# Grouped public-data cohorts; all stocks are in 2026-price dollars internally.
pension_parameter <- function(x,key) x$pension_cohort_parameters$value[match(key,x$pension_cohort_parameters$parameter)]
pension_work <- function(age,sex,retirement_age,x) {
 ans<-numeric(length(age))
 for(s in unique(sex)){
  r<-x$rates[x$rates$sex==s,];v<-r$participation_rate*(1-r$unemployment_rate)
  k<-sex==s;j<-findInterval(pmax(age[k],15),r$age_min);peak<-max(v[r$age_min>=30&r$age_min<=54])
  ans[k]<-ifelse(age[k]<60,pmin(1,v[j]/peak),1)*as.numeric(age[k]<retirement_age[k])
 }
 ans
}
pension_cell_payment <- function(d,x,price=1,wage=1) {
 work<-pension_work(d$age,d$sex,d$retirement_age,x)
 hours_factor<-ifelse(d$age>=65,.65,ifelse(d$age>=60,.85,1)) # judgement, conditional hours
 earnings<-d$earnings*work*hours_factor*price
 rental<-d$property*pension_parameter(x,'property_income_yield')*price
 # Only the regular annual Work Bonus is included; no banked balance or grandfathering.
 other_income<-pmax(0,earnings-7800*(1+as.numeric(d$couple)))*as.numeric(d$age>=67)+rental
 # Accumulation super below pension age is outside this person's test; spouses have same age.
 assets<-(d$liquid+ifelse(d$age>=67,d$super,0))*price
 pay<-pension_entitlement(x,assets,d$other*price,other_income,d$couple,d$homeowner,price,wage)
 pay[d$age<67]<-0
 pay
}
pension_advance <- function(d,x,real_return,real_wage_growth=1) {
 work<-pension_work(d$age,d$sex,d$retirement_age,x)
 h<-ifelse(d$age>=65,.65,ifelse(d$age>=60,.85,1))
 contribution<-d$earnings*work*h*policy_value(x,'super_guarantee')*(1-policy_value(x,'super_contributions_tax'))
 retired<-d$age>=d$retirement_age
 # Withdrawal is a transfer into liquid wealth; only consumption reduces combined assets.
 minimum<-c(.04,.05,.06,.07,.09,.11,.14)[findInterval(d$age,c(0,65,75,80,85,90,95))]
 withdrawal<-ifelse(retired & d$age>=60,minimum*d$super,0)
 consumption<-ifelse(retired,pension_parameter(x,'retirement_consumption_rate')*(d$super+d$liquid),0)
 sup<-d$super*(1+real_return)+contribution-withdrawal
 cash<-d$liquid*(1+pension_parameter(x,'liquid_real_return'))+withdrawal
 extra<-pmin(pmax(consumption-cash,0),pmax(sup,0))*as.numeric(d$age>=60)
 d$super<-pmax(0,sup-extra);d$liquid<-pmax(0,cash+extra-consumption)
 d$earnings<-d$earnings*real_wage_growth;d$age<-d$age+1L
 d
}
pension_allocate_people <- function(d,x,year) {
 pop<-x$population[x$population$year==year,];key<-paste(d$sex,pmin(d$age,100))
 # Open-age group: distribute its sourced population across surviving older cohorts.
 score<-d$weight*ifelse(d$age>=100,d$source_population*pension_parameter(x,'open_age_survival')^pmax(d$age-100,0),1)
 denom<-as.numeric(tapply(score,key,sum)[key]);popkey<-paste(pop$sex,pop$age)
 score/denom*pop$population[match(key,popkey)]
}
pension_cohort_projection <- function(x,m,a) {
 # Cache excludes fiscal/health/tax controls, which do not affect household entitlements.
 key<-paste(a$policy,a$super_real_return,paste(signif(c(m$domestic_price_index,m$nominal_earnings_index,
   m$population_index),12),collapse=','),x$pension_seed_signature,paste(x$pension_cohort_parameters$value,collapse=','),paste(x$policy$value,collapse=','),sep='|')
 if(exists(key,envir=.pension_cohort_cache,inherits=FALSE))return(get(key,envir=.pension_cohort_cache))
 d<-x$pension_cohort_seed;annual<-vector('list',nrow(m));summary<-vector('list',nrow(m));entries<-cohorts<-list()
 P0<-1.025^4;W0<-(1.025*1.012)^4;anchor_age<-NULL
 for(i in seq_len(nrow(m))){
  if(i>1){real_wage<-(m$nominal_earnings_index[i]/m$domestic_price_index[i])/(m$nominal_earnings_index[i-1]/m$domestic_price_index[i-1]);d<-pension_advance(d,x,a$super_real_return,real_wage)}
  price<-P0*m$domestic_price_index[i];wage<-W0*max(m$domestic_price_index[1:i],m$nominal_earnings_index[1:i])
  payment<-pension_cell_payment(d,x,price,wage);people<-pension_allocate_people(d,x,m$year[i])
  max_payment<-ifelse(d$couple,policy_value(x,'pension_couple_each_fortnight'),policy_value(x,'pension_single_fortnight'))*26*wage
  age<-pmin(d$age,100);eligible<-d$age>=67
  z<-data.frame(age=age[eligible],people=people[eligible],recipients=people[eligible]*(payment[eligible]>.01),
    full_recipients=people[eligible]*(payment[eligible]>=max_payment[eligible]-.01),
    pension_dollars=people[eligible]*payment[eligible],super_dollars=people[eligible]*d$super[eligible],
    liquid_dollars=people[eligible]*d$liquid[eligible],other_dollars=people[eligible]*d$other[eligible])
  co<-aggregate(z[setdiff(names(z),'age')],list(birth_year=d$birth_year[eligible]),sum)
  co$year<-m$year[i];co$age<-co$year-co$birth_year;co$coverage<-co$recipients/co$people
  cohorts[[i]]<-co
  ag<-aggregate(.~age,z,sum);ag$coverage<-ag$recipients/ag$people
  ag$mean_payment<-ifelse(ag$recipients>0,ag$pension_dollars/ag$recipients,0)
  if(i==1)anchor_age<-ag
  if(a$policy=='standards'){
   base<-anchor_age[match(ag$age,anchor_age$age),];ag$coverage<-base$coverage
   ag$recipients<-ag$people*base$coverage;ag$full_recipients<-ag$people*base$full_recipients/base$people
   ag$mean_payment<-base$mean_payment*wage/W0;ag$pension_dollars<-ag$recipients*ag$mean_payment
   co<-cohorts[[i]];base<-anchor_age[match(pmin(co$age,100),anchor_age$age),]
   co$coverage<-base$coverage;co$recipients<-co$people*co$coverage
   co$full_recipients<-co$people*base$full_recipients/base$people;co$pension_dollars<-co$recipients*base$mean_payment*wage/W0;cohorts[[i]]<-co
  }
  ag$year<-m$year[i];ag$mean_super_2026_dollars<-ag$super_dollars/ag$people
  ag$mean_liquid_2026_dollars<-ag$liquid_dollars/ag$people
  annual[[i]]<-ag
  summary[[i]]<-data.frame(year=m$year[i],raw_spending_bn=sum(ag$pension_dollars)/1e9,
    coverage=sum(ag$recipients)/sum(ag$people),eligible_population=sum(ag$people),recipients=sum(ag$recipients),
    mean_payment=sum(ag$pension_dollars)/max(sum(ag$recipients),1))
  en<-ag[ag$age==67,c('year','coverage','mean_payment')];en$cohort_birth_year<-en$year-67;entries[[i]]<-en
 }
 result<-do.call(rbind,summary);result$payment_index<-result$raw_spending_bn/result$raw_spending_bn[1]
 attr(result,'age_profiles')<-do.call(rbind,annual);attr(result,'entry_profiles')<-do.call(rbind,entries)
 attr(result,'cohort_profiles')<-do.call(rbind,cohorts)
 if(length(ls(.pension_cohort_cache))>=64)rm(list=head(ls(.pension_cohort_cache),1),envir=.pension_cohort_cache)
 assign(key,result,envir=.pension_cohort_cache);result
}
.pension_cohort_cache<-new.env(parent=emptyenv())
