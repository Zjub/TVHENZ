args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1]);setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions();p<-project_model(x,a,cal)
n<-0;check<-function(v,label){if(!isTRUE(v))stop(label);n<<-n+1};near<-function(x,y,tol=1e-7)max(abs(x-y))<tol
seed<-x$pension_cohort_seed
check(any(seed$super[seed$age<67]>0),'Workers have existing super wealth')
check(any(seed$super[seed$age>=67]>0),'Retirees have existing remaining super wealth')
check(all(seed$super>=0 & seed$liquid>=0 & seed$other>=0),'Starting asset stocks nonnegative')
check(all(abs(tapply(seed$weight,paste(seed$sex,seed$source_age),sum)-1)<1e-10),'Each source-age/sex distribution sums to one')
audit<-read.csv('outputs/pension_cohort_preparation/initial_age_calibration.csv')
t<-x$pension_public_age[findInterval(audit$age,x$pension_public_age$age_min),]
check(near(audit$coverage,t$coverage),'Starting age-specific receipt matches public calibration targets')
check(near(audit$full_share,t$full_share_of_recipients),'Full/part marginal calibration matches')
profiles<-attr(p$pension,'age_profiles');co<-attr(p$pension,'cohort_profiles');en<-attr(p$pension,'entry_profiles')
check(all(profiles$coverage>=0 & profiles$coverage<=1),'Age receipt probabilities bounded')
check(all(is.finite(unlist(profiles))),'Age outputs finite')
check(any(profiles$age==100),'Open-age population retained')
check(nrow(en)==37 && all(en$cohort_birth_year==en$year-67),'Each year has an entry-age cohort')
for(y in p$path$year){
 d<-profiles[profiles$year==y,];z<-p$pension[p$pension$year==y,];c<-co[co$year==y,]
 expected<-sum(subset(economic_inputs(x,a)$population,year==y & age>=67)$population)
 check(near(sum(d$people),expected,1e-6),'Sourced age-eligible population reconciles')
 check(near(sum(d$recipients),z$recipients,1e-6),'Age recipient counts reconcile')
 check(near(sum(d$pension_dollars)/1e9,z$raw_spending_bn),'Age expenditure reconciles')
 check(near(sum(c$pension_dollars),sum(d$pension_dollars),.001),'Cohort expenditure reconciles including open age')
}
high<-project_model(x,modifyList(a,list(super_real_return=.07)),cal)
check(near(unlist(p$pension[1,]),unlist(high$pension[1,])),'Returns cannot rewrite the starting stock or first-year entitlements')
check(tail(high$pension$coverage,1)<tail(p$pension$coverage,1),'Higher returns reduce final pension receipt')
check(tail(high$pension$payment_index,1)<tail(p$pension$payment_index,1),'Higher returns reduce final pension spending')
check(identical(p$path$nominal_gdp,high$path$nominal_gdp),'Private returns leave nominal GDP unchanged')
check(near(p$components$spending_bn[p$components$year==2030],high$components$spending_bn[high$components$year==2030]),'Shared fiscal anchors retained')
stand<-project_model(x,modifyList(a,list(policy='standards')),cal)
standhigh<-project_model(x,modifyList(a,list(policy='standards',super_real_return=.07)),cal)
check(near(stand$pension$payment_index,standhigh$pension$payment_index),'Constant relative standards freezes age entitlements against returns')
# Transfers between accounts must not destroy financial wealth.
d<-seed[which(seed$age>=75 & seed$super>0)[1],];d$super<-100000;d$liquid<-10000;d$earnings<-0;d$retirement_age<-60
xx<-x;xx$pension_cohort_parameters$value[xx$pension_cohort_parameters$parameter=='retirement_consumption_rate']<-0
xx$pension_cohort_parameters$value[xx$pension_cohort_parameters$parameter=='liquid_real_return']<-0
v<-pension_advance(d,xx,0)
check(near(v$super+v$liquid,110000),'Unconsumed super withdrawals remain assessable liquid wealth')
check(v$super<d$super && v$liquid>d$liquid,'Retirement withdrawal transfers accounts')
d$age<-50;d$retirement_age<-65;v<-pension_advance(d,xx,.04)
check(near(v$super,104000)&&near(v$liquid,10000),'A worker is not forced to draw retirement balances')
check(!any(p$pension$coverage[1:12]>.75),'No inherited synthetic 2038 receipt spike')
cat('PASS:',n,'public-data pension cohort checks.\n')
