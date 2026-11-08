args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');source('R/sector_review.R')
x<-load_inputs();cal<-calibrate(x);base<-project_model(x,default_assumptions(),cal)
changed<-project_model(x,modifyList(default_assumptions(),list(productivity=.017,population_growth=.015,
 health_service_productivity_share=.5,ndis_service_growth_adjustment=.01)),cal)
checks<-data.frame(check=character(),passed=logical())
check<-function(label,z){checks[nrow(checks)+1L,]<<-list(label,isTRUE(z));if(!isTRUE(z))stop(label)}
near<-function(a,b)length(a)==length(b)&&all(is.finite(c(a,b)))&&max(abs(a-b))<1e-8
for(p in c('NDIS','Health','Education')){
 ids<-c('all',cal$subfunctions$id[cal$subfunctions$programme==p])
 for(scenario in c('base','changed'))for(id in ids)for(year in c(2040L,2066L))for(unit in c('pct','bn','real_bn')){
   m<-get(scenario);z<-sector_review_data(m,p,id,year,unit)
   label<-paste(p,scenario,id,year,unit)
   check(paste(label,'contributions reconcile'),near(sum(z$contributions$contribution),tail(z$totals$value,1)-z$totals$value[1]))
   expected<-m$subfunctions;expected<-expected[expected$programme==p&(id=='all'|expected$id==id)&expected$year==year,]
   amount<-sum(expected$spending_bn)
   expected_value<-switch(unit,bn=amount,pct=100*amount/m$path$nominal_gdp[m$path$year==year],
     real_bn=amount/m$path$domestic_price_index[m$path$year==year])
   check(paste(label,'endpoint equals engine'),near(tail(z$totals$value,1),expected_value))
   f<-setdiff(names(spending_factor_labels()),'gdp_factor')
   check(paste(label,'factors reconstruct dollars'),near(expected$anchor_2030_bn*apply(expected[,f],1,prod),expected$spending_bn))
 }
 check(paste(p,'anchor unchanged under scenario changes'),near(subset(base$subfunctions,programme==p&year==2030)$spending_bn,subset(changed$subfunctions,programme==p&year==2030)$spending_bn))
 check(paste(p,'discussion questions supplied'),length(sector_review_questions(p))==5)
}
check('Invalid cross-sector selection rejected',inherits(try(sector_review_data(base,'NDIS','h_hospital'),silent=TRUE),'try-error'))
check('Invalid endpoint rejected',inherits(try(sector_review_data(base,'Health',end_year=2030),silent=TRUE),'try-error'))
write.csv(checks,'outputs/sector_review_validation_checks.csv',row.names=FALSE)
cat(nrow(checks),'sector review checks passed.\n')
