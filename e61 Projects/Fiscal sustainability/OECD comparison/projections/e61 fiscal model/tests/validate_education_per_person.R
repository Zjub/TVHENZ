args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1]);setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');source('R/sector_review.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions()
near<-function(x,y)max(abs(x-y))<1e-7;n<-0
check<-function(ok,label){if(!isTRUE(ok))stop(label);n<<-n+1}
targets<-a$spending_gdp_targets;targets['Education']<-0
for(z in list(a,modifyList(a,list(productivity=.017,population_growth=.02,education_income_elasticity=.5)),
 modifyList(a,list(spending_gdp_targets=targets)))){
 p<-project_model(x,z,cal)
 for(id in c('all','e_school','e_higher','e_vet','e_other'))for(end in c(2040,2066))for(relative in c(FALSE,TRUE)){
  d<-sector_per_person_data(p,id,end,relative,programme='Education');v<-d$annual;k<-match(v$year,p$path$year)
  expected<-v$spending_bn*1e9/(p$path$domestic_price_index[k]*p$path$population[k])
  if(relative)expected<-100*expected/(cal$gdp0*1e9*p$path$real_output_index[k]/p$path$population[k])
  check(near(v$value,expected),'Selected measure equals direct identity')
  check(near(sum(d$contributions$contribution),tail(d$totals$value,1)-d$totals$value[1]),'Contributions sum to aggregate change')
  check(all(d$contributions$programme=='Education'),'Correct sector label')
  check(all(abs(d$contributions$contribution[d$contributions$factor=='Population size (removed per person)'])<1e-8),'Population size cancels')
  check(all(abs(d$contributions$contribution[d$contributions$factor=='Relative education prices'])<1e-8),'Common domestic prices cancel')
  if(!relative)check(all(d$contributions$contribution[d$contributions$factor=='Real GDP per person denominator']==0),'No GDP denominator in dollar measure')
 }
}
cat('PASS:',n,'education per-person checks.\n')
