args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1]);setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');source('R/sector_review.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions()
n<-0;check<-function(v,label){if(!isTRUE(v))stop(label);n<<-n+1};near<-function(x,y)max(abs(x-y))<1e-7
targets<-a$spending_gdp_targets;targets['Health']<-0
cases<-list(a,modifyList(a,list(health_cpi_wedge=.005,tot_change=.2)),
  modifyList(a,list(population_growth=.02,productivity=.017)),modifyList(a,list(spending_gdp_targets=targets)))
for(z in cases){p<-project_model(x,z,cal)
 for(id in c('all','h_hospital','h_medicines'))for(end in c(2040,2066))for(relative in c(FALSE,TRUE)){
  d<-health_per_person_data(p,id,end,relative);v<-d$annual;c<-d$contributions;k<-match(v$year,p$path$year)
  expected<-v$spending_bn*1e9/(p$path$domestic_price_index[k]*p$path$population[k])
  check(near(v$real_spending_per_person,expected),'Direct real spending per person identity')
  if(relative)expected<-100*expected/(cal$gdp0*1e9*p$path$real_output_index[k]/p$path$population[k])
  check(near(v$value,expected),'Selected measure identity')
  check(near(sum(c$contribution),tail(d$totals$value,1)-d$totals$value[1]),'Contributions reconcile to aggregate change')
  check(all(abs(c$contribution[c$factor=='Population size (removed per person)'])<1e-8),'Population size cancels before attribution')
  if(!relative)check(all(c$contribution[c$factor=='Real GDP per person denominator']==0),'No GDP denominator in dollar measure')
 }
}
# Known two-driver example tests symmetric interactions, dollar conversion and denominator.
p<-project_model(x,a,cal);p$path<-p$path[p$path$year%in%c(2030,2066),]
p$path$population<-c(2e6,3e6);p$path$population_index<-c(1,1.5)
p$path$domestic_price_index<-c(1,2);p$path$real_output_index<-c(1,2)
d<-p$subfunctions[p$subfunctions$id=='h_hospital' & p$subfunctions$year%in%c(2030,2066),]
for(f in setdiff(names(spending_factor_labels()),'gdp_factor'))d[[f]]<-1
d$population_factor<-c(1,1.5);d$price_factor<-c(1,2);d$income_factor<-c(1,1.2);d$wage_factor<-c(1,1.3)
d$anchor_2030_bn<-10;d$spending_bn<-c(10,46.8);p$subfunctions<-d
v<-health_per_person_data(p)
check(near(v$totals$value,c(5000,7800)),'Known real dollars per resident')
check(near(v$contributions$contribution[v$contributions$factor=='Income-related demand'],1150),'Income receives symmetric wage interaction')
check(near(v$contributions$contribution[v$contributions$factor=='Real wages / funding indexation'],1650),'Wages receive symmetric income interaction')
r<-health_per_person_data(p,relative=TRUE)
check(near(r$totals$value,100*c(10,10*1.2*1.3/(2/1.5))/cal$gdp0),'Relative measure uses real GDP per person')
check(near(sum(r$contributions$contribution),diff(r$totals$value)),'Relative contributions reconcile in known example')
cat('PASS:',n,'health per-person decomposition checks.\n')
