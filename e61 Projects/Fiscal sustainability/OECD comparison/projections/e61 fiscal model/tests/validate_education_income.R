args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1]);setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions()
b<-project_model(x,a,cal);p<-project_model(x,modifyList(a,list(education_income_elasticity=.5)),cal)
same<-function(x,y)isTRUE(all.equal(x,y,tolerance=1e-9,check.attributes=FALSE))
n<-0;check<-function(ok,label){if(!isTRUE(ok))stop(label);n<<-n+1}
be<-subset(b$subfunctions,programme=='Education');pe<-subset(p$subfunctions,programme=='Education')
check(a$education_income_elasticity==.2 && same(be$income_factor,be$real_gdp_per_capita_index^.2),'Default 0.2 income response')
zero<-project_model(x,modifyList(a,list(education_income_elasticity=0)),cal)
check(all(subset(zero$subfunctions,programme=='Education')$income_factor==1),'Zero elasticity excludes additional income demand')
check(same(pe$income_factor,pe$real_gdp_per_capita_index^.5),'Correct real income factor')
check(same(pe$spending_bn/be$spending_bn,pe$income_factor/be$income_factor),'Only the additional income factor changes education spending')
check(same(subset(pe,year==2030)$spending_bn,subset(be,year==2030)$spending_bn),'2030 anchor unchanged')
for(name in setdiff(unique(b$components$programme),'Education'))check(same(subset(p$components,programme==name)$spending_bn,subset(b$components,programme==name)$spending_bn),paste(name,'unchanged'))
for(key in c('nominal_gdp','real_output_index','population_index'))check(same(p$path[[key]],b$path[[key]]),paste(key,'unchanged'))
for(key in c('wage_factor','efficiency_factor','intensity_factor','coverage_factor'))check(same(pe[[key]],be[[key]]),paste(key,'unchanged'))
f<-setdiff(names(spending_factor_labels()),'gdp_factor')
check(same(p$subfunctions$spending_bn,p$subfunctions$anchor_2030_bn*apply(p$subfunctions[f],1,prod)),'Factors reconstruct spending')
cc<-subset(service_cost_comparison(p,x),programme=='Education');check(all(cc$selected_income_elasticity==.5)&all(cc$income_growth>0),'Cost inspection reports income channel')
for(unit in c('pct','bn','real_bn')){
 d<-spending_contributions(p,measure=unit,include_financing=TRUE);ends<-d[!duplicated(d$id),]
 check(abs(sum(d$contribution)-sum(ends$change))<1e-6,'Shapley reconciliation')
}
for(v in list(-.1,1.6,NA_real_,c(0,1)))check(inherits(try(project_model(x,modifyList(a,list(education_income_elasticity=v)),cal),silent=TRUE),'try-error'),'Invalid elasticity rejected')
z<-a;z$education_income_elasticity<-.5;z$spending_gdp_targets['Education']<-5
target<-project_model(x,z,cal);check(max(abs(subset(target$components,programme=='Education'&year>2030)$share_gdp-5))<1e-9,'Fixed GDP share overrides income demand')
cat('PASS:',n,'education income checks.\n')
