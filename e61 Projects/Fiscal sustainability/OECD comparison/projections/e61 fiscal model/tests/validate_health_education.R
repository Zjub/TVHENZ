args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions();b<-project_model(x,a,cal)
checks<-data.frame(check=character(),passed=logical())
check<-function(label,z){checks[nrow(checks)+1L,]<<-list(label,isTRUE(z));if(!isTRUE(z))stop('Failed: ',label)}
near<-function(x,y,tol=1e-7)all(is.finite(c(x,y)))&&max(abs(x-y))<tol
hp<-x$health_education_parameters;t<-b$path$year-2030
check('Nine health/education specifications cover exactly the selected subfunctions',setequal(hp$id,subset(cal$subfunctions,programme%in%c('Health','Education'))$id))
check('Source and judgment status is explicit for every parameter block',all(nzchar(hp$evidence_class)&nzchar(hp$locator)&nzchar(hp$limitation)))
check('Observed hospital input share matches national workbook',near(hp$wage_weight[hp$id=='h_hospital'],sum(as.numeric(readxl::read_excel('data/raw/health_education/hospital_costs.xlsm','5 Acute Overview',col_names=FALSE,.name_repair='minimal')[[28]][47:52]))/6415.579944))
check('School input share excludes notional user cost of capital',near(hp$wage_weight[hp$id=='e_school'],(31447043+11301892+19259)/56279069))
check('University employee share matches published accounts',near(hp$wage_weight[hp$id=='e_higher'],23891495/43068392))
check('VET index weight follows stated agreement arithmetic',near(hp$wage_weight[hp$id=='e_vet'],.85*.75+.15*.4))
check('Separate higher/VET allocations sum to original ABS tertiary control',near(sum(subset(cal$subfunctions,id%in%c('e_higher','e_vet'))$source_2025_bn),51.439))
check('VET and university demand respond to different age bases',!near(subset(b$subfunctions,id=='e_vet')$exposure_index,subset(b$subfunctions,id=='e_higher')$exposure_index))
for(id in hp$id){
 d<-b$subfunctions[b$subfunctions$id==id,];p<-hp[hp$id==id,]
 expected<-if(p$cost_rule=='funding_proxy')(1+p$wage_weight*a$productivity)^t else 1-p$wage_weight+p$wage_weight*b$path$productivity_index
 check(paste(id,'unit cost follows its own rule'),near(d$real_unit_factor,expected))
 check(paste(id,'coverage/intensity/price assumptions are separate'),near(d$coverage_factor,(1+p$coverage_growth)^t)&&near(d$intensity_factor,(1+p$intensity_growth)^t)&&near(d$relative_price_factor,(1+a$relative_price)^(t*p$relative_price_loading)))
 check(paste(id,'aggregate scaling neutral at default'),near(d$aggregate_cost_factor,rep(1,length(t))))
}
check('Medicine unit prices have no inherited wage exposure or service premium',all(subset(b$subfunctions,id=='h_medicines')$real_unit_factor==1)&all(subset(b$subfunctions,id=='h_medicines')$relative_price_factor==1))
for(n in c('Health','Education')){
 key<-cost_pass_through_parameters()[[n]];aa<-a;aa[[key]]<-.01;z<-project_model(x,aa,cal)
 d<-b$subfunctions[b$subfunctions$programme==n,];e<-z$subfunctions[z$subfunctions$programme==n,]
 check(paste(n,'common multiplier applies to every cost'),near(e$spending_bn/d$spending_bn,1.01^(e$year-2030)))
 check(paste(n,'relative subfunction differences preserved'),near(e$real_unit_factor,d$real_unit_factor)&&near(e$exposure_index,d$exposure_index)&&near(e$intensity_factor,d$intensity_factor))
 check(paste(n,'multiplier has its own exact contribution'),sum(abs(subset(spending_contributions(z),programme==n & factor=='Aggregate cost scaling')$contribution))>0)
 aa$productivity<-0;zz<-project_model(x,aa,cal);a0<-a;a0$productivity<-0;z0<-project_model(x,a0,cal)
 check(paste(n,'cost scaling works at zero productivity'),near(subset(zz$components,programme==n)$spending_bn/subset(z0$components,programme==n)$spending_bn,1.01^t))
 aa<-a;aa[[key]]<--.01;zz<-project_model(x,aa,cal)
 check(paste(n,'negative cost adjustment compounds correctly'),near(subset(zz$components,programme==n)$spending_bn/subset(b$components,programme==n)$spending_bn,.99^t))
 aa[[key]]<-.021;check(paste(n,'out of range cost adjustment rejected'),inherits(try(project_model(x,aa,cal),silent=TRUE),'try-error'))
}
for(field in c('service_productivity_growth','coverage_growth','intensity_growth')){
 xx<-x;xx$health_education_parameters[[field]][xx$health_education_parameters$id=='h_hospital']<-.01
 z<-project_model(xx,a,cal);d<-subset(z$subfunctions,id=='h_hospital');e<-subset(b$subfunctions,id=='h_hospital')
 expected<-switch(field,service_productivity_growth=1.01^(-t),coverage_growth=1.01^t,intensity_growth=(1.01/1.005)^t)
 check(paste(field,'changes only intended factor without double counting'),near(d$spending_bn/e$spending_bn,expected))
 check(paste(field,'other health blocks unchanged'),near(subset(z$subfunctions,programme=='Health' & id!='h_hospital')$spending_bn,subset(b$subfunctions,programme=='Health' & id!='h_hospital')$spending_bn))
}
# A frozen pre-change engine and inputs make revisions reviewable.
old<-new.env(parent=globalenv());root<-'archive/pre_health_education_2026_10_27'
sys.source(file.path(root,'model_v2.R'),envir=old);sys.source(file.path(root,'spending_bottom_up.R'),envir=old)
ox<-old$load_inputs();ox$subfunctions<-read.csv(file.path(root,'spending_subfunctions.csv'))
ob<-old$project_model(ox,old$default_assumptions(),old$calibrate(ox))
check('All eight category anchor dollars remain unchanged',near(subset(ob$components,year==2030)$spending_bn,subset(b$components,year==2030)$spending_bn))
check('Other six primary spending categories unchanged in every year',near(subset(ob$components,!programme%in%c('Health','Education'))$spending_bn,subset(b$components,!programme%in%c('Health','Education'))$spending_bn))
check('GDP path and tax projection rules unchanged',identical(ob$path$nominal_gdp,b$path$nominal_gdp)&&near(ob$revenue$personal_income,b$revenue$personal_income)&&near(ob$revenue$company,b$revenue$company))
report<-merge(subset(ob$components,year==2066,c('programme','spending_bn','share_gdp')),subset(b$components,year==2066,c('programme','spending_bn','share_gdp')),by='programme',suffixes=c('_previous','_updated'))
report$change_pp<-report$share_gdp_updated-report$share_gdp_previous
write.csv(report,'outputs/health_education_revisions.csv',row.names=FALSE)
write.csv(checks,'outputs/health_education_validation_checks.csv',row.names=FALSE)
write.csv(b$subfunctions,'outputs/health_education_annual_factors.csv',row.names=FALSE)
cat(nrow(checks),'health/education mechanism checks passed.\n');print(report,row.names=FALSE)
cat('2066 net debt: previous',tail(ob$path$net_debt_pct,1),'updated',tail(b$path$net_debt_pct,1),'\n')
