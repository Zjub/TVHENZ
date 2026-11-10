args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1]);setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();cal<-calibrate(x)
r<-project_model(x,default_assumptions(),cal)
a<-default_assumptions();a$productivity<-.017;a$spending_gdp_targets['Education']<-5
s<-project_model(x,a,cal)
all<-fiscal_outlook_data(s,r);choices<-fiscal_outlook_measures()
stopifnot(nrow(all)==2*37*length(choices),!anyDuplicated(all[c('year','case','measure')]))
for(key in unname(choices)){
 d<-fiscal_outlook_data(s,r,key)
 stopifnot(nrow(d)==74,identical(d$value[d$case=='Your scenario'],s$path[[key]]),
   identical(d$value[d$case=='Reference'],r$path[[key]]),
   isTRUE(all.equal(d,all[all$measure==key,],check.attributes=FALSE)),
   all(d$unit==if(key=='real_primary_per_person')'2030 dollars per person' else '% of GDP'))
}
stopifnot(all(all$assumption_productivity[all$case=='Your scenario']==.017),
  all(all$assumption_productivity[all$case=='Reference']==.012))
target<-jsonlite::fromJSON(all$assumption_spending_gdp_targets[1])
stopifnot(target$Education==5,is.null(target$Health))
file<-tempfile(fileext='.csv');write.csv(all,file,row.names=FALSE);back<-read.csv(file,check.names=FALSE)
stopifnot(max(abs(back$value-all$value))<1e-8,identical(back$case,all$case),identical(back$unit,all$unit))
for(key in list('not_a_measure',c('net_debt_pct','net_debt_pct'),character()))
 stopifnot(inherits(try(fiscal_outlook_data(s,r,key),silent=TRUE),'try-error'))
cat('PASS: all six outlook measures, both cases, annual values, units, case-specific assumptions and CSV round-trip.\n')
