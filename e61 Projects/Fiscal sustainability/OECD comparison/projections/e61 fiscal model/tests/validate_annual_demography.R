args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();a<-default_assumptions()
raw<-as.data.frame(readxl::read_excel('data/raw/v2/igr_2026_population_age_sex.xlsx','PROJECTIONS',skip=4))
n<-0;check<-function(v,msg){if(!isTRUE(v))stop(msg);n<<-n+1L}
for(y in 2026:2066){
 z<-subset(x$population,year==y)
 source<-raw[raw$Sex%in%c('Males','Females'),]
 key<-paste(ifelse(source$Sex=='Males','Male','Female'),source$Age)
 check(all(z$population==as.numeric(source[[sprintf('%d-%02d',y-1,y%%100)]])[match(paste(z$sex,z$age),key)]),
   paste('Exact source cells',y))
}
lo<-economic_inputs(x,modifyList(a,list(population_growth=0)))
hi<-economic_inputs(x,modifyList(a,list(population_growth=.02)))
for(y in 2030:2066){
 l<-subset(lo$population,year==y);h<-subset(hi$population,year==y)
 check(max(abs(l$population/sum(l$population)-h$population/sum(h$population)))<1e-14,'Neutral age/sex shares')
 check(abs(sum(h$population)/sum(l$population)-1.02^(y-2030))<1e-12,'Population growth identity')
 check(abs(hi$ndis$participants[hi$ndis$year==y]/lo$ndis$participants[lo$ndis$year==y]-1.02^(y-2030))<1e-12,'NDIS shares remain neutral')
}
check(identical(sort(unique(x$population$year)),2025:2066),'Every model year covered')
check(all(table(x$population$year)==202),'Complete annual age/sex grids')
p<-project_model(x,a,calibrate(x));check(all(is.finite(p$path$net_debt_pct)),'Projection finite')
cat(n,'annual demographic source and integration checks passed.\n')
