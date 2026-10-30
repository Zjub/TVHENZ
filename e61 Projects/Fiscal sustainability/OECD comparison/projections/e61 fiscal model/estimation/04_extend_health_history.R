# Separate, reproducible historical work; no model is estimated when a slider moves.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(dirname(script),mustWork=TRUE))
dir.create('outputs/extended',recursive=TRUE,showWarnings=FALSE)
file<-'data/raw/national_accounts_gfce_2024_25.xlsx'
url<-'https://www.abs.gov.au/statistics/economy/national-accounts/australian-system-national-accounts/2024-25/5204035_Government_FCE.xlsx'
if(!file.exists(file)){
 if(!'--acquire'%in%commandArgs(TRUE))stop('Run with --acquire to cache the archived ABS workbook')
 curl::curl_download(url,file,quiet=TRUE)
}
rx<-function(p,s='Data1')as.data.frame(suppressMessages(readxl::read_excel(p,s,col_names=FALSE)))
num<-function(v)suppressWarnings(as.numeric(v))
series<-function(p,label,s='Data1'){
 d<-rx(p,s);j<-which(trimws(as.character(d[1,]))==trimws(label));stopifnot(length(j)==1)
 date<-as.Date(num(d[-(1:10),1]),origin='1899-12-30');v<-num(d[-(1:10),j])
 data.frame(year=as.integer(format(date,'%Y')),value=v,series_id=as.character(d[10,j]))[is.finite(v),]
}
gfce<-do.call(rbind,lapply(c('Health','Education'),function(n){d<-series(file,paste0('General government ;  ',n,' ;'));d$programme<-n;d$spending_bn<-d$value/1000;d}))
write.csv(gfce,'outputs/extended/gfce_history_1963_2025.csv',row.names=FALSE)
pop<-do.call(rbind,lapply(c('Data1','Data2'),function(s){d<-rx('data/raw/population.xlsx',s)
 js<-which(grepl('Population ;  Persons ;  [0-9]+( and over)? ;$',as.character(d[1,])))
 do.call(rbind,lapply(js,function(j)data.frame(year=as.integer(format(as.Date(num(d[-(1:10),1]),origin='1899-12-30'),'%Y')),
 age=as.integer(sub('.*Persons ;  ([0-9]+).*','\\1',as.character(d[1,j]))),population=num(d[-(1:10),j]))))
}))
stopifnot(!anyDuplicated(pop[c('year','age')]),all(table(pop$year)==101))
mid<-merge(transform(pop,year=year+1),pop,by=c('year','age'),suffixes=c('_start','_end'))
mid$population<-(mid$population_start+mid$population_end)/2
ap<-read.csv('data/raw/age_profiles.csv');spec<-read.csv('data/raw/subfunctions.csv');params<-read.csv('data/raw/service_parameters.csv')
spec<-merge(spec,params[c('id','wage_weight','cost_rule')],by='id')
demand<-function(profile){
 weight<-switch(profile,population=rep(1,nrow(mid)),school_age=as.numeric(mid$age>=5&mid$age<=17),
 tertiary_age=as.numeric(mid$age>=18&mid$age<=24),vet_age=as.numeric(mid$age>=15&mid$age<=64),{
 p<-ap[ap$profile==profile,];vapply(mid$age,function(age)p$weight[which(age>=p$age_min & age<=p$age_max)],numeric(1))})
 v<-tapply(mid$population*weight,mid$year,sum);v/v['2025']
}
cpi<-series('data/raw/cpi.xlsx','Index Numbers ;  All groups CPI ;  Australia ;')
# Financial-year average: preserve quarterly dates instead of year-only helper.
d<-rx('data/raw/cpi.xlsx');j<-which(as.character(d[1,])=='Index Numbers ;  All groups CPI ;  Australia ;')
dates<-as.Date(num(d[-(1:10),1]),origin='1899-12-30');cy<-as.integer(format(dates,'%Y'))+as.integer(format(dates,'%m')>'06')
cpi<-aggregate(value~year,data.frame(year=cy,value=num(d[-(1:10),j])),mean)
na<-'data/raw/national_accounts.xlsx'
ypc<-series(na,'GDP per capita: Chain volume measures ;');prod<-series(na,'GDP per hour worked: Index ;')
coe<-series(na,'Compensation of employees per hour: Current prices ;')
wpi<-readRDS('data/raw/wpi.rds');wpi<-wpi[wpi$series=='Financial Year Index ;  Total hourly rates of pay excluding bonuses ;  Australia ;  Private and Public ;  All industries ;',]
wpi<-data.frame(year=as.integer(format(wpi$date,'%Y')),value=wpi$value)
gfs<-read.csv('data/raw/gfs_purpose.csv');gfs<-gfs[gfs$item%in%c('Health','Education'),]
gfs<-data.frame(year=gfs$year,programme=gfs$item,spending_bn=gfs$value/1000,series_id='ABS GFS expenses')
bridge<-merge(gfs[c('year','programme','spending_bn')],gfce[c('year','programme','spending_bn')],by=c('year','programme'),suffixes=c('_gfs','_gfce'))
bridge$gfs_to_gfce_ratio<-bridge$spending_bn_gfs/bridge$spending_bn_gfce
write.csv(bridge,'outputs/extended/gfs_gfce_overlap.csv',row.names=FALSE)
make<-function(source,n,wage_basis,start){
 z<-if(source=='GFCE')gfce[gfce$programme==n,] else gfs[gfs$programme==n,]
 z<-z[z$year>=start & z$year<=2025,];z<-z[order(z$year),];y<-z$year
 P<-cpi$value[match(y,cpi$year)];P<-P/P[y==2025]
 w<-if(wage_basis=='WPI')wpi else coe;W<-w$value[match(y,w$year)];W<-W/W[y==2025]/P
 A<-prod$value[match(y,prod$year)];A<-A/A[y==2025]
 Q<-cumprod(c(1,1+.25*(A[-1]/head(A,-1)-1)));Q<-Q/Q[y==2025]
 s<-spec[spec$programme==n,];weights<-s$base_2025_bn/sum(s$base_2025_bn);D<-DU<-rep(0,length(y))
 for(i in seq_len(nrow(s))){
  E<-demand(s$profile[i])[as.character(y)]
  U<-switch(s$cost_rule[i],input_mix=(1-s$wage_weight[i])+s$wage_weight[i]*W/Q,cpi_proxy=rep(1,length(y)),
    funding_proxy={v<-cumprod(c(1,1+s$wage_weight[i]*(W[-1]/head(W,-1)-1)));v/tail(v,1)})
  D<-D+weights[i]*E;DU<-DU+weights[i]*E*U
 }
 U<-DU/D;D<-D/D[1];U<-U/U[1];Y<-ypc$value[match(y,ypc$year)];Y<-Y/Y[1]
 epsilon<-if(n=='Health').8 else 0
 out<-data.frame(source,programme=n,wage_basis,year=y,spending_bn=z$spending_bn,demographic_index=as.numeric(D),
 price_index=P/P[1],unit_cost_index=U,real_gdp_per_capita_index=Y,income_elasticity=epsilon,income_index=Y^epsilon,
 residual_index=z$spending_bn/(D*(P/P[1])*U*Y^epsilon))
 out$residual_index<-out$residual_index/out$residual_index[1];out
}
annual<-do.call(rbind,lapply(c('Health','Education'),function(n)rbind(make('GFCE',n,'WPI',1999),make('GFCE',n,'Compensation/hour',1987),make('GFS',n,'WPI',2016))))
stopifnot(all(is.finite(annual$residual_index)))
fit<-function(d,label){m<-lm(log(residual_index)~year,d);data.frame(source=d$source[1],programme=d$programme[1],wage_basis=d$wage_basis[1],window=label,
 start=min(d$year),end=max(d$year),n=nrow(d),income_elasticity=d$income_elasticity[1],productivity_share=.25,residual_growth=expm1(unname(coef(m)[2])),
 log_slope_se=summary(m)$coefficients[2,2],evidence='Conditional net residual; policy, mix and measurement remain; GFCE is not GFS expenses')}
fits<-do.call(rbind,lapply(split(annual,interaction(annual$source,annual$programme,annual$wage_basis,drop=TRUE)),function(d){
 a<-list(fit(d,'Full common sample'))
 for(window in list(c(1999,2019),c(2009,2025),c(2016,2025))){z<-d[d$year>=window[1]&d$year<=window[2],];if(nrow(z)>=8)a[[length(a)+1]]<-fit(z,paste(window,collapse='-'))};do.call(rbind,a)
}))
write.csv(annual,'outputs/extended/annual_decomposition.csv',row.names=FALSE)
write.csv(fits,'outputs/extended/residual_estimates.csv',row.names=FALSE)
# Comparable recent subfunction spending: re-estimate after removing income demand.
short<-read.csv('outputs/annual_estimation_data.csv');short<-subset(short,wage_basis=='all_industry_wpi' & service_productivity_share==.25)
map<-c(h_hospital='Hospitals',h_primary='Primary/community care',h_referred='Referred medical services',h_medicines='Medicines',h_other='Other health / reconciliation')
runtime<-do.call(rbind,lapply(names(map),function(id){
 z<-short[short$group==map[id],];z<-z[order(z$year),];y<-z$year
 P<-z$price_index;Y<-ypc$value[match(y,ypc$year)];Y<-Y/Y[1]
 A<-z$aggregate_productivity_index;Q<-cumprod(c(1,1+.25*(A[-1]/head(A,-1)-1)));Q<-Q/tail(Q,1)
 p<-params[params$id==id,];U<-if(p$cost_rule=='cpi_proxy')rep(1,length(y)) else (1-p$wage_weight)+p$wage_weight*z$real_wage_index/Q
 U<-U/U[1];z$residual_index<-z$spending_bn/(z$demographic_index*P*U*Y^.8)
 z$income_index<-Y^.8;z$unit_cost_index<-U
 write.csv(z,paste0('outputs/extended/',id,'_income_residual.csv'),row.names=FALSE)
 b<-lm(log(residual_index)~year,z)
 data.frame(id,source='subfunctions',residual_growth=expm1(unname(coef(b)[2])),start=2016,end=2025,
  evidence='Matched AIHW government funding / GFS reconciliation; conditional on elasticity 0.8 and productivity share 25%')
}))
long<-subset(fits,source=='GFCE' & programme=='Health' & wage_basis=='WPI' & window=='Full common sample')
runtime<-rbind(runtime,data.frame(id=names(map),source='aggregate_long',residual_growth=long$residual_growth,start=1999,end=2025,
 evidence='Aggregate GFCE net residual applied to GFS subfunctions as explicit scope/allocation proxy; not matched subfunction estimates'))
write.csv(runtime,'../data/health_income_residuals.csv',row.names=FALSE)
manifest<-data.frame(file,source_url=url,bytes=file.info(file)$size,md5=unname(tools::md5sum(file)),vintage='2024-25 ASNA, last published Table 35',access_date='2026-10-30')
write.csv(manifest,'outputs/extended/source_manifest.csv',row.names=FALSE)
print(fits[fits$window=='Full common sample',],row.names=FALSE)
cat('Health residuals re-estimated; no fiscal anchors or other spending rules changed.\n')
