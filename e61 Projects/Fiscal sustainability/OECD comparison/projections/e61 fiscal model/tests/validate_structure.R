args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions()
checks<-data.frame(check=character(),passed=logical())
check<-function(label,z){checks[nrow(checks)+1,]<<-list(label,isTRUE(z));if(!isTRUE(z))stop('Failed: ',label)}
near<-function(x,y,tol=1e-6)all(is.finite(c(x,y)))&&max(abs(x-y))<tol
b<-project_model(x,a,cal);p<-b$path;t<-p$year-2030
check('Requested defaults: constant hours, 1.2 percent population, 3.2 percent NGDP per capita',
  a$hours_growth==0 && a$population_growth==.012 && a$ngdp_per_capita_growth==.032)
hi<-project_model(x,modifyList(a,list(productivity=.017)),cal)$path
hours<-project_model(x,modifyList(a,list(hours_growth=.002)),cal)$path
pop_a<-modifyList(a,list(population_growth=.016));pop_x<-economic_inputs(x,pop_a)
pop<-project_model(x,pop_a,cal)
nom<-project_model(x,modifyList(a,list(ngdp_per_capita_growth=.04)),cal)$path
check('GDP exactly unchanged at 1.2 and 1.7 percent productivity',identical(p$nominal_gdp,hi$nominal_gdp))
check('GDP exactly unchanged when average hours growth changes',identical(p$nominal_gdp,hours$nominal_gdp))
check('GDP growth is product of population and per-capita factors',near(p$nominal_gdp/p$nominal_gdp[1],((1+a$population_growth)*(1+a$ngdp_per_capita_growth))^t))
check('Per capita GDP compounds at the selected rate',near(p$nominal_gdp_per_capita/p$nominal_gdp_per_capita[1],(1+a$ngdp_per_capita_growth)^t))
check('Population compounds at the selected rate',near(p$population/p$population[1],(1+a$population_growth)^t))
check('Average hours compound at the selected rate',near(p$average_hours/p$average_hours[1],(1+a$hours_growth)^t))
check('More productivity raises real output and lowers prices exactly',near(hi$real_output_index/p$real_output_index,(1.017/1.012)^t)&&near(hi$domestic_price_index/p$domestic_price_index,(1.012/1.017)^t))
check('More hours raises real output without raising nominal GDP',near(hours$real_output_index/p$real_output_index,1.002^t)&&all(hours$domestic_price_index[-1]<p$domestic_price_index[-1]))
check('Accounting identity reconciles GDP real output prices and ToT',near(p$nominal_gdp/p$nominal_gdp[1],p$real_output_index*p$domestic_price_index*p$income_effect))
check('More population does not change per-capita GDP',near(pop$path$nominal_gdp_per_capita,p$nominal_gdp_per_capita))
check('More population scales employment and total GDP together',near(pop$path$employed/p$employed,(1.016/1.012)^t)&&near(pop$path$nominal_gdp/p$nominal_gdp,(1.016/1.012)^t))
check('Age-sex population cells sum to the macro population',near(as.numeric(tapply(pop_x$population$population,pop_x$population$year,sum)),pop_x$labour$population))
q<-x$population;z<-pop_x$population
shares<-function(d)d$population/as.numeric(tapply(d$population,d$year,sum))[match(d$year,sort(unique(d$year)))]
check('Population scenario preserves each age-sex share',near(shares(q),shares(z)))
check('Scaled NDIS path equals full cohort recomputation',near(pop_x$ndis$participants,ndis_projection(pop_x)$participants,.001))
check('Eligible pensioners use the same scenario population',near(pop$pension$eligible_population,
  as.numeric(tapply(ifelse(z$age>=67,z$population,0),z$year,sum))[match(p$year,sort(unique(z$year)))]))
e<-subset(pop$components,year==2066 & programme=='Health')$exposure_index
f<-subset(b$components,year==2066 & programme=='Health')$exposure_index
check('Population also changes service demand',near(e/f,(1.016/1.012)^36))
check('Shared 2030 fiscal and GDP anchors unchanged',near(pop$path$nominal_gdp[1],p$nominal_gdp[1])&&near(pop$path$primary_expense_bn[1],p$primary_expense_bn[1])&&near(hi$net_debt_bn[1],p$net_debt_bn[1]))
check('Higher per-capita GDP raises NGDP but not real output',all(nom$nominal_gdp[-1]>p$nominal_gdp[-1])&&near(nom$real_output_index,p$real_output_index))
check('Legacy NGDP arguments fail explicitly',inherits(try(project_model(x,modifyList(a,list(ngdp_override=TRUE)),cal),silent=TRUE),'try-error'))
write.csv(checks,'outputs/structure_validation_checks.csv',row.names=FALSE)
cases<-list(Reference=b$path,'Productivity 1.7 percent'=hi,'Average hours growth 0.2 percent'=hours,'Population growth 1.6 percent'=pop$path)
report<-do.call(rbind,lapply(names(cases),function(n)transform(cases[[n]],case=n)))
write.csv(report,'outputs/structure_regression.csv',row.names=FALSE)
cat(nrow(checks),'new-structure checks passed.\n')
