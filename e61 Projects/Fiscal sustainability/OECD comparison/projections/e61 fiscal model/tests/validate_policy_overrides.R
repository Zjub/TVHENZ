args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions();b<-project_model(x,a,cal)
checks<-data.frame(check=character(),passed=logical())
check<-function(label,v){checks[nrow(checks)+1L,]<<-list(label,isTRUE(v));if(!isTRUE(v))stop(label)}
near<-function(x,y)length(x)==length(y)&&all(is.finite(c(x,y)))&&max(abs(x-y))<1e-8
for(p in names(spending_target_parameters())){
 t<-a;t$spending_gdp_targets[p]<-3;z<-project_model(x,t,cal)
 d<-subset(z$components,programme==p)
 check(paste(p,'fixed at target after anchor'),near(d$share_gdp[d$year>2030],rep(3,36)))
 check(paste(p,'common anchor preserved'),near(subset(z$subfunctions,year==2030)$spending_bn,subset(b$subfunctions,year==2030)$spending_bn))
 u<-z$subfunctions[z$subfunctions$programme==p & z$subfunctions$year>2030,]
 v<-b$subfunctions[b$subfunctions$programme==p & b$subfunctions$year>2030,]
 check(paste(p,'underlying spending exported'),near(u$unconstrained_spending_bn,v$spending_bn))
 ratio<-u$spending_bn/u$unconstrained_spending_bn
 check(paste(p,'preserves projected within-function shares'),all(vapply(split(ratio,u$year),function(v)diff(range(v))<1e-10,logical(1))))
 other<-subset(z$components,programme!=p)
 check(paste(p,'other primary functions unaffected'),near(other$spending_bn,subset(b$components,programme!=p)$spending_bn))
 f<-setdiff(names(spending_factor_labels()),'gdp_factor')
 check(paste(p,'policy factor reconstructs spending'),near(u$spending_bn,u$anchor_2030_bn*apply(u[,f],1,prod)))
}
t<-a;t$spending_gdp_targets[c('Health','NDIS','Defence')]<-c(8,0,2)
z<-project_model(x,t,cal)
for(unit in c('pct','bn','real_bn')){
 d<-spending_contributions(z,2030,2066,unit,include_financing=TRUE);u<-d[!duplicated(d$id),]
 check(paste('Multiple/zero targets: exact decomposition',unit),near(sum(d$contribution),sum(u$change)))
}
check('Zero NDIS target gives zero future spending',all(subset(z$components,programme=='NDIS'&year>2030)$spending_bn==0))
t<-a;t$interest_debt_sensitivity_bps<-3;z<-project_model(x,t,cal);d<-z$path
anchor<-b$path$net_debt_pct[1]
expected<-pmax(0,d$base_interest_rate[-1]+3/10000*(head(d$net_debt_pct,-1)-anchor))
check('Three-bp coefficient uses lagged net debt and correct units',near(d$effective_interest_rate[-1],expected))
check('First projection year has zero debt feedback',d$debt_interest_adjustment[2]==0)
check('Interest is still charged on opening gross debt',near(d$interest_bn[-1],d$effective_interest_rate[-1]*head(d$gross_debt_bn,-1)))
check('Positive feedback increases default debt endpoint',tail(d$net_debt_pct,1)>tail(b$path$net_debt_pct,1))
check('Zero coefficient preserves base rates',identical(b$path$effective_interest_rate,b$path$base_interest_rate))
check('Debt feedback preserves macro GDP',identical(d$nominal_gdp,b$path$nominal_gdp))
check('Debt feedback does not reprice financial assets',identical(d$financial_assets_bn,b$path$financial_assets_bn))
t<-a;t$spending_gdp_targets[]<-0;t$interest_debt_sensitivity_bps<-5
low<-project_model(x,t,cal)$path
check('Falling net debt gives negative adjustment',any(low$debt_interest_adjustment<0))
check('Borrowing rate floor prevents negative rates',all(low$effective_interest_rate>=0))
check('Zero borrowing rate reached under low-debt scenario',any(low$effective_interest_rate==0))
t<-a;t$spending_gdp_targets['Health']<-8;t$interest_debt_sensitivity_bps<-3
z<-project_model(x,t,cal)
check('Policy/debt-rate attribution reconciles',near(sum(scenario_attribution(x,t,cal)$net_debt_change_pp),tail(z$path$net_debt_pct-b$path$net_debt_pct,1)))
for(bad in list(list(spending_gdp_targets=c(Health=-1)),list(spending_gdp_targets=c(Unknown=3)),
 list(interest_debt_sensitivity_bps=-1),list(interest_debt_sensitivity_bps=Inf)))
 check('Invalid target/sensitivity rejected',inherits(try(project_model(x,modifyList(a,bad),cal),silent=TRUE),'try-error'))
write.csv(checks,'outputs/policy_override_validation_checks.csv',row.names=FALSE)
cat(nrow(checks),'fixed-share and debt-rate checks passed.\n')
