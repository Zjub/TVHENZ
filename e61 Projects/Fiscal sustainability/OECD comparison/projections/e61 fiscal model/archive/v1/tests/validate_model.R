args <- commandArgs(FALSE)
script <- sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R')
x <- load_inputs(); c <- calibrate(x); a <- default_assumptions()
b <- project_model(x,a,c); m <- b$path
checks <- data.frame(check=character(),passed=logical())
check <- function(label, condition) {
  checks[nrow(checks)+1,] <<- list(label,isTRUE(condition))
  if(!isTRUE(condition)) stop('Failed: ',label)
}
near <- function(x,y,tol=1e-7) max(abs(x-y)) < tol
check('All annual results are finite',all(is.finite(as.matrix(m[,-c(which(names(m) %in% c('nominal_gdp_growth','real_growth','domestic_inflation')))]))))
check('Annual horizon and shared 2030 anchor',identical(m$year,2030:2066))
check('Programme allocations reconcile to PBO primary expenses',
      near(sum(c$programmes$anchor_2030_bn),c$gdp0*(c$official$expenses_ratio_gdp-c$official$public_debt_interest_ratio_gdp)))
check('No duplicate pension in 2025 GFS primary expenses',
      near(sum(c$programmes$base_2025_bn),sum(subset(x$purpose,year==2025)$value)/1000-
        subset(x$operating,year==2025 & item=='Interest expenses n.e.c.')$value/1000))
check('2030 revenue matches PBO',near(m$revenue_pct[1],100*c$official$revenue_ratio_gdp))
check('2030 total spending matches PBO',near(m$total_spending_pct[1],100*(c$official$expenses_ratio_gdp+c$official$net_capital_investment_ratio_gdp)))
check('Gross minus financial assets equals net debt',near(m$gross_debt_bn-m$financial_assets_bn,m$net_debt_bn))
check('Gross debt financing identity',near(diff(m$gross_debt_bn),-m$fiscal_balance_bn[-1]+m$net_financial_asset_acquisition_bn[-1]))
check('Net debt stock-flow identity',near(diff(m$net_debt_bn),-m$fiscal_balance_bn[-1]))
check('Interest uses opening gross debt',near(m$interest_bn[-1],head(m$gross_debt_bn,-1)*m$effective_interest_rate[-1]))
check('Component spending adds to aggregate',near(as.numeric(tapply(b$components$spending_bn,b$components$year,sum)),m$primary_expense_bn))
h <- project_model(x,modifyList(a,list(productivity=.022)),c)
check('Productivity changes real output by exact compounded amount',
      near(tail(h$path$real_output_index/m$real_output_index,1),(1.022/1.012)^36))
check('Linked productivity does not alter domestic inflation',near(h$path$domestic_price_index,m$domestic_price_index))
check('Calibration unchanged by scenario',near(h$path$primary_expense_bn[1],m$primary_expense_bn[1]))
n <- project_model(x,modifyList(a,list(ngdp_override=TRUE,ngdp_growth=.04)),c)
check('NGDP override is an exact nominal growth target',near(n$path$nominal_gdp_growth[-1],rep(.04,36)))
check('NGDP override leaves real output unchanged',near(n$path$real_output_index,m$real_output_index))
check('Nominal-real-price identity including terms of trade',
      near(n$path$nominal_gdp/c$gdp0,n$path$real_output_index*n$path$domestic_price_index*n$path$income_effect))
rp <- project_model(x,modifyList(a,list(relative_price=.013)),c)
check('Service price shock does not change real output',near(rp$path$real_output_index,m$real_output_index))
check('Service price shock does not directly change Age Pension',
      near(subset(rp$components,programme=='Age Pension')$spending_bn,subset(b$components,programme=='Age Pension')$spending_bn))
check('Service price shock raises Health spending',tail(subset(rp$components,programme=='Health')$spending_bn,1)>tail(subset(b$components,programme=='Health')$spending_bn,1))
tt <- project_model(x,modifyList(a,list(tot_change=.2)),c)
check('Terms-of-trade shock leaves real output unchanged',near(tt$path$real_output_index,m$real_output_index))
check('Terms-of-trade level reaches target and then stops growing',near(tt$path$tot_index[tt$path$year>=2040],rep(1.2,27)))
check('Resource receipts respond to terms of trade',tail(tt$revenue$resources,1)>tail(b$revenue$resources,1))
check('Negative productivity is allowed without a floor',tail(project_model(x,modifyList(a,list(productivity=-.005)),c)$path$productivity_index,1)<1)
all_controls <- modifyList(a,list(productivity=.02,ngdp_override=TRUE,ngdp_growth=.05,relative_price=.01,tot_change=.2))
s <- project_model(x,all_controls,c)
at <- scenario_attribution(x,all_controls,c)
check('Four-control attribution reconciles including interactions',
      near(sum(at$net_debt_change_pp),tail(s$path$net_debt_pct-m$net_debt_pct,1)))
# Stress both debt accumulation and gross-debt retirement with asset accumulation.
for (z in list(list(productivity=-.005,relative_price=.02,interest_rate=.07),
               list(productivity=.03,relative_price=-.005,interest_rate=.02))) {
  v <- project_model(x,modifyList(a,z),c)$path
  check(paste('Stress stock-flow identity',z$productivity),near(diff(v$net_debt_bn),-v$fiscal_balance_bn[-1],1e-6))
  check(paste('Stress gross financing identity',z$productivity),
        near(diff(v$gross_debt_bn),-v$fiscal_balance_bn[-1]+v$net_financial_asset_acquisition_bn[-1],1e-6))
  check(paste('Stress debt is non-negative',z$productivity),all(v$gross_debt_bn>=0))
  check(paste('Stress interest receipts use opening assets',z$productivity),
        near(v$interest_receipts_bn[-1],head(v$financial_assets_bn,-1)*c$asset_yield))
}
dir.create('outputs',showWarnings=FALSE)
write.csv(checks,'outputs/validation_checks.csv',row.names=FALSE)
write.csv(m,'outputs/reference_projection.csv',row.names=FALSE)
write.csv(b$components,'outputs/reference_components.csv',row.names=FALSE)
write.csv(c$programmes,'outputs/programme_calibration.csv',row.names=FALSE)
capture.output(sessionInfo(),file='outputs/session_info.txt')
cat(nrow(checks),'model and scenario checks passed.\n')
print(tail(m[,c('year','nominal_gdp','revenue_pct','total_spending_pct','net_debt_pct')],1))
