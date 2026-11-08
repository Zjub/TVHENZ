# Regression: changing tax indexation must move scenario debt, not the reference.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
invisible(source('app.R'))
tax_cases<-c('wage_indexed','cpi','fixed')
expected<-sapply(tax_cases,function(rule)tail(project_model(inputs,
  modifyList(default_assumptions(),list(tax_policy=rule)),calibration)$path$net_debt_pct,1))
stopifnot(expected['fixed']<expected['cpi'],expected['cpi']<expected['wage_indexed'])
shiny::testServer(server,{
  session$setInputs(productivity=1.2,ngdp_override=FALSE,ngdp=4.4,relative_price=.3,tot=0,
    policy='indexed',tax_policy='cpi',super_return=4,ndis_intensity=.5,interest=4.4,
    measure='net_debt_pct',debt_units='pct',debt_year=2066)
  initial_reference<-reference()$path
  stopifnot(a()$super_real_return==.04,
    grepl('5.5% a year in 2030-35',output$ndis_growth_readout,fixed=TRUE))
  session$setInputs(ndis_intensity=2)
  stopifnot(grepl('7.1% a year in 2030-35',output$ndis_growth_readout,fixed=TRUE))
  session$setInputs(relative_price=1.3)
  stopifnot(grepl('8.2% a year in 2030-35',output$ndis_growth_readout,fixed=TRUE))
  session$setInputs(ndis_intensity=.5,relative_price=.3,super_return=7)
  stopifnot(all(is.finite(scenario()$path$net_debt_pct)),
    tail(scenario()$path$net_debt_pct,1)<tail(initial_reference$net_debt_pct,1))
  session$setInputs(super_return=4)
  for(rule in tax_cases) {
    session$setInputs(tax_policy=rule)
    stopifnot(identical(reference()$path,initial_reference),
      abs(tail(scenario()$path$net_debt_pct,1)-expected[rule])<1e-7,
      identical(ref_a()$tax_policy,'cpi'))
    delta<-tail(scenario()$path$net_debt_pct-reference()$path$net_debt_pct,1)
    if(rule!='cpi')stopifnot(abs(delta)>1) else stopifnot(abs(delta)<1e-9)
    exported<-both()
    stopifnot(identical(exported$net_debt_pct[exported$case=='Reference'],initial_reference$net_debt_pct))
  }
  # Other policy/financing controls must also leave the comparison baseline fixed.
  session$setInputs(policy='standards',super_return=7,ndis_intensity=1,interest=5)
  stopifnot(identical(reference()$path,initial_reference))
})
for(rule in c('wage_indexed','fixed')) {
  a<-modifyList(default_assumptions(),list(tax_policy=rule))
  at<-scenario_attribution(inputs,a,calibration)
  stopifnot(abs(sum(at$net_debt_change_pp)-(expected[rule]-expected['cpi']))<1e-7,
    abs(at$net_debt_change_pp[at$driver=='Policy and financing'])>1,
    all(abs(at$net_debt_change_pp[at$driver!='Policy and financing'])<1e-9))
}
mixed<-modifyList(default_assumptions(),list(tax_policy='fixed',productivity=.02,relative_price=.008,tot_change=.1,
  ngdp_override=TRUE,ngdp_growth=.05,interest_rate=.05))
at<-scenario_attribution(inputs,mixed,calibration)
stopifnot(abs(sum(at$net_debt_change_pp)-(tail(project_model(inputs,mixed,calibration)$path$net_debt_pct,1)-expected['cpi']))<1e-7)
write.csv(data.frame(rule=tax_cases,net_debt_2066_pct=unname(expected),
  difference_from_fixed_reference_pp=unname(expected-expected['cpi'])),
  'outputs/indexation_regression_results.csv',row.names=FALSE)
cat('Indexation regression checks passed: scenario debt, fixed reference, exports and policy attribution.\n')
print(round(expected,2))
