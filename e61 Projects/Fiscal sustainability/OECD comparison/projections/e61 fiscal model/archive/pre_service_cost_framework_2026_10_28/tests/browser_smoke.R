# Real-browser smoke check against the running app. Default is local;
# --url=https://e61-institute.shinyapps.io/Fiscal_tester/ checks deployment.
# Requires chromote, curl and an installed Chrome.
args <- commandArgs(FALSE)
script <- sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
url_argument<-grep('^--url=',commandArgs(TRUE),value=TRUE)
app_url<-if(length(url_argument))sub('^--url=','',url_argument[1]) else 'http://127.0.0.1:7461'
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
# The R server can still be loading packages immediately after a restart.
ready<-FALSE
for(attempt in 1:80) {
  ready<-isTRUE(tryCatch(curl::curl_fetch_memory(app_url,
    handle=curl::new_handle(timeout=5))$status_code==200,error=function(e)FALSE))
  if(ready)break
  Sys.sleep(.25)
}
stopifnot(ready)
b <- chromote::ChromoteSession$new(width=1440,height=1100)
b$default_timeout <- 45
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
on.exit(b$close())
js <- function(code) b$Runtime$evaluate(code,returnByValue=TRUE)$result$value
wait <- function(condition) {
  for(attempt in 1:160) {
    if(isTRUE(js(condition)))return(invisible(TRUE))
    Sys.sleep(.25)
  }
  stop('Browser check timed out: ',condition)
}
check_errors <- function() stopifnot(js("document.querySelectorAll('.shiny-output-error').length") == 0)
reset <- function(baseline) {
  js("document.getElementById('reset').click(); true")
  wait(paste0("document.querySelector('#metrics').innerText === ",jsonlite::toJSON(baseline,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
}
set_slider <- function(id,value) {
  js(sprintf("$('#%s').data('ionRangeSlider').update({from:%s}); Shiny.setInputValue('%s',%s,{priority:'event'}); true",id,value,id,value))
}
b$Page$navigate(app_url)
wait("document.querySelectorAll('.metric-value').length===4 && document.querySelector('#outlook img') && document.querySelector('#attribution img') && !document.documentElement.classList.contains('shiny-busy')")
check_errors()
baseline <- js("document.querySelector('#metrics').innerText")
stopifnot(js("$('#ngdp_per_capita').data('ionRangeSlider').result.from")==3.2,
  js("$('#population_growth').data('ionRangeSlider').result.from")==1.2,
  js("$('#hours_growth').data('ionRangeSlider').result.from")==0)
b$screenshot(filename='outputs/app_reference.png',selector='body')
# Check net-debt level AND the comparison gap for each threshold rule, with all
# economic assumptions at defaults. A generic metrics-change check misses a
# moving reference that leaves the reported gap at zero.
stopifnot(js("$('#tax_policy')[0].selectize.getValue()")=='wage_indexed')
source('R/model.R');test_inputs<-load_inputs();test_calibration<-calibrate(test_inputs)
reference_debt<-tail(project_model(test_inputs,default_assumptions(),test_calibration)$path$net_debt_pct,1)
ndis_label<-function(intensity){
  z<-project_model(test_inputs,modifyList(default_assumptions(),list(ndis_intensity_growth=intensity)),test_calibration)
  d<-subset(z$components,programme=='NDIS')
  sprintf('%.1f%% a year in 2030-35',100*((d$spending_bn[d$year==2035]/d$spending_bn[d$year==2030])^.2-1))
}
tax_tests<-lapply(c('cpi','fixed'),function(rule){
  debt<-tail(project_model(test_inputs,modifyList(default_assumptions(),list(tax_policy=rule)),test_calibration)$path$net_debt_pct,1)
  c(rule,sprintf('%.1f%%',debt),sprintf('%+.1f pp',debt-reference_debt))
})
js("document.getElementById('policy_financing_controls').open=true; true")
wait(paste0("document.querySelector('#ndis_growth_readout').innerText.includes(",jsonlite::toJSON(ndis_label(.005),auto_unbox=TRUE),")"))
stopifnot(js("$('#super_return').data('ionRangeSlider').result.from")==4,
  js("$('#super_return').data('ionRangeSlider').options.max")==7)
set_slider('ndis_intensity',2)
wait(paste0("document.querySelector('#ndis_growth_readout').innerText.includes(",jsonlite::toJSON(ndis_label(.02),auto_unbox=TRUE),") && !document.documentElement.classList.contains('shiny-busy')"))
reset(baseline)
set_slider('super_return',7)
wait(paste0("document.querySelector('#metrics').innerText !== ",jsonlite::toJSON(baseline,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
check_errors();reset(baseline)
stopifnot(js("$('#super_return').data('ionRangeSlider').result.from")==4)
for(test in tax_tests) {
  js(sprintf("$('#tax_policy')[0].selectize.setValue('%s'); true",test[1]))
  condition<-paste0("document.querySelector('#metrics .metric-value').innerText === ",
    jsonlite::toJSON(test[2],auto_unbox=TRUE)," && document.querySelector('#metrics .metric-change').innerText.includes(",
    jsonlite::toJSON(test[3],auto_unbox=TRUE),") && !document.documentElement.classList.contains('shiny-busy')")
  wait(condition);check_errors()
  reset(baseline)
}
baseline_gdp<-js("document.querySelectorAll('#metrics .metric-value')[2].innerText")
stopifnot(js("document.getElementById('ngdp_override')===null"))
for(control in c('productivity','ngdp_per_capita','population_growth','hours_growth','relative_price','tot')) {
  set_slider(control,switch(control,productivity=1.7,ngdp_per_capita=4,population_growth=1.6,hours_growth=.2,relative_price=.8,tot=15))
  wait(paste0("document.querySelector('#metrics').innerText !== ",jsonlite::toJSON(baseline,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
  check_errors()
  if(control%in%c('productivity','hours_growth'))stopifnot(identical(js("document.querySelectorAll('#metrics .metric-value')[2].innerText"),baseline_gdp))
  if(control=='productivity') {
    downloaded<-b$Runtime$evaluate("fetch(document.getElementById('download').href).then(r=>r.text())",awaitPromise=TRUE,returnByValue=TRUE)$result$value
    exported<-read.csv(text=downloaded)
    stopifnot(identical(exported$nominal_gdp[exported$case=='Your scenario'],exported$nominal_gdp[exported$case=='Reference']))
  }
  reset(baseline)
}
js("document.getElementById('cost_pass_through_controls').open=true; true")
for(control in unname(cost_pass_through_parameters())) {
  set_slider(control,if(grepl('cost_growth_adjustment',control))1 else 100)
  wait(paste0("document.querySelector('#metrics').innerText !== ",jsonlite::toJSON(baseline,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
  check_errors();reset(baseline)
  stopifnot(js(sprintf("$('#%s').data('ionRangeSlider').result.from",control))==100*default_assumptions()[[control]])
}
set_slider('ngdp_per_capita',4);set_slider('health_cost_growth_adjustment',1)
js("document.getElementById('reset_pass_through').click(); true")
wait("$('#health_cost_growth_adjustment').data('ionRangeSlider').result.from===0 && !document.documentElement.classList.contains('shiny-busy')")
stopifnot(js("$('#ngdp_per_capita').data('ionRangeSlider').result.from")==4)
reset(baseline)
js("$('#productivity').data('ionRangeSlider').update({from:2}); $('#ngdp_per_capita').data('ionRangeSlider').update({from:4}); $('#relative_price').data('ionRangeSlider').update({from:.8}); $('#tot').data('ionRangeSlider').update({from:15}); Shiny.setInputValue('productivity',2); Shiny.setInputValue('ngdp_per_capita',4); Shiny.setInputValue('relative_price',.8); Shiny.setInputValue('tot',15); true")
wait(paste0("document.querySelector('#metrics').innerText !== ",jsonlite::toJSON(baseline,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
changed <- js("document.querySelector('#metrics').innerText")
stopifnot(!identical(baseline,changed))
b$screenshot(filename='outputs/app_scenario.png',selector='body')
for(tab in c('channels','debt','components','revenue','method')) {
  js(sprintf("document.querySelector('a[data-value=\"%s\"]').click(); true",tab))
  cond <- switch(tab,channels="document.querySelector('#macro img')",
    debt="document.querySelector('#debt_stocks img') && document.querySelector('#debt_interest img') && document.querySelector('#debt_balance_table table') && document.querySelector('#debt_interest_table table') && document.querySelector('#debt_explanation').innerText.includes('gross debt minus')",
    components="document.querySelector('#components img') && document.querySelector('#spending_paths img') && document.querySelector('#spending_ngdp_comparison table') && document.querySelector('#spending_ngdp_link').innerText.includes('primary spending') && document.querySelector('#spending_contribution_plot img') && document.querySelector('#spending_subfunctions table') && document.querySelector('#spending_drivers table') && !document.querySelector('#revenue_table') && document.querySelector('#ndis_benchmark table') && document.querySelector('#ndis_summary').innerText.includes('nominal growth')",
    revenue="document.querySelector('#revenue_path img') && document.querySelector('#revenue_mix img') && document.querySelector('#company_tax_table table') && document.querySelector('#company_tax_summary').innerText.includes('Structural starting share') && document.querySelector('#tax_rules table') && document.querySelector('#revenue_endpoints table')",
    method="document.querySelector('#parameters table') && document.querySelector('#sources table') && document.querySelector('#coverage_bridge table') && document.querySelector('#assumption_register table')")
  wait(paste0(cond," && !document.documentElement.classList.contains('shiny-busy')"))
  check_errors()
  if(tab=='method') {
    stopifnot(js("document.querySelector('#assumption_register').innerText.includes('Superannuation tax')"))
    js("$('#register_domain')[0].selectize.setValue('Spending'); true")
    wait("document.querySelector('#assumption_register').innerText.includes('Relative service prices') && !document.documentElement.classList.contains('shiny-busy')")
    check_errors();b$screenshot(filename='outputs/app_evidence_register.png',selector='body')
  }
  if(tab=='debt') {
    js("$('#debt_units')[0].selectize.setValue('bn'); $('#debt_year')[0].selectize.setValue('2040'); true")
    wait("document.querySelector('#debt_explanation').innerText.includes('In 2040') && !document.documentElement.classList.contains('shiny-busy')")
    check_errors();b$screenshot(filename='outputs/app_debt.png',selector='body')
  }
  if(tab=='components') {
    stopifnot(js("document.querySelectorAll('#spending_subfunctions tbody tr').length")==24)
    stopifnot(js("document.querySelectorAll('#spending_drivers tbody tr').length")==24)
    js("$('#spending_group')[0].selectize.setValue('Health'); $('#contribution_measure')[0].selectize.setValue('real_bn'); $('#contribution_start')[0].selectize.setValue('2040'); true")
    wait("document.querySelectorAll('#spending_subfunctions tbody tr').length===5 && document.querySelector('#spending_contribution_summary').innerText.includes('From 2040') && document.querySelector('#spending_contribution_summary').innerText.includes('2030-dollar') && !document.documentElement.classList.contains('shiny-busy')")
    factor_csv<-b$Runtime$evaluate("fetch(document.getElementById('spending_factor_download').href).then(r=>r.text())",awaitPromise=TRUE,returnByValue=TRUE)$result$value
    fd<-read.csv(text=factor_csv)
    stopifnot(length(unique(fd$id))==5,all(fd$programme=='Health'),all(fd$start_year==2040),
      abs(sum(fd$contribution)-sum(fd$change[!duplicated(fd$id)]))<1e-6)
    wait("document.querySelectorAll('#spending_drivers tbody tr').length===5 && document.querySelector('#spending_driver_period').innerText.includes('2040') && document.querySelector('#spending_drivers').innerText.includes('2040 ($2030 bn)') && !document.documentElement.classList.contains('shiny-busy')")
    levels_csv<-b$Runtime$evaluate("fetch(document.getElementById('spending_driver_download').href).then(r=>r.text())",awaitPromise=TRUE,returnByValue=TRUE)$result$value
    levels<-read.csv(text=levels_csv)
    stopifnot(nrow(levels)==50,all(levels$start_year==2040),all(levels$programme=='Health'),
      all(levels$factor_ratio[levels$factor=='General prices']==1),
      max(abs(tapply(levels$factor_ratio,levels$id,prod)*tapply(levels$start_value,levels$id,unique)-tapply(levels$end_value,levels$id,unique)))<1e-6)
    js("$('#spending_group')[0].selectize.setValue('all'); true")
    wait("document.querySelectorAll('#spending_drivers tbody tr').length===26 && document.querySelector('#spending_drivers').innerText.includes('Net capital investment') && !document.documentElement.classList.contains('shiny-busy')")
    js("$('#spending_group')[0].selectize.setValue('primary'); $('#contribution_measure')[0].selectize.setValue('pct'); $('#contribution_start')[0].selectize.setValue('2030'); true")
    js("$('#spending_units')[0].selectize.setValue('bn'); true")
    wait("document.querySelector('#spending_paths img') && !document.documentElement.classList.contains('shiny-busy')")
    check_errors();b$screenshot(filename='outputs/app_spending.png',selector='#spending_paths')
    js("$('#spending_units')[0].selectize.setValue('pct'); true")
  }
  if(tab=='revenue') {
    before<-js("document.querySelector('#metrics').innerText")
    js("$('#tax_policy')[0].selectize.setValue('fixed'); true")
    wait(paste0("document.querySelector('#metrics').innerText !== ",jsonlite::toJSON(before,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
    check_errors()
    b$screenshot(filename='outputs/app_revenue.png',selector='body')
    # Reproduce the user's high-inflation/zero-productivity example. Company
    # tax/GDP must remain at its structural share; ToT is a separate level input.
    reset(baseline)
    js("$('#ngdp_per_capita').data('ionRangeSlider').update({from:8}); $('#productivity').data('ionRangeSlider').update({from:0}); Shiny.setInputValue('ngdp_per_capita',8); Shiny.setInputValue('productivity',0); true")
    wait("document.querySelector('#company_tax_summary').innerText.includes('your scenario has 4.90% of GDP') && document.querySelector('#metrics').innerText.includes('$86.0 tn') && !document.documentElement.classList.contains('shiny-busy')")
    company_csv<-b$Runtime$evaluate("fetch(document.getElementById('revenue_download').href).then(r=>r.text())",awaitPromise=TRUE,returnByValue=TRUE)$result$value
    company_data<-subset(read.csv(text=company_csv),head=='Company tax')
    stopifnot(max(abs(company_data$share_gdp-100*test_calibration$revenue_anchor['company']/test_calibration$gdp0))<1e-8)
    set_slider('tot',20)
    wait("document.querySelector('#company_tax_summary').innerText.includes('your scenario has 5.88% of GDP') && !document.documentElement.classList.contains('shiny-busy')")
    set_slider('company_tot_elasticity',2)
    wait("document.querySelector('#company_tax_summary').innerText.includes('your scenario has 7.06% of GDP') && !document.documentElement.classList.contains('shiny-busy')")
    check_errors();reset(baseline)
    stopifnot(js("$('#company_tot_elasticity').data('ionRangeSlider').result.from")==1)
  }
}
for(id in c('download','audit','revenue_download','debt_download','register_download','spending_detail_download','spending_factor_download')) {
  code <- sprintf("fetch(document.getElementById('%s').href).then(async r=>({status:r.status,text:await r.text()}))",id)
  result <- b$Runtime$evaluate(code,awaitPromise=TRUE,returnByValue=TRUE)$result$value
  stopifnot(result$status==200,nchar(result$text)>200)
  if(id=='download') stopifnot(grepl('scenario_assumption_productivity',result$text,fixed=TRUE),
                             grepl('Your scenario',result$text,fixed=TRUE),grepl('Reference',result$text,fixed=TRUE))
  if(id=='audit') stopifnot(grepl('programme_calibration',result$text,fixed=TRUE))
  if(id=='revenue_download') stopifnot(grepl('bracket_creep_revenue_bn',result$text,fixed=TRUE),grepl('Personal income tax',result$text,fixed=TRUE))
  if(id=='debt_download') stopifnot(grepl('opening_gross_debt_bn',result$text,fixed=TRUE),grepl('net_interest_pct',result$text,fixed=TRUE))
  if(id=='register_download') {
    d<-read.csv(text=result$text)
    stopifnot(nrow(d)==64,all(c('classification','source_url','model_addition','limitation')%in%names(d)))
  }
}
pdf_result<-b$Runtime$evaluate("fetch(document.getElementById('spending_background').href).then(async r=>({status:r.status,signature:new TextDecoder().decode((await r.arrayBuffer()).slice(0,5))}))",awaitPromise=TRUE,returnByValue=TRUE)$result$value
stopifnot(pdf_result$status==200,pdf_result$signature=='%PDF-')
reset(baseline)
js("document.querySelector('a[data-value=\"outlook\"]').click(); true")
wait("document.querySelector('#outlook img') && !document.documentElement.classList.contains('shiny-busy')")
check_errors()
writeLines(c(paste('PASS: HTTP and Shiny websocket connection at',app_url),
  'PASS: all six economic controls independently change rendered metrics',
  'PASS: all six cost pass-through controls change metrics and reset; cost-only reset preserves NGDP choice',
  'PASS: spending charts switch to nominal dollars and NGDP-only spending comparisons render',
  'PASS: 24 subfunctions render; group/year/currency controls update exact factor contributions and CSV exports; background PDF downloads',
  'PASS: productivity and hours leave the GDP headline unchanged; exported full GDP path is identical at 1.2 and 1.7 percent productivity',
  'PASS: all tabs including Revenue render without output errors',
  'PASS: evidence register renders, filters to spending and downloads all 64 source/assumption entries',
  'PASS: structural company-tax share and terms-of-trade diagnostics render',
  'PASS: company tax/GDP is constant at zero productivity and 8% NGDP per capita; ToT and sensitivity change its level and reset correctly',
  'PASS: explicit tax threshold control changes projected revenue and fiscal results',
  'PASS: Wage indexation is the default; CPI and fixed thresholds change debt levels and gaps versus the fixed wage-indexed reference',
  'PASS: Spending tab renders the live NDIS projection and total-Scheme IGR comparison',
  'PASS: subfunction factor table follows group/year/currency selections, includes financing rows, and exports endpoint factors that reconstruct spending',
  'PASS: Debt and interest tab reconciles gross/net debt and asset income; units, year selector and detail download work',
  'PASS: live NDIS growth readout responds to intensity; real super returns default to 4%, allow 7% and reset correctly',
  'PASS: results revenue-detail and assumptions downloads return valid content',
  'PASS: reset restores reference metrics',baseline,changed),'outputs/browser_smoke_results.txt')
cat('Browser smoke checks passed.\n')
b$close()
