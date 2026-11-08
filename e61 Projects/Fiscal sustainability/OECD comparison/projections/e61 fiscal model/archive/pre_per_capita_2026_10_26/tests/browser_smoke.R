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
b$screenshot(filename='outputs/app_reference.png',selector='body')
# Check net-debt level AND the comparison gap for each threshold rule, with all
# economic assumptions at defaults. A generic metrics-change check misses a
# moving reference that leaves the reported gap at zero.
stopifnot(js("$('#tax_policy')[0].selectize.getValue()")=='cpi')
source('R/model.R');test_inputs<-load_inputs();test_calibration<-calibrate(test_inputs)
reference_debt<-tail(project_model(test_inputs,default_assumptions(),test_calibration)$path$net_debt_pct,1)
tax_tests<-lapply(c('wage_indexed','fixed'),function(rule){
  debt<-tail(project_model(test_inputs,modifyList(default_assumptions(),list(tax_policy=rule)),test_calibration)$path$net_debt_pct,1)
  c(rule,sprintf('%.1f%%',debt),sprintf('%+.1f pp',debt-reference_debt))
})
js("document.querySelector('details').open=true; true")
wait("document.querySelector('#ndis_growth_readout').innerText.includes('5.5% a year in 2030-35')")
stopifnot(js("$('#super_return').data('ionRangeSlider').result.from")==4,
  js("$('#super_return').data('ionRangeSlider').options.max")==7)
set_slider('ndis_intensity',2)
wait("document.querySelector('#ndis_growth_readout').innerText.includes('7.1% a year in 2030-35') && !document.documentElement.classList.contains('shiny-busy')")
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
for(control in c('productivity','ngdp','relative_price','tot')) {
  if(control=='ngdp') js("document.getElementById('ngdp_override').click(); true")
  set_slider(control,switch(control,productivity=2,ngdp=5,relative_price=.8,tot=15))
  wait(paste0("document.querySelector('#metrics').innerText !== ",jsonlite::toJSON(baseline,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
  check_errors()
  reset(baseline)
}
js("$('#productivity').data('ionRangeSlider').update({from:2}); $('#ngdp').data('ionRangeSlider').update({from:5}); $('#relative_price').data('ionRangeSlider').update({from:.8}); $('#tot').data('ionRangeSlider').update({from:15}); document.getElementById('ngdp_override').click(); Shiny.setInputValue('productivity',2); Shiny.setInputValue('ngdp',5); Shiny.setInputValue('relative_price',.8); Shiny.setInputValue('tot',15); true")
wait(paste0("document.querySelector('#metrics').innerText !== ",jsonlite::toJSON(baseline,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
changed <- js("document.querySelector('#metrics').innerText")
stopifnot(!identical(baseline,changed))
b$screenshot(filename='outputs/app_scenario.png',selector='body')
for(tab in c('channels','debt','components','revenue','method')) {
  js(sprintf("document.querySelector('a[data-value=\"%s\"]').click(); true",tab))
  cond <- switch(tab,channels="document.querySelector('#macro img')",
    debt="document.querySelector('#debt_stocks img') && document.querySelector('#debt_interest img') && document.querySelector('#debt_balance_table table') && document.querySelector('#debt_interest_table table') && document.querySelector('#debt_explanation').innerText.includes('gross debt minus')",
    components="document.querySelector('#components img') && document.querySelector('#spending_paths img') && document.querySelector('#spending_drivers table') && document.querySelector('#revenue_table table') && document.querySelector('#ndis_benchmark table') && document.querySelector('#ndis_summary').innerText.includes('nominal growth')",
    revenue="document.querySelector('#revenue_path img') && document.querySelector('#revenue_mix img') && document.querySelector('#tax_rules table') && document.querySelector('#elasticities table')",
    method="document.querySelector('#parameters table') && document.querySelector('#sources table') && document.querySelector('#coverage_bridge table')")
  wait(paste0(cond," && !document.documentElement.classList.contains('shiny-busy')"))
  check_errors()
  if(tab=='debt') {
    stopifnot(js("document.querySelector('#debt_balance_table').innerText.includes('Same scenario, linked NGDP')"))
    js("$('#debt_units')[0].selectize.setValue('bn'); $('#debt_year')[0].selectize.setValue('2040'); true")
    wait("document.querySelector('#debt_explanation').innerText.includes('In 2040') && !document.documentElement.classList.contains('shiny-busy')")
    check_errors();b$screenshot(filename='outputs/app_debt.png',selector='body')
  }
  if(tab=='components')b$screenshot(filename='outputs/app_spending.png',selector='#spending_paths')
  if(tab=='revenue') {
    before<-js("document.querySelector('#metrics').innerText")
    js("$('#tax_policy')[0].selectize.setValue('fixed'); true")
    wait(paste0("document.querySelector('#metrics').innerText !== ",jsonlite::toJSON(before,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
    check_errors()
    b$screenshot(filename='outputs/app_revenue.png',selector='body')
  }
}
for(id in c('download','audit','revenue_download','debt_download')) {
  code <- sprintf("fetch(document.getElementById('%s').href).then(async r=>({status:r.status,text:await r.text()}))",id)
  result <- b$Runtime$evaluate(code,awaitPromise=TRUE,returnByValue=TRUE)$result$value
  stopifnot(result$status==200,nchar(result$text)>200)
  if(id=='download') stopifnot(grepl('scenario_assumption_productivity',result$text,fixed=TRUE),
                             grepl('Your scenario',result$text,fixed=TRUE),grepl('Reference',result$text,fixed=TRUE))
  if(id=='audit') stopifnot(grepl('programme_calibration',result$text,fixed=TRUE))
  if(id=='revenue_download') stopifnot(grepl('bracket_creep_revenue_bn',result$text,fixed=TRUE),grepl('Personal income tax',result$text,fixed=TRUE))
  if(id=='debt_download') stopifnot(grepl('opening_gross_debt_bn',result$text,fixed=TRUE),grepl('net_interest_pct',result$text,fixed=TRUE))
}
reset(baseline)
js("document.querySelector('a[data-value=\"outlook\"]').click(); true")
wait("document.querySelector('#outlook img') && !document.documentElement.classList.contains('shiny-busy')")
check_errors()
writeLines(c(paste('PASS: HTTP and Shiny websocket connection at',app_url),
  'PASS: each of four scenario controls independently changes rendered metrics',
  'PASS: all tabs including Revenue render without output errors',
  'PASS: explicit tax threshold control changes projected revenue and fiscal results',
  'PASS: CPI is the default; wage and fixed thresholds change debt levels and gaps versus the fixed CPI reference',
  'PASS: Spending tab renders the live NDIS projection and total-Scheme IGR comparison',
  'PASS: annual programme GDP-share paths and number, real-cost and intensity audit render',
  'PASS: Debt and interest tab reconciles gross/net debt and asset income; units, year selector and detail download work',
  'PASS: live NDIS growth readout responds to intensity; real super returns default to 4%, allow 7% and reset correctly',
  'PASS: results revenue-detail and assumptions downloads return valid content',
  'PASS: reset restores reference metrics',baseline,changed),'outputs/browser_smoke_results.txt')
cat('Browser smoke checks passed.\n')
b$close()
