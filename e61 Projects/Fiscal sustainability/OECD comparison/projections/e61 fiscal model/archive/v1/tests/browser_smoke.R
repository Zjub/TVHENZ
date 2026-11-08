# Optional real-browser smoke check against an already-running local app.
# Requires chromote and an installed Chrome; only the local app is accessed.
args <- commandArgs(FALSE)
script <- sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b <- chromote::ChromoteSession$new(width=1440,height=1100)
on.exit(b$close())
js <- function(code) b$Runtime$evaluate(code,returnByValue=TRUE)$result$value
wait <- function(condition) {
  expression <- sprintf("new Promise((resolve,reject)=>{let n=0;const timer=setInterval(()=>{if(%s){clearInterval(timer);resolve(true)}else if(++n>120){clearInterval(timer);reject(new Error('Browser check timeout'))}},250)})",condition)
  z <- b$Runtime$evaluate(expression,awaitPromise=TRUE,returnByValue=TRUE)
  if(!is.null(z$exceptionDetails)) stop(z$exceptionDetails$text)
  stopifnot(isTRUE(z$result$value))
}
check_errors <- function() stopifnot(js("document.querySelectorAll('.shiny-output-error').length") == 0)
reset <- function(baseline) {
  js("document.getElementById('reset').click(); true")
  wait(paste0("document.querySelector('#metrics').innerText === ",jsonlite::toJSON(baseline,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
}
set_slider <- function(id,value) {
  js(sprintf("$('#%s').data('ionRangeSlider').update({from:%s}); Shiny.setInputValue('%s',%s,{priority:'event'}); true",id,value,id,value))
}
b$Page$navigate('http://127.0.0.1:7461')
wait("document.querySelectorAll('.metric-value').length===4 && document.querySelector('#outlook img') && document.querySelector('#attribution img') && !document.documentElement.classList.contains('shiny-busy')")
check_errors()
baseline <- js("document.querySelector('#metrics').innerText")
b$screenshot(filename='outputs/app_reference.png',selector='body')
for(control in c('productivity','ngdp','relative_price','tot')) {
  if(control=='ngdp') js("document.getElementById('ngdp_override').click(); true")
  set_slider(control,switch(control,productivity=2,ngdp=5,relative_price=.8,tot=15))
  wait(paste0("document.querySelector('#metrics').innerText !== ",jsonlite::toJSON(baseline,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
  check_errors()
  reset(baseline)
}
set_slider('productivity',2)
js("document.getElementById('ngdp_override').click(); true")
set_slider('ngdp',5); set_slider('relative_price',.8); set_slider('tot',15)
wait(paste0("document.querySelector('#metrics').innerText !== ",jsonlite::toJSON(baseline,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
changed <- js("document.querySelector('#metrics').innerText")
stopifnot(!identical(baseline,changed))
b$screenshot(filename='outputs/app_scenario.png',selector='body')
for(tab in c('channels','components','method')) {
  js(sprintf("document.querySelector('a[data-value=\"%s\"]').click(); true",tab))
  cond <- switch(tab,channels="document.querySelector('#macro img')",
    components="document.querySelector('#components img') && document.querySelector('#revenue_table table')",
    method="document.querySelector('#parameters table') && document.querySelector('#sources table')")
  wait(paste0(cond," && !document.documentElement.classList.contains('shiny-busy')"))
  check_errors()
}
for(id in c('download','audit')) {
  code <- sprintf("fetch(document.getElementById('%s').href).then(async r=>({status:r.status,text:await r.text()}))",id)
  result <- b$Runtime$evaluate(code,awaitPromise=TRUE,returnByValue=TRUE)$result$value
  stopifnot(result$status==200,nchar(result$text)>200)
  if(id=='download') stopifnot(grepl('scenario_assumption_productivity',result$text,fixed=TRUE),
                             grepl('Your scenario',result$text,fixed=TRUE),grepl('Reference',result$text,fixed=TRUE))
  if(id=='audit') stopifnot(grepl('programme_calibration',result$text,fixed=TRUE))
}
reset(baseline)
js("document.querySelector('a[data-value=\"outlook\"]').click(); true")
wait("document.querySelector('#outlook img') && !document.documentElement.classList.contains('shiny-busy')")
check_errors()
writeLines(c('PASS: local HTTP and Shiny websocket connection',
  'PASS: each of four scenario controls independently changes rendered metrics',
  'PASS: all tabs render without output errors', 'PASS: results and assumptions downloads return valid content',
  'PASS: reset restores reference metrics',baseline,changed),'outputs/browser_smoke_results.txt')
cat('Browser smoke checks passed.\n')
b$close()
