args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
app_url<-Sys.getenv('FISCAL_TEST_URL','http://127.0.0.1:7461')
ready<-FALSE
for(i in 1:120){
 ready<-isTRUE(tryCatch(curl::curl_fetch_memory(app_url,handle=curl::new_handle(timeout=10))$status_code==200,error=function(e)FALSE))
 if(ready)break
 Sys.sleep(.25)
}
stopifnot(ready)
b<-chromote::ChromoteSession$new(width=1500,height=1100);b$default_timeout<-45
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
js<-function(s)b$Runtime$evaluate(s,returnByValue=TRUE)$result$value
wait<-function(s){for(i in 1:200){if(isTRUE(js(s)))return(invisible(TRUE));Sys.sleep(.25)};stop('Timed out: ',s)}
download<-function(id){value<-b$Runtime$evaluate(sprintf("fetch(document.getElementById('%s').href).then(r=>r.text())",id),awaitPromise=TRUE,returnByValue=TRUE)$result$value;read.csv(text=value)}
b$Page$navigate(app_url)
wait("document.querySelector('#international_debt_plot img') && document.querySelector('#international_debt_endpoints table') && !document.documentElement.classList.contains('shiny-busy')")
stopifnot(js("document.querySelectorAll('#international_debt_endpoints tbody tr').length")==10,
 js("document.getElementById('international_debt_window') === null"),
 js("document.getElementById('international_debt_summary').innerText.includes('2050')"),
 js("document.querySelectorAll('.shiny-output-error').length")==0)
imf<-download('imf_debt_download');oecd<-download('oecd_debt_download');d<-download('international_debt_download')
stopifnot(nrow(imf)==75,nrow(oecd)==169,max(subset(d,provider=='IMF')$year)==2054,
 all(d$year>=2030),all(grepl('^imf_long_',subset(d,provider=='IMF')$series_id)),
 setequal(subset(oecd,concept=='gross_debt')$series_id,c('oecd_survey_adjustment','oecd_survey_no_adjustment')),
 max(subset(d,grepl('oecd_survey',series_id))$year)==2050)
js("$('#international_debt_view')[0].selectize.setValue('change'); true")
Sys.sleep(.5)
wait("!document.documentElement.classList.contains('shiny-busy') && document.querySelector('#international_debt_plot img') !== null")
e<-download('international_debt_download')
stopifnot(all(e$year>=2030&e$year<=2066),all(abs(subset(e,year==2030)$plotted_value)<1e-8))
baseline<-js("document.querySelector('#metrics').innerText")
js("$('#productivity').data('ionRangeSlider').update({from:1.7}); Shiny.setInputValue('productivity',1.7,{priority:'event'}); true")
wait(paste0("document.querySelector('#metrics').innerText !== ",jsonlite::toJSON(baseline,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
f<-download('international_debt_download')
stopifnot(identical(subset(e,provider!='e61'),subset(f,provider!='e61')),
 !identical(subset(e,series_id=='e61_scenario_net')$value_pct_gdp,subset(f,series_id=='e61_scenario_net')$value_pct_gdp),
 js("document.querySelectorAll('.shiny-output-error').length")==0)
js("$('#international_debt_view')[0].selectize.setValue('levels'); document.getElementById('reset').click(); true")
wait(paste0("document.querySelector('#metrics').innerText === ",jsonlite::toJSON(baseline,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
js("document.body.innerHTML=document.getElementById('international_debt_plot').closest('.panel-card').outerHTML; window.scrollTo(0,0); true")
b$screenshot(filename=if(grepl('127.0.0.1',app_url,fixed=TRUE))'outputs/app_international_debt.png' else 'outputs/hosted_international_debt.png',selector='body');b$close()
cat('International debt browser checks passed: long-term IMF/OECD scenarios, no short-run option, two measures, rebasing, downloads and scenario response.\n')
