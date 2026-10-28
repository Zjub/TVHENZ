args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
url_argument<-grep('^--url=',commandArgs(TRUE),value=TRUE)
app_url<-if(length(url_argument))sub('^--url=','',url_argument[1]) else 'http://127.0.0.1:7461'
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b<-chromote::ChromoteSession$new(width=1500,height=1000)
b$default_timeout<-45
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
js<-function(s)b$Runtime$evaluate(s,returnByValue=TRUE)$result$value
wait<-function(s){for(i in 1:200){if(isTRUE(js(s)))return(invisible(TRUE));Sys.sleep(.25)};stop('Timed out: ',s)}
b$Page$navigate(app_url)
wait("document.querySelectorAll('.metric-value').length===4 && !document.documentElement.classList.contains('shiny-busy')")
js("document.querySelector('a[data-value=\"components\"]').click(); true")
wait("document.querySelector('#igr_table_Health table') && document.querySelector('#ndis_benchmark table') && !document.documentElement.classList.contains('shiny-busy')")
stopifnot(js("document.querySelectorAll('#health_education_costs tbody tr').length")==9)
parameters_csv<-b$Runtime$evaluate("fetch(document.getElementById('health_education_download').href).then(r=>r.text())",awaitPromise=TRUE,returnByValue=TRUE)$result$value
parameters<-read.csv(text=parameters_csv)
stopifnot(nrow(parameters)==9,all(parameters$annual_aggregate_cost_adjustment==0),
  all(c('source_url','locator','evidence_class','limitation')%in%names(parameters)),
  all(c('e_higher','e_vet','h_medicines')%in%parameters$id))
source('R/igr_benchmarks.R')
for(p in setdiff(names(igr_spending_notes()),'NDIS')){
 id<-make.names(p)
 stopifnot(js(sprintf("document.getElementById('igr_table_%s').querySelectorAll('tbody tr').length",id))==if(p=='Defence')7 else 4)
 stopifnot(nzchar(js(sprintf("document.getElementById('igr_summary_%s').innerText",id))))
}
stopifnot(js("document.getElementById('igr_table_Other.public.services').innerText.includes('Not available')"))
stopifnot(js("document.querySelector('#igr_summary_Defence').innerText.includes('2036-2066')"))
csv<-b$Runtime$evaluate("fetch(document.getElementById('igr_spending_download').href).then(r=>r.text())",awaitPromise=TRUE,returnByValue=TRUE)$result$value
d<-read.csv(text=csv)
stopifnot(nrow(d)==35,length(unique(d$programme))==8,all(nzchar(d$coverage_note)),
 all(is.na(d$igr_pct[d$programme=='Other public services'])),
 all(is.na(d$igr_pct[d$programme=='Defence'&d$year%in%c(2030,2035,2050)])))
baseline<-js("document.getElementById('igr_summary_Health').innerText")
js("$('#health_cost_growth_adjustment').data('ionRangeSlider').update({from:1}); Shiny.setInputValue('health_cost_growth_adjustment',1,{priority:'event'}); true")
wait(paste0("document.getElementById('igr_summary_Health').innerText !== ",jsonlite::toJSON(baseline,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
csv2<-b$Runtime$evaluate("fetch(document.getElementById('igr_spending_download').href).then(r=>r.text())",awaitPromise=TRUE,returnByValue=TRUE)$result$value
e<-read.csv(text=csv2);k<-d$programme=='Health'&d$year==2066
stopifnot(e$scenario_pct[k]>d$scenario_pct[k],identical(e$igr_pct,d$igr_pct),identical(e$reference_pct,d$reference_pct),
 js("document.querySelectorAll('.shiny-output-error').length")==0)
changed_csv<-b$Runtime$evaluate("fetch(document.getElementById('health_education_download').href).then(r=>r.text())",awaitPromise=TRUE,returnByValue=TRUE)$result$value
changed<-read.csv(text=changed_csv)
stopifnot(all(abs(changed$aggregate_cost_factor_2066[changed$programme=='Health']-1.01^36)<1e-8),
  all(changed$aggregate_cost_factor_2066[changed$programme=='Education']==1),
  identical(changed$real_unit_cost_2066,parameters$real_unit_cost_2066))
js("document.body.innerHTML=document.getElementById('igr_table_Health').closest('.panel-card').outerHTML; window.scrollTo(0,0); true")
stopifnot(js("document.querySelector('#igr_table_Health table').getBoundingClientRect().height > 0"))
b$screenshot(filename='outputs/app_igr_health.png',selector='body')
stopifnot(file.exists('outputs/app_igr_health.png'))
b$close();cat('Browser IGR checks passed: eight categories, published/missing values, source download and live scenario response.\n')
