args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b<-chromote::ChromoteSession$new(width=1500,height=1100);b$default_timeout<-45
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
js<-function(s)b$Runtime$evaluate(s,returnByValue=TRUE)$result$value
wait<-function(s){for(i in 1:240){if(isTRUE(js(s)))return(invisible(TRUE));Sys.sleep(.25)};stop('Timeout: ',s)}
download<-function(id){v<-b$Runtime$evaluate(sprintf("fetch(document.getElementById('%s').href).then(r=>r.text())",id),awaitPromise=TRUE,returnByValue=TRUE)$result$value;read.csv(text=v)}
set_slider<-function(id,value){js(sprintf("$('#%s').data('ionRangeSlider').update({from:%s});Shiny.setInputValue('%s',%s,{priority:'event'});true",id,value,id,value))}
b$Page$navigate('http://127.0.0.1:7461')
wait("window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && !document.documentElement.classList.contains('shiny-busy')")
js("$('a[data-value=sector_health]').click();document.querySelector('#sector_health-section a[data-value=construction]').click();true")
wait("document.querySelector('#sector_health-cost_comparison table') !== null && !document.documentElement.classList.contains('shiny-busy')")
before<-download('sector_health-evidence_download');annual0<-download('sector_health-annual_download')
baseline<-js("document.getElementById('sector_health-summary').innerText")
set_slider('health_service_productivity_share',50);set_slider('health_additional_cost_growth',1.5)
wait(paste0("document.getElementById('sector_health-summary').innerText !== ",jsonlite::toJSON(baseline,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
after<-download('sector_health-evidence_download');annual<-download('sector_health-annual_download')
stopifnot(all(after$selected_additional_service_growth==.015),
 all(subset(after,cost_rule=='input_mix')$selected_service_productivity_share==.5),
 identical(before$reference_benchmark_growth,after$reference_benchmark_growth),
 identical(before$reference_combined_non_demographic_growth,after$reference_combined_non_demographic_growth),
 identical(annual0$nominal_gdp_bn,annual$nominal_gdp_bn),
 all(subset(annual,id=='h_hospital'&year>2030)$efficiency_factor<1))
med<-subset(annual,id=='h_medicines');stopifnot(all(med$real_unit_factor==1),all(med$efficiency_factor==1))
previous<-js("document.getElementById('sector_health-summary').innerText")
set_slider('productivity',0)
wait(paste0("document.getElementById('sector_health-summary').innerText !== ",jsonlite::toJSON(previous,auto_unbox=TRUE)," && !document.documentElement.classList.contains('shiny-busy')"))
zero<-download('sector_health-annual_download');stopifnot(all(zero$real_unit_factor==1),identical(annual$nominal_gdp_bn,zero$nominal_gdp_bn))
stopifnot(js("document.getElementById('relative_price')===null && document.getElementById('ndis_intensity')===null"),
 js("document.querySelectorAll('.shiny-output-error').length")==0)
set_slider('productivity',1.2);set_slider('health_service_productivity_share',0);set_slider('health_additional_cost_growth',.5)
Sys.sleep(.5);wait("!document.documentElement.classList.contains('shiny-busy')")
b$screenshot(filename='outputs/app_service_cost_framework.png',selector='body');b$close()
cat('Browser service-cost controls, efficiency, additional growth, fixed comparators, GDP invariance and source downloads passed.\n')
