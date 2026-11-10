args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1]);setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b<-chromote::ChromoteSession$new(width=1500,height=1100);b$default_timeout<-60
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
js<-function(s)b$Runtime$evaluate(s,returnByValue=TRUE)$result$value
wait<-function(s){for(i in 1:180){if(isTRUE(js(s)))return(invisible(TRUE));Sys.sleep(.5)};stop('Timeout: ',s)}
dl<-function(id){v<-b$Runtime$evaluate(sprintf("fetch(document.getElementById('%s').href).then(r=>{if(!r.ok)throw Error(r.status);return r.text()})",id),awaitPromise=TRUE,returnByValue=TRUE)$result$value;read.csv(text=v)}
b$Page$navigate(Sys.getenv('FISCAL_TEST_URL','http://127.0.0.1:7461'),wait_=FALSE)
wait("window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && document.getElementById('outlook_all_download') && !document.documentElement.classList.contains('shiny-busy')")
wait("document.getElementById('outlook_download').href.includes('/download/') && document.getElementById('outlook_all_download').href.includes('/download/')")
base<-dl('outlook_all_download');stopifnot(nrow(base)==444,all(table(base$measure,base$case)==37))
for(key in unique(base$measure)){
 js(sprintf("document.getElementById('measure').selectize.setValue('%s');true",key));Sys.sleep(.5)
 wait("!document.documentElement.classList.contains('shiny-busy')")
 d<-dl('outlook_download');expected<-base[base$measure==key,]
 stopifnot(nrow(d)==74,all(d$measure==key),max(abs(d$value-expected$value))<1e-8)
}
js("$('#productivity').data('ionRangeSlider').update({from:1.7});Shiny.setInputValue('productivity',1.7,{priority:'event'});true")
Sys.sleep(1);wait("!document.documentElement.classList.contains('shiny-busy')")
changed<-dl('outlook_all_download');ref<-base$case=='Reference';sel<-!ref
stopifnot(identical(base$value[ref],changed$value[ref]),any(abs(base$value[sel]-changed$value[sel])>1e-6),
 all(changed$assumption_productivity[sel]==.017),all(changed$assumption_productivity[ref]==.012),
 js("document.querySelectorAll('.shiny-output-error').length")==0)
b$screenshot(filename='outputs/outlook_downloads.png');b$close()
cat('PASS: browser downloads match all six selected charts, all measures, scenario changes and fixed reference.\n')
