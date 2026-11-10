args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1]);setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b<-chromote::ChromoteSession$new(width=1500,height=1100);b$default_timeout<-60
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
js<-function(s)b$Runtime$evaluate(s,returnByValue=TRUE)$result$value
wait<-function(s){for(i in 1:180){if(isTRUE(js(s)))return(invisible(TRUE));Sys.sleep(.5)};stop('Timeout: ',s)}
dl<-function(id){v<-b$Runtime$evaluate(sprintf("fetch(document.getElementById('%s').href).then(r=>{if(!r.ok)throw Error(r.status);return r.text()})",id),awaitPromise=TRUE,returnByValue=TRUE)$result$value;read.csv(text=v)}
sel<-function(id,value){js(sprintf("document.getElementById('%s').selectize.setValue('%s');true",id,value));Sys.sleep(.5);wait("!document.documentElement.classList.contains('shiny-busy')")}
b$Page$navigate(Sys.getenv('FISCAL_TEST_URL','http://127.0.0.1:7461'),wait_=FALSE)
wait("window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && !document.documentElement.classList.contains('shiny-busy')")
js("$('#productivity').data('ionRangeSlider').update({from:1.7});Shiny.setInputValue('productivity',1.7,{priority:'event'});true")
Sys.sleep(1);wait("!document.documentElement.classList.contains('shiny-busy')")
for(sector in c('health','education')){
 ns<-paste0('sector_',sector,'-');js(sprintf("$('a[data-value=sector_%s]').click();true",sector))
 wait(sprintf("document.getElementById('%spath_download').href.includes('/download/') && !document.documentElement.classList.contains('shiny-busy')",ns))
 for(scope in c('all',if(sector=='health')'h_hospital' else 'e_school')){
   sel(paste0(ns,'scope'),scope);sel(paste0(ns,'end'),if(scope=='all')'2066' else '2040')
   for(measure in c('pct','bn','real_bn')){
     sel(paste0(ns,'measure'),measure);d<-dl(paste0(ns,'path_download'))
     expected<-switch(measure,pct=d$share_gdp,bn=d$spending_bn,real_bn=d$spending_bn/d$domestic_price_index)
     stopifnot(nrow(d)==2*(if(scope=='all')37 else 11),all(table(d$case)==nrow(d)/2),
       all(d$scope==scope),all(d$measure==measure),max(d$year)==if(scope=='all')2066 else 2040,
       max(abs(d$value-expected))<1e-7,max(abs(d$share_gdp-100*d$spending_bn/d$nominal_gdp_bn))<1e-9,
       all(d$assumption_productivity[d$case=='Your scenario']==.017),
       all(d$assumption_productivity[d$case=='Reference']==.012))
   }
 }
 cat('PASS:',sector,'whole-function and subfunction downloads, all units and both cases.\n')
}
stopifnot(js("document.querySelectorAll('.shiny-output-error').length")==0)
b$close();cat('PASS: no browser output errors.\n')
