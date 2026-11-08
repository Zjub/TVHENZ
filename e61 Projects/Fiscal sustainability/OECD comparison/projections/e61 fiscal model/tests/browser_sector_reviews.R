args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
app_url<-Sys.getenv('FISCAL_TEST_URL','http://127.0.0.1:7461')
b<-chromote::ChromoteSession$new(width=1500,height=1100);b$default_timeout<-45
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
js<-function(s)b$Runtime$evaluate(s,returnByValue=TRUE)$result$value
wait<-function(s){for(i in 1:200){if(isTRUE(js(s)))return(invisible(TRUE));Sys.sleep(.25)};stop('Timeout: ',s)}
download<-function(id){v<-b$Runtime$evaluate(sprintf("fetch(document.getElementById('%s').href).then(r=>r.text())",id),awaitPromise=TRUE,returnByValue=TRUE)$result$value;read.csv(text=v)}
b$Page$navigate(app_url)
wait("window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && !document.documentElement.classList.contains('shiny-busy')")
for(p in c('ndis','health','education')){
 prefix<-paste0('sector_',p,'-')
 js(sprintf("$('a[data-value=sector_%s]').click(); true",p))
 wait(sprintf("document.querySelector('#%spath img') !== null && document.querySelector('#%scontribution_table table') !== null && !document.documentElement.classList.contains('shiny-busy')",prefix,prefix))
 stopifnot(js(sprintf("document.querySelectorAll('#%sendpoints tbody tr').length",prefix))==switch(p,ndis=1,health=5,education=4))
 c<-download(paste0(prefix,'contribution_download'));stopifnot(nrow(c)==switch(p,ndis=9,health=45,education=36),all(c$programme==switch(p,ndis='NDIS',health='Health',education='Education')))
 js(sprintf("$('a[data-value=construction]',document.getElementById('%ssection')).click(); true",prefix))
 # Nested tabs are siblings of their content, so target their containing tabset.
 js(sprintf("document.querySelector('#%ssection a[data-value=construction]').click(); true",prefix))
 wait(sprintf("document.querySelector('#%sbridge table') !== null && document.querySelector('#%sfactor_check table') !== null && document.querySelector('#%sevidence details') !== null && !document.documentElement.classList.contains('shiny-busy')",prefix,prefix,prefix))
 annual<-download(paste0(prefix,'annual_download'));evidence<-download(paste0(prefix,'evidence_download'))
 stopifnot(js(sprintf("Array.from(document.querySelectorAll('#%sevidence a')).every(a => !/\\s/.test(a.getAttribute('href'))) ",prefix)))
 stopifnot(all(annual$year>=2030&annual$year<=2066),nrow(evidence)==switch(p,ndis=1,health=5,education=4),
   js("document.querySelectorAll('.shiny-output-error').length")==0)
 if(p=='health'){
   js("$('#sector_health-scope')[0].selectize.setValue('h_medicines'); true")
   wait("document.querySelector('#sector_health-evidence').innerText.includes('CPI') && document.querySelectorAll('#sector_health-factor_check tbody tr').length===1 && !document.documentElement.classList.contains('shiny-busy')")
   a<-download(paste0(prefix,'annual_download'));stopifnot(all(a$id=='h_medicines'),all(a$real_unit_factor==1),all(a$relative_price_factor==1))
   js("$('#sector_health-measure')[0].selectize.setValue('real_bn'); $('#sector_health-end')[0].selectize.setValue('2040'); true")
   Sys.sleep(.5);wait("document.getElementById('sector_health-summary').innerText.includes('2040') && !document.documentElement.classList.contains('shiny-busy')")
   c<-download(paste0(prefix,'contribution_download'));stopifnot(all(c$end_year==2040),all(c$measure=='real_bn'),all(subset(c,factor=='General prices')$contribution==0))
 }
 js(sprintf("document.querySelector('#%ssection a[data-value=discussion]').click(); true",prefix))
 stopifnot(js(sprintf("document.querySelector('#%ssection a[data-value=discussion]').getAttribute('aria-selected')==='true'",prefix)))
}
js("$('#productivity').data('ionRangeSlider').update({from:1.7}); Shiny.setInputValue('productivity',1.7,{priority:'event'}); $('a[data-value=sector_education]').click(); document.querySelector('#sector_education-section a[data-value=projection]').click(); true")
Sys.sleep(1);wait("!document.documentElement.classList.contains('shiny-busy') && document.querySelector('#sector_education-path img') !== null")
stopifnot(js("document.querySelectorAll('.shiny-output-error').length")==0)
a<-download('sector_education-annual_download');stopifnot(any(a$real_unit_factor>1))
js("document.body.innerHTML=document.querySelector('#sector_education-summary').closest('[role=tabpanel]').outerHTML; window.scrollTo(0,0); true")
b$screenshot(filename=if(grepl('127.0.0.1',app_url,fixed=TRUE))'outputs/app_sector_education.png' else 'outputs/hosted_sector_education.png',selector='body');b$close()
cat('Sector browser checks passed: three tabs, decompositions, source registers, downloads, medicine cost exception, real dollars, endpoint selection and scenario response.\n')
