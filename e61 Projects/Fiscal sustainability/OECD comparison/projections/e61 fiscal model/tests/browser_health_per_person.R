args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1]);setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b<-chromote::ChromoteSession$new(width=1500,height=1100);b$default_timeout<-45
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
js<-function(s)b$Runtime$evaluate(s,returnByValue=TRUE)$result$value
wait<-function(s){for(i in 1:240){if(isTRUE(js(s)))return(invisible(TRUE));Sys.sleep(.25)};stop('Timeout: ',s)}
dl<-function(id){v<-b$Runtime$evaluate(sprintf("fetch(document.getElementById('%s').href).then(r=>r.text())",id),awaitPromise=TRUE,returnByValue=TRUE)$result$value;read.csv(text=v)}
sel<-function(id,v){js(sprintf("document.getElementById('%s').selectize.setValue('%s');true",id,v));Sys.sleep(.5);wait("!document.documentElement.classList.contains('shiny-busy')")}
check<-function(relative=FALSE){d<-dl('sector_health-per_person_download');a<-dl('sector_health-per_person_annual_download')
 totals<-aggregate(value~year,a,sum)
 stopifnot(abs(sum(d$contribution)-diff(totals$value[c(1,nrow(totals))]))<1e-7,
  all(d$contribution[d$factor=='Population size (removed per person)']==0))
 if(relative)stopifnot(any(d$contribution[d$factor=='Real GDP per person denominator']!=0),all(a$unit=='% of real GDP per person'))
 else stopifnot(all(a$unit=='2030 dollars per person'))
 list(d=d,a=a)
}
b$Page$navigate(Sys.getenv('FISCAL_TEST_URL','http://127.0.0.1:7461'))
wait("window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && !document.documentElement.classList.contains('shiny-busy')")
js("$('a[data-value=sector_health]').click();true")
wait("document.querySelector('#sector_health-per_person_table table')!==null && document.getElementById('sector_health-per_person_download').href.includes('/download/') && !document.documentElement.classList.contains('shiny-busy')")
stopifnot(js("Boolean(document.getElementById('sector_health-contribution_table').compareDocumentPosition(document.getElementById('sector_health-per_person_table')) & Node.DOCUMENT_POSITION_FOLLOWING)"))
stopifnot(js("document.querySelector('#macro_mode input:checked').value==='prices' && document.getElementById('domestic_price_growth').value==='2.5'"),js("document.body.innerText.includes('Health categories follow AIHW service areas')"))
base<-check()
sel('sector_health-measure','bn');unchanged<-invisible(check());stopifnot(isTRUE(all.equal(base,unchanged)))
sel('sector_health-per_person_measure','relative_real_gdp')
wait("document.getElementById('sector_health-per_person_summary').innerText.includes('% of real GDP per person') && !document.documentElement.classList.contains('shiny-busy')")
r<-check(TRUE)
sel('sector_health-scope','h_hospital');sel('sector_health-end','2040')
wait("document.getElementById('sector_health-per_person_summary').innerText.includes('2030 to 2040') && !document.documentElement.classList.contains('shiny-busy')")
h<-check(TRUE);stopifnot(all(h$d$id=='h_hospital'),max(h$a$year)==2040)
sel('sector_health-per_person_measure','real_per_person')
wait("document.getElementById('sector_health-per_person_summary').innerText.includes('2030 dollars per person') && !document.documentElement.classList.contains('shiny-busy')")
invisible(check());stopifnot(js("document.querySelectorAll('.shiny-output-error').length")==0)
js("document.getElementById('sector_health-per_person_summary').scrollIntoView({block:'center'});true")
shot<-b$Page$captureScreenshot(format='png',captureBeyondViewport=FALSE)
writeBin(jsonlite::base64_dec(shot$data),'outputs/health_per_person_panel.png')
b$close();cat('PASS: health per-person browser panel, units, exact reconciliation, scope/end selectors, independent main measure and both CSV downloads.\n')
