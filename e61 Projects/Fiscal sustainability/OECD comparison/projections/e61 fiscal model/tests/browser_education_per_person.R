args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1]);setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b<-chromote::ChromoteSession$new(width=1500,height=1100);b$default_timeout<-60
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
js<-function(s)b$Runtime$evaluate(s,returnByValue=TRUE)$result$value
wait<-function(s){for(i in 1:180){if(isTRUE(js(s)))return(invisible(TRUE));Sys.sleep(.5)};stop('Timeout: ',s)}
dl<-function(id){v<-b$Runtime$evaluate(sprintf("fetch(document.getElementById('%s').href).then(r=>r.text())",id),awaitPromise=TRUE,returnByValue=TRUE)$result$value;read.csv(text=v)}
sel<-function(id,v){js(sprintf("document.getElementById('%s').selectize.setValue('%s');true",id,v));Sys.sleep(.5);wait("!document.documentElement.classList.contains('shiny-busy')")}
b$Page$navigate(Sys.getenv('FISCAL_TEST_URL','http://127.0.0.1:7461'),wait_=FALSE)
wait("window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && !document.documentElement.classList.contains('shiny-busy')")
js("$('a[data-value=sector_education]').click();true")
wait("document.getElementById('sector_education-per_person_download').href.includes('/download/') && !document.documentElement.classList.contains('shiny-busy')")
for(scope in c('all','e_school','e_vet')){
 sel('sector_education-scope',scope);sel('sector_education-end',if(scope=='all')'2066' else '2040')
 for(measure in c('real_per_person','relative_real_gdp')){
  sel('sector_education-per_person_measure',measure)
  d<-dl('sector_education-per_person_download');a<-dl('sector_education-per_person_annual_download');v<-aggregate(value~year,a,sum)
  stopifnot(all(d$programme=='Education'),all(a$measure==measure),
   abs(sum(d$contribution)-diff(v$value[c(1,nrow(v))]))<1e-7,
   all(abs(d$contribution[d$factor=='Population size (removed per person)'])<1e-8))
  if(scope!='all')stopifnot(all(a$id==scope),max(a$year)==2040)
  if(measure=='relative_real_gdp')stopifnot(all(a$unit=='% of real GDP per person'),any(d$contribution[d$factor=='Real GDP per person denominator']!=0))
  else stopifnot(all(a$unit=='2030 dollars per person'))
 }
}
stopifnot(js("document.querySelectorAll('.shiny-output-error').length")==0)
b$screenshot(filename='outputs/education_per_person_panel.png');b$close()
cat('PASS: Education per-person chart, table, both measures, scope/end-year selectors and both CSV downloads.\n')
