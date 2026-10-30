args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1]);setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b<-chromote::ChromoteSession$new(width=1500,height=1100);b$default_timeout<-60
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
js<-function(s)b$Runtime$evaluate(s,returnByValue=TRUE)$result$value
wait<-function(s){for(i in 1:240){if(isTRUE(js(s)))return(invisible(TRUE));Sys.sleep(.25)};stop('Timeout: ',s)}
dl<-function(id){v<-b$Runtime$evaluate(sprintf("fetch(document.getElementById('%s').href).then(r=>r.text())",id),awaitPromise=TRUE,returnByValue=TRUE)$result$value;read.csv(text=v)}
b$Page$navigate(Sys.getenv('FISCAL_TEST_URL','http://127.0.0.1:7461'))
wait("window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && !document.documentElement.classList.contains('shiny-busy')")
js("$('a[data-value=age_pension]').click();true")
wait("document.querySelector('#pension_review-spending_table table')!==null && document.getElementById('pension_review-age_download').href.includes('/download/') && !document.documentElement.classList.contains('shiny-busy')")
stopifnot(js("document.body.innerText.includes('future estimation and validation using HILDA')"),
 js("document.getElementById('health_legacy_plot')===null"))
base<-dl('pension_review-age_download');cohort<-dl('pension_review-cohort_download');sources<-dl('pension_review-source_download')
stopifnot(all(base$coverage>=0 & base$coverage<=1),length(unique(base$year))==37,nrow(sources)==4,all(base$calibrated_spending_bn>=0))
summary<-js("document.getElementById('pension_review-summary').innerText")
js("$('#super_return').data('ionRangeSlider').update({from:7});Shiny.setInputValue('super_return',7,{priority:'event'});true")
wait(paste0("document.getElementById('pension_review-summary').innerText!==",jsonlite::toJSON(summary,auto_unbox=TRUE)," && document.getElementById('pension_review-summary').innerText.includes('7.0%') && !document.documentElement.classList.contains('shiny-busy')"))
high<-dl('pension_review-age_download')
stopifnot(isTRUE(all.equal(subset(base,year==2030),subset(high,year==2030))),sum(subset(high,year==2066)$recipients)<sum(subset(base,year==2066)$recipients))
js("document.getElementById('pension_review-years').selectize.setValue(['2030','2066']);document.getElementById('pension_review-cohort').selectize.setValue('1973');true")
Sys.sleep(.5);wait("!document.documentElement.classList.contains('shiny-busy')")
stopifnot(js("document.querySelectorAll('#pension_review-age_table th').length===3"),nrow(subset(cohort,birth_year==1973))>0)
js("document.getElementById('reset').click();true")
wait("document.getElementById('super_return').value==='4' && document.getElementById('pension_review-summary').innerText.includes('4.0%') && !document.documentElement.classList.contains('shiny-busy')")
restored<-dl('pension_review-age_download');stopifnot(isTRUE(all.equal(restored,base)),js("document.querySelectorAll('.shiny-output-error').length")==0)
js("window.scrollTo(0,0);true");b$Runtime$evaluate('new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)))',awaitPromise=TRUE)
shot<-b$Page$captureScreenshot(format='png',captureBeyondViewport=FALSE);writeBin(jsonlite::base64_dec(shot$data),'outputs/age_pension_tab.png')
b$close();cat('PASS: Age Pension tab, source/method disclosure, age/cohort displays, return scenario, fixed initial wealth, CSV downloads and reset.\n')
