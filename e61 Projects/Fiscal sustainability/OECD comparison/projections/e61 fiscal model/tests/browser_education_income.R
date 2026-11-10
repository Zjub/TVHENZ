args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b<-chromote::ChromoteSession$new(width=1500,height=1100);b$default_timeout<-60
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
js<-function(s)b$Runtime$evaluate(s,returnByValue=TRUE)$result$value
wait<-function(s){for(i in 1:180){if(isTRUE(js(s)))return(invisible(TRUE));Sys.sleep(.5)};stop('Timeout: ',s)}
dl<-function(id){v<-b$Runtime$evaluate(sprintf("fetch(document.getElementById('%s').href).then(r=>r.text())",id),awaitPromise=TRUE,returnByValue=TRUE)$result$value;read.csv(text=v)}
education<-function(){js("$('a[data-value=sector_education]').click();$('#sector_education-section a[data-value=construction]').click();true");wait("document.getElementById('sector_education-annual_download').href.includes('/download/') && !document.documentElement.classList.contains('shiny-busy')");dl('sector_education-annual_download')}
cat('Loading local app\n');b$Page$navigate('http://127.0.0.1:7461',wait_=FALSE)
wait("window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && document.getElementById('education_income_elasticity') && !document.documentElement.classList.contains('shiny-busy')")
stopifnot(js("document.getElementById('education_income_elasticity').value==='0.2'"))
base<-education();stopifnot(nrow(base)==148,all(base$income_elasticity==.2))
js("document.getElementById('cost_pass_through_controls').open=true;$('#education_income_elasticity').data('ionRangeSlider').update({from:0.5});Shiny.setInputValue('education_income_elasticity',0.5,{priority:'event'});true")
Sys.sleep(1);wait("!document.documentElement.classList.contains('shiny-busy')")
changed<-education();stopifnot(all(changed$income_elasticity==.5),any(changed$income_factor>1),max(abs(changed$spending_bn/base$spending_bn-changed$income_factor/base$income_factor))<1e-8)
stopifnot(isTRUE(all.equal(base$nominal_gdp_bn,changed$nominal_gdp_bn)))
js("$('#sector_education-section a[data-value=projection]').click();true")
Sys.sleep(.5)
b$screenshot(filename='outputs/education_income_slider.png')
js("document.getElementById('reset_pass_through').click();true")
wait("document.getElementById('education_income_elasticity').value==='0.2' && !document.documentElement.classList.contains('shiny-busy')")
js("$('#education_income_elasticity').data('ionRangeSlider').update({from:0.5});Shiny.setInputValue('education_income_elasticity',0.5,{priority:'event'});true")
Sys.sleep(1);wait("!document.documentElement.classList.contains('shiny-busy')")
js("document.getElementById('reset').click();true")
wait("document.getElementById('education_income_elasticity').value==='0.2' && !document.documentElement.classList.contains('shiny-busy')")
restored<-education();stopifnot(max(abs(restored$spending_bn-base$spending_bn))<1e-7,js("document.querySelectorAll('.shiny-output-error').length")==0)
b$close();cat('PASS: education slider, annual export, fixed GDP, reset and no output errors.\n')
