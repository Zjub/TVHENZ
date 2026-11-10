setwd('e61 fiscal model')
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b<-chromote::ChromoteSession$new(width=1500,height=1100);b$default_timeout<-60
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
js<-function(s)b$Runtime$evaluate(s,returnByValue=TRUE)$result$value
wait<-function(s){for(i in 1:180){if(isTRUE(js(s)))return(invisible(TRUE));Sys.sleep(.5)};stop('Timeout: ',s)}
b$Page$navigate('https://e61-institute.shinyapps.io/Fiscal_tester/',wait_=FALSE)
wait("window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && document.getElementById('education_income_elasticity') && !document.documentElement.classList.contains('shiny-busy')")
stopifnot(js("document.getElementById('education_income_elasticity').value==='0.2'"),js("document.body.textContent.includes('0.2 default is an unverified assumption')"))
js("$('a[data-value=method]').click();document.getElementById('register_domain').selectize.setValue('All');true")
wait("document.getElementById('assumption_register').innerText.includes('Education income elasticity of service intensity')")
stopifnot(js("document.getElementById('assumption_register').innerText.includes('Unverified assumption')"),js("document.querySelectorAll('.shiny-output-error').length")==0)
b$screenshot(filename='outputs/hosted_education_assumption.png')
b$close();cat('PASS: hosted education default 0.2 and unverified assumption in register.\n')
