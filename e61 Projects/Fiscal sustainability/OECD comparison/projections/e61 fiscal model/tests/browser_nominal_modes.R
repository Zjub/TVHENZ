args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1]);setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b<-chromote::ChromoteSession$new(width=1500,height=1100);b$default_timeout<-45
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
js<-function(s)b$Runtime$evaluate(s,returnByValue=TRUE)$result$value
wait<-function(s){for(i in 1:240){if(isTRUE(js(s)))return(invisible(TRUE));Sys.sleep(.25)};stop('Timeout: ',s)}
dl<-function(){v<-b$Runtime$evaluate("fetch(document.getElementById('download').href).then(r=>r.text())",awaitPromise=TRUE,returnByValue=TRUE)$result$value;read.csv(text=v)}
slider<-function(id,v){js(sprintf("$('#%s').data('ionRangeSlider').update({from:%f});Shiny.setInputValue('%s',%f,{priority:'event'});true",id,v,id,v));Sys.sleep(.5);wait("!document.documentElement.classList.contains('shiny-busy')")}
mode<-function(v){js(sprintf("$('#macro_mode input[value=%s]').prop('checked',true).trigger('change');true",v));Sys.sleep(.5);wait(sprintf("document.getElementById('macro_identity').innerText.includes('%s') && !document.documentElement.classList.contains('shiny-busy')",if(v=='prices')'Price growth fixed' else 'NGDP per capita growth fixed'))}
same<-function(x,y)isTRUE(all.equal(x,y,check.attributes=FALSE,tolerance=1e-9))
b$Page$navigate('http://127.0.0.1:7461')
wait("window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && document.getElementById('macro_identity').innerText.includes('Price growth fixed') && !document.documentElement.classList.contains('shiny-busy')")
js("$('a[data-value=method]').click();true");wait("document.getElementById('download').href.includes('/download/')")
base<-dl();mode('prices');prices<-dl();p<-subset(prices,case=='Your scenario')
stopifnot(all(abs(p$domestic_inflation[-1]-.025)<1e-10),js("$('#domestic_price_growth').closest('.shiny-input-container').is(':visible') && !$('#ngdp_per_capita').closest('.shiny-input-container').is(':visible')"),
 js("document.getElementById('macro_identity').innerText.includes('annual average (compound)')"))
slider('productivity',1.7);high<-subset(dl(),case=='Your scenario')
stopifnot(tail(high$nominal_gdp_per_capita,1)>tail(p$nominal_gdp_per_capita,1),same(high$domestic_price_index,p$domestic_price_index))
slider('tot',20);d<-dl();tot<-subset(d,case=='Your scenario')
stopifnot(tail(tot$nominal_gdp_per_capita,1)>tail(high$nominal_gdp_per_capita,1),same(tot$domestic_price_index,high$domestic_price_index))
keep<-names(base)[!grepl('scenario_assumption_',names(base))]
stopifnot(same(subset(base,case=='Reference')[keep],subset(d,case=='Reference')[keep]))
mode('ngdp_per_capita');fixed<-subset(dl(),case=='Your scenario')
stopifnot(all(abs(fixed$nominal_gdp_per_capita_growth[-1]-.032)<1e-10))
js("document.getElementById('reset').click();true")
wait("document.getElementById('productivity').value==='1.2' && document.querySelector('#macro_mode input:checked').value==='prices' && document.getElementById('domestic_price_growth').value==='2.5' && !document.documentElement.classList.contains('shiny-busy')")
restored<-dl();stopifnot(same(restored,base),js("document.querySelectorAll('.shiny-output-error').length")==0)
mode('prices');js("$('a[data-value=outlook]').click();true")
b$close();cat('PASS: browser nominal modes, price invariance, productivity and ToT effects, implied-growth display, fixed reference, CSV and reset.\n')
