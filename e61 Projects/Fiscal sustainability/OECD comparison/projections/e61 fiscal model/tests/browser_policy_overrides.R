args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b<-chromote::ChromoteSession$new(width=1500,height=1100);b$default_timeout<-45
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
js<-function(s)b$Runtime$evaluate(s,returnByValue=TRUE)$result$value
wait<-function(s){for(i in 1:240){if(isTRUE(js(s)))return(invisible(TRUE));Sys.sleep(.25)};stop('Timeout: ',s)}
download<-function(id){v<-b$Runtime$evaluate(sprintf("fetch(document.getElementById('%s').href).then(r=>r.text())",id),awaitPromise=TRUE,returnByValue=TRUE)$result$value;read.csv(text=v)}
b$Page$navigate('http://127.0.0.1:7461')
wait("window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && !document.documentElement.classList.contains('shiny-busy')")
js("document.getElementById('spending_target_controls').open=true;document.getElementById('cost_pass_through_controls').open=true;$('a[data-value=sector_health]').click();document.querySelector('#sector_health-section a[data-value=construction]').click();true")
wait("document.querySelector('#sector_health-cost_comparison table') !== null && !document.documentElement.classList.contains('shiny-busy')")
baseline<-download('sector_health-annual_download')
stopifnot(all(!baseline$gdp_target_active),
 js("document.querySelector('label[for=health_service_productivity_share]').textContent.includes('cost')"),
 js("document.querySelector('label[for=health_additional_cost_growth]').textContent.includes('cost')"),
 js("document.getElementById('interest_debt_sensitivity_bps').value==='0'"),
 js("Boolean(document.getElementById('policy_financing_controls').compareDocumentPosition(document.getElementById('spending_target_controls')) & Node.DOCUMENT_POSITION_FOLLOWING)"))
js("$('#target_health').val(8).trigger('change');$('#fix_health').prop('checked',true).trigger('change');true")
wait("document.getElementById('sector_health-summary').innerText.includes('FIXED-SHARE POLICY') && !document.documentElement.classList.contains('shiny-busy')")
d<-download('sector_health-annual_download');agg<-aggregate(share_gdp~year,d,sum)
stopifnot(max(abs(subset(agg,year>2030)$share_gdp-8))<1e-8,
 all(subset(d,year>2030)$gdp_target_active),
 identical(subset(d,year==2030)$spending_bn,subset(baseline,year==2030)$spending_bn))
js("$('#target_ndis').val(0).trigger('change');$('#fix_ndis').prop('checked',true).trigger('change');$('a[data-value=sector_ndis]').click();document.querySelector('#sector_ndis-section a[data-value=construction]').click();true")
wait("document.querySelector('#sector_ndis-cost_comparison table')!==null && document.getElementById('sector_ndis-summary').innerText.includes('FIXED-SHARE POLICY') && !document.documentElement.classList.contains('shiny-busy')")
n<-download('sector_ndis-annual_download');stopifnot(all(subset(n,year>2030)$spending_bn==0))
js("document.getElementById('reset_targets').click();$('a[data-value=debt]').click();true")
wait("document.querySelector('#debt_interest_table table')!==null && document.getElementById('spending_override_summary').innerText.includes('All functions') && !document.documentElement.classList.contains('shiny-busy')")
base_debt<-download('debt_download')
js("$('#interest_debt_sensitivity_bps').data('ionRangeSlider').update({from:3});Shiny.setInputValue('interest_debt_sensitivity_bps',3,{priority:'event'});true")
wait("document.getElementById('debt_explanation').innerText.includes('3.00 bps') && !document.documentElement.classList.contains('shiny-busy')")
debt<-download('debt_download');s<-subset(debt,case=='Your scenario');r<-subset(debt,case=='Reference')
stopifnot(max(abs(s$effective_interest_rate[-1]-pmax(0,s$base_interest_rate[-1]+3/10000*(head(s$net_debt_pct,-1)-s$net_debt_pct[1]))))<1e-8,
 isTRUE(all.equal(r,subset(base_debt,case=='Reference'),check.attributes=FALSE)),
 tail(s$net_debt_pct,1)>tail(subset(base_debt,case=='Your scenario')$net_debt_pct,1),
 js("document.querySelectorAll('.shiny-output-error').length")==0)
js("document.getElementById('reset').click();true")
wait("document.getElementById('interest_debt_sensitivity_bps').value==='0' && !document.documentElement.classList.contains('shiny-busy')")
stopifnot(js("Array.from(document.querySelectorAll('[id^=fix_]')).every(e=>!e.checked)"))
js("document.getElementById('cost_pass_through_controls').open=true;document.getElementById('spending_target_controls').open=true;true")
b$screenshot(filename='outputs/app_fixed_shares_and_debt_sensitivity.png',selector='body');b$close()
cat('Browser policy checks passed: cost labels, fixed shares including zero, reset, debt feedback, reference invariance and downloads.\n')
