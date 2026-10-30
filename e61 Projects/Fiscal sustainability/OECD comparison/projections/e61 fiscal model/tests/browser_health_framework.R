args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b<-chromote::ChromoteSession$new(width=1500,height=1100);b$default_timeout<-45
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
js<-function(s)b$Runtime$evaluate(s,returnByValue=TRUE)$result$value
wait<-function(s){for(i in 1:240){if(isTRUE(js(s)))return(invisible(TRUE));Sys.sleep(.25)};stop('Timeout: ',s)}
dl<-function(id){v<-b$Runtime$evaluate(sprintf("fetch(document.getElementById('%s').href).then(r=>r.text())",id),awaitPromise=TRUE,returnByValue=TRUE)$result$value;read.csv(text=v)}
slider<-function(id,v){js(sprintf("$('#%s').data('ionRangeSlider').update({from:%f});Shiny.setInputValue('%s',%f,{priority:'event'});true",id,v,id,v));Sys.sleep(.5);wait("!document.documentElement.classList.contains('shiny-busy')")}
sel<-function(id,v){js(sprintf("document.getElementById('%s').selectize.setValue('%s');true",id,v));Sys.sleep(.5);wait("!document.documentElement.classList.contains('shiny-busy')")}
health<-function(){js("$('a[data-value=sector_health]').click();$('#sector_health-section a[data-value=construction]').click();true");wait("document.getElementById('sector_health-annual_download').href.includes('/download/') && !document.documentElement.classList.contains('shiny-busy')");dl('sector_health-annual_download')}
same<-function(x,y)isTRUE(all.equal(x,y,check.attributes=FALSE,tolerance=1e-10))
b$Page$navigate(Sys.getenv('FISCAL_TEST_URL','http://127.0.0.1:7461'))
wait("window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && !document.documentElement.classList.contains('shiny-busy')")
stopifnot(js("document.getElementById('health_income_elasticity').value==='0.8' && document.getElementById('health_remaining_growth').value==='0' && document.getElementById('health_service_productivity_share').value==='25'"),
 js("document.querySelectorAll('#health_service_productivity_share').length===1 && !document.querySelector('#health_demand_controls details').open"))
base<-health();stopifnot(all(base$income_elasticity==.8),all(base$intensity_factor==1),any(base$income_factor>1))
js("$('a[data-value=method]').click();true");wait("document.getElementById('download').href.includes('/download/')")
fiscal0<-dl('download')
slider('health_income_elasticity',0);zero<-health()
stopifnot(all(zero$income_factor==1),same(zero$price_factor,base$price_factor),same(zero$wage_factor,base$wage_factor),same(zero$efficiency_factor,base$efficiency_factor),same(zero$nominal_gdp_bn,base$nominal_gdp_bn))
slider('health_income_elasticity',.8);slider('health_cpi_wedge',.5);price<-health()
stopifnot(same(price$nominal_hourly_wage_index,base$nominal_hourly_wage_index),same(price$nominal_gdp_bn,base$nominal_gdp_bn),any(price$price_factor>base$price_factor),same(price$general_price_index,price$price_factor))
expected<-with(price,anchor_2030_bn*population_factor*age_factor*coverage_factor*price_factor*income_factor*wage_factor*efficiency_factor*intensity_factor*policy_factor)
stopifnot(max(abs(expected-price$spending_bn))<1e-7)
slider('health_cpi_wedge',0);sel('health_pressure_source','subfunctions');hist<-health()
params<-read.csv('data/health_income_residuals.csv');params<-subset(params,source=='subfunctions')
stopifnot(max(abs(hist$additional_service_growth-params$residual_growth[match(hist$id,params$id)]))<1e-10)
slider('productivity',1.7);high<-health();stopifnot(same(high$additional_service_growth,hist$additional_service_growth),same(high$nominal_gdp_bn,hist$nominal_gdp_bn))
sel('health_pressure_source','aggregate_long');long<-health();stopifnot(max(abs(long$additional_service_growth-.0129096492447735))<1e-10)
sel('health_specification','legacy');legacy<-health();stopifnot(all(legacy$income_factor==1),all(legacy$additional_service_growth==.005))
js("$('a[data-value=method]').click();true");fiscal1<-dl('download')
# Compare only actual reference output fields, excluding the selected scenario metadata.
keep<-names(fiscal0)[!grepl('scenario_assumption_',names(fiscal0))]
stopifnot(same(subset(fiscal0,case=='Reference')[keep],subset(fiscal1,case=='Reference')[keep]))
js("document.getElementById('reset').click();true")
wait("document.getElementById('health_specification').value==='income_costs' && document.getElementById('health_pressure_source').value==='assumed' && document.getElementById('health_income_elasticity').value==='0.8' && document.getElementById('health_framework_summary').innerText.includes('Selected income elasticity 0.80') && !document.documentElement.classList.contains('shiny-busy')")
restored<-health();stopifnot(same(restored,base),js("document.querySelectorAll('.shiny-output-error').length")==0)
stopifnot(js("document.getElementById('health_legacy_plot')===null && document.getElementById('health_framework_download')===null"))
b$screenshot(filename='outputs/app_health_income_framework.png');b$close()
cat('PASS: browser sliders, price/wage consistency, historical residual selection, fixed GDP/reference, legacy comparison, annual exports and reset.\n')
