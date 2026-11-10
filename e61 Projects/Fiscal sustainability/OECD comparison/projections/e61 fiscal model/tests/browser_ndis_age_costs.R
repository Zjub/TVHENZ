args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b<-chromote::ChromoteSession$new(width=1500,height=1100);b$default_timeout<-90
options(error=function(){try(b$close(),silent=TRUE);q(status=1,save='no')})
js<-function(s)b$Runtime$evaluate(s,returnByValue=TRUE)$result$value
wait<-function(s){
  cat('Checking:',s,'\n')
  for(i in 1:180){
    if(isTRUE(js(s)))return(invisible(TRUE))
    if(i%%20==0)print(js("JSON.stringify({url:location.href,ready:document.readyState,shiny:typeof window.Shiny,busy:document.documentElement.classList.contains('shiny-busy'),summary:document.getElementById('sector_ndis-ndis_age_cost_summary')?.innerText,errors:Array.from(document.querySelectorAll('.shiny-output-error')).map(x=>x.innerText)})"))
    Sys.sleep(.25)
  }
  stop('Timeout: ',s)
}
dl<-function(id){r<-b$Runtime$evaluate(sprintf("fetch(document.getElementById('%s').href).then(r=>r.text())",id),awaitPromise=TRUE,returnByValue=TRUE)$result$value;read.csv(text=r)}
cat('Chrome connected.\n');b$Page$navigate(Sys.getenv('FISCAL_TEST_URL','http://127.0.0.1:7461'),wait_=FALSE)
wait("window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && !document.documentElement.classList.contains('shiny-busy')")
js("$('a[data-value=sector_ndis]').first().click();true")
wait("document.querySelector('#sector_ndis-ndis_age_cost_path img')!==null && document.getElementById('sector_ndis-ndis_age_cost_summary').innerText.includes('16.0%') && !document.documentElement.classList.contains('shiny-busy')")
stopifnot(js("document.querySelector('#sector_ndis-ndis_age_cost_inputs table')!==null"),
 js("document.body.textContent.includes('not observed annual payments')"),
 !js("document.querySelector('.shiny-output-error')!==null"))
source<-dl('sector_ndis-ndis_age_cost_source_download');d<-dl('sector_ndis-ndis_age_projection_download')
stopifnot(nrow(source)==9,sum(source$observed_participants)==739414,
 nrow(d)==37*101,all(abs(tapply(d$cost_proxy_share,d$year,sum)-1)<1e-12))
js("$('#sector_ndis-section a[data-value=construction]').first().click();true")
wait("document.querySelector('#sector_ndis-factor_check table')!==null && document.getElementById('sector_ndis-annual_download').href.includes('/download/') && !document.documentElement.classList.contains('shiny-busy')")
annual<-dl('sector_ndis-annual_download')
stopifnot(nrow(annual)==37,'spending_bn'%in%names(annual))
checkspend<-aggregate(allocated_spending_bn~year,d,sum)
stopifnot(max(abs(checkspend$allocated_spending_bn-annual$spending_bn))<1e-7)
js("$('#sector_ndis-section a[data-value=projection]').first().click();true")
counts<-d$participants;shares<-d$cost_proxy_share
js("Shiny.setInputValue('ndis_service_growth_adjustment',1,{priority:'event'});true")
wait("!document.documentElement.classList.contains('shiny-busy')")
# Wait until downloaded model spending actually reflects the changed common cost rule.
for(i in 1:60){Sys.sleep(.25);high<-dl('sector_ndis-ndis_age_projection_download');if(tail(high$allocated_spending_bn,1)>tail(d$allocated_spending_bn,1))break}
stopifnot(identical(high$participants,counts),identical(high$cost_proxy_share,shares),
 all(subset(high,year==2030)$allocated_spending_bn==subset(d,year==2030)$allocated_spending_bn),
 sum(subset(high,year==2066)$allocated_spending_bn)>sum(subset(d,year==2066)$allocated_spending_bn))
js("Shiny.setInputValue('ndis_service_growth_adjustment',0,{priority:'event'});true")
wait("!document.documentElement.classList.contains('shiny-busy')")
js("$('a[data-value=sector_health]').first().click();true")
wait("document.querySelector('#sector_health-path img')!==null && !document.documentElement.classList.contains('shiny-busy')")
stopifnot(!js("document.querySelector('.shiny-output-error')!==null"))
js("$('a[data-value=sector_ndis]').first().click();true")
wait("!document.documentElement.classList.contains('shiny-busy')")
b$screenshot(filename='outputs/ndis_age_costs_tab.png');b$close()
cat('PASS: NDIS cost-mix graph, source table, age shares, CSV reconciliation, common-cost scenario and other sector tab.\n')
