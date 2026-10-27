args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();cal<-calibrate(x);a<-default_assumptions();b<-project_model(x,a,cal)
imf<-read.csv('data/processed/imf_debt_projections.csv');oecd<-read.csv('data/processed/oecd_debt_projections.csv')
checks<-data.frame(check=character(),passed=logical())
check<-function(label,z){checks[nrow(checks)+1L,]<<-list(label,isTRUE(z));if(!isTRUE(z))stop('Failed: ',label)}
near<-function(x,y)all(is.finite(c(x,y)))&&max(abs(x-y))<1e-7
for(n in c('IMF','OECD')){
 d<-if(n=='IMF')imf else oecd
 check(paste(n,'unique series/year observations'),!anyDuplicated(d[c('series_id','year')]))
 check(paste(n,'Australian percent-GDP data only'),all(d$country=='AUS'&d$unit=='% GDP'&is.finite(d$value_pct_gdp)))
 check(paste(n,'traceable vintage and source'),all(nzchar(d$vintage)&nzchar(d$source_url)&nzchar(d$source_locator)&nzchar(d$time_basis)))
}
check('IMF vintage from live indicator metadata',all(imf$vintage=='Fiscal Monitor (October 2026)'))
check('IMF ends at 2031',max(imf$year)==2031)
for(n in c('net','gross')){
 code<-if(n=='net')'GGXWDN_G01_GDP_PT' else 'G_XWDG_G01_GDP_PT'
 j<-jsonlite::fromJSON(paste0('data/raw/international_debt/imf_',n,'_debt.json'),simplifyVector=FALSE)$values[[code]]$AUS
 q<-imf[imf$series_id==paste0('imf_',n),]
 check(paste('IMF',n,'equals raw AUS observations'),near(q$value_pct_gdp,as.numeric(unlist(j[as.character(q$year)]))))
}
check('Both OECD Survey scenarios are gross debt',setequal(subset(oecd,concept=='gross_debt')$series_id,c('oecd_survey_adjustment','oecd_survey_no_adjustment')))
check('No invented OECD net-debt scenario',!any(oecd$concept=='net_debt'))
check('OECD net financial liabilities available to 2100',max(subset(oecd,series_id=='oecd_lts_net')$year)==2100)
q<-subset(oecd,year==2050);check('Published OECD 2050 no-adjustment endpoint preserved',near(q$value_pct_gdp[q$series_id=='oecd_survey_no_adjustment'],113.22773700100977))
check('Published OECD 2050 adjustment endpoint preserved',near(q$value_pct_gdp[q$series_id=='oecd_survey_adjustment'],57.282972469294037))
medium<-imf
imf<-read.csv('data/processed/imf_long_term_debt_projections.csv')
check('IMF long-term three gross-debt cases',nrow(imf)==75&&length(unique(imf$series_id))==3&&all(imf$concept=='gross_debt'))
check('IMF digitisation explicitly approximate',all(grepl('Approximate chart digitisation',imf$notes)))
check('IMF long-term horizon preserved',identical(range(imf$year),c(2030L,2054L)))
check('IMF final baseline read to chart accuracy',abs(subset(imf,series_id=='imf_long_baseline'&year==2054)$value_pct_gdp-24)<.6)
check('IMF stabilising case roughly constant',diff(range(subset(imf,series_id=='imf_long_stabilising')$value_pct_gdp))<1)
check('Medium-term IMF rejected from plot',!any(international_debt_comparison(b,b,medium,oecd)$provider=='IMF'))
check('Short-run window rejected',inherits(try(international_debt_comparison(b,b,imf,oecd,2024,2031),silent=TRUE),'try-error'))
for(view in c('levels','change'))for(window in list(c(2030L,2066L))){
 d<-international_debt_comparison(b,b,imf,oecd,window[1],window[2],view)
 check(paste(view,window[1],'has all ten comparison series'),length(unique(d$series_id))==10)
 check(paste(view,window[1],'respects window and source horizon'),all(d$year>=window[1]&d$year<=window[2])&&max(subset(d,provider=='IMF')$year)==2054&&max(subset(d,grepl('oecd_survey',series_id))$year)<=2050)
 check(paste(view,window[1],'net and gross panels separated'),all(grepl('Gross debt',as.character(d$panel[d$concept=='gross_debt'])))&&all(grepl('Net measures',as.character(d$panel[d$concept!='gross_debt']))))
 if(view=='change')check(paste('All ten series zero at 2030',window[1]),near(subset(d,year==2030)$plotted_value,rep(0,10)))
 check(paste(view,window[1],'model values match engine'),near(subset(d,series_id=='e61_scenario_net')$value_pct_gdp,b$path$net_debt_pct[b$path$year>=window[1]&b$path$year<=window[2]]))
}
changed<-project_model(x,modifyList(a,list(productivity=.017)),cal)
d<-international_debt_comparison(b,b,imf,oecd);e<-international_debt_comparison(changed,b,imf,oecd)
check('Scenario control changes model net-debt path',!near(subset(d,series_id=='e61_scenario_net')$value_pct_gdp,subset(e,series_id=='e61_scenario_net')$value_pct_gdp))
check('Scenario control preserves all published observations',identical(subset(d,provider!='e61'),subset(e,provider!='e61')))
check('Fixed model reference remains unchanged',identical(subset(d,grepl('e61_reference',series_id)),subset(e,grepl('e61_reference',series_id))))
write.csv(checks,'outputs/international_debt_validation_checks.csv',row.names=FALSE)
write.csv(d,'outputs/international_debt_comparison.csv',row.names=FALSE)
cat(nrow(checks),'international debt comparison checks passed.\n')
