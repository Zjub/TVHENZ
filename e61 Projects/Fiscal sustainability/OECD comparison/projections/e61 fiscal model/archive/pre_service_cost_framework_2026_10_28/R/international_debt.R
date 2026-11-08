# Keep the published measures and horizons intact; do not infer OECD net debt from gross debt.
international_debt_comparison<-function(scenario,reference,imf,oecd,start_year=2030L,end_year=2066L,view='levels'){
 stopifnot(start_year>=2030L,start_year<=end_year,view%in%c('levels','change'))
 # Medium-term Fiscal Monitor observations are deliberately excluded, even if
 # supplied by a caller. The common PBO anchor handles the forecast period.
 imf<-imf[grepl('^imf_long_',imf$series_id),]
 official<-rbind(imf,oecd)
 official$series_label[official$series_id=='oecd_lts_net']<-'OECD net financial liabilities (EO117 BAU1)'
 official$series_label[official$series_id=='oecd_survey_adjustment']<-'OECD Survey: with fiscal adjustment'
 official$series_label[official$series_id=='oecd_survey_no_adjustment']<-'OECD Survey: without fiscal adjustment'
 rows<-lapply(c('net','gross'),function(concept){
   field<-paste0(concept,'_debt_pct')
   do.call(rbind,lapply(c('scenario','reference'),function(case){
     m<-if(case=='scenario')scenario$path else reference$path
     data.frame(provider='e61',country='AUS',series_id=paste('e61',case,concept,sep='_'),
       series_label=if(case=='scenario')'Your scenario' else 'Fixed reference',concept=paste0(concept,'_debt'),
       scenario=case,year=m$year,value_pct_gdp=m[[field]],unit='% GDP',vintage='Current e61 model',
       source_url='',source_locator='Projection engine; common PBO 2030 anchor',
       time_basis='Australian financial year ending',notes='Model assumptions; no automatic fiscal adjustment',snapshot_date=NA_character_)
   }))
 })
 d<-rbind(official,do.call(rbind,rows));d<-d[order(d$series_id,d$year),]
 d$base_2030_pct<-vapply(d$series_id,function(id){z<-d$value_pct_gdp[d$series_id==id&d$year==2030];if(length(z)==1)z else NA_real_},numeric(1))
 d$plotted_value<-if(view=='change')d$value_pct_gdp-d$base_2030_pct else d$value_pct_gdp
 d$plot_unit<-if(view=='change')'Change from 2030 (pp GDP)' else '% GDP'
 d$panel<-ifelse(d$concept=='gross_debt','Gross debt: both OECD Survey scenarios',
   'Net measures: OECD liabilities differ from net debt')
 d<-d[d$year>=start_year&d$year<=end_year,]
 d$panel<-factor(d$panel,levels=c('Net measures: OECD liabilities differ from net debt','Gross debt: both OECD Survey scenarios'))
 rownames(d)<-NULL;d
}
