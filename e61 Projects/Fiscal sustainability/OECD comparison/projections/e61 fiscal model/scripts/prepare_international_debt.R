# Offline extraction of source observations. Never extrapolate, rebase or convert debt concepts here.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
root<-'data/raw/international_debt';manifest<-read.csv(file.path(root,'source_manifest.csv'))
meta<-jsonlite::fromJSON(file.path(root,'imf_indicators.json'),simplifyVector=FALSE)$indicators
row<-function(provider,id,label,concept,scenario,year,value,vintage,url,locator,notes){
 data.frame(provider,country='AUS',series_id=id,series_label=label,concept,scenario,year=as.integer(year),
   value_pct_gdp=as.numeric(value),unit='% GDP',vintage,source_url=url,source_locator=locator,
   time_basis='Published annual source year; no fiscal/calendar conversion',notes,snapshot_date=manifest$snapshot_date[1])
}
imf<-do.call(rbind,lapply(c('net','gross'),function(n){
 code<-if(n=='net')'GGXWDN_G01_GDP_PT' else 'G_XWDG_G01_GDP_PT'
 j<-jsonlite::fromJSON(file.path(root,paste0('imf_',n,'_debt.json')),simplifyVector=FALSE)
 values<-j$values[[code]]$AUS;stopifnot(length(values)>0,meta[[code]]$unit=='% of GDP')
 row('IMF',paste0('imf_',n),paste('IMF',n,'debt'),paste0(n,'_debt'),'IMF published baseline',names(values),unlist(values),
   meta[[code]]$source,manifest$url[manifest$file==paste0('imf_',n,'_debt.json')],paste('DataMapper',code,'AUS'),
   if(n=='net')'General government debt obligations less corresponding financial assets. National reporting can differ. Historical/forecast flags are not supplied by this API.' else
     'General government debt obligations before deducting financial assets. Coverage and valuation can differ from the model. Historical/forecast flags are not supplied by this API.')
}))
imf<-imf[is.finite(imf$value_pct_gdp),];imf<-imf[order(imf$series_id,imf$year),]
flow<-xml2::read_xml(file.path(root,'oecd_dataflow.xml'))
vintage<-xml2::xml_text(xml2::xml_find_first(flow,"//*[local-name()='Dataflow']/*[local-name()='Name'][@xml:lang='en']"))
stopifnot(nzchar(vintage),!is.na(vintage))
doc<-xml2::read_xml(file.path(root,'oecd_net_liabilities.xml'));ns<-xml2::xml_ns(doc)
series<-xml2::xml_find_all(doc,'.//generic:Series',ns)
chosen<-series[vapply(series,function(s){k<-xml2::xml_find_all(s,'./generic:SeriesKey/generic:Value',ns);z<-setNames(xml2::xml_attr(k,'value'),xml2::xml_attr(k,'id'));identical(unname(z[c('REF_AREA','MEASURE','SCENARIO','FREQ')]),c('AUS','GNFLQ','BAU1','A'))},logical(1))]
stopifnot(length(chosen)==1L)
obs<-xml2::xml_find_all(chosen[[1]],'./generic:Obs',ns)
year<-xml2::xml_attr(xml2::xml_find_first(obs,'./generic:ObsDimension',ns),'value')
value<-xml2::xml_attr(xml2::xml_find_first(obs,'./generic:ObsValue',ns),'value')
oecd<-row('OECD','oecd_lts_net','OECD net financial liabilities (EO117 BAU1)','net_financial_liabilities',
 'BAU1 long-term model with fiscal reaction rule',year,value,vintage,
 manifest$url[manifest$file=='oecd_net_liabilities.xml'],'AUS.GNFLQ.BAU1.A',
 'Broader balance-sheet measure than net debt. Technical fiscal reaction rule stabilises debt. Separate dataflow/vintage from Survey Figure 1.14; not a net-debt conversion of either Survey path.')
w<-as.data.frame(suppressMessages(readxl::read_excel(file.path(root,'oecd_survey_figure_1_14.xlsx'),'fig1-14',col_names=FALSE)))
stopifnot(grepl('Gross public debt',w[[10]][30],fixed=TRUE),grepl('fiscal adjustment',w[[13]][31],fixed=TRUE))
years<-suppressWarnings(as.integer(w[[10]]));keep<-is.finite(years)&is.finite(suppressWarnings(as.numeric(w[[12]])))&is.finite(suppressWarnings(as.numeric(w[[13]])))
for(n in c('adjustment','no_adjustment')){
 oecd<-rbind(oecd,row('OECD',paste0('oecd_survey_',n),if(n=='adjustment')'OECD with fiscal adjustment (Survey 2026)' else 'OECD without fiscal adjustment (Survey 2026)',
   'gross_debt',if(n=='adjustment')'Path with fiscal adjustment' else 'Current tax/spending structure with future ageing/health costs',
   years[keep],w[[if(n=='adjustment')13 else 12]][keep],'OECD Economic Surveys: Australia 2026; workbook updated 21 January 2026',
   'https://stat.link/gdxr47','Figure 1.14 panel B; fig1-14 columns J,L,M',
   'Published gross public debt; not net debt. Both Survey scenarios share a vintage and definition. Source stops in 2050; no extrapolation.'))
}
oecd<-oecd[order(oecd$series_id,oecd$year),]
stopifnot(!anyDuplicated(imf[c('series_id','year')]),!anyDuplicated(oecd[c('series_id','year')]),
 all(is.finite(oecd$value_pct_gdp)),max(subset(oecd,concept=='gross_debt')$year)==2050)
write.csv(imf,'data/processed/imf_debt_projections.csv',row.names=FALSE)
write.csv(oecd,'data/processed/oecd_debt_projections.csv',row.names=FALSE)
cat('Saved',nrow(imf),'IMF and',nrow(oecd),'OECD observations, with source and concept metadata.\n')
print(aggregate(year~series_label+vintage,rbind(imf,oecd),function(y)paste(range(y),collapse='-')),row.names=FALSE)
