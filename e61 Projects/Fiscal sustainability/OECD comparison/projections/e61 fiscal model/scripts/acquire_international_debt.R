# Official IMF/OECD snapshots. No network access is needed by the application.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
root<-'data/raw/international_debt';dir.create(root,recursive=TRUE,showWarnings=FALSE)
urls<-c(imf_indicators.json='https://www.imf.org/external/datamapper/api/v1/indicators',
 imf_net_debt.json='https://www.imf.org/external/datamapper/api/v1/GGXWDN_G01_GDP_PT/AUS',
 imf_gross_debt.json='https://www.imf.org/external/datamapper/api/v1/G_XWDG_G01_GDP_PT/AUS',
 oecd_net_liabilities.xml='https://sdmx.oecd.org/public/rest/v1/data/OECD.ECO.MAD,DSD_EO_LTB@DF_EO_LTB,/AUS.GNFLQ.BAU1.A?startPeriod=1990&endPeriod=2100',
 oecd_dataflow.xml='https://sdmx.oecd.org/public/rest/v1/dataflow/OECD.ECO.MAD/DSD_EO_LTB@DF_EO_LTB/latest?references=none',
 oecd_survey_figure_1_14.xlsx='https://stat.link/gdxr47')
manifest<-do.call(rbind,lapply(names(urls),function(n){
 p<-file.path(root,n);status<-'cached'
 if(!file.exists(p)){
   curl::curl_download(urls[[n]],p,quiet=TRUE,handle=curl::new_handle(timeout=90));status<-'downloaded'
 }
 stopifnot(file.info(p)$size>100)
 data.frame(file=n,url=urls[[n]],snapshot_date='2026-10-27',status,bytes=file.info(p)$size,md5=unname(tools::md5sum(p)))
}))
write.csv(manifest,file.path(root,'source_manifest.csv'),row.names=FALSE)
cat('Cached',nrow(manifest),'official international debt sources.\n')
