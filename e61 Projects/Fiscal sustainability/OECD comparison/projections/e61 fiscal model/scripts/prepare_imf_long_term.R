# Reproducible chart digitisation, NOT an official numerical IMF data download.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
root<-'data/raw/international_debt'
url<-'https://www.imf.org/en/-/media/files/publications/cr/2026/english/1ausea2026001-source-pdf.pdf'
p<-file.path(root,'imf_article_iv_2026.pdf')
if(!file.exists(p))curl::curl_download(url,p,quiet=TRUE)
stopifnot(pdftools::pdf_info(p)$pages==110)
img<-file.path(root,'imf_long_term_page_600.png')
if(!file.exists(img))suppressWarnings(pdftools::pdf_convert(p,format='png',pages=58,dpi=600,filenames=img))
a<-png::readPNG(img);stopifnot(identical(dim(a),c(6600L,5100L,3L)))
# Coordinates below are in the 150 dpi reference image. Right-hand debt chart:
# y=807.5 is 80%; y=1010 is zero. Category centres: 2017 at x=704.25,
# annual spacing 9 pixels; final unlabelled category is 2054 at x=1037.25.
# Labels are every third year (last label 2053); there is one final annual point.
yy<-(807*4):(1010*4);xx<-(800*4):(1040*4);b<-a[yy,xx,]
r<-b[,,1];g<-b[,,2];bl<-b[,,3]
labels<-c(baseline='IMF: medium-term assumptions extended (approx.)',
 stabilising='IMF: debt-stabilising primary balance (approx.)',
 historical='IMF: historical-average assumptions (approx.)')
d<-do.call(rbind,lapply(names(labels),function(n){
 mask<-switch(n,baseline=bl>.2&bl>r*1.5&bl>g*1.3,
   stabilising=g>.35&g>r*1.5&g>bl*1.3,historical=r>.7&g<.3&bl<.3)
 points<-data.frame(x=xx[col(mask)[mask]]/4,y=yy[row(mask)[mask]]/4)
 points<-aggregate(y~x,points,median)
 years<-2030:2054;target<-704.25+(years-2017)*9
 # Linear interpolation only bridges dashed-line gaps in the published chart.
 # It does not extend any scenario beyond the plotted domain.
 y<-approx(points$x,points$y,xout=target,rule=1)$y
 stopifnot(all(is.finite(y)))
 data.frame(provider='IMF',country='AUS',series_id=paste0('imf_long_',n),
   series_label=labels[[n]],concept='gross_debt',scenario=switch(n,
     baseline='Baseline with t+5',stabilising='Baseline with t+5 and DSPB',historical='Historical 10-year average'),
   year=years,value_pct_gdp=round((1010-y)/(1010-807.5)*80,1),unit='% GDP',
   vintage='IMF Country Report 26/38 (2026)',source_url=url,
   source_locator='Annex III, Figure 7, printed page 53 (PDF page 58), right-hand chart',
   time_basis='Published source year; no fiscal/calendar conversion',
   notes='Approximate chart digitisation; allow at least +/-0.5 pp GDP reading uncertainty. Not official numerical observations. Gross debt only. Large-amortisation scenarios, not a complete ageing-cost projection; no extrapolation beyond 2054.',
   snapshot_date='2026-10-27')
}))
write.csv(d,'data/processed/imf_long_term_debt_projections.csv',row.names=FALSE)
write.csv(data.frame(file=basename(p),url,snapshot_date='2026-10-27',md5=unname(tools::md5sum(p)),
 extraction='prepare_imf_long_term.R; 600 dpi colour-line digitisation; approximate +/-0.5 pp GDP or more'),
 file.path(root,'imf_long_term_source_manifest.csv'),row.names=FALSE)
print(subset(d,year%in%c(2030,2054),select=c(series_label,year,value_pct_gdp)),row.names=FALSE)
