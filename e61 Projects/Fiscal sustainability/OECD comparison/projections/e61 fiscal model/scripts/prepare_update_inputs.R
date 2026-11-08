# Reproducible source extraction and reconciliation; no network activity.
args <- commandArgs(FALSE)
script <- sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
suppressPackageStartupMessages({library(readxl);library(data.table)})
raw <- 'data/raw/v2'; dir.create('data/processed',showWarnings=FALSE)
read_sheet <- function(f,s,...) as.data.frame(suppressMessages(read_excel(file.path(raw,f),sheet=s,...,.name_repair='minimal')))
igr <- function(ch,s,skip=0) read_sheet(sprintf('igr_charts/IGR-2026-Chapter-%02d.xlsx',ch),s,skip=skip)
fy <- function(x) as.integer(substr(as.character(x),1,4))+1L
save <- function(x,f) fwrite(x,file.path('data/processed',f))

# PBO national, Commonwealth and state totals. National already eliminates flows.
v <- read_sheet('pbo_nfo_2026_27.xlsx','Table 1',col_names=FALSE)
r <- read_sheet('pbo_nfo_2026_27.xlsx','Table 2',col_names=FALSE)
years <- fy(unlist(v[2,-1]))
blocks <- c(net_operating_balance=2,fiscal_balance=15,net_capital_investment=28,
  revenue=41,expenses=54,net_debt=67,gross_debt=80,public_debt_interest=93)
rows <- c(national=11,commonwealth=10,states=9)
pbo <- do.call(rbind,lapply(names(rows),function(level) {
  d <- data.frame(year=years,level=level)
  for(n in names(blocks)) {
    k<-blocks[n]+rows[level]
    d[[paste0(n,'_billion_dollars')]]<-as.numeric(unlist(v[k,-1]))
    d[[paste0(n,'_ratio_gdp')]]<-as.numeric(unlist(r[k,-1]))/100
  }
  d$nominal_gdp_billion <- d$gross_debt_billion_dollars/d$gross_debt_ratio_gdp
  d
}))
save(pbo,'pbo_by_level.csv')
national <- subset(pbo,level=='national')
national$status <- ifelse(national$year<=2025,'Historical/actual','Official forecast')
save(national,'official_pbo_nfo_wide.csv')
bridge <- subset(pbo,level=='commonwealth')[,c('year','revenue_billion_dollars','expenses_billion_dollars')]
st<-subset(pbo,level=='states'); nat<-subset(pbo,level=='national')
bridge$states_revenue_bn<-st$revenue_billion_dollars
bridge$states_expenses_bn<-st$expenses_billion_dollars
bridge$national_revenue_bn<-nat$revenue_billion_dollars
bridge$national_expenses_bn<-nat$expenses_billion_dollars
bridge$revenue_consolidation_adjustment_bn<-nat$revenue_billion_dollars-bridge$revenue_billion_dollars-st$revenue_billion_dollars
bridge$expense_consolidation_adjustment_bn<-nat$expenses_billion_dollars-bridge$expenses_billion_dollars-st$expenses_billion_dollars
bridge$interpretation<-'Net elimination plus local-government/coverage adjustments; not a pure transfer estimate'
save(bridge,'consolidation_bridge.csv')

# Official annual IGR age/sex projections, plus the 2025 pre-projection bridge.
source('R/prepare_annual_population.R');prepare_annual_population(raw)

# Observed sex/age participation and unemployment, April 2025 to March 2026.
lf_rows<-list()
for(s in paste0('Data',1:4)) {
  d<-read_sheet('abs_lf_age_mar2026.xlsx',s,col_names=FALSE)
  h<-as.character(d[1,]); dates<-as.Date(suppressWarnings(as.numeric(d[[1]])),origin='1899-12-30')
  date_rows<-which(!is.na(dates)&dates>=as.Date('2025-04-01')&dates<=as.Date('2026-03-01'))
  for(j in which(grepl('Participation rate ;|Unemployment rate ;',h)&!grepl('looked|Married|married',h))) {
    pieces<-trimws(gsub('>','',strsplit(h[j],';',fixed=TRUE)[[1]]))
    if(length(pieces)!=3 || !pieces[3]%in%c('Males','Females'))next
    ag<-pieces[1]
    if(!grepl('^(15-19|20-24|25-29|30-34|35-39|40-44|45-49|50-54|55-59|60-64|65 years)',ag))next
    amin<-as.integer(substr(ag,1,2));amax<-if(amin==65)100 else amin+4
    vals<-as.numeric(unlist(d[date_rows,j]));stopifnot(length(vals)==12)
    lf_rows[[length(lf_rows)+1]]<-data.table(age_min=amin,age_max=amax,
      sex=ifelse(pieces[3]=='Males','Male','Female'),measure=pieces[2],rate=mean(vals)/100,
      source_series=as.character(d[10,j]))
  }
}
lf<-rbindlist(lf_rows)
stopifnot(nrow(lf)==44)
lf<-dcast(lf,age_min+age_max+sex~measure,value.var='rate')
setnames(lf,c('Participation rate','Unemployment rate'),c('participation_rate','unemployment_rate'))
save(lf,'observed_labour_rates.csv')

# Actual hours by age and sex; no arbitrary age employment/hour propensities.
hh<-as.data.table(read_sheet('abs_hours_age_mar2026.xlsx','Data 1',skip=3))
setnames(hh,1:8,c('date','sex','age_group','hours_group','ft','pt','ft_hours','pt_hours'))
hh[,date:=as.Date(date)]
hh<-hh[date>=as.Date('2025-04-01')&date<=as.Date('2026-03-01') & sex%in%c('Males','Females')]
for(n in c('ft','pt','ft_hours','pt_hours'))set(hh,j=n,value=as.numeric(hh[[n]]))
hours<-hh[,.(workers=sum(ft+pt),hours=sum(ft_hours+pt_hours)),by=.(sex,age_group,date)]
hours<-hours[,.(hours_per_worker=sum(hours)/sum(workers),workers=mean(workers)*1000),by=.(sex,age_group)]
hours[,`:=`(sex=ifelse(sex=='Males','Male','Female'),age_min=as.integer(substr(age_group,1,2)))]
hours[,age_max:=ifelse(grepl('over',age_group),100L,age_min+4L)]
# Remove overlapping total-age rows, retain the finest published groups.
hours<-hours[(age_max-age_min==4L)|age_min==65L]
stopifnot(nrow(hours)==22,all(is.finite(hours$hours_per_worker)))
save(hours,'observed_hours_by_age.csv')

part<-igr(9,'C9.13'); part<-data.frame(year=fy(part[[1]]),Male=as.numeric(part[[2]])/100,Female=as.numeric(part[[3]])/100)
save(subset(part,year>=2026),'participation_targets.csv')
save(data.frame(parameter='average_hours_growth',value=-0.001,source='2026 IGR C7.2 next 40 years; composition-adjusted aggregate target'), 'hours_assumption.csv')

# Published cost profiles, not hand-assigned broad age weights.
h<-igr(12,'C12.10');amin<-as.integer(sub('[^0-9].*$','',as.character(h[[1]])))
health<-data.frame(age_min=amin,age_max=c(head(amin,-1)+4,100),weight=rowSums(as.matrix(h[,-1])))
save(health,'health_age_cost_profile.csv')
allcost<-igr(6,'C6.6');amin<-as.integer(sub('[^0-9].*$','',as.character(allcost[[1]])))
care<-data.frame(age_min=amin,age_max=c(head(amin,-1)+4,100),weight=abs(as.numeric(allcost[['Aged care']])) )
save(care,'aged_care_age_cost_profile.csv')
edu<-data.frame(age_min=amin,age_max=c(head(amin,-1)+4,100),
  weight=pmax(abs(as.numeric(allcost[['Education and other']]))-min(abs(as.numeric(allcost[['Education and other']]))),0))
save(edu,'education_age_cost_proxy.csv')
ps<-igr(12,'C12.19');save(data.frame(year=fy(ps[[1]]),full=ps[[2]]/100,assets_tested=ps[[3]]/100,
  income_tested=ps[[4]]/100,no_pension=ps[[5]]/100),'igr_pension_status_benchmark.csv')
sp<-igr(12,'C12.5');save(data.frame(year=fy(sp[[1]]),sp[,-1],check.names=FALSE),'igr_commonwealth_spending_benchmark.csv')
ndis_total<-read_sheet('igr_charts/IGR-2026-Chapter-12.xlsx','C12.17',col_names=FALSE)
keep_ndis<-grepl('^[0-9]{4}',ndis_total[[1]]) & !is.na(ndis_total[[1]])
save(data.frame(year=fy(ndis_total[[1]][keep_ndis]),
  igr_2026_pct=as.numeric(ndis_total[[2]][keep_ndis]),
  igr_2023_pct=as.numeric(ndis_total[[3]][keep_ndis]),
  coverage='Total NDIS Scheme; own GDP denominator; rounded published shares',
  source='2026 IGR chart C12.17',url='https://treasury.gov.au/publication/2026-intergenerational-report'),
  'ndis_igr_benchmark.csv')
rev_igr<-igr(13,'C13.5');save(data.frame(year=fy(rev_igr[[1]]),rev_igr[,-1],check.names=FALSE),'igr_commonwealth_revenue_benchmark.csv')

# ATO actual taxable-income distribution: national sum across mutually exclusive
# state/sex cells, one record per percentile. Do not sum subtotal rows.
ato<-as.data.table(read_sheet('ato_individuals_2023_24_table16.xlsx','Table 16A',skip=1))
setnames(ato,1:7,c('percentile','range','state','sex','count','taxable_income','net_tax'))
ato[,percentile:=suppressWarnings(as.integer(percentile))]
ato<-ato[percentile%in%1:100 & sex%in%c('Male','Female')]
for(n in c('count','taxable_income','net_tax'))set(ato,j=n,value=as.numeric(ato[[n]]))
dist<-ato[,.(count=sum(count),taxable_income=sum(taxable_income),observed_net_tax=sum(net_tax)),by=percentile]
dist[,mean_income:=taxable_income/count]
stopifnot(nrow(dist)==100,all(dist$count>0))
save(dist,'ato_income_distribution.csv')

# ABS tax categories: Commonwealth + all-state/local tables. GST is collected
# once at Commonwealth level. Explicitly remove taxes paid by other GGS levels.
tax<-list()
for(z in list(c('Table_1','Commonwealth'),c('Table_10','State/local'))) {
  d<-read_sheet('abs_tax_2024_25.xlsx',z[1],col_names=FALSE)
  yrs<-fy(unlist(d[5,-1]))
  tax[[z[2]]]<-rbindlist(lapply(1:nrow(d),function(i)data.table(item=trimws(as.character(d[i,1])),
    year=yrs,value=as.numeric(unlist(d[i,-1]))/1000,level=z[2])))
}
tax<-rbindlist(tax);save(tax,'abs_tax_history_long.csv')
pick<-function(items,lev='Commonwealth')tax[tax$level==lev & tax$item%in%items,.(value=sum(value,na.rm=TRUE)),by=year]
heads<-list(
 personal_income=pick('Personal income tax'),company=pick('Company income tax'),
 superannuation=pick('Income tax paid by superannuation funds'),
 medicare_fbt=pick(c('Government health insurance levy','Fringe benefits tax')),
 gst=pick('Goods and services tax (GST)'),
 excises_customs=pick(c('Total excises and levies','Taxes on international trade')),
 payroll=pick('Taxes on employers payroll and labour force','State/local'),
 property=pick('Total taxes on property','State/local'),
 stamp_duties=pick('Total taxes on financial and capital transactions','State/local'))
hist<-data.table(year=2016:2025)
for(n in names(heads))hist[[n]]<-heads[[n]]$value[match(hist$year,heads[[n]]$year)]
common_total<-pick('Total Taxation on Commonwealth Government')
state_total<-pick('Total Taxation all States State and Local Government','State/local')
inter_common<-pick('Taxes received from other levels of government')
inter_state<-pick('Taxes received from other levels of government','State/local')
gross_tax<-common_total$value[match(hist$year,common_total$year)]+state_total$value[match(hist$year,state_total$year)]
hist$other_taxes<-gross_tax-rowSums(hist[,-1])
inter<-inter_common$value[match(hist$year,inter_common$year)]+inter_state$value[match(hist$year,inter_state$year)]
gfs_tax<-fread('data/historical_gfs_operating_statement.csv')[item=='Taxation revenue']
consolidated<-gfs_tax$value[match(hist$year,gfs_tax$year)]/1000
# Detailed taxation tables and GFS differ in revisions and consolidation. Use
# the observed national consolidated GFS total as control, retain the full bridge.
factor<-consolidated/gross_tax
for(n in setdiff(names(hist),'year'))set(hist,j=n,value=hist[[n]]*factor)
save(hist,'tax_head_history.csv')
save(data.frame(year=hist$year,gross_tax=gross_tax,reported_intergovernmental_tax=inter,
  consolidated_tax=consolidated,net_elimination_and_revision=gross_tax-consolidated,
  allocation='Tax-head values reconciled pro rata to observed consolidated GFS total; no GST grants added'), 'tax_consolidation_bridge.csv')

# Descriptive short-sample estimates retained for provenance only.
# No coefficient or intercept from these regressions enters any projection.
gdp<-national$nominal_gdp_billion[match(hist$year,national$year)]
estimates<-rbindlist(lapply(setdiff(names(hist),'year'),function(n) {
  fit<-lm(diff(log(hist[[n]]))~diff(log(gdp)))
  ct<-coef(summary(fit));beta<-ct[2,1];se<-ct[2,2]
  prior_sd<-.5;posterior<-(beta/se^2+1/prior_sd^2)/(1/se^2+1/prior_sd^2)
  data.table(head=n,raw_elasticity=beta,standard_error=se,applied_elasticity=posterior,
    observations=nobs(fit),r_squared=summary(fit)$r.squared,
    estimation='Descriptive only, not used in projections; 2017-2025 annual changes versus nominal GDP; Normal(1,0.5^2) shrinkage')
}))
save(estimates,'tax_elasticity_estimates.csv')

source_files<-list.files(raw,recursive=TRUE,full.names=TRUE)
source_files<-source_files[!grepl('[.]txt$',source_files)]
save(data.frame(file=source_files,bytes=file.info(source_files)$size,
  md5=unname(tools::md5sum(source_files)),snapshot='2026-10-25'),'source_checksums.csv')
cat('Prepared v2 source extracts: fiscal consolidation, age/sex population, labour, care and tax.\n')
