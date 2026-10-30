# Diagnostic comparison only: no projection assumptions or Shiny outputs are changed.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');x<-load_inputs();p<-project_model(x)
out<-'outputs/health_igr_check';dir.create(out,recursive=TRUE,showWarnings=FALSE)
book<-'data/raw/v2/igr_charts/IGR-2026-Chapter-12.xlsx'
raw<-as.data.frame(suppressMessages(readxl::read_excel(book,'C12.11',col_names=FALSE)))
year<-suppressWarnings(as.integer(substr(as.character(raw[[1]]),1,4))+1L)
igr<-do.call(rbind,lapply(2:6,function(j)data.frame(year,programme=as.character(raw[1,j]),
  igr_pct=suppressWarnings(as.numeric(raw[[j]])),locator='IGR 2026 C12.11',
  source_url='https://treasury.gov.au/publication/2026-intergenerational-report')))
igr<-igr[!is.na(igr$year)&is.finite(igr$igr_pct),]
write.csv(igr,file.path(out,'igr_health_programmes.csv'),row.names=FALSE)
h<-subset(p$subfunctions,programme=='Health')
write.csv(h,file.path(out,'e61_health_subfunctions.csv'),row.names=FALSE)
total<-aggregate(share_gdp~year,h,sum);names(total)[2]<-'e61_consolidated_pct'
official<-subset(read.csv('data/processed/igr_spending_benchmarks.csv'),programme=='Health')
total$igr_commonwealth_pct<-official$igr_pct[match(total$year,official$year)]
total$implied_non_commonwealth_pct_gdp<-total$e61_consolidated_pct-total$igr_commonwealth_pct
total$implied_non_commonwealth_share_percent<-100*total$implied_non_commonwealth_pct_gdp/total$e61_consolidated_pct
total$assumption<-'IGR Commonwealth GDP share imposed on common GDP denominator; residual includes state/local and accounting differences, not gross state hospital expenditure'
write.csv(total,file.path(out,'aggregate_implied_funding_shares.csv'),row.names=FALSE)
groups<-list(Hospitals=list(ids='h_hospital',igr='Public Hospitals',status='Residual is an upper bound on state share: Commonwealth MBS/PBS/PHI/other hospital funding also belongs here'),
  'Primary plus referred'=list(ids=c('h_primary','h_referred'),igr='Medical Benefits',status='Not an identified state share: MBS also funds hospitals; Other Health and PHI also fund primary/referred areas'),
  'Outside-hospital medicines'=list(ids='h_medicines',igr='Pharmaceutical Benefits',status='Invalid direct match: IGR PBS includes hospital medicines excluded from this model block; negative residual is not negative state spending'),
  'Other health and reconciliation'=list(ids='h_other',igr='Other Health',status='Not an identified state share: IGR Other Health includes DVA and programmes assigned across AIHW service areas; GFS reconciliation is also present'))
diagnostic<-do.call(rbind,lapply(names(groups),function(n){g<-groups[[n]];d<-aggregate(share_gdp~year,h[h$id%in%g$ids,],sum)
  o<-igr[igr$programme==g$igr,];d$group<-n;d$igr_programme<-g$igr;d$igr_pct<-o$igr_pct[match(d$year,o$year)]
  d$naive_residual_pct_gdp<-d$share_gdp-d$igr_pct;d$naive_residual_share_percent<-100*d$naive_residual_pct_gdp/d$share_gdp
  d$interpretation<-g$status;d
}))
write.csv(diagnostic,file.path(out,'unreconciled_programme_diagnostics.csv'),row.names=FALSE)
# Observed financing by service area, not a programme-to-area allocation guess.
aihw_file<-'estimation/data/raw/health_tables.xlsx'
at<-as.data.frame(suppressMessages(readxl::read_excel(aihw_file,'Table A6',col_names=FALSE)))
number<-function(v){v<-as.character(v);suppressWarnings(as.numeric(v))}
numrow<-function(label,column){k<-which(as.character(at[[1]])==label);stopifnot(length(k)==1);v<-number(at[k,column]);if(is.na(v))0 else v}
rows<-c('Hospital services','Primary health care','Referred medical services','Benefit-paid pharmaceuticals','Other services','Research','Total recurrent expenditure')
observed<-data.frame(area=rows,commonwealth_bn=vapply(rows,numrow,numeric(1),column=5)/1000,
  state_local_bn=vapply(rows,numrow,numeric(1),column=6)/1000)
observed$government_total_bn<-observed$commonwealth_bn+observed$state_local_bn
observed$state_local_share_percent<-100*observed$state_local_bn/observed$government_total_bn
# Match the primary block's exclusion of benefit-paid pharmaceuticals.
k<-which(observed$area=='Primary health care');j<-which(observed$area=='Benefit-paid pharmaceuticals')
primary<-observed[k,];primary$area<-'Primary/community excluding benefit-paid pharmaceuticals'
for(v in c('commonwealth_bn','state_local_bn','government_total_bn'))primary[[v]]<-observed[[v]][k]-observed[[v]][j]
primary$state_local_share_percent<-100*primary$state_local_bn/primary$government_total_bn
observed<-rbind(observed,primary);observed$year<-2025;observed$locator<-'AIHW HEA 2024-25 Table A6; rounded current-year amounts, government funders only'
write.csv(observed,file.path(out,'observed_2025_funding_by_service.csv'),row.names=FALSE)
# Reconciliation and scope checks; do not force a programme-to-area mapping.
stopifnot(!anyDuplicated(igr[c('year','programme')]),all(is.finite(h$share_gdp)),
 all(observed$state_local_share_percent>=0 & observed$state_local_share_percent<=100),
 all(subset(diagnostic,group=='Outside-hospital medicines' & year%in%c(2030,2066))$naive_residual_share_percent<0))
for(y in c(2030,2066))stopifnot(abs(sum(subset(igr,year==y)$igr_pct)-official$igr_pct[match(y,official$year)])<1e-8)
manifest<-data.frame(file=c(book,aihw_file,'data/spending_subfunctions.csv'),md5=unname(tools::md5sum(c(book,aihw_file,'data/spending_subfunctions.csv'))))
write.csv(manifest,file.path(out,'input_manifest.csv'),row.names=FALSE)
cat('Conditional aggregate residuals (not independently estimated state budgets):\n')
print(total[total$year%in%c(2030,2066),1:5],row.names=FALSE)
cat('\nUnreconciled programme diagnostics; do not interpret these as state shares:\n')
print(diagnostic[diagnostic$year%in%c(2030,2066),c('year','group','share_gdp','igr_pct','naive_residual_share_percent')],row.names=FALSE)
cat('\nObserved service-area funding shares:\n');print(observed[,c('area','state_local_share_percent')],row.names=FALSE)
