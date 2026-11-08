# Offline preparation. Run after prepare_bottom_up_inputs.R when rebuilding all inputs.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
raw<-'data/raw/health_education'
h<-as.data.frame(suppressMessages(readxl::read_excel(file.path(raw,'hospital_costs.xlsm'),'5 Acute Overview',col_names=FALSE)))
stopifnot(h[[28]][46]=='2023-24',grepl('Nursing',h[[1]][47]),h[[1]][69]=='Total')
hospital_labour<-sum(as.numeric(h[[28]][47:52]))/as.numeric(h[[28]][69])
# Published ACARA Table 9.3, government schools, 2023/24 ($000).
# Remove notional user cost of capital to improve alignment with the GFS expense concept.
school_salary<-31447043+11301892+19259;school_total<-56279069
# Published university Finance 2024 Table 4 ($000), includes research and all funding sources.
university_labour<-23891495/43068392
vet<-as.data.frame(suppressMessages(readxl::read_excel(file.path(raw,'vet_tables.xlsx'),'Table 5A.1',col_names=FALSE)))
stopifnot(vet[[3]][14]=='2024',abs(as.numeric(vet[[22]][14])-7617.2)<.01)
vet_expense<-as.numeric(vet[[22]][14])/1000
uni_expense<-43068392/1e6
# Proxy allocation weights, not a claim of exact COFOG comparability or common vintage.
vet_share<-vet_expense/(vet_expense+uni_expense)
d<-read.csv('data/spending_subfunctions.csv',check.names=FALSE)
if('e_tertiary'%in%d$id){
 old<-d[d$id=='e_tertiary',];un<-ve<-old
 un$id<-'e_higher';un$subfunction<-'Higher education';un$base_2025_bn<-old$base_2025_bn*(1-vet_share)
 un$source<-paste(old$source,'Department of Education Finance 2024 Table 4; RoGS 2026 Table 5A.1',sep='; ')
 un$locator<-'ABS tertiary control allocated using 2024 university total expenses versus VET recurrent expenses excluding user cost of capital'
 un$status<-'Sourced tertiary total; cross-source allocation proxy'
 un$limits<-'18-24 demand proxy omits mature-age/international composition; university expenses include research and private funding; source concepts differ'
 ve$id<-'e_vet';ve$subfunction<-'Vocational education and training';ve$base_2025_bn<-old$base_2025_bn*vet_share;ve$profile<-'vet_age'
 ve$source<-un$source;ve$locator<-un$locator;ve$status<-un$status
 ve$limits<-'15-64 demand proxy; allocation is not an exact GFS split; government-funded training participation and hours held constant'
 k<-which(d$id=='e_tertiary');d<-rbind(d[seq_len(k-1),],un,ve,d[seq.int(k+1,nrow(d)),])
}else{
 stopifnot(all(c('e_higher','e_vet')%in%d$id))
 total<-sum(d$base_2025_bn[d$id%in%c('e_higher','e_vet')])
 d$base_2025_bn[d$id=='e_higher']<-total*(1-vet_share);d$base_2025_bn[d$id=='e_vet']<-total*vet_share
}
write.csv(d,'data/spending_subfunctions.csv',row.names=FALSE)
source_h<-'https://www.ihacpa.gov.au/resources/national-hospital-cost-data-collection-public-sector-2023-24'
source_s<-'https://dataandreporting.blob.core.windows.net/anrdataportal/ANR-Documents/ANR2024/nationalreportonschoolinginaustralia_2024_Chapter9.pdf'
source_u<-'https://www.education.gov.au/download/19832/finance-2024-financial-reports-higher-education-providers/43005/2024-university-finance-summary-information/pdf'
source_v<-'https://federalfinancialrelations.gov.au/sites/federalfinancialrelations.gov.au/files/2023-12/national-skills-agreement.pdf'
ids<-c('h_hospital','h_primary','h_referred','h_medicines','h_other','e_school','e_higher','e_vet','e_other')
p<-data.frame(id=ids,
 cost_rule=c('input_mix','input_mix','input_mix','cpi_proxy','input_mix','input_mix','input_mix','funding_proxy','input_mix'),
 wage_weight=c(hospital_labour,.7,.7,0,.7,school_salary/school_total,university_labour,.85*.75+.15*.40,.8),
 service_productivity_growth=0,coverage_growth=0,
 intensity_growth=c(rep(.005,5),rep(0,4)),relative_price_loading=c(1,1,1,0,1,1,1,0,1),
 evidence_class=c('Observed input share; scope proxy','Retained judgment','Retained judgment','Explicit judgment','Retained judgment',
   'Observed input share; scope proxy','Observed input share; scope proxy','Published funding rule; scope proxy','Retained judgment'),
 source_url=c(source_h,'','','','',source_s,source_u,source_v,''),
 locator=c('Appendix 5.2, national 2023-24 column: salary/wage line items plus oncosts / total line-item cost',
   'Previous 70% health exposure retained as an unestimated input-mix proxy; not the unpublished WCI-5 formula',
   'Previous 70% health exposure retained; no claimed estimate of future MBS indexation',
   'Government cost per prescription follows CPI; no extra real price trend fitted',
   'Previous 70% health exposure retained for heterogeneous residual',
   'ACARA Table 9.3: salaries including redundancies / recurrent spending excluding user cost of capital',
   'Finance 2024 Table 4: employee benefits 23,891,495 / total expenses 43,068,392 ($000)',
   'National Skills Agreement flexible funding: 85% WCI-1 and 15% WCI-6; wage weights 75% and 40%',
   'Previous 80% education exposure retained for heterogeneous residual'),
 limitation=c('Public admitted-acute cost mix applied to all hospital funding; includes VMOs and oncosts; does not identify future efficiency',
   'Community/public health and Medicare funding need separate models; not measured labour share',
   'Government rebates differ from provider costs and include policy changes; not measured labour share',
   'Ignores medicine innovation, price disclosure, confidential rebates and future mix; 0.5% intensity retained, not estimated',
   'Includes reconciliation and research; not a single observed service',
   'Government-school cost structure applied to all government school funding; funding rules differ, oncost scope differs',
   'Whole-provider structure includes research/international activity; not a pure government-funded teaching cost share',
   'Commonwealth flexible-funding formula extended as proxy to all VET; productivity discount set to zero, not estimated',
   'Includes preschool and residual functions; not independently estimated'))
stopifnot(all(p$wage_weight>=0&p$wage_weight<=1))
write.csv(p,'data/health_education_parameters.csv',row.names=FALSE)
audit<-data.frame(parameter=c('hospital_labour_share','school_labour_share','university_labour_share','vet_wage_weight','vet_allocation_share','vet_2024_expenses_bn','university_2024_expenses_bn'),
 value=c(hospital_labour,school_salary/school_total,university_labour,.85*.75+.15*.4,vet_share,vet_expense,uni_expense))
write.csv(audit,'data/processed/health_education_source_calculations.csv',row.names=FALSE)
cat('Prepared',nrow(d),'subfunctions and',nrow(p),'health/education cost specifications.\n');print(audit,row.names=FALSE)
