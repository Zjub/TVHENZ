# Offline extraction and explicit allocation rules. All amounts AUD bn.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
library(readxl)
edu<-as.data.frame(suppressMessages(read_excel('data/raw/bottom_up/abs_education_2025.xlsx',sheet='Table_1',col_names=FALSE)))
stopifnot(edu[[11]][6]=='2024-25',edu[[1]][9]=='School education')
gfs<-subset(read.csv('data/historical_gfs_expenses_by_purpose.csv'),year==2025)
gv<-function(n)gfs$value[match(n,gfs$item)]/1000
interest<-subset(read.csv('data/historical_gfs_operating_statement.csv'),year==2025 & item=='Interest expenses n.e.c.')$value/1000
rows<-list()
add<-function(id,programme,subfunction,value,profile,source,locator,status='Sourced allocation; model demand proxy',limits='') {
 rows[[length(rows)+1L]]<<-data.frame(id,programme,subfunction,base_2025_bn=value,profile,source,locator,status,limits)
}
aihw<-'https://www.aihw.gov.au/reports/health-welfare-expenditure/health-expenditure-australia-2024-25/contents/spending-trends-by-source/'
# Rounded government-funder amounts, not total public+private health spending.
# AIHW state amounts are own-source funding, not gross state spending plus grants.
add('h_hospital','Health','Hospital services',35.4+8.3+59.5,'hospital',paste0(aihw,'australian-government-spending'),
 'Figures 11 and 16: Commonwealth public 35.4 + private 8.3 + state hospital 59.5',limits='Rounded AIHW funding amounts; public-hospital age profile used for both public and private hospitals; no grants added separately')
add('h_primary','Health','Primary and community care, excluding PBS medicines',39.7+12.9-15.8,'medical',paste0(aihw,'australian-government-spending'),
 'Figures 11/16: Commonwealth primary 39.7 + state primary 12.9 - Commonwealth PBS within primary 15.8',limits='Medical-benefits age profile is a proxy for heterogeneous community/public-health services')
add('h_referred','Health','Referred medical services',18.1,'medical',paste0(aihw,'australian-government-spending'),
 'Figure 11: referred medical services; state page notes no referred medical spending',limits='IGR medical-benefits age profile used as a proxy')
add('h_medicines','Health','PBS medicines outside hospitals',15.8,'pharma',paste0(aihw,'australian-government-spending'),
 'Area of spending: PBS excluding drugs allocated to hospitals',limits='Does not separately model medicine innovation, patents, utilisation or price negotiations')
used<-sum(vapply(rows,function(z)z$base_2025_bn,numeric(1)))
add('h_other','Health','Other health and GFS reconciliation',gv('Health')-used,'population',
 'ABS GFS Health control less named AIHW funding blocks','ABS Table 4; residual identity','Residual allocation; population proxy',
 'Includes research, other services, classification/rounding and accounting differences; not an independently measured service group')
for(j in 9:11) {
 nm<-c('School education','Tertiary education','Other education')[j-8]
 add(c('e_school','e_tertiary','e_other')[j-8],'Education',nm,as.numeric(edu[[11]][j])/1000,
   c('school_age','tertiary_age','population')[j-8],
   'https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/2024-25/55120DO095_202425.xlsx',
   paste('Table_1 row',j,'2024-25'),limits=c('Ages 5-17 proxy; constant enrolment/coverage; includes government funding of all school sectors',
   'Ages 18-24 proxy; includes higher education/VET and mature-age/international activity not separately modelled',
   'Residual purpose includes preschool/other education; total-population proxy, not a fitted age profile')[j-8])
}
gen<-'https://www.gen-agedcaredata.gov.au/topics/spending-on-aged-care'
for(j in 1:4)add(c('a_residential','a_home','a_flexible','a_other')[j],'Aged care',
 c('Residential aged care','Home care and support','Flexible aged care','Other aged care')[j],c(24.2,12.9,1.2,1.5)[j],'aged_care',gen,
 'Figure 1, government expenditure 2024-25; published rounded AUD bn',limits='Uses common IGR aged-care age profile; distinct residential/home utilisation profiles not yet estimated')
policy<-read.csv('data/policy_parameters.csv');pv<-function(n)policy$value[match(n,policy$parameter)]
add('p_age','Age Pension','Age Pension',pv('age_pension_expenses_2025'),'pension',
 'https://www.anao.gov.au/work/performance-audit/administration-of-the-age-pension','2024-25 outlay used in existing module','Sourced outlay; synthetic entitlement model','Wealth/household archetypes, indexation approximations and cash-to-GFS mapping retained')
add('n_ndis','NDIS','NDIS participant plans',pv('ndis_plan_expenses_2025'),'ndis',
 'https://www.ndis.gov.au/media/8108/download','Annual Report Note 1.1G; participant plan expenses','Sourced plan expense; existing cohort model','NDIS pass-through/intensity settings unchanged; administration remains in broader residual')
add('s_other','Other social protection','Other social protection (unallocated programmes)',
 gv('Social Protection')-pv('age_pension_expenses_2025')-pv('aged_care_expenses_2025')-pv('ndis_plan_expenses_2025'),'working_age',
 'ABS GFS Social Protection control less separate pension/care/NDIS allocations','Residual identity','Residual allocation; working-age proxy',
 'Not a separate simulation of JobSeeker, disability pensions, family assistance, child care or state welfare; remains a material modelling limitation')
others<-setdiff(gfs$item,c('Health','Education','Social Protection','Defence'))
for(j in seq_along(others)) {
 n<-others[j];v<-gv(n)-if(n=='General public services')interest else 0
 add(paste0('o_',j),'Other public services',if(n=='General public services')'General public services excluding conventional interest' else n,v,'population',
 'https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/2024-25',
 paste('All-government Table_4:',n),limits='Total-population demand proxy; parent cost rule retained; functional expenses include depreciation/capital transfers; gross capital formation is not added')
}
add('d_defence','Defence','Defence',gv('Defence'),'target','ABS GFS Defence; prototype target',
 'Table_4; target reaches 2.5% GDP by 2035','Sourced starting expense; policy judgment','GDP target overrides demand/cost equation; not a sourced defence projection')
spec<-do.call(rbind,rows);stopifnot(all(spec$base_2025_bn>0),!anyDuplicated(spec$id))
write.csv(spec,'data/spending_subfunctions.csv',row.names=FALSE)
# Retain distinct published health age shapes, with exact source columns.
h<-as.data.frame(read_excel('data/raw/v2/igr_charts/IGR-2026-Chapter-12.xlsx',sheet='C12.10'))
amin<-as.integer(sub('[^0-9].*$','',h[[1]]))
profiles<-do.call(rbind,lapply(2:4,function(j)data.frame(profile=c('hospital','medical','pharma')[j-1],
 age_min=amin,age_max=c(head(amin,-1)+4L,100L),weight=as.numeric(h[[j]]),
 source=paste('2026 IGR C12.10:',names(h)[j]))))
write.csv(profiles,'data/processed/subfunction_age_profiles.csv',row.names=FALSE)
cat('Prepared',nrow(spec),'spending subfunctions and',nrow(profiles),'health-profile rows.\n')
