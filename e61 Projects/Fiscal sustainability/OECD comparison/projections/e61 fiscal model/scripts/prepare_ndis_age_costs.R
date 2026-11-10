# Offline extraction: national all-disability/all-support totals only; no subtotal sums.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
library(data.table);raw<-'data/raw/ndis_age_costs'
b<-fread(file.path(raw,'participant_budgets_june_2025.csv'))
u<-fread(file.path(raw,'utilisation_june_2025.csv'))
b<-b[RprtDt=='30JUN2025' & StateCd=='ALL' & SrvcDstrctNm=='ALL' & DsbltyGrpNm=='ALL' & SuppClass=='ALL']
u<-u[RprtDt=='30JUN2025' & StateCd=='ALL' & SrvcDstrctNm=='ALL' & DsbltyGrpNm=='ALL' & SILorSDA=='ALL' & suppclass=='ALL']
stopifnot(nrow(b)==10,nrow(u)==10,!anyDuplicated(b$AgeBnd),!anyDuplicated(u$AgeBnd))
num<-function(v){stopifnot(!any(grepl('<|>|[[:alpha:]]',v)));as.numeric(gsub('[,%]','',v))}
all_count<-num(b[AgeBnd=='ALL']$ActvPrtcpnt)
b<-b[AgeBnd!='ALL'];u<-u[AgeBnd!='ALL'];b<-merge(b,u[,.(AgeBnd,Utlstn)],by='AgeBnd',all=TRUE)
d<-data.frame(age_group=b$AgeBnd,age_min=as.integer(sub(' .*|[+]','',b$AgeBnd)),
 age_max=ifelse(b$AgeBnd=='65+',100L,as.integer(sub('.* to ','',sub('65+','100',b$AgeBnd,fixed=TRUE)))),
 observed_participants=num(b$ActvPrtcpnt),annual_plan_budget=num(b$AvgAnlsdCmtdSuppBdgt),
 utilisation=num(b$Utlstn)/100)
d<-d[order(d$age_min),];d$annual_cost_proxy<-d$annual_plan_budget*d$utilisation
d$relative_cost_weight<-d$annual_cost_proxy/weighted.mean(d$annual_cost_proxy,d$observed_participants)
stopifnot(nrow(d)==9,all(is.finite(as.matrix(d[-1]))),all(d$annual_cost_proxy>0),
 all(d$utilisation>0 & d$utilisation<=1),sum(d$observed_participants)==all_count,
 all(d$age_min[-1]==head(d$age_max,-1)+1),d$age_min[1]==0,tail(d$age_max,1)==100)
d$source_date<-'2025-06-30';d$budget_url<-'https://dataresearch.ndis.gov.au/media/4366/download?attachment='
d$utilisation_url<-'https://dataresearch.ndis.gov.au/media/4367/download?attachment='
d$interpretation<-'Annual plan budget times lagged utilisation: relative cost proxy, not observed annual payments per participant'
write.csv(d,'data/ndis_age_costs_2025.csv',row.names=FALSE)
cat('Prepared nine national NDIA age bands; observed participant counts reconcile to ',all_count,'.\n',sep='')
print(d[c('age_group','annual_plan_budget','utilisation','annual_cost_proxy','relative_cost_weight')],row.names=FALSE)
