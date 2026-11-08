# Explicitly snapshot historical residuals paired with an arbitrary 25% efficiency share.
# Run after estimation/run_estimation.R; this does not re-estimate or deploy.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
d<-read.csv('estimation/outputs/window_and_structure_sensitivity.csv')
h<-read.csv('estimation/outputs/candidate_service_cost_parameters.csv')
z<-subset(d,window=='full_2016_2025' & wage_basis=='all_industry_wpi' & service_productivity_share==.25)
stopifnot(nrow(z)==10,!anyDuplicated(z$group))
h$additional_service_growth<-z$additional_growth[match(h$estimation_group,z$group)]
stopifnot(all(is.finite(h$additional_service_growth)))
h$calibration_service_productivity_share<-.25
h$estimate_limitation<-paste(h$estimate_limitation,'Conditional on arbitrary 25% service-productivity share; not independently identified.')
write.csv(h[,c('id','additional_service_growth','estimation_group','evidence_class','estimate_limitation','calibration_service_productivity_share')],
 'data/historical_service_cost_parameters.csv',row.names=FALSE)
cat('Historical runtime snapshot uses residuals conditional on arbitrary 25% service productivity.\n')
