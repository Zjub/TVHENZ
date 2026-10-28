# Validate the service-cost scenario model.
args<-commandArgs(FALSE)
script<-sub('^--file=','',args[grepl('^--file=',args)][1])
source(file.path(dirname(script),'validate_service_costs.R'))
