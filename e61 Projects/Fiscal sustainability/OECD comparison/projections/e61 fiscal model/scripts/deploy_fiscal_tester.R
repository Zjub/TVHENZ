# Check or deploy Fiscal_tester using the calculators' rsconnect workflow.
# From this project: Rscript scripts/deploy_fiscal_tester.R [--deploy]
# Optional: --account=e61-institute --server=shinyapps.io
#           --app-name=Fiscal_tester --app-title="e61 Fiscal Tester"
# Credentials are read from the existing local rsconnect account configuration.

if (!requireNamespace('rsconnect',quietly=TRUE))
  stop('Install rsconnect before preparing a deployment.',call.=FALSE)
full_args<-commandArgs(FALSE)
file_arg<-grep('^--file=',full_args,value=TRUE)
script_dir<-if(length(file_arg))dirname(normalizePath(sub('^--file=','',file_arg[1]),mustWork=TRUE)) else {
  # Also support source("scripts/deploy_fiscal_tester.R") in RStudio.
  source_files<-vapply(sys.frames(),function(f)if(is.null(f$ofile))'' else f$ofile,character(1))
  source_files<-source_files[nzchar(source_files)]
  if(length(source_files))dirname(normalizePath(tail(source_files,1),mustWork=TRUE)) else file.path(getwd(),'scripts')
}
project_root<-normalizePath(file.path(script_dir,'..'),winslash='/',mustWork=TRUE)
arguments<-commandArgs(TRUE)
argument_value<-function(name,default) {
  prefix<-paste0('--',name,'=');value<-arguments[startsWith(arguments,prefix)]
  if(length(value))substring(value[1],nchar(prefix)+1L) else default
}
deploy<-'--deploy'%in%arguments
account<-argument_value('account','e61-institute')
server<-argument_value('server','shinyapps.io')
app_name<-argument_value('app-name','Fiscal_tester')
app_title<-argument_value('app-title','e61 Fiscal Tester')

# Explicit runtime inputs: do not upload archive, reports, scripts, raw source
# workbooks/PDFs, local logs or cached browser assets. The source URL manifest
# is required by the assumptions download.
processed<-c('official_pbo_nfo_wide.csv','population_age_sex.csv',
  'observed_labour_rates.csv','observed_hours_by_age.csv','participation_targets.csv',
  'health_age_cost_profile.csv','aged_care_age_cost_profile.csv','education_age_cost_proxy.csv',
  'igr_pension_status_benchmark.csv','tax_head_history.csv','tax_elasticity_estimates.csv',
  'ato_income_distribution.csv','ndis_igr_benchmark.csv','igr_spending_benchmarks.csv','consolidation_bridge.csv','subfunction_age_profiles.csv',
  'imf_long_term_debt_projections.csv','oecd_debt_projections.csv')
app_files<-c('app.R','R/model.R','R/model_v2.R','R/spending_bottom_up.R','R/igr_benchmarks.R','R/international_debt.R',file.path('data','processed',processed),
  'data/spending_subfunctions.csv','data/health_education_parameters.csv','documentation/spending_background.pdf',
  'data/policy_parameters.csv','data/ndis_incidence_2025.csv','data/assumption_register.csv',
  'data/historical_gfs_expenses_by_purpose.csv','data/historical_gfs_operating_statement.csv',
  'data/raw/v2/download_manifest.csv')
app_files<-gsub('\\','/',app_files,fixed=TRUE)
missing_files<-app_files[!file.exists(file.path(project_root,app_files))]
if(length(missing_files))stop('Missing deployment files: ',paste(missing_files,collapse=', '),call.=FALSE)
deployment_files<-rsconnect::listDeploymentFiles(appDir=project_root,appFiles=app_files)
dependencies<-rsconnect::appDependencies(appDir=project_root,appFiles=app_files,appMode='shiny')

# Check the exact bundle, not just the working tree. Linux hosting also needs
# valid UTF-8 source. Use an isolated staged copy to find missing runtime data.
stage<-tempfile('fiscal-deployment-');dir.create(stage)
for(f in deployment_files) {
  dest<-file.path(stage,f);dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
  if(!file.copy(file.path(project_root,f),dest))stop('Could not stage ',f)
}
for(f in c('app.R','R/model.R','R/model_v2.R')) {
  contents<-readLines(file.path(stage,f),encoding='UTF-8',warn=FALSE)
  if(anyNA(iconv(contents,from='UTF-8',to='UTF-8')))stop('Source must be UTF-8: ',f)
}
previous_wd<-getwd();setwd(stage)
app_env<-new.env(parent=globalenv())
app<-tryCatch(source('app.R',local=app_env)$value,finally=setwd(previous_wd))
stopifnot(inherits(app,'shiny.appobj'))
defaults<-get('default_assumptions',envir=app_env,inherits=TRUE)()
reference<-get('project_model',envir=app_env,inherits=TRUE)(app_env$inputs,defaults,app_env$calibration)
stopifnot(all(is.finite(reference$path$net_debt_pct)),
  defaults$tax_policy=='wage_indexed',defaults$super_real_return==.04)

dir.create(file.path(project_root,'outputs'),showWarnings=FALSE)
writeLines(deployment_files,file.path(project_root,'outputs/deployment_files.txt'))
write.csv(dependencies,file.path(project_root,'outputs/deployment_dependencies.csv'),row.names=FALSE)
cat('Fiscal_tester deployment checks passed.\n',
  '  Account: ',account,' @ ',server,'\n',
  '  App: ',app_name,'\n',
  '  Runtime files: ',length(deployment_files),'\n',
  '  Source bundle: ',round(sum(file.info(file.path(project_root,deployment_files))$size)/1024^2,2),' MB\n',
  '  Staged default 2066 net debt: ',round(tail(reference$path$net_debt_pct,1),2),'% GDP\n',sep='')
print(dependencies[,intersect(c('Package','Version','Source','Repository'),names(dependencies)),drop=FALSE],row.names=FALSE)

if (!deploy) {
  cat('\nNo upload requested. Add --deploy to publish this bundle.\n')
} else {
  configured<-rsconnect::accounts()
  if(!any(configured$name==account & configured$server==server))
    stop('No configured rsconnect account ',account,' for ',server,'. Configure it locally; do not put tokens in this script.',call.=FALSE)
  rsconnect::deployApp(appDir=project_root,appFiles=app_files,appPrimaryDoc='app.R',
    appName=app_name,appTitle=app_title,appMode='shiny',account=account,server=server,
    recordDir=project_root,launch.browser=FALSE,logLevel='normal')
}
