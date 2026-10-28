# Standalone discussion paper and current-model audit tables. Offline.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');library(htmltools)
x<-load_inputs();cal<-calibrate(x);z<-project_model(x,default_assumptions(),cal)
p<-service_cost_comparison(z,x);s<-cal$subfunctions
out<-'outputs/spending_background';dir.create(out,recursive=TRUE,showWarnings=FALSE)
dir.create('documentation',showWarnings=FALSE)
write.csv(s,file.path(out,'calibration.csv'),row.names=FALSE)
write.csv(z$subfunctions,file.path(out,'annual_subfunctions.csv'),row.names=FALSE)
write.csv(p,file.path(out,'service_cost_assumptions_and_benchmarks.csv'),row.names=FALSE)
for(unit in c('pct','bn','real_bn'))write.csv(spending_contributions(z,2030,2066,unit),file.path(out,paste0('contributions_',unit,'.csv')),row.names=FALSE)
tab<-function(d){d<-as.data.frame(d,check.names=FALSE);for(n in names(d))if(is.numeric(d[[n]]))d[[n]]<-ifelse(is.na(d[[n]]),'Not available',formatC(d[[n]],digits=3,format='f'))
 tags$table(tags$thead(tags$tr(lapply(names(d),tags$th))),tags$tbody(lapply(seq_len(nrow(d)),function(i)tags$tr(lapply(d[i,],function(v)tags$td(as.character(v)))))))}
link<-function(u,label){if(is.na(u)||!startsWith(u,'https://'))return(tags$span('Modelling judgment / allocation identity'));tags$a(href=strsplit(u,'[;[:space:]]')[[1]][1],label)}
sector<-function(n){ss<-s[s$programme==n,];pp<-p[p$programme==n,]
 d<-spending_contributions(z,2030,2066,'pct');d<-aggregate(contribution~factor,d[d$programme==n,],sum)
 tags$section(tags$h2(n),
  tags$h3('Allocation and spending'),tab(data.frame(Subfunction=ss$subfunction,'2025 source ($bn)'=ss$source_2025_bn,'2030 anchor ($bn)'=ss$anchor_2030_bn,check.names=FALSE)),
  tags$h3('Cost and productivity assumptions'),tab(data.frame(Subfunction=pp$subfunction,'Labour weight (%)'=100*pp$wage_weight,'Productivity response (%)'=100*pp$selected_service_productivity_share,'Additional growth (%/yr)'=100*pp$selected_additional_service_growth,'Combined real growth (%/yr)'=100*pp$combined_non_demographic_growth,'Comparator (%/yr)'=100*pp$reference_benchmark_growth,check.names=FALSE)),
  tags$h3('Contribution to the 2030-2066 GDP-share change'),tab(d),
  tags$h3('Sources, scope and discussion'),lapply(seq_len(nrow(pp)),function(i){v<-pp[i,];b<-ss[ss$id==v$id,]
   tags$div(tags$h4(v$subfunction),tags$p('Demand: ',b$profile,'. ',b$limits),tags$p('Allocation: ',b$locator,'. ',link(b$source,'Allocation source')),
   tags$p(v$evidence_class,': ',v$locator,'. ',link(v$source_url,'Unit-cost source')),
   tags$p('Comparator: ',v$benchmark_description,'. ',v$benchmark_scope),tags$p('Limits: ',v$limitation))}))}
body<-tagList(tags$h1('Constructing Australian government spending'),
 tags$p('e61 fiscal model | Discussion paper | 28 October 2026'),
 tags$p('This paper documents a consolidated-government scenario framework. Sources support the identified inputs, not every parameter choice. Sourced amounts, calibrated allocations, scope proxies, modelling judgments and accounting identities are distinguished.'),
 tags$h2('Starting data and calibration'),tags$p('PBO national primary expenses provide the common FY2029-30 anchor of $1,171.9bn. ABS GFS functional controls, AIHW allocations, NDIA plan expenses and other programme sources allocate it across eight functions and 24 subfunctions. The 2025-2030 bridge is an allocation calculation; post-2030 scenario controls do not recalibrate it. Internal transfers are not added as extra national spending.'),
 tags$h2('A consistent identity'),tags$p('Spending = anchor x population x age/cohort demand x coverage x general prices x real unit cost x additional service growth. Defence and pension payments have explicit policy rules.'),
 tags$p('Real unit cost = (1 - L) + L x W / Q. L is the fixed labour share, W the real wage index and Q the service-productivity index. Q = (1 + theta x economy-wide productivity growth)^years. Real wages follow economy-wide productivity. Efficiency reduces labour requirements, while non-labour inputs follow general inflation.'),
 tags$p('A wage-productivity gap generates Baumol cost pressure inside the input basket. Additional service growth, (1 + r)^years, represents intensity, quality and mix beyond that basket. It is not an additional Baumol premium. Funding-index, CPI and cash-indexation proxies use their stated rules rather than resource-efficiency deductions.'),
 tags$p('Health and education efficiency defaults to 25% of economy-wide productivity (0.3% annually at the macro default). Additional cost growth defaults to 0.5% annually above baseline CPI and wage costs in each function. Both are arbitrary discussion assumptions, not empirical estimates. The controls display actual additional-growth rates and preserve subfunction differences by applying a common shift. NDIS and cash transfers receive no service-efficiency deduction. Aged care retains 0.3% additional growth and other public services zero, with no new efficiency default.'),
 tags$h2('Cost assumption sets'),tags$p('Current assumptions remain the reference. The historical alternative replaces health/education additional-growth baselines with 2016-2025 net residual estimates from the independent estimation pack. Aggregate adjustments add percentage points to each subfunction rate. Estimates bundle policy, coverage, quality and mix, and do not identify separate efficiency effects. Higher education and VET share a tertiary estimate. The reconciliation block is a heterogeneous accounting residual. The Spending tab compares both sets with selected economic and policy settings held fixed.'),
 tags$p('NDIS uses the existing participant/cohort projection, original plan-cost index and 0.5% additional cost/use growth, plus its policy adjustment. Other transfers use the original demand and payment-indexation proxy. Age Pension retains means tests and wealth accumulation. Transfers have no service-efficiency deductions. These controls are presented separately from health and education efficiency and cost growth.'),
 tags$h2('IGR and literature discipline'),tags$p('Commonwealth IGR Appendix A3 describes programme-specific real age/sex-adjusted health trends; numeric coefficients are not supplied in the reviewed appendix. NDIS costs by age/disability transition to earnings growth. Commonwealth schools use 75% wages/25% CPI and university grants use CPI. VET uses a wage-cost index; the numerical 69.75% comparator here is the National Skills Agreement proxy.'),
 tags$p('Comparators and reference implied growth are fixed at the reference macro assumptions. They are not targets for differently defined consolidated expenditure. State resource costs and Commonwealth funding rules need not match. A blank coefficient is reported as unavailable rather than invented.'),
 tags$p(link('https://treasury.gov.au/sites/default/files/2026-09/p2026-797917.pdf','Commonwealth IGR, printed pages 307-312'),' | ',link('https://www.nsw.gov.au/sites/default/files/noindex/2026-08/2026-27-nsw-igr-technical-note.pdf','NSW IGR technical note, printed pages 12-15'),' | ',link('https://www.oecd.org/en/publications/fiscal-sustainability-of-health-systems_880f3195-en/full-report/component-6.html','OECD projection framework')),
 tags$h2('What the decomposition measures'),tags$p('Exact Shapley accounting allocates interactions across population, age/cohort demand, coverage, general prices, real wages/funding indexation, service productivity, additional service growth, policy/financing and the GDP denominator. Contributions reconcile before rounding. They describe arithmetic, not estimated causal effects. Price growth is removed for 2030-dollar results; the denominator is included only for GDP shares.'),
 sector('NDIS'),sector('Health'),sector('Education'),
 tags$h2('Other functions and fiscal financing'),tags$p('Aged care uses an IGR demand profile with four allocations. Other public services use population demand. Other social protection is a residual cash/indexation proxy, not a complete benefit microsimulation. Pension eligibility and payments follow synthetic means tests and wealth accumulation; defence follows a stated GDP-share target. Gross debt finances deficits and asset acquisitions. Interest is charged on opening gross debt, with separate asset receipts.'),
 tags$h2('Fixed-share policies and borrowing-rate sensitivity'),tags$p('Each function can optionally be fixed at a selected GDP share from 2031, preserving the common 2030 anchor. The target replaces the function total and is allocated across its subfunctions proportionately to unconstrained spending. Annual exports show unconstrained costs, actual costs and the policy factor. The override feeds into the fiscal balance and debt; a zero target is supported.'),
 tags$p('The borrowing-rate sensitivity defaults to zero. A setting of 3 basis points per percentage point of net debt/GDP matches the historical Treasury research benchmark in Yan and Brittle (2010). The rate adjustment is sensitivity/10000 times the previous-year net-debt/GDP percentage-point gap from 2030. It can be negative when debt falls, with a zero borrowing-rate floor. Interest is charged on opening gross debt; asset yields do not change. Applying a long-run real ten-year yield-margin estimate immediately to the effective rate is a scenario assumption, without refinancing lags.'),
 tags$p(link('https://treasury.gov.au/sites/default/files/2019-03/Tsy_Working_Paper_2010_04.pdf','Treasury Working Paper 2010-04, printed pages 13 and 17')),
 tags$h2('Discussion questions'),tags$p('What quality-adjusted service-productivity response is credible? How much additional growth remains after explicit costs? Do enrolment, morbidity and participant-mix changes warrant separate demand assumptions? Which funding rules require their own blocks? What data and uncertainty range support each proposal?'),
 tags$p('Annual factors, allocation bridges, evidence and exact contributions can be downloaded from the local app. The source/assumption register provides source classifications and locators. This paper and its CSV audit tables are reproduced by scripts/build_spending_background.R.'))
css<-'@page{size:A4;margin:17mm}body{font-family:Arial,sans-serif;font-size:10pt;line-height:1.45;color:#183343}h1,h2{color:#007e80}h2,h3,h4{break-after:avoid}h2{margin-top:22pt}table{border-collapse:collapse;width:100%;font-size:8pt}td,th{padding:5pt;border:1px solid #d5dfdc;text-align:left}thead{display:table-header-group}tr{break-inside:avoid}th{background:#eaf2ef}a{color:#007e80}'
html<-paste0('<!doctype html><html><head><meta charset="utf-8"><style>',css,'</style></head><body>',as.character(body),'</body></html>')
writeLines(enc2utf8(html),'documentation/spending_background.html',useBytes=TRUE)
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b<-chromote::ChromoteSession$new();on.exit(b$close())
f<-b$Page$getFrameTree()$frameTree$frame$id;b$Page$setDocumentContent(frameId=f,html=html)
b$Runtime$evaluate('document.fonts.ready.then(()=>true)',awaitPromise=TRUE)
pdf<-b$Page$printToPDF(printBackground=TRUE,preferCSSPageSize=TRUE)
writeBin(jsonlite::base64_dec(pdf$data),'documentation/spending_background.pdf');b$close()
text<-paste(pdftools::pdf_text('documentation/spending_background.pdf'),collapse='\n')
stopifnot(grepl('Baumol',text),grepl('Service productivity',text),grepl('Comparator',text))
cat('Current spending background PDF and audit tables generated.\n')
