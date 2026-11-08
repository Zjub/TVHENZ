library(ggplot2)
main<-read.csv('outputs/central_estimates.csv');all<-read.csv('outputs/window_and_structure_sensitivity.csv')
d<-read.csv('outputs/annual_estimation_data.csv')
d<-d[d$wage_basis=='all_industry_wpi' & d$service_productivity_share==0,]
d$fitted_residual_index<-NA_real_
for(group in unique(d$group)){
 k<-d$group==group;fit<-lm(log_residual~year,d[k,]);v<-exp(predict(fit));d$fitted_residual_index[k]<-v/v[1]
}
cols<-c('Observed net residual'='#16848a','Fitted trend'='#d79420')
chart<-rbind(data.frame(d[,c('group','year')],series='Observed net residual',index=d$residual_index),
 data.frame(d[,c('group','year')],series='Fitted trend',index=d$fitted_residual_index))
p<-ggplot(chart,aes(year,index,colour=series))+geom_line(linewidth=.65)+
 facet_wrap(~group,ncol=2,scales='free_y')+scale_colour_manual(values=cols)+
 labs(x='Financial year ending',y='Index (2016 = 1)',colour=NULL,
 title='Spending after demographic demand, CPI and wage costs',
 subtitle='A net residual includes policy, coverage, intensity, quality, technology and measurement')+
 theme_minimal(base_size=10)+theme(legend.position='bottom',panel.grid.minor=element_blank())
ggsave('outputs/residual_trends.png',p,width=10,height=12,dpi=160)
ggsave('outputs/residual_trends.pdf',p,width=10,height=12)
sens<-all[all$wage_basis=='all_industry_wpi' & all$service_productivity_share==0,]
p2<-ggplot(sens,aes(100*additional_growth,group,colour=window))+geom_point(position=position_dodge(width=.6),size=2)+
 geom_vline(xintercept=0,colour='grey60')+labs(x='Additional net growth (% per year)',y=NULL,colour=NULL,
 title='How much does the estimation period matter?')+theme_minimal(base_size=10)+theme(legend.position='bottom')
ggsave('outputs/window_sensitivity.png',p2,width=10,height=6,dpi=160)
ggsave('outputs/window_sensitivity.pdf',p2,width=10,height=6)
library(htmltools)
table_html<-function(z)tags$table(tags$thead(tags$tr(lapply(names(z),tags$th))),
 tags$tbody(lapply(seq_len(nrow(z)),function(i)tags$tr(lapply(z[i,,drop=FALSE],function(v)tags$td(as.character(v)))))))
fmt<-function(v)sprintf('%.2f',100*v)
summary<-data.frame(Function=main$group,'Net residual (%/year)'=fmt(main$additional_growth),
 'Descriptive 95% lower'=fmt(main$descriptive_ci_lower),'Upper'=fmt(main$descriptive_ci_upper),check.names=FALSE)
windows<-unique(sens$window)
wide<-data.frame(Function=main$group,check.names=FALSE)
for(window in windows){z<-sens[sens$window==window,];wide[[window]]<-fmt(z$additional_growth[match(wide$Function,z$group)])}
ig<-read.csv('outputs/igr_provisional_rules.csv')
igr_table<-data.frame(Function=ig$function_name,'Published rule'=ig$published_rule,'Use in this model'=ig$provisional_use,'IGR page'=ig$page,check.names=FALSE)
ndis<-read.csv('outputs/ndis_earnings_rule_translation.csv')
refs<-data.frame(Source=c('AIHW Health Expenditure Australia 2024-25','ABS GFS 2024-25','ABS GFS Education Table 1',
 'ABS estimated resident population','ABS CPI June 2025','ABS WPI Table 5a','ABS National Accounts Table 34',
 '2026 IGR Appendix A3','IHACPA hospital input-cost shares','ACARA school cost shares','Department of Education university finance'),
 locator=c('Datacube: AUS, Australian Government + State and local government, current amounts. Four named recurrent groups; exclude non-government and capital.',
 'Table 4 Health; 2015-16 to 2024-25 consolidated all-government operating expenses.',
 'Rows 9-12 school, tertiary, other and total; 2015-16 to 2024-25.',
 'Table 59, Persons, single age 0-99 and 100+; mean of opening/closing June observations.',
 '640101, All groups Australia, index numbers; four-quarter FY mean.',
 '634505a, financial-year total hourly rates excluding bonuses, private and public: all industries and sector alternatives.',
 '5206034, GDP/hour index and compensation/hour. Productivity-index power is a sensitivity assumption, not an efficiency estimate.',
 'Printed pp 307-312, especially aged care 308, NDIS 309, transfers Table A3.1 p311, other p312.',
 '2023-24 admitted acute labour cost mix; same scope proxy as fiscal model.',
 '2024 Table 9.3 salary share excluding user cost of capital; same proxy as fiscal model.',
 'Finance 2024 Table 4 employee expenses / total; all-provider scope proxy.'),
 url=c('https://www.aihw.gov.au/reports/health-welfare-expenditure/health-expenditure-australia-2024-25/data',
 'https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/2024-25',
 'https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/2024-25/55120DO095_202425.xlsx',
 'https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population/mar-2026',
 'https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/jun-quarter-2025',
 'https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/wage-price-index-australia',
 'https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-national-income-expenditure-and-product',
 'https://treasury.gov.au/sites/default/files/2026-09/p2026-797917.pdf',
 'https://www.ihacpa.gov.au/resources/national-hospital-cost-data-collection-public-sector-2023-24',
 'https://dataandreporting.blob.core.windows.net/anrdataportal/ANR-Documents/ANR2024/nationalreportonschoolinginaustralia_2024_Chapter9.pdf',
 'https://www.education.gov.au/download/19832/finance-2024-financial-reports-higher-education-providers/43005/2024-university-finance-summary-information/pdf'))
write.csv(refs,'outputs/source_register.csv',row.names=FALSE)
html<-tags$html(tags$head(tags$meta(charset='utf-8'),tags$title('Health and education: estimating spending assumptions'),
 tags$style(HTML('body{font-family:Arial,sans-serif;color:#193b46;max-width:1100px;margin:30px auto;line-height:1.45;font-size:14px}h1,h2{color:#087f84}h2{margin-top:26px}table{border-collapse:collapse;width:100%;font-size:11px;margin:14px 0}th,td{text-align:left;vertical-align:top;padding:7px;border-bottom:1px solid #ccd8db}th{background:#edf5f5}img{width:100%}.note{background:#edf5f5;padding:14px}pre{white-space:pre-wrap}a{color:#087f84}@page{size:A4;margin:15mm}@media print{body{margin:0;font-size:10pt}h2,h3{break-after:avoid}tr{break-inside:avoid}.page{break-before:page}img{max-height:245mm;object-fit:contain}a{overflow-wrap:anywhere}}'))),
 tags$body(tags$h1('Health and education: estimating spending assumptions'),
 tags$p(paste('Independent estimation pack |',Sys.Date(),'| financial years ending 2016-2025')),
 tags$div(class='note',tags$b('Purpose and status. '),'Estimate historical net spending pressure conditional on the fiscal model\'s demographic and input-cost structure. The outputs are candidates for discussion. This workflow does not edit the fiscal model, change Shiny defaults, or deploy anything.'),
 tags$h2('What is estimated'),tags$p('We fit an exponential time trend to spending after removing demographic demand, general inflation and a stated wage-cost basket. The result combines service intensity, quality, coverage, technology, policy, unmodelled wage mix and measurement. It does not identify a separate causal service-productivity effect.'),
 tags$p('Calculator defaults: the runtime historical alternative uses the 25% productivity specification from the sensitivity output, selected by scripts/prepare_cost_defaults.R. That 25% share is arbitrary, not independently estimated. The zero-efficiency tables below remain a diagnostic specification; paired residuals at 25% are slightly higher. Current calculator cost-growth defaults are also arbitrary choices rather than fitted coefficients. This estimation workflow does not automatically install any set as defaults.'),
 tags$p('The central specification has zero service-productivity cost savings and uses all-industry hourly WPI. Alternative specifications use sector WPI or compensation per hour and service-productivity growth equal to 25% or 50% of observed aggregate productivity growth. Across ten groups, four time windows, three wage measures and three productivity assumptions, 360 specifications are exported.'),
 tags$h2('Consistent accounting identity'),tags$pre('Spending_t = demographic demand_t x CPI_t x wage/efficiency basket_t x net residual_t\nlog(net residual_t) = intercept + beta x (year - 2016) + error_t\nEstimated additional annual growth = exp(beta) - 1'),
 tags$p('For an input basket, real unit cost is (1 - labour share) + labour share x real hourly wage index / service productivity index. All raw baskets and demographic subfunction weights refer to 2025; final time-series indices are rebased to 2016. General CPI is a deliberate proxy for the model\'s common domestic price index. Using a health-specific deflator here would remove part of the relative cost pressure we are trying to measure.'),
 tags$p('Aggregate demand and cost series are sums of subfunction amounts, not averages of growth rates. Ageing changes the mix of their contributions. Main estimates use ten annual levels and nine years of elapsed growth, with calendar time preserved when years are omitted.'),
 tags$h2('Results: additional growth beyond the specified drivers'),table_html(summary),
 tags$p('Intervals use a one-lag Newey-West covariance and a small-sample t critical value. With only ten observations they are descriptive fitting diagnostics. They exclude uncertainty about demographics, input shares, policy, measurement and future structural change; they are not forecast uncertainty bands.'),
 tags$h2('Spending and demographic boundaries'),
 tags$p('Health: four named blocks use AIHW current-dollar government funding from Commonwealth and state/local sources, excluding non-government funders. Hospitals combine public and private hospitals. Primary/community care comprises primary health care excluding benefit-paid pharmaceuticals. Referred medical services and benefit-paid pharmaceuticals are separate. Other health is ABS GFS Health less those four blocks; it includes accounting and classification differences and is not a pure service category. AIHW capital expenditure is not separately added to GFS operating expenses.'),
 tags$p('Hospital, medical and pharmaceutical demand use the same fixed IGR C12.10 age-cost profiles as the fiscal model, applied to historical ABS single-age populations. Primary/community and referred medical services share the medical profile. The health reconciliation block uses total population. These are age-cost proxies, not service counts: a historical change in morbidity, relative age costs or coverage remains in the residual.'),
 tags$p('Education: ABS consolidated operating expenses give schools, tertiary and other education. Demand uses ages 5-17 for schools, 18-24 for higher education, 15-64 for VET and total population for other education. A common tertiary residual is supplied to higher education and VET because this GFS history does not split them. Their different demand and cost rules remain. Applying the same residual to them is an explicit allocation assumption. These age proxies omit changes in enrolment, mature-age participation and other composition. Other education includes preschool, making total population a weak demand proxy.'),
 tags$p('Historical populations use the mean of opening and closing June single-age counts. CPI is the four-quarter financial-year average. Hourly WPI holds job quality approximately constant; compensation per hour includes composition and other remuneration. Neither automatically reproduces average weekly earnings. Employment and hours affect the broader model but are not separate multipliers for the labour required per service here.'),
 tags$h2(class='page','Observed residuals and fitted trends'),tags$img(src='residual_trends.png'),
 tags$h2(class='page','Sensitivity and interpretation'),table_html(wide),tags$img(src='window_sensitivity.png'),
 tags$p('Excluding 2020-2022 removes those observations, not lasting pandemic effects or policy shifts. The pre-COVID estimate has only four observations. Medicines are particularly sensitive to the estimation period; the health reconciliation and other education blocks are heterogeneous. School funding changes and health staffing, coverage and treatment changes are inside the estimates. This exercise does not econometrically remove those policy effects.'),
 tags$p('The aggregate health/education trends are cross-checks, not additional parameters to stack on top of subfunction trends. Applying all subfunction trend estimates will not exactly reproduce an independently fitted aggregate trend because composition and fitted residuals differ. The sensitivity CSV should be reviewed alongside these estimates before selecting long-run defaults.'),
 tags$h2('How to use the candidates'),tags$p('candidate_service_cost_parameters.csv has the fiscal model\'s parameter fields plus estimation group, evidence class, adoption status and limitations. For review, compare each candidate with current settings, decide how much of historical policy/coverage growth should persist, and select a long-run rate. Service-productivity shares remain zero in the candidate file as an identification convention, not an empirical estimate. Increasing efficiency savings while retaining the same historical combined trend requires recalibrating the residual in this separate exercise; a prospective efficiency scenario should instead hold the selected residual fixed.'),
 tags$p('A 2% residual compounded for 36 years raises spending roughly 104% above the other drivers. This illustrates why an observed historical trend should not automatically become a central long-term assumption. In particular, the reconciliation-block estimates should not be presented as technical cost estimates.'),
 tags$h2(class='page','Provisional IGR rules for the remaining blocks'),table_html(igr_table),
 tags$p('The IGR supplies useful indexation rules, but generally not a single growth coefficient for each consolidated function. Commonwealth funding and consolidated service spending have different boundaries. Scheme-wide NDIS participant support costs are the appropriate comparator; Commonwealth contributions alone omit state funding.'),
 tags$p(paste0('NDIS translation: at 1.2% real earnings growth, zero service-productivity savings and an 85% labour input share, a constant extra rate of ',fmt(ndis$implied_extra_growth),'% annually would match the 36-year earnings-index endpoint. This is our derived calibration, not an IGR coefficient. It ignores the IGR\'s post-medium-term five-year transition and age/disability cost mix. The current 0.5% additional rate implies a different, faster cost path. A direct earnings-index rule is clearer than this endpoint approximation.')),
 tags$p('Aged care: the reviewed IGR appendix describes a fitted residential-care trend applied from 2037-38 but does not supply its numerical coefficient. Its total aged-care path cannot identify that coefficient independently of utilisation, age/sex costs, reforms and the home/residential mix. An endpoint residual derived using our demographics would be an e61 calibration to the IGR path. The current 0.3% remains an explicit judgement until separately estimated or calibrated.'),
 tags$p('Transfers: split the existing residual block before assigning CPI versus earnings indexation. A 65% earnings exposure is currently an e61 proxy, not a published IGR aggregate weight. Recipient shares and age/unemployment profiles matter alongside indexation. Age Pension should retain its explicit means-testing and wealth mechanism.'),
 tags$p('Other spending: the IGR residual follows nominal GDP. Our constant-GDP-share control can implement this as an illustrative scenario, but adopting it for consolidated other public services is a scope assumption. Do not add a separate wage/intensity premium on top of a fixed GDP-share total.'),
 tags$h2('Sources and reproducibility'),
 tags$pre('Rscript run_estimation.R --acquire   # first run: cache sources and estimate\nRscript run_estimation.R             # offline rebuild from the snapshot\nRscript validate_estimation.R        # accounting, extraction and synthetic checks'),
 tags$p('Dependencies: readxl, curl, ggplot2, htmltools. Snapshot bytes and MD5 checksums are in data/source_manifest.csv. Cached downloads and copied source inputs are not silently refreshed. PDF rendering is optional and uses an installed Chrome browser when BUILD_PDF=true. The estimation code does not source or run Shiny.'),
 tags$ul(lapply(seq_len(nrow(refs)),function(i)tags$li(tags$a(href=refs$url[i],refs$Source[i]),paste0(': ',refs$locator[i])))),
 tags$p('Outputs also include the exact observed spending series, government-funder source rows, age populations, annual driver indices and exact annual log-change accounting. Parameters inherit the input-share source limitations recorded in the copied service_parameters.csv. These limitations remain even if the fitted trend is precisely estimated.')))
save_html(html,'outputs/estimation_report.html')
if(tolower(Sys.getenv('BUILD_PDF'))=='true'){
 chrome<-'C:/Program Files/Google/Chrome/Application/chrome.exe'
 profile<-normalizePath(file.path(tempdir(),'fiscal_estimation_pdf'),mustWork=FALSE)
 dir.create(profile,recursive=TRUE,showWarnings=FALSE)
 target<-normalizePath('outputs/estimation_report.pdf',mustWork=FALSE)
 url<-paste0('file:///',gsub(' ','%20',normalizePath('outputs/estimation_report.html',winslash='/',mustWork=TRUE),fixed=TRUE))
 system2(chrome,c('--headless','--disable-gpu','--no-pdf-header-footer',
  shQuote(paste0('--user-data-dir=',profile)),shQuote(paste0('--print-to-pdf=',target)),shQuote(url)),wait=TRUE)
}
cat('Report and diagnostics written to outputs/.\n')
