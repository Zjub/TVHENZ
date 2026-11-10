# Standalone short PDF describing the active health model. No model edits or deployment.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');library(htmltools)
x<-load_inputs();cal<-calibrate(x);a<-default_assumptions();p<-project_model(x,a,cal)
h<-subset(cal$programmes,programme=='Health');s<-subset(cal$subfunctions,programme=='Health')
e<-subset(p$components,programme=='Health' & year==2066)
f<-function(v,d=2)formatC(v,digits=d,format='f',big.mark=',')
link<-function(url,label)tags$a(href=url,label)
refs<-c(
 abs='https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/2024-25',
 aihw='https://www.aihw.gov.au/reports/health-welfare-expenditure/health-expenditure-australia-2024-25/contents/spending-trends-by-source/australian-government-spending',
 pbo='https://www.pbo.gov.au/publications-and-data/publications/2026-27-National-Fiscal-Outlook',
 igr='https://treasury.gov.au/publication/2026-intergenerational-report',
 pop='https://population.gov.au/data-and-forecasts/projections',
 ihacpa='https://www.ihacpa.gov.au/resources/national-hospital-cost-data-collection-public-sector-2023-24')
tab<-function(headers,rows)tags$table(tags$thead(tags$tr(lapply(headers,tags$th))),
 tags$tbody(lapply(rows,function(r)tags$tr(lapply(r,tags$td)))))
allocation<-lapply(seq_len(nrow(s)),function(i)c(s$subfunction[i],f(s$source_2025_bn[i]),f(s$anchor_2030_bn[i])))
allocation<-c(allocation,list(c('Total health',f(sum(s$source_2025_bn)),f(sum(s$anchor_2030_bn)))))
dem<-programme_exposures(x,c(2025,2030))$Health;dem<-dem[2]/dem[1]
raw<-h$anchor_2030_bn/h$calibration_factor
body<-tagList(
 tags$section(class='page',
  tags$div(class='brand','e61'),tags$h1('How health spending is constructed'),
  tags$p(class='subtitle','Short methods note | 3 November 2026 | Current default model'),
  tags$h2('1. Establishing the 2030 starting point'),
  tags$p('Years refer to financial years ending in June; 2030 means 2029-30. Amounts are nominal Australian dollars. The model covers consolidated government health expenses, including Commonwealth and state/territory funding. Private household and insurer spending is excluded. This is an expense measure, not a projection of health capital investment.'),
  tags$p(tags$b('The observed total: '),'ABS Government Finance Statistics (GFS), 2024-25, Table 4, provides the all-Australia general-government Health function (COFOG-A): $',f(h$base_2025_bn),' billion. Consolidation avoids counting intergovernmental transfers as additional national spending. [1]'),
  tags$p(tags$b('The service split: '),'AIHW government-funding figures allocate hospitals, primary/community care, referred medical services and PBS medicines. Commonwealth funding of public and private hospitals is combined with state hospital funding. PBS is removed from primary care and shown separately. The remaining ABS total becomes the other-health/reconciliation block. These are AIHW-style service categories, not IGR Commonwealth programme categories. [2]'),
  tab(c('Health category','2025 ($bn)','2030 ($bn)'),allocation),
  tags$p(class='small','2025 construction: hospitals 35.4 + 8.3 + 59.5 = 103.2; primary/community 39.7 + 12.9 - 15.8 = 36.8; referred medical 18.1; PBS 15.8. Other health is the ABS total less those four blocks. Rounded AIHW figures and scope differences are absorbed by this residual.'),
  tags$p(tags$b('Carry the total to 2030: '),'the model multiplies 2025 health spending by the change in age-weighted demand (',f(dem,4),' times), prices of 2.5% a year, wage growth linked to 1.2% productivity with 70% pass-through, a 0.3% annual relative-cost allowance and 0.5% annual additional cost growth. These five-year bridge rates are model assumptions, not an official health forecast.'),
  tags$div(class='equation','Raw 2030 health = 2025 health x demand ratio x 1.025^5 x 1.012^(5 x 0.70) x 1.003^5 x 1.005^5'),
  tags$p('This gives $',f(raw),' billion before reconciliation. All spending functions are then multiplied by one common factor (',f(h$calibration_factor,4),') so their sum equals PBO national primary expenses: $1,249.1bn total expenses less $77.2bn interest = $1,171.9bn. Health becomes $',f(h$anchor_2030_bn),'bn, or ',f(100*h$anchor_2030_bn/cal$gdp0),'% of GDP. This is our allocation of the PBO total, not a PBO health forecast. [3]'),
  tags$p('Within health, each service block uses its own age-demand profile and the same bridge cost assumptions. Their resulting weights are rescaled to the health total above. Thus the aggregate anchor uses a combined IGR health age profile; the service allocation uses the distinct hospital, medical and pharmaceutical profiles. Post-2030 sliders do not recalculate the starting allocation.')
 ),
 tags$section(class='page',
  tags$h2('2. Projecting health spending from 2030'),
  tags$p('The model projects each of the five blocks annually to 2066 and adds them. Every factor equals one in 2030. In the default framework:'),
  tags$div(class='equation','Health spending = 2030 allocation x population x ageing x coverage x health prices x income-related demand x real input costs x remaining pressures'),
  tab(c('Driver','Construction and current default'),list(
   c('Population and ageing','Official annual 2026 IGR population by single-year age and sex, 2026-2066. Fixed IGR age-cost weights generate hospital, medical and pharmaceutical demand; other health uses total population. The population slider scales every age/sex group equally, preserving each year\'s projected age distribution. [4, 5]'),
   c('Age-cost profile mapping','IGR chart C12.10 public-hospital weights apply to all hospital funding; medical-benefits weights apply to primary/community and referred medical care; pharmaceutical weights apply to PBS. These broader-category mappings are proxies. Coverage is held constant.'),
   c('General health prices','Domestic inflation is the CPI proxy: 2.5% a year by default. An optional health-specific price wedge adds to that annual inflation rate; default zero. The model does not use an observed health price index as its future baseline.'),
   c('Income-related demand','Real GDP per person, relative to 2030, raised to an elasticity of 0.8. This allows richer populations to demand more services, quality or treatment. The coefficient is an international scenario assumption, not an Australian causal estimate. [7]'),
   c('Wages and efficiency','Real hourly wages follow economy-wide productivity. For resource-cost blocks, costs are (1 - L) + L x real health wage / service productivity. Hospital L is 67.25%; other resource blocks use an assumed 70%. Sector efficiency grows at 25% of economy-wide productivity: 0.3% annually at the 1.2% default. A separate health wage premium defaults to zero. [6]'),
   c('Medicines','The PBS unit-cost basket follows the health CPI proxy, with no labour-cost or service-efficiency adjustment. Ageing, population, income-related demand and remaining pressures still apply.'),
   c('Remaining pressures','An additional compounded rate represents intensity, quality, technology, service mix or policy beyond the explicit factors. Default zero for all health blocks; users can change it or select historical residual calibrations. It is not a second automatic Baumol premium.')
  )),
  tags$p('The wage-efficiency gap generates cost-disease pressure inside the input-cost basket. Health and education efficiency settings do not imply productivity savings on transfers. The 25% health efficiency response and the 70% labour weights are arbitrary discussion assumptions; the measured hospital cost share is itself a scope proxy for consolidated hospital funding.'),
  tags$p('Spending as a share of GDP is the sum of nominal health spending divided by the model\'s nominal GDP. GDP reflects productivity, employment/IGR participation, hours, prices, population and terms of trade. The alternative fixed-NGDP-per-person mode derives domestic inflation instead. Real income demand uses real output per person, excluding the separate terms-of-trade income factor.'),
  tags$p('Under current defaults, health reaches $',f(e$spending_bn,1),'bn (',f(e$share_gdp),'% of GDP) in 2066. This is a conditional scenario. A fixed health/GDP policy overrides projected spending from 2031; the decomposition records the resulting policy adjustment.')
 ),
 tags$section(class='page',
  tags$h2('3. Labour and non-labour input costs'),
  tags$p('The model uses a labour/non-labour cost basket. The non-labour remainder is not a measured capital share: it combines purchased goods, intermediate inputs, contracted services and any capital-related costs represented within the expense boundary. A separate labour-capital production function is not estimated.'),
  tab(c('Health category','Labour weight','Non-labour weight','Basis'),list(
   c('Hospital services','67.25%','32.75%','IHACPA salary/wage items plus oncosts / total line-item costs; public admitted-acute scope proxy. [6]'),
   c('Primary/community care','70%','30%','Arbitrary modelling assumption; not a measured factor share or funding formula.'),
   c('Referred medical services','70%','30%','Arbitrary modelling assumption; not a measured factor share or funding formula.'),
   c('Other health/reconciliation','70%','30%','Arbitrary modelling assumption for a heterogeneous residual.'),
   c('PBS medicines','0%','100%','CPI-only unit-cost proxy; not a claim that medicine production uses no labour.')
  )),
  tags$p('These are fixed starting cost weights. They do not hold the realised labour-related share of future spending constant: the labour component grows at its own wage/efficiency-adjusted rate. There is no substitution response between labour and other inputs.'),
  tags$div(class='equation','Nominal unit-cost index = (1 - L) x health price index + L x nominal hourly wage index x health wage premium / service-productivity index'),
  tags$p('L is the labour weight above. All indices start at one in 2030. The cost index applies after population, age demand and income-related demand; remaining pressures multiply the resulting spending. General prices are therefore counted once, not added again to the nominal wage term.'),
  tags$p(tags$b('Labour costs: '),'nominal hourly wages grow with domestic inflation and economy-wide productivity. Health service efficiency reduces the labour input needed per unit of service, and an optional health wage premium raises relative wages. With defaults, wages grow by (1.025 x 1.012 - 1) = 3.73% annually before efficiency. Efficiency grows by 0.3% annually, so the labour-cost component grows by (1.025 x 1.012 / 1.003 - 1) = 3.42% annually.'),
  tags$p(tags$b('Non-labour and capital-related costs: '),'the entire non-labour component follows the health price index: 2.5% annually by default. This means zero real price growth relative to that index. There is no separate excess capital-price growth, capital-productivity deduction, capital-stock accumulation, depreciation schedule or user-cost/rental-price model. Any capital-related expenses contained in the health allocation inherit this same proxy. New health capital investment is outside this expense projection.'),
  tags$p('At the default input prices and efficiency rate, the first-year hospital unit-cost increase is approximately 3.12%; the 70%-labour baskets increase by approximately 3.14%; the PBS basket by 2.50%. These figures exclude population, ageing, income-related demand, coverage and remaining pressures. Later basket growth can change as labour costs take a larger weight in the total.'),
  tags$p('The health CPI wedge changes the non-labour price path; there is no independent capital-price control. Nominal labour costs retain the economy-wide wage path plus any selected health wage premium. The remaining-pressure rate applies to the full spending block and cannot be interpreted as an estimated capital-cost shock.'),
  tags$p(class='small','The 2025-2030 bridge is distinct: its 70% wage pass-through is an assumed elasticity in 1.012^(5 x 0.70), rather than this additive input basket. Its separate 0.3% and 0.5% allowances are not assigned to capital. The detailed basket above governs the post-2030 default projection.'),
 ),
 tags$section(class='page',
  tags$h2('4. Evidence, assumptions and limits'),
  tags$p(tags$b('Taken from public sources: '),'the ABS functional total, AIHW allocation amounts, PBO aggregate fiscal anchor, annual IGR population projections, IGR age-cost shapes and the IHACPA hospital input-cost share.'),
  tags$p(tags$b('Added by the model: '),'the 2025-2030 carry-forward and reconciliations; the mapping of IGR age profiles onto AIHW categories; the income elasticity and efficiency response; assumed labour shares outside hospitals; and the remaining-pressure and price/wage settings. The default 0.8 income elasticity is rounded from the OECD-style international approach discussed in the model methods; this model is not an exact OECD or OBR replication.'),
  tags$p(tags$b('Consequences of the structure: '),'more people increase demand; an older population changes age-weighted demand; higher real income raises demand when the elasticity is positive; real wages growing faster than health efficiency raise resource costs. Whether spending/GDP rises depends on all these effects and the GDP denominator.'),
  tags$p('Age-cost profiles remain fixed: the model does not separately forecast healthy ageing, proximity to death, disease prevalence, disability severity, individual utilisation, treatment innovation or future funding reforms. Historical residual options combine unobserved policy, coverage, quality and measurement effects, rather than identifying a pure cost parameter. The other-health residual is heterogeneous. The consolidated boundary is broader than the IGR Commonwealth health boundary, so an IGR programme forecast is a comparator rather than a direct control.'),
  tags$h3('Primary sources and their role'),
  tags$ol(
   tags$li(link(refs['abs'],'ABS, Government Finance Statistics, Annual 2024-25'),'. Table 4: consolidated Health expenses; source of the 2025 functional total.'),
   tags$li(link(refs['aihw'],'AIHW, Health expenditure Australia 2024-25'),'. Australian-government spending Figure 11 and state/territory spending Figure 16: rounded government-funder amounts used in the service allocation. The private hospital category here means government funding of private hospitals.'),
   tags$li(link(refs['pbo'],'PBO, 2026-27 National Fiscal Outlook'),'. Data Tables 1-2: national 2030 expenses, interest and the internally consistent GDP denominator inferred from gross debt dollars and its published GDP ratio.'),
   tags$li(link(refs['igr'],'Treasury, 2026 Intergenerational Report'),'. Chart C12.10: hospital, medical and pharmaceutical age-cost profiles. The aggregate bridge sums those profile weights; absolute profile scale cancels in the demand ratio.'),
   tags$li(link(refs['pop'],'Centre for Population, national population projections'),'. Full annual IGR age/sex workbook, updated 27 October 2026. Population Statement 2025 supplies the 2025 pre-projection population used in the bridge.'),
   tags$li(link(refs['ihacpa'],'IHACPA, National Hospital Cost Data Collection, public sector 2023-24'),'. Appendix 5.2: salary/wage items and oncosts divided by total line-item costs, yielding the 67.25% hospital labour-share proxy.'),
   tags$li(link('https://www.oecd.org/en/publications/fiscal-sustainability-of-health-systems_880f3195-en/full-report/component-6.html','OECD, Fiscal Sustainability of Health Systems: long-term projections'),'. The conditional income coefficient near 0.77 motivates a rounded 0.8 scenario; it does not supply the model labour share or efficiency response.')
  ),
  tags$h3('Reproducing the note'),
  tags$p(class='small','Figures are generated directly from the current default model and cached inputs, not copied from a browser scenario. The relevant implementation is R/model_v2.R (aggregate calibration and macro), R/spending_bottom_up.R (service allocation and demand), R/service_costs.R and R/health_framework.R (projection factors). Run scripts/build_health_methods_note.R to rebuild this PDF. The note does not change model settings or deploy the app.'),
  tags$p(class='small','Prepared 3 November 2026. All numbers shown are rounded; calculations use unrounded values. Source vintages follow the model\'s cached inputs.')
 ))
css<-'@page{size:A4;margin:14mm 17mm}body{font:9.5pt/1.33 Arial,sans-serif;color:#233747}h1{font-size:23pt;line-height:1.1;margin:6pt 0 9pt;color:#122b3f}h2{font-size:15pt;color:#007e80;margin:0 0 10pt}h3{font-size:11pt;color:#007e80;margin:14pt 0 7pt}p{margin:7pt 0}a{color:#007e80;text-decoration:none}.brand{font-size:23pt;font-weight:bold;color:#007e80}.subtitle,.small{font-size:8.7pt;color:#52646f}.page{break-after:page}.page:last-child{break-after:auto}table{border-collapse:collapse;width:100%;font-size:9pt;line-height:1.3;margin:10pt 0}th,td{padding:5pt;border-bottom:1px solid #dce4e2;text-align:left;vertical-align:top}th{background:#eaf2ef}td:first-child{width:29%}.page:nth-child(3) td:first-child{width:25%}.page:nth-child(3) td:nth-child(2),.page:nth-child(3) td:nth-child(3){width:13%}.page:first-child td:first-child{width:60%}tr{break-inside:avoid}thead{display:table-header-group}.equation{font-size:9.5pt;background:#eaf2ef;border-left:3px solid #007e80;padding:9pt;margin:10pt 0}li{margin:8pt 0}ol{padding-left:18pt}'
html<-paste0('<!doctype html><html><head><meta charset="utf-8"><title>e61 health spending methods</title><style>',css,'</style></head><body>',as.character(body),'</body></html>')
dir.create('documentation',showWarnings=FALSE)
writeLines(enc2utf8(html),'documentation/health_spending_methods.html',useBytes=TRUE)
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
b<-chromote::ChromoteSession$new();on.exit(b$close(),add=TRUE)
frame<-b$Page$getFrameTree()$frameTree$frame$id
b$Page$setDocumentContent(frameId=frame,html=html)
b$Runtime$evaluate('document.fonts.ready.then(()=>true)',awaitPromise=TRUE)
pdf<-b$Page$printToPDF(printBackground=TRUE,preferCSSPageSize=TRUE)
writeBin(jsonlite::base64_dec(pdf$data),'documentation/health_spending_methods.pdf');b$close()
text<-pdftools::pdf_text('documentation/health_spending_methods.pdf')
stopifnot(length(text)==4L,all(nchar(text)>500),grepl(f(h$anchor_2030_bn),paste(text,collapse=' '),fixed=TRUE))
cat('Created four-page health methods PDF. Verified page count, readable text and current health anchor.\n')
