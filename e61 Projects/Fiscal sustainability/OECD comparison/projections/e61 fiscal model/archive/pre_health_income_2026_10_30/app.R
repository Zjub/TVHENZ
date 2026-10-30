dir.create(file.path('outputs','cache'),recursive=TRUE,showWarnings=FALSE)
Sys.setenv(R_USER_CACHE_DIR=normalizePath(file.path('outputs','cache')))
library(shiny)
library(ggplot2)
source(file.path('R','model.R'))
source(file.path('R','sector_review.R'),local=TRUE,encoding='UTF-8')
inputs <- load_inputs()
calibration <- calibrate(inputs)
igr_spending_official<-read.csv('data/processed/igr_spending_benchmarks.csv',check.names=FALSE)
igr_notes<-igr_spending_notes()
imf_debt_official<-read.csv('data/processed/imf_long_term_debt_projections.csv',check.names=FALSE)
oecd_debt_official<-read.csv('data/processed/oecd_debt_projections.csv',check.names=FALSE)

theme_e61 <- function() theme_minimal(base_size=13) + theme(
  panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(),
  plot.title=element_text(face='bold',colour='#122b3f'),
  axis.title=element_text(colour='#41515d'), legend.position='bottom',
  plot.margin=margin(12,18,8,8))
colours <- c('Your scenario'='#007e80','Reference'='#9b6171')
reference_text <- 'Fixed reference: NGDP per capita growth 3.2%; population growth 1.2%; productivity 1.2%; constant average hours; service productivity at 25% of economy-wide growth for health and education resource-cost blocks; stated additional service-growth assumptions; unchanged terms of trade; wage-indexed tax thresholds and 4% real super returns. These are discussion assumptions, not a forecast.'

ui <- fluidPage(
  theme=bslib::bs_theme(version=5,base_font='system-ui',primary='#007e80',bg='#f4f6f5',fg='#122b3f'),
  tags$head(tags$style(HTML('
    .container-fluid{max-width:1500px;padding:24px 30px} body{background:#f4f6f5}
    .masthead{display:flex;justify-content:space-between;align-items:center;margin-bottom:24px}
    .wordmark{font-size:38px;font-weight:850;letter-spacing:-3px;color:#007e80}
    .eyebrow{font-size:11px;letter-spacing:2px;font-weight:700;text-transform:uppercase;color:#567078}
    h1{font-size:30px;font-weight:750;letter-spacing:-.8px;margin:4px 0 7px}
    h3{font-size:19px;font-weight:700}.subhead{color:#51646e;max-width:850px}
    .well{background:white;border:1px solid #dce4e2;border-radius:14px;padding:22px;box-shadow:none}
    .panel-card{background:white;border:1px solid #dce4e2;border-radius:14px;padding:20px;margin-bottom:18px}
    .metrics{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:12px;margin-bottom:18px}
    .metric{padding:16px;background:white;border:1px solid #dce4e2;border-top:3px solid #007e80;border-radius:10px}
    .metric-label{color:#51646e;font-size:12px;min-height:32px}.metric-value{font-size:27px;font-weight:740;margin:5px 0}
    .metric-change{font-size:12px;color:#546b73}.note{font-size:12px;color:#60717b;line-height:1.6}
    .model-badge{background:#e3eeea;color:#225b52;border-radius:30px;padding:8px 13px;font-size:12px;white-space:nowrap}
    .nav-tabs{margin-bottom:18px}.nav-tabs .nav-link{color:#46606a}.nav-tabs .active{font-weight:700}
    .control-label{font-size:13px;font-weight:650}.help-block{font-size:12px;line-height:1.45}
    details{margin:18px 0}summary{font-weight:650;cursor:pointer} .btn{border-radius:7px}
    table{font-size:12px}.shiny-output-error-validation{color:#8a4a20}
    @media(max-width:1050px){.metrics{grid-template-columns:repeat(2,minmax(0,1fr))}}
    @media(max-width:600px){.container-fluid{padding:14px}.masthead{align-items:flex-start}.model-badge{display:none}}
  '))),
  div(class='masthead',div(div(class='wordmark','e61'),div(class='eyebrow','Fiscal futures laboratory')),
      div(class='model-badge','Research prototype · 28 October 2026')),
  h1('What changes Australia’s fiscal future?'),
  p(class='subhead','Explore how economic assumptions flow through services, revenue and debt. Consolidated general government · financial years ending 2030–2066.'),
  p(class='note','Illustrative scenarios: the reference is a modelling starting point, not an e61 forecast. Results depend on the programme and policy assumptions in Method & Data.'),
  sidebarLayout(
    sidebarPanel(width=3,
      h3('Build a scenario'),
      sliderInput('productivity','Labour productivity growth (% a year)',min=-0.5,max=3,value=1.2,step=0.1),
      helpText('Real output per hour. At fixed NGDP per capita and population, productivity changes real output and implied inflation, not nominal GDP.'),
      sliderInput('ngdp_per_capita','NGDP per capita growth (% a year)',min=1,max=8,value=3.2,step=0.1),
      sliderInput('population_growth','Population growth (% a year)',min=-0.5,max=2.5,value=1.2,step=0.1),
      helpText('Scales the population path from the shared 2030 anchor, retaining the sourced age/sex shares each year. Employment and service demand use that same population. This does not simulate different migration or fertility patterns.'),
      sliderInput('hours_growth','Annual change in average hours per worker (%)',min=-1,max=1,value=0,step=0.1),
      div(class='help-block',textOutput('macro_identity')),
      sliderInput('tot','Terms-of-trade level change by 2040 (%)',min=-30,max=30,value=0,step=5),
      helpText('A permanent export/import price shift, phased in over ten years. Changes implied domestic prices, royalties and the structural company-tax share. Real output and the nominal GDP path set by population and NGDP per capita remain unchanged.'),
      tags$details(id='cost_pass_through_controls',tags$summary('Cost pass-through and financing sensitivity'),
        selectInput('service_cost_set','Health and education cost assumptions',
          c('Current assumptions (reference)'='current','Historical trend estimates (2016-2025)'='historical')),
        helpText('The 25% service-productivity default and the current cost-growth baselines are arbitrary modelling choices, not estimated or neutral values. Historical residuals are fitted conditional on that arbitrary productivity assumption. The cost controls show actual selected annual rates.'),
        helpText('Real wages follow economy-wide productivity. Service productivity reduces the labour required per service. A slower service-productivity response generates Baumol cost pressure. Additional service growth covers intensity, quality and mix.'),
        lapply(setdiff(names(cost_pass_through_parameters()),c('NDIS','Other social protection')),function(n) {
          key<-cost_pass_through_parameters()[[n]]
          theta<-service_productivity_parameters()[[n]]
          efficiency_control<-sliderInput(theta,paste0(n,': service productivity cost savings (% of economy-wide growth)'),
            min=0,max=150,value=100*default_assumptions()[[theta]],step=5)
          tagList(if(n%in%c('Health','Education'))efficiency_control,
            if(n%in%c('Health','Education'))sliderInput(paste0(tolower(n),'_additional_cost_growth'),
              paste0(n,': additional cost growth above CPI and wage costs (%/year)'),min=-1.5,max=2.5,value=.5,step=.01) else
              sliderInput(key,paste0(n,': additional cost growth adjustment (pp/year)'),min=-2,max=2,value=0,step=.1),
            div(class='note',textOutput(paste0(key,'_defaults'))))
        }),
        helpText('0% service productivity means no productivity improvement within the service; 100% means it keeps pace with the economy. The additional-growth adjustment adds to each published parameter-table baseline, preserving subfunction differences. CPI and funding-index rules do not receive service-efficiency offsets. The social-protection residual uses a cash-indexation proxy, so its productivity-response control is inactive.'),
        sliderInput('interest_debt_sensitivity_bps','Interest sensitivity to net debt (basis points per +1 pp of net debt/GDP)',
          min=0,max=5,value=0,step=.25),
        helpText('Zero disables debt-rate feedback. At 3, net debt 10 percentage points of GDP above its 2030 anchor adds 0.30 percentage points to the borrowing rate. Uses the previous year\'s debt/GDP; lower debt reduces the rate, with a zero rate floor. Applies to the effective rate on opening gross debt; asset yields are unchanged.'),
        p(class='note','Treasury research benchmark: about 3 basis points per percentage point of net debt/GDP, with a plausible range of 0-5. Yan and Brittle (2010) estimate a long-run Australian-US real 10-year yield margin. Applying that estimate to the effective borrowing rate is a scenario assumption, not a Treasury forecast.'),
        p(tags$a(href='https://treasury.gov.au/sites/default/files/2019-03/Tsy_Working_Paper_2010_04.pdf#page=16',target='_blank','Treasury Working Paper 2010-04')),
        actionButton('reset_pass_through','Reset cost and sensitivity assumptions',width='100%')),
      tags$details(id='policy_financing_controls',tags$summary('Policy and financing assumptions'),
        h4('NDIS and transfers'),
        sliderInput('ndis_service_growth_adjustment','NDIS: additional plan cost / use growth adjustment (pp/year)',min=-2,max=2,value=0,step=.1),
        helpText('NDIS spending combines the existing participant/cohort projection, general prices, the original plan-cost index and 0.5% annual additional cost/use growth plus this adjustment. No service-efficiency saving is applied. This is not a new estimate of future participation.'),
        sliderInput('social_service_growth_adjustment','Other transfers: additional payment growth adjustment (pp/year)',min=-2,max=2,value=0,step=.1),
        helpText('Other social protection retains the earlier payment-indexation and working-age demand proxy. Age Pension retains wealth accumulation, means tests and payment indexation. These payments receive no service-productivity deduction.'),
        selectInput('other_spending_rule','Other public services',c('Project demographic demand and input costs'='input_costs','Constant anchor share of GDP (IGR-inspired)'='gdp_share')),
        helpText('The fixed-share rule replaces projected costs from 2031. An explicit fixed-share target below takes precedence. Applying the Commonwealth IGR residual rule to consolidated other services is a scope assumption.'),
        selectInput('policy','Service and benefit convention',
          c('Indexed rules (illustrative)'='indexed','Constant relative standards'='standards')),
        helpText('Indexed rules apply pension means tests and cohort superannuation accumulation. Relative standards preserve pension entitlements per eligible person and fully wage-link other social protection.'),
        selectInput('tax_policy','Personal tax thresholds',
          c('Wage indexation (default policy assumption)'='wage_indexed','CPI indexation (policy alternative)'='cpi',
            'Fixed nominal thresholds (legislated convention)'='fixed')),
        sliderInput('super_return','Real superannuation return (%)',min=0,max=7,value=4,step=0.5),
        helpText('Affects private retirement balances and Age Pension means tests under indexed rules. Higher returns reduce pensions and debt. Does not change GDP, superannuation tax receipts or government asset returns; inactive under constant relative standards.'),
        sliderInput('company_tot_elasticity','Company tax share: terms-of-trade elasticity',min=0,max=2,value=1,step=0.1),
        helpText('Illustrative sensitivity, not an estimate. At 1, a 10% rise in terms of trade raises company tax as a share of GDP by 10%. NGDP growth does not change the company-tax share.'),
        div(class='help-block',textOutput('ndis_growth_readout')),
        helpText('NDIS total growth combines participants, inflation, the wage-productivity gap and additional service growth. Its assumptions are in Service costs and productivity; the NDIS tab shows the evidence and IGR comparator.'),
        sliderInput('interest','Long-run borrowing rate before debt feedback (%)',min=2,max=7,value=4.4,step=0.1)),
      tags$details(id='spending_target_controls',tags$summary('Fixed spending shares of GDP'),
        helpText('Optional overrides apply from 2031 through 2066, immediately holding the selected function at the chosen share of nominal GDP. The common 2030 allocation remains fixed. Each function replaces its projected total, rather than adding spending; subfunctions share that total in proportion to their underlying projected amounts.'),
        lapply(names(spending_target_parameters()),function(n){
          key<-spending_target_parameters()[[n]]
          base<-100*calibration$programmes$anchor_2030_bn[calibration$programmes$programme==n]/calibration$gdp0
          tagList(checkboxInput(paste0('fix_',key),paste('Fix',n,'as % of GDP'),FALSE),
            conditionalPanel(condition=paste0('input.fix_',key),
              numericInput(paste0('target_',key),paste(n,'target (% GDP)'),value=round(base,1),min=0,max=100,step=.1)))
        }),
        textOutput('spending_override_summary'),
        actionButton('reset_targets','Clear fixed-share overrides',width='100%')),
      actionButton('reset','Reset assumptions',width='100%'),
      hr(),p(class='note','Changes begin in 2030–31. The 2029–30 PBO aggregate is a shared starting point. No automatic tax increase or spending cut closes the budget.')
    ),
    mainPanel(width=9,
      uiOutput('metrics'),
      tabsetPanel(id='tabs',
        tabPanel('Fiscal outlook',value='outlook',
          div(class='panel-card',
            fluidRow(column(8,h3('Your scenario and the reference')),
              column(4,selectInput('measure',NULL,c('Net debt (% GDP)'='net_debt_pct',
                'Gross debt (% GDP)'='gross_debt_pct','Total expenditure (% GDP)'='total_spending_pct',
                'Fiscal balance (% GDP)'='fiscal_balance_pct','Revenue (% GDP)'='revenue_pct',
                'Real primary spending per person ($2030)'='real_primary_per_person')))),
            plotOutput('outlook',height=340),
            p(class='note',reference_text),textOutput('endpoint')),
          div(class='panel-card',h3('International debt comparisons'),
            p('Long-term scenarios from the common 2030 starting year. The PBO supplies our starting point; short-run international forecasts are excluded. The lower panel compares gross debt with both OECD Survey scenarios and three IMF long-term scenarios.'),
            selectInput('international_debt_view','Display',c('Source/model levels (% GDP)'='levels','Change from each series in 2030 (pp GDP)'='change')),
            plotOutput('international_debt_plot',height=700),
            textOutput('international_debt_summary'),
            div(style='overflow-x:auto',tableOutput('international_debt_endpoints')),
            p(class='note','The IMF long-term assessment is Annex III, Figure 7 of Country Report 26/38, printed page 53. These are gross-debt scenarios for the large-amortisation risk module, extending to 2054, not a full bottom-up ageing projection. Values are approximately digitised from the chart (allow at least +/-0.5 percentage points of GDP reading uncertainty). The debt-stabilising case assumes the primary balance needed to stabilise debt; its flat path is a modelling assumption.'),
            p(class='note','OECD Survey Figure 1.14 publishes both gross-debt scenarios through 2050. No net-debt versions are inferred for either organisation. The upper panel uses the separate EO117 BAU1 net-financial-liabilities series, with broader asset netting and a fiscal reaction rule; it is not a net conversion of the Survey paths. No source series is extended beyond its horizon.'),
            p(class='note','Model years are financial years ending in June; provider year labels are retained without fiscal/calendar conversion. Each provider uses its own GDP, accounting perimeter, valuation and policy assumptions. Changes from 2030 remove starting-level differences but do not harmonise definitions. Controls change only the model scenario; published benchmarks and the fixed reference remain unchanged.'),
            p(tags$a(href='https://www.imf.org/en/-/media/files/publications/cr/2026/english/1ausea2026001-source-pdf.pdf#page=58',target='_blank','IMF long-term assessment (Figure 7)'),' | ',
              tags$a(href='https://stat.link/gdxr47',target='_blank','OECD Survey Figure 1.14 source workbook'),' | ',
              tags$a(href='https://data-explorer.oecd.org/vis?df[ag]=OECD.ECO.MAD&df[id]=DSD_EO_LTB@DF_EO_LTB',target='_blank','OECD long-term scenarios')),
            downloadButton('imf_debt_download','Download IMF long-term CSV (approx.)'),
            downloadButton('oecd_debt_download','Download OECD source CSV'),
            downloadButton('international_debt_download','Download displayed comparison')),
          div(class='panel-card',h3('What drives the change in debt?'),
              plotOutput('attribution',height=240),
              p(class='note','An exact Shapley allocation of economic controls and policy/financing changes, including their interactions, relative to the fixed default reference. This explains model arithmetic, not estimated causal effects.'))),
        tabPanel('Economic channels',value='channels',
          div(class='panel-card',h3('Real output, nominal output and prices'),
              plotOutput('macro',height=390),textOutput('price_message')),
          div(class='panel-card',h3('The productivity mechanism'),
            p('Nominal GDP equals population times nominal GDP per capita. Holding those two inputs fixed keeps the entire nominal GDP path fixed when productivity or hours change.'),
            p('Real output equals employment times average hours times output per hour. Implied domestic prices reconcile real output with the chosen nominal GDP path and terms-of-trade income effect. More productivity or hours therefore lowers implied inflation at unchanged nominal income.'),
            p('Participation and employment rates reflect the sourced age structure. Terms of trade retain an illustrative income elasticity of 0.20 and royalty elasticity of 2. The structural company-tax share has its own ToT sensitivity under advanced assumptions. Domestic prices adjust to preserve the selected nominal income path.'))),
        tabPanel('Debt & interest',value='debt',
          div(class='panel-card',h3('Gross debt, financial assets and net debt'),
            selectInput('debt_units','Chart units',c('% of GDP'='pct','AUD billion'='bn'),selected='pct'),
            plotOutput('debt_stocks',height=320),
            p(class='note','Net debt = gross debt minus the modelled financial assets. Gross debt finances deficits and net financial asset acquisitions. The asset stock normally grows with population and domestic prices; surplus funds accumulate in assets once gross debt reaches zero. This is a simplified balance sheet, not a full valuation of public assets.')),
          div(class='panel-card',h3('Interest paid and received'),plotOutput('debt_interest',height=320),
            p('Interest payments = the effective borrowing rate times opening gross debt. They are not calculated on net debt. Financial assets earn a separate calibrated yield; net interest subtracts those receipts from interest payments.'),
            p(class='note','The borrowing-rate assumption does not adjust automatically with inflation. Changing NGDP at fixed real growth changes implied inflation and the real borrowing rate. Higher interest/GDP can therefore accompany lower dollar interest payments if nominal GDP is proportionately lower.')),
          div(class='panel-card',h3('Reconcile the stocks and interest bill'),
            selectInput('debt_year','Financial year ending',c(2030,2040,2050,2066),selected=2066),
            textOutput('debt_explanation'),
            h4('Closing stocks and GDP'),div(style='overflow-x:auto',tableOutput('debt_balance_table')),
            h4('Opening debt and the interest calculation'),div(style='overflow-x:auto',tableOutput('debt_interest_table')),
            p(class='note','Stocks are measured at year end; interest is based on the opening stock from the preceding year. The common 2030 published anchor is not reconstructed: opening-stock fields are blank for that year. The reference remains at all default settings, including 1.2% productivity. It does not show the scenario immediately before your last control change.'),
            downloadButton('debt_download','Download annual debt and interest detail'))),
        tabPanel('Spending',value='components',
          div(class='panel-card',h3('How spending shares change over time'),
            textOutput('active_spending_targets'),
            selectInput('spending_units','Show spending as',c('% of GDP'='pct','Nominal AUD billions'='bn'),selected='pct'),
            plotOutput('spending_paths',height=620),
            p(class='note','Each panel has its own vertical scale. Both paths include demographic ageing and programme cost growth; overlapping lines mean the scenario matches the reference, not that spending is constant over time. Years identify financial years by ending year.')),
          div(class='panel-card',h3('How NGDP changes spending'),textOutput('spending_ngdp_link'),
            div(style='overflow-x:auto',tableOutput('spending_ngdp_comparison')),
            p(class='note','This comparison changes only NGDP per capita growth, keeping your productivity, population, hours, terms of trade, cost pass-through and policy settings fixed. Fully price-indexed services rise with nominal prices, so a change in dollar spending can leave spending/GDP unchanged. Terms of trade instead change the real-income wedge between domestic prices and GDP.')),
          div(class='panel-card',h3('What contributes to the change in spending?'),
            selectInput('spending_group','Spending coverage',c('All primary spending'='primary','All spending including interest and net investment'='all',setNames(calibration$programmes$programme,calibration$programmes$programme))),
            fluidRow(column(4,selectInput('contribution_start','From financial year ending',2030:2065,selected=2030)),
              column(4,selectInput('contribution_end','To financial year ending',2031:2066,selected=2066)),
              column(4,selectInput('contribution_measure','Measure',c('Change in GDP share (pp)'='pct','Nominal AUD billions'='bn','2030-dollar billions'='real_bn'),selected='pct'))),
            textOutput('spending_contribution_summary'),plotOutput('spending_contribution_plot',height=360),
            tableOutput('spending_contribution_table'),
            p(class='note','Exact Shapley accounting shares interactions symmetrically and reconciles to spending changes. Real wages/funding indexation and service productivity are separate contributions. Additional service growth represents intensity, quality and mix. Age/cohort demand includes age-weighted health use and NDIS retention. Pension, defence and financing have explicit policy rules. The GDP denominator applies only to shares.'),
            downloadButton('spending_factor_download','Download contributions by subfunction')),
          div(class='panel-card',h3('Bottom-up spending subfunctions'),
            p('The 24 blocks sum to eight broad functions. Each service block has demographic demand, an input-cost or funding rule, a service-productivity response and additional service growth. The selection above filters this table.'),
            div(style='overflow-x:auto',tableOutput('spending_subfunctions')),
            downloadButton('spending_detail_download','Download annual subfunctions and factors'),
            downloadButton('spending_background','Download spending background PDF')),
          div(class='panel-card',h3('Service cost assumptions and IGR benchmarks'),
            textOutput('cost_set_summary'),
            p('Observed hospital, school and university input weights inform labour exposure. VET uses a funding-index proxy; medical and residual weights are modelling judgments. Service productivity reduces labour requirements. Additional service growth covers intensity, quality and mix beyond explicit unit costs.'),
            div(style='overflow-x:auto',tableOutput('health_education_costs')),
            p(class='note','Unit cost = general prices x [(1 - labour share) + labour share x real wage index / service-productivity index]. Real wages follow economy-wide productivity. Additional service growth multiplies this cost path separately. Funding-index and CPI proxies have no service-efficiency offset. IGR rules are scope-specific comparators; no unpublished growth coefficient is invented.'),
            downloadButton('health_education_download','Download service assumptions and benchmarks'),
            downloadButton('cost_estimation_report','Download historical cost estimation report')),
          div(class='panel-card',h3('Compare cost assumption sets'),
            p('Both paths use your economic and policy settings. Only health and education additional-growth baselines change. Each estimate remains fixed when economic controls move.'),
            plotOutput('cost_set_comparison',height='300px'),
            tableOutput('cost_set_endpoints')),
          div(class='panel-card',h3('Subfunction factors: inspect the mechanism'),
            textOutput('spending_driver_period'),
            div(style='overflow-x:auto',tableOutput('spending_drivers')),
            p(class='note','Each factor is its end-year level divided by its start-year level: 1 means unchanged. This table follows the spending coverage, years and measure selected above and uses exactly the same factors as the contribution calculation. Multiplying the unrounded factors by a row\'s starting spending reproduces its ending spending. Aggregate cost scaling is separate from each subfunction\'s unit-cost rule. Do not add or average factors across rows.'),
            p(class='note','Population size and age/cohort demand are separate. General prices are removed in the 2030-dollar measure; the GDP denominator applies only to GDP shares and is the inverse change in nominal GDP. Real wages and service productivity are separated from additional service growth. Defence follows its GDP-share target; interest follows the financing rule. A dash means a factor starts at zero, so its ratio is undefined; the download retains both endpoint levels and the contribution calculation still handles the change.'),
            downloadButton('spending_driver_download','Download factor levels and changes')),
          div(class='panel-card',h3('Spending components in 2066'),plotOutput('components',height=330),
            p(class='note','NDIS and aged care have their own expenditure boundaries and are excluded from residual social protection. Total expenditure also includes conventional interest and net capital investment.')),
          div(class='panel-card',h3('Checking projections against the IGR'),
            p('Each comparison uses its own GDP denominator. The fixed reference uses the model defaults; your scenario responds to the controls. Published IGR shares are rounded, usually to one decimal. Differences reflect coverage, starting allocations, assumptions and methods; they are not estimates of forecast error.'),
            downloadButton('igr_spending_download','Download all IGR comparisons')),
          lapply(setdiff(names(igr_notes),'NDIS'),function(programme){
            id<-make.names(programme);n<-igr_notes[[programme]]
            div(class='panel-card',h3(paste0(programme,': checking the projection')),
              textOutput(paste0('igr_summary_',id)),
              div(style='overflow-x:auto',tableOutput(paste0('igr_table_',id))),
              p(class='note',n$coverage),p(n$mechanism),
              p(tags$a(href='https://treasury.gov.au/publication/2026-intergenerational-report',target='_blank',paste('2026 IGR:',n$source))))
          }),
          div(class='panel-card',h3('NDIS: checking the projection'),textOutput('ndis_summary'),
            tableOutput('ndis_benchmark'),
            p(class='note','Whole-Scheme spending as a share of each projection\'s own GDP; IGR figures are rounded to one decimal. These are not Commonwealth net funding shares. Our allocation starts at the common PBO aggregate anchor, with fixed under-65 age-specific participation rates, an explicit wage-productivity gap and additional service growth. It does not reproduce the Scheme Actuary\'s entrant, exit, disability or support-cost model, or explicitly cost the 2026 reforms.'),
            p('The 2026 IGR transitions average cost by age and disability type toward earnings growth after the medium term. The NDIS sector tab compares that rule with the scenario cost basket. Participant-mix and reform effects require separate evidence; similar total spending shares do not establish that the mechanisms match.'),
            p(tags$a(href='https://ndis.gov.au/media/8183/download?attachment=',target='_blank','NDIA 2024-25 actuarial report'), ' | ',
              tags$a(href='https://www.pbo.gov.au/publications-and-data/publications/2026-27-medium-term-budget-outlook',target='_blank','PBO July 2026 outlook'), ' | ',
              tags$a(href='https://treasury.gov.au/publication/2026-intergenerational-report',target='_blank','2026 IGR chart C12.17')))),
        sector_review_ui('NDIS',calibration$subfunctions),
        sector_review_ui('Health',calibration$subfunctions),
        sector_review_ui('Education',calibration$subfunctions),
        tabPanel('Revenue',value='revenue',
          div(class='panel-card',h3('How revenue responds'),
            plotOutput('revenue_path',height=310),textOutput('revenue_summary'),
            p(class='note','The reference uses wage-indexed thresholds, which remove bracket creep from growth in average earnings. CPI indexation remains an alternative and allows bracket creep from real earnings growth. The wage-indexed counterfactual holds your economic scenario fixed and changes only personal tax thresholds; it coincides with your scenario under wage indexation. No revenue/GDP cap is imposed.')),
          div(class='panel-card',h3('Structural company tax'),textOutput('company_tax_summary'),tableOutput('company_tax_table'),
            p(class='note','Company tax = nominal GDP x the calibrated 2030 company-tax share x the terms-of-trade index raised to the selected sensitivity. Terms of trade phase in to 2040, then the share remains at that level. Higher NGDP growth changes dollar receipts proportionately, with no effect on their GDP share. This is a structural scenario rule, not a cycle or firm-level profits model. The historical company-tax GDP elasticity is not used.')),
          div(class='panel-card',h3('Revenue composition over time'),
            plotOutput('revenue_mix',height=350),
            p(class='note','GST appears once at Commonwealth collection. State GST grants and NDIS funding contributions are internal flows, not extra national revenue. Fees and other revenue include dividends; royalties and interest receipts are separate.')),
          div(class='panel-card',h3('Income tax and bracket creep'),
            tableOutput('tax_rules'),
            p('Personal income tax applies the legislated resident schedule and Low Income Tax Offset to 100 observed ATO taxable-income percentile cells. Income grows with earnings; taxpayer numbers scale with employment. This approximates the income distribution and is not a household tax-benefit microsimulation.'),
            p('Choose fixed nominal, CPI-indexed or wage-indexed thresholds in the sidebar. Indexation is annual from the common 2030 starting point. Bracket creep is measured against wage-indexed thresholds under the same economic scenario.'),
            p(class='note','The 2027-28 14% lower rate is included. Medicare/FBT are a separate observed revenue block. WATO eligibility, non-resident schedules, capital-gains realisations and individual deductions are not simulated; starting aggregate differences are absorbed by calibration.')),
          div(class='panel-card',h3('Revenue mechanisms and evidence'),tableOutput('revenue_mechanisms'),
            p('Receipts follow explicit tax-base proxies at constant effective yields. GST assumes constant consumption/GDP and taxable coverage. Payroll and related heads follow the wage bill. Property values/GDP and transaction turnover are held constant. Excise assumes constant taxable quantities per resident and CPI-indexed unit rates; its GDP share can fall as real output per resident grows.'),
            p(class='note','Superannuation earnings and contributions taxes remain combined in a wage-bill proxy. Medicare/FBT remain combined. Fuel, tobacco, alcohol and customs are not separately modelled. These are transparent interim assumptions, not estimated structural forecasts.'),
            h3('Revenue by type: first and last year'),
            div(style='overflow-x:auto',tableOutput('revenue_endpoints')),
            p(class='note','Your selected scenario, including interest receipts. Dollar amounts are nominal AUD billions. Years denote financial years by ending year. Totals use unrounded amounts, so displayed rows may differ slightly from their sum.'),
            downloadButton('revenue_download','Download annual revenue detail'))),
        tabPanel('Method & data',value='method',
          div(class='panel-card',h3('Arbitrary defaults and fitted historical residuals'),
            p('Health and education service efficiency defaults to 25% of economy-wide growth. This is arbitrary: a modelling choice for discussion, not an empirical Australian estimate, a neutral assumption or an official recommendation. At 1.2% economy-wide growth it implies 0.3% annual efficiency growth. The efficiency deduction applies to resource-cost baskets; the medicine CPI proxy and VET funding-index rule retain their separate treatment.'),
            p('Health and education each default to 0.5% additional cost growth a year above baseline CPI and standard wage pass-through. These values are also arbitrary, not empirical estimates. The controls display the actual selected rate. With the historical alternative, the rate is the anchor-spending-weighted mean of subfunction residuals; changing it shifts every subfunction by the same number of percentage points, preserving differences. Aged care retains 0.3% additional growth and other public services zero, with no new efficiency default.'),
            p('The historical alternative uses fitted 2016-2025 net residuals conditional on the arbitrary 25% productivity assumption. These residuals include policy, coverage, quality, mix and measurement effects. Selecting their estimation period and extrapolating them are modelling judgements; the data do not independently identify service efficiency. Higher education and VET share a tertiary residual, and other health includes accounting reconciliation. Once selected, residuals are held fixed when scenario controls move.'),
            p('NDIS uses the existing participant/cohort path, original plan-cost index and 0.5% additional cost/use growth. Other transfers retain the original payment indexation and demand proxy; Age Pension retains its explicit wealth and means-testing module. None receives a service-efficiency deduction. Economy-wide productivity can still affect wage-linked payment indexation; it does not make transfers more efficient.')),
          div(class='panel-card',h3('How to interpret this version'),
            p('This is an independent, transparent scenario prototype. It is not a validated forecast, a full current-policy simulation or a replication of the PBO or IGR.'),
            tags$ul(tags$li('The aggregate starting point uses the 2026-27 PBO National Fiscal Outlook. National totals are already consolidated. The source bridge reports Commonwealth and state totals and the net local-government/consolidation adjustment.'),
              tags$li('Programme allocations use 2024–25 ABS GFS and an Age Pension outlay of $62.2bn from the ANAO. A single calibration reconciles their 2030 projection to PBO primary expenses. These are e61 allocations, not published PBO programme forecasts.'),
              tags$li('Population totals from 2030 follow the chosen growth rate, preserving the sourced age/sex shares each year. The 2026 IGR age structure and participation targets and ABS observed employment profiles are retained. Average hours follow their own control. This is not a migration/fertility cohort model.'),
              tags$li('Health uses IGR hospital, medical and pharmaceutical age-cost profiles. Education uses age-band demand proxies. Resource-cost baskets separate real wages and service productivity; funding-index rules follow their specified index. Additional service growth covers intensity, quality and mix. Source classifications and IGR comparator scopes are explicit in the sector tabs.'),
              tags$li('Pension age 67, income/assets tests, deeming and statutory tax brackets are explicit. Pension wealth archetypes and superannuation accumulation are stylised: household wealth distributions and full current-policy reforms remain to be validated.'),
              tags$li('Debt is simulated from the common 2030 stock. Interest is charged on gross debt; receipts on financial assets. Financial asset acquisitions are accounted for separately. Surpluses after gross debt repayment accumulate in assets.'),
              tags$li('No statistical uncertainty bands are supplied: model structure and policy uncertainty dominate the precision of this prototype.')),
            h3('Evidence and assumption register'),
            p('Sourced figures are taken from named publications. Calibrated values reconcile those figures to our starting totals. Proxy and judgment entries are modelling assumptions, including guesses with no estimated coefficient. Identities are consequences of equations, conditional on those assumptions. A cited source supports only the input or method identified; it does not endorse our parameter choices.'),
            selectInput('register_domain','Show choices for',choices=c('All','Anchor','Macro','Demography','Revenue','Spending','Pension','Policy parameter','Financing'),selected='Revenue'),
            div(style='overflow-x:auto',tableOutput('assumption_register')),
            downloadButton('register_download','Download full evidence register'),
            h3('Programme parameters'),tableOutput('parameters'),
            h3('Fiscal coverage and reconciliation (2030)'),tableOutput('coverage_bridge'),
            p(class='note','The adjustment combines transfer elimination, local-government coverage and reporting differences; it is not a pure measure of transfers. IGR benchmarks cover the Commonwealth, whereas our projection covers consolidated government. We do not construct independent long-run jurisdiction budgets in this version.'),
            h3('Source files and provenance'),tableOutput('sources'),
            p(tags$a(href='https://www.anao.gov.au/work/performance-audit/administration-of-the-age-pension',target='_blank','ANAO Age Pension anchor'),
              ' | ',tags$a(href='https://www.pbo.gov.au/publications-and-data/publications/2026-27-National-Fiscal-Outlook',target='_blank','PBO 2026-27 anchor'),
              ' | ',tags$a(href='https://treasury.gov.au/publication/2026-intergenerational-report',target='_blank','2026 IGR data and methods')),
            downloadButton('download','Download annual reference and scenario'),
            downloadButton('audit','Download assumptions and calibration')))
      )
    )
  ),
  p(class='note','e61 fiscal model · Local research prototype · Amounts are AUD billions unless stated. Year 2030 denotes FY2029–30. See README.md for equations, limitations and validation.')
)

server <- function(input,output,session) {
  cost_growth_centre<-function(n,z){
    p<-effective_service_parameters(inputs,z);s<-calibration$subfunctions
    s<-s[s$programme==n,];round(100*weighted.mean(p$additional_service_growth[match(s$id,p$id)],s$anchor_2030_bn),2)/100
  }
  reset_cost_rates<-function(z=default_assumptions()){
    for(n in c('Health','Education')){
      key<-paste0(tolower(n),'_additional_cost_growth');centre<-100*cost_growth_centre(n,z)
      freezeReactiveValue(input,key)
      updateSliderInput(session,key,min=centre-2,max=centre+2,value=centre)
    }
  }
  observeEvent(input$service_cost_set,{
    z<-default_assumptions();z$service_cost_set<-input$service_cost_set;reset_cost_rates(z)
  },ignoreInit=TRUE)
  a <- reactive({
    z <- default_assumptions()
    z$productivity <- input$productivity/100
    z$ngdp_per_capita_growth <- input$ngdp_per_capita/100
    z$population_growth <- input$population_growth/100
    z$hours_growth <- input$hours_growth/100
    z$tot_change <- input$tot/100
    z$policy <- input$policy
    z$tax_policy <- input$tax_policy
    z$super_real_return <- input$super_return/100
    z$company_tot_elasticity <- input$company_tot_elasticity
    z$interest_rate <- input$interest/100
    for(key in c('service_cost_set','other_spending_rule'))
      if(!is.null(input[[key]]))z[[key]]<-input[[key]]
    if(!is.null(input$interest_debt_sensitivity_bps))z$interest_debt_sensitivity_bps<-input$interest_debt_sensitivity_bps
    for(n in names(spending_target_parameters())){
      key<-spending_target_parameters()[[n]]
      if(isTRUE(input[[paste0('fix_',key)]])){
        value<-input[[paste0('target_',key)]]
        validate(need(is.numeric(value)&&length(value)==1&&is.finite(value)&&value>=0&&value<=100,
          paste(n,'target must be a percentage between 0 and 100.')))
        z$spending_gdp_targets[n]<-value
      }
    }
    for(key in c(unname(cost_pass_through_parameters()),unname(service_productivity_parameters())))
      if(!is.null(input[[key]]))z[[key]]<-input[[key]]/100
    for(n in c('Health','Education')){
      value<-input[[paste0(tolower(n),'_additional_cost_growth')]]
      if(!is.null(value))z[[cost_pass_through_parameters()[[n]]]]<-round(value/100-cost_growth_centre(n,z),10)
    }
    z
  })
  # Fix every reference assumption. Reusing selected policies here masks their
  # fiscal effect by moving both comparison paths together.
  ref_a <- reactive(default_assumptions())
  scenario <- reactive(project_model(inputs,a(),calibration))
  reference <- reactive(project_model(inputs,ref_a(),calibration))
  for(programme in c('NDIS','Health','Education'))sector_review_server(programme,inputs,scenario,reference)
  both <- reactive(rbind(transform(scenario()$path,case='Your scenario'),
                         transform(reference()$path,case='Reference')))
  observeEvent(input$reset,{
    reset_cost_rates()
    updateSliderInput(session,'productivity',value=1.2)
    updateSliderInput(session,'ngdp_per_capita',value=3.2)
    updateSliderInput(session,'population_growth',value=1.2)
    updateSliderInput(session,'hours_growth',value=0)
    updateSliderInput(session,'tot',value=0)
    updateSelectInput(session,'policy',selected='indexed')
    updateSelectInput(session,'service_cost_set',selected='current')
    updateSelectInput(session,'other_spending_rule',selected='input_costs')
    updateSelectInput(session,'tax_policy',selected='wage_indexed')
    updateSliderInput(session,'super_return',value=4)
    updateSliderInput(session,'company_tot_elasticity',value=1)
    updateSliderInput(session,'interest',value=4.4)
    updateSliderInput(session,'interest_debt_sensitivity_bps',value=0)
    for(key in unname(spending_target_parameters()))updateCheckboxInput(session,paste0('fix_',key),value=FALSE)
    for(key in c(unname(cost_pass_through_parameters()),unname(service_productivity_parameters())))
      updateSliderInput(session,key,value=100*default_assumptions()[[key]])
  })
  observeEvent(input$reset_pass_through,{
    reset_cost_rates()
    updateSelectInput(session,'service_cost_set',selected='current')
    updateSliderInput(session,'interest_debt_sensitivity_bps',value=0)
    for(key in c(unname(cost_pass_through_parameters()),unname(service_productivity_parameters())))
      updateSliderInput(session,key,value=100*default_assumptions()[[key]])
  })
  observeEvent(input$reset_targets,{
    for(key in unname(spending_target_parameters()))updateCheckboxInput(session,paste0('fix_',key),value=FALSE)
  })
  cost_sets<-reactive({
    do.call(rbind,lapply(c('current','historical'),function(set){z<-a();z$service_cost_set<-set
      p<-project_model(inputs,z,calibration)$path
      transform(p,cost_set=if(set=='current')'Current assumptions' else 'Historical trend estimates')
    }))
  })
  output$cost_set_summary<-renderText({paste('Selected set:',if(a()$service_cost_set=='historical')'Historical trend estimates (2016-2025).' else 'Current assumptions.',
    'The table shows each baseline, aggregate adjustment and resulting rate. Fixed-share policies can override the spending total.')})
  output$cost_set_comparison<-renderPlot({ggplot(cost_sets(),aes(year,net_debt_pct,colour=cost_set))+
    geom_line(linewidth=1)+scale_colour_manual(values=c('Current assumptions'='#9b6171','Historical trend estimates'='#007e80'))+
    labs(x='Financial year ending',y='Net debt (% GDP)',colour=NULL)+theme_e61()})
  output$cost_set_endpoints<-renderTable({d<-subset(cost_sets(),year==2066)
    data.frame('Assumption set'=d$cost_set,'Primary spending (% GDP)'=round(100*d$primary_expense_bn/d$nominal_gdp,1),
      'Net debt (% GDP)'=round(d$net_debt_pct,1),check.names=FALSE)
  },striped=TRUE)
  output$cost_estimation_report<-downloadHandler(filename=function()'health_education_cost_estimation.pdf',
    content=function(file)file.copy('documentation/health_education_estimation.pdf',file,overwrite=TRUE))
  fixed_share_text<-reactive({
    d<-a()$spending_gdp_targets;d<-d[!is.na(d)]
    if(!length(d))return('All functions follow their projected spending rules.')
    paste0('Fixed from 2031: ',paste(sprintf('%s %.2f%% GDP',names(d),d),collapse='; '),
      '. These targets override the function totals. The policy contribution reconciles the override with underlying cost and demand drivers.')
  })
  for(programme in names(cost_pass_through_parameters()))local({
    n<-programme;key<-cost_pass_through_parameters()[[n]]
    output[[paste0(key,'_defaults')]]<-renderText({
      p<-effective_service_parameters(inputs,a());p<-p[p$id%in%calibration$subfunctions$id[calibration$subfunctions$programme==n],]
      base<-paste(sprintf('%.2f%%',100*sort(unique(p$additional_service_growth))),collapse=', ')
      selected<-paste(sprintf('%.2f%%',100*sort(unique(p$additional_service_growth+a()[[key]]))),collapse=', ')
      theta<-a()[[service_productivity_parameters()[[n]]]]
      active<-any(p$cost_rule=='input_mix')
      paste0('Service productivity: ',if(active)sprintf('default %.0f%% of economy-wide growth; selected %.0f%% (= %.2f%%/year). ',100*default_assumptions()[[service_productivity_parameters()[[n]]]],100*theta,100*theta*a()$productivity) else 'not applied under the selected indexation rule. ',
        'Additional growth: baseline ',base,'; adjustment ',sprintf('%+.2f pp',100*a()[[key]]),'; selected ',selected,'.')
    })
  })
  output$spending_override_summary<-renderText(fixed_share_text())
  output$active_spending_targets<-renderText(fixed_share_text())
  output$macro_identity <- renderText({
    m<-scenario()$path
    sprintf('Total NGDP growth: %.3f%% a year = (1 + %.1f%% per capita) x (1 + %.1f%% population) - 1. Average weekly hours: %.1f in 2030, %.1f in 2066. Implied inflation: %.2f%% in 2031, %.2f%% in 2066.',
      100*tail(m$nominal_gdp_growth,1),100*a()$ngdp_per_capita_growth,100*a()$population_growth,
      m$average_hours[1],tail(m$average_hours,1),100*m$domestic_inflation[2],100*tail(m$domestic_inflation,1))
  })
  output$metrics <- renderUI({
    s <- tail(scenario()$path,1); r <- tail(reference()$path,1)
    card <- function(label,value,change) div(class='metric',div(class='metric-label',label),
      div(class='metric-value',value),div(class='metric-change',change))
    div(class='metrics',
      card('2066 net debt / GDP',sprintf('%.1f%%',s$net_debt_pct),sprintf('%+.1f pp versus reference',s$net_debt_pct-r$net_debt_pct)),
      card('2066 fiscal balance / GDP',sprintf('%+.1f%%',s$fiscal_balance_pct),sprintf('%+.1f pp versus reference',s$fiscal_balance_pct-r$fiscal_balance_pct)),
      card('2066 nominal GDP',sprintf('$%.1f tn',s$nominal_gdp/1000),sprintf('%+.1f%% versus reference',100*(s$nominal_gdp/r$nominal_gdp-1))),
      card('2066 real primary spend / person',sprintf('$%s',format(round(s$real_primary_per_person),big.mark=',')),
           sprintf('%+.1f%% versus reference',100*(s$real_primary_per_person/r$real_primary_per_person-1))))
  })
  international_debt_data<-reactive({
    years<-c(2030L,2066L)
    view<-if(is.null(input$international_debt_view))'levels' else input$international_debt_view
    international_debt_comparison(scenario(),reference(),imf_debt_official,oecd_debt_official,years[1],years[2],view)
  })
  output$international_debt_plot<-renderPlot({
    d<-international_debt_data()
    palette<-c('Your scenario'='#007e80','Fixed reference'='#9b6171',
      'IMF: medium-term assumptions extended (approx.)'='#a87300',
      'IMF: debt-stabilising primary balance (approx.)'='#578c32',
      'IMF: historical-average assumptions (approx.)'='#bd5134',
      'OECD net financial liabilities (EO117 BAU1)'='#446a9b','OECD Survey: with fiscal adjustment'='#446a9b',
      'OECD Survey: without fiscal adjustment'='#95499a')
    types<-setNames(c('solid','dashed','solid','dotdash','dotted','longdash','longdash','dotted'),names(palette))
    ggplot(d,aes(year,plotted_value,colour=series_label,linetype=series_label,group=series_id))+
      geom_hline(yintercept=0,colour='#c5cece',linewidth=.3)+geom_line(linewidth=.95)+
      facet_wrap(~panel,ncol=1,scales='free_y')+
      scale_colour_manual(values=palette)+scale_linetype_manual(values=types)+
      labs(x='Year (model: FY ending; provider years unchanged)',y=d$plot_unit[1],colour=NULL,linetype=NULL)+
      theme_e61()+theme(legend.text=element_text(size=10))+guides(colour=guide_legend(ncol=2))
  })
  output$international_debt_summary<-renderText({
    sprintf('Long-term horizons: IMF %s (approximate chart extraction); OECD Survey gross-debt scenarios %s; OECD EO117 net financial liabilities %s (chart capped at 2066). Table shows each series\' last available level, not a common endpoint. Short-run forecasts are excluded.',
      max(imf_debt_official$year),max(subset(oecd_debt_official,concept=='gross_debt')$year),max(subset(oecd_debt_official,concept=='net_financial_liabilities')$year))
  })
  output$international_debt_endpoints<-renderTable({
    d<-international_debt_data();d<-d[!duplicated(d$series_id,fromLast=TRUE),]
    data.frame(Series=d$series_label,Measure=gsub('_',' ',d$concept),'Last year in view'=d$year,
      'Published/model level (% GDP)'=round(d$value_pct_gdp,2),Vintage=d$vintage,check.names=FALSE)
  },striped=TRUE)
  output$imf_debt_download<-downloadHandler(filename=function()'imf_long_term_debt_projections.csv',
    content=function(file)file.copy('data/processed/imf_long_term_debt_projections.csv',file,overwrite=TRUE))
  output$oecd_debt_download<-downloadHandler(filename=function()'oecd_debt_projections.csv',
    content=function(file)file.copy('data/processed/oecd_debt_projections.csv',file,overwrite=TRUE))
  output$international_debt_download<-downloadHandler(filename=function()'e61_international_debt_comparison.csv',
    content=function(file)write.csv(international_debt_data(),file,row.names=FALSE))
  output$outlook <- renderPlot({
    d <- both(); d$value <- d[[input$measure]]
    label <- if(input$measure=='real_primary_per_person') '2030 dollars per person' else '% of GDP'
    ggplot(d,aes(year,value,colour=case,linetype=case))+geom_line(linewidth=1.1)+
      scale_colour_manual(values=colours)+scale_linetype_manual(values=c('Reference'='dashed','Your scenario'='solid'))+
      labs(x='Financial year ending',y=label,colour=NULL,linetype=NULL)+theme_e61()
  })
  output$endpoint <- renderText({
    d <- tail(scenario()$path,1)
    sprintf('2066: revenue %.1f%% of GDP; total spending %.1f%%; primary expenses %.1f%%; interest %.1f%%; net capital investment %.1f%%.',
      d$revenue_pct,d$total_spending_pct,d$primary_expense_pct,d$interest_pct,d$net_capital_investment_pct)
  })
  output$attribution <- renderPlot({
    d <- scenario_attribution(inputs,a(),calibration)
    if(all(abs(d$net_debt_change_pp)<1e-10)) return(
      ggplot()+annotate('text',x=0,y=0,label='Reference settings selected\nChange a scenario control to see its contribution',
                       colour='#60717b',size=4.5,lineheight=1.5)+theme_void())
    ggplot(d,aes(net_debt_change_pp,reorder(driver,net_debt_change_pp)))+
      geom_vline(xintercept=0,colour='#ccd7d5')+geom_col(fill='#007e80',width=.55)+
      labs(x='Change in 2066 net debt / GDP (percentage points)',y=NULL)+theme_e61()
  })
  output$macro <- renderPlot({
    d <- both()
    v <- c('real_output_index','domestic_price_index','nominal_gdp','government_unit_price_index')
    labels <- c('Real output','Domestic prices (proxy)','Nominal GDP','Government unit price (70% wages)')
    long <- do.call(rbind,lapply(seq_along(v),function(i) {
      value <- d[[v[i]]]
      if(v[i]=='nominal_gdp') value <- value/calibration$gdp0
      data.frame(year=d$year,case=d$case,channel=labels[i],index=100*value)
    }))
    ggplot(long,aes(year,index,colour=case,linetype=case))+geom_line(linewidth=1)+
      facet_wrap(~channel,scales='free_y',ncol=2)+scale_colour_manual(values=colours)+
      scale_linetype_manual(values=c('Reference'='dashed','Your scenario'='solid'))+
      labs(x='Financial year ending',y='Index (2030 = 100)',colour=NULL,linetype=NULL)+theme_e61()
  })
  output$price_message <- renderText({
    m <- scenario()$path
    sprintf('Implied domestic inflation: %.2f%% in 2031 and %.2f%% in 2066. The GDP deflator also includes the terms-of-trade income effect. Participation follows the sourced age structure; average hours follow your separate control.',
            100*m$domestic_inflation[2],100*tail(m$domestic_inflation,1))
  })
  debt_both <- reactive({
    d<-rbind(transform(debt_diagnostics(scenario()),case='Your scenario'),
      transform(debt_diagnostics(reference()),case='Reference'))
    d
  })
  debt_plot <- function(d,fields,labels,unit) {
    long<-do.call(rbind,lapply(seq_along(fields),function(i)
      data.frame(year=d$year,case=d$case,measure=labels[i],value=d[[paste0(fields[i],'_',unit)]])))
    long$measure<-factor(long$measure,levels=labels)
    ggplot(long,aes(year,value,colour=case,linetype=case))+geom_line(linewidth=.9)+
      facet_wrap(~measure,ncol=3)+scale_colour_manual(values=colours)+
      scale_linetype_manual(values=c('Reference'='dashed','Your scenario'='solid'))+
      labs(x='Financial year ending',y=if(unit=='pct')'% of GDP' else 'AUD billion',colour=NULL,linetype=NULL)+theme_e61()
  }
  output$debt_stocks <- renderPlot(debt_plot(debt_both(),
    c('gross_debt','financial_assets','net_debt'),c('Gross debt','Financial assets','Net debt'),input$debt_units))
  output$debt_interest <- renderPlot(debt_plot(debt_both(),
    c('interest','interest_receipts','net_interest'),c('Interest payments','Interest receipts','Net interest'),input$debt_units))
  output$debt_balance_table <- renderTable({
    d<-subset(debt_both(),year==as.integer(input$debt_year))
    data.frame(Case=d$case,'GDP ($bn)'=round(d$nominal_gdp,1),
      'Gross debt ($bn)'=round(d$gross_debt_bn,1),'Assets ($bn)'=round(d$financial_assets_bn,1),
      'Net debt ($bn)'=round(d$net_debt_bn,1),'Gross debt (% GDP)'=round(d$gross_debt_pct,2),
      'Net debt (% GDP)'=round(d$net_debt_pct,2),check.names=FALSE)
  },striped=TRUE)
  output$debt_interest_table <- renderTable({
    d<-subset(debt_both(),year==as.integer(input$debt_year))
    data.frame(Case=d$case,'Opening gross debt ($bn)'=round(d$opening_gross_debt_bn,1),
      'Base borrowing rate (%)'=round(100*d$base_interest_rate,2),
      'Debt feedback (basis points)'=round(10000*d$debt_interest_adjustment,2),
      'Opening net-debt gap vs 2030 (pp GDP)'=round(d$net_debt_gap_pp,2),
      'Borrowing rate (%)'=round(100*d$effective_interest_rate,2),
      'Interest paid ($bn)'=round(d$interest_bn,1),'Interest paid (% GDP)'=round(d$interest_pct,2),
      'Interest received ($bn)'=round(d$interest_receipts_bn,1),
      'Asset yield (%)'=round(d$asset_yield_pct,2),'Net interest (% GDP)'=round(d$net_interest_pct,2),
      'Implied domestic inflation (%)'=round(100*d$domestic_inflation,2),
      'Real borrowing rate (%)'=round(d$real_borrowing_rate_pct,2),check.names=FALSE)
  },striped=TRUE,na='Anchor')
  output$debt_explanation <- renderText({
    d<-debt_both();s<-subset(d,case=='Your scenario' & year==as.integer(input$debt_year))
    r<-subset(d,case=='Reference' & year==as.integer(input$debt_year))
    message<-sprintf('In %s your scenario has $%.1fbn gross debt minus $%.1fbn financial assets = $%.1fbn net debt. Interest paid is $%.1fbn / $%.1fbn GDP = %.2f%%, compared with %.2f%% in the fixed reference. After $%.1fbn asset interest receipts, net interest is %.2f%% of GDP.',
      s$year,s$gross_debt_bn,s$financial_assets_bn,s$net_debt_bn,s$interest_bn,s$nominal_gdp,s$interest_pct,r$interest_pct,s$interest_receipts_bn,s$net_interest_pct)
    paste(message,sprintf('The base borrowing rate is %.2f%%; debt feedback adds %+.2f basis points, giving %.2f%%. The sensitivity is %.2f bps per percentage point of lagged net debt/GDP relative to the 2030 anchor.',
      100*s$base_interest_rate,10000*s$debt_interest_adjustment,100*s$effective_interest_rate,a()$interest_debt_sensitivity_bps))
  })
  output$debt_download <- downloadHandler(filename=function()'e61_debt_interest_detail.csv',content=function(file){
    d<-debt_both()
    fields<-c('year','case','nominal_gdp','gross_debt_bn','financial_assets_bn','net_debt_bn',
      'gross_debt_pct','financial_assets_pct','net_debt_pct','opening_gross_debt_bn','opening_financial_assets_bn',
      'base_interest_rate','debt_interest_adjustment','net_debt_gap_pp',
      'effective_interest_rate','asset_yield_pct','domestic_inflation','real_borrowing_rate_pct',
      'interest_bn','interest_pct','interest_receipts_bn','interest_receipts_pct','net_interest_bn','net_interest_pct')
    write.csv(d[,fields],file,row.names=FALSE)
  })
  output$spending_paths <- renderPlot({
    d<-rbind(transform(scenario()$components,case='Your scenario'),
      transform(reference()$components,case='Reference'))
    dollars<-identical(input$spending_units,'bn')
    d$value<-if(dollars)d$spending_bn else d$share_gdp
    ggplot(d,aes(year,value,colour=case,linetype=case))+geom_line(linewidth=.9)+
      facet_wrap(~programme,scales='free_y',ncol=3)+scale_colour_manual(values=colours)+
      scale_linetype_manual(values=c('Reference'='dashed','Your scenario'='solid'))+
      labs(x='Financial year ending',y=if(dollars)'Spending (nominal $bn)' else 'Spending (% GDP)',colour=NULL,linetype=NULL)+theme_e61()
  })
  spending_ngdp_comparison<-reactive({
    baseline<-a();baseline$ngdp_per_capita_growth<-default_assumptions()$ngdp_per_capita_growth
    project_model(inputs,baseline,calibration)
  })
  output$spending_ngdp_link<-renderText({
    s<-tail(scenario()$path,1);r<-tail(spending_ngdp_comparison()$path,1)
    sprintf('In 2066, changing only NGDP per capita growth from %.1f%% to your %.1f%% changes nominal GDP from $%.1fbn to $%.1fbn and primary spending from $%.1fbn to $%.1fbn. Primary spending/GDP moves from %.2f%% to %.2f%%. Domestic prices (2030 = 100) move from %.1f to %.1f.',
      100*default_assumptions()$ngdp_per_capita_growth,100*a()$ngdp_per_capita_growth,
      r$nominal_gdp,s$nominal_gdp,r$primary_expense_bn,s$primary_expense_bn,
      r$primary_expense_pct,s$primary_expense_pct,100*r$domestic_price_index,100*s$domestic_price_index)
  })
  output$spending_ngdp_comparison<-renderTable({
    s<-subset(scenario()$components,year==2066);r<-subset(spending_ngdp_comparison()$components,year==2066)
    r<-r[match(s$programme,r$programme),]
    data.frame(Programme=s$programme,'At 3.2% NGDP/person ($bn)'=round(r$spending_bn,1),
      'Selected NGDP/person ($bn)'=round(s$spending_bn,1),
      'At 3.2% (% GDP)'=round(r$share_gdp,2),'Selected (% GDP)'=round(s$share_gdp,2),check.names=FALSE)
  },striped=TRUE)
  output$spending_drivers <- renderTable({
    d<-spending_factor_data();u<-d[!duplicated(d$id),]
    unit<-switch(d$measure[1],pct='% GDP',bn='$bn',real_bn='$2030 bn')
    result<-data.frame(Category=u$programme,Subfunction=u$subfunction,check.names=FALSE)
    result[[sprintf('%s (%s)',d$start_year[1],unit)]]<-round(u$start_value,3)
    result[[sprintf('%s (%s)',d$end_year[1],unit)]]<-round(u$end_value,3)
    for(f in unname(spending_factor_labels())){
      z<-d[d$factor==f,];z<-z[match(u$id,z$id),]
      result[[paste0(f,' (x)')]]<-round(ifelse(z$factor_start==0,NA_real_,z$factor_end/z$factor_start),3)
    }
    result
  },striped=TRUE,na='\u2014',digits=3)
  spending_factor_data<-reactive({
    start<-if(is.null(input$contribution_start))2030L else as.integer(input$contribution_start)
    end<-if(is.null(input$contribution_end))2066L else as.integer(input$contribution_end)
    validate(need(start<end,'Choose an end year later than the start year.'))
    measure<-if(is.null(input$contribution_measure))'pct' else input$contribution_measure
    group<-if(is.null(input$spending_group))'primary' else input$spending_group
    d<-spending_contributions(scenario(),start,end,measure,include_financing=group=='all')
    if(!group%in%c('primary','all'))d<-d[d$programme==group,]
    d
  })
  output$spending_contribution_summary<-renderText({
    d<-spending_factor_data();u<-d[!duplicated(d$id),]
    unit<-switch(d$measure[1],pct='percentage points of GDP',bn='nominal AUD billion',real_bn='2030-dollar billion')
    sprintf('From %s to %s, selected spending changes from %.2f to %.2f (%+.2f %s). Factor contributions below sum to that change.',
      d$start_year[1],d$end_year[1],sum(u$start_value),sum(u$end_value),sum(u$change),unit)
  })
  output$spending_contribution_plot<-renderPlot({
    d<-aggregate(contribution~factor,spending_factor_data(),sum)
    d$factor<-factor(d$factor,levels=rev(unname(spending_factor_labels())))
    ggplot(d,aes(factor,contribution,fill=contribution>=0))+geom_col()+coord_flip()+
      scale_fill_manual(values=c('TRUE'='#007e80','FALSE'='#9b6171'),guide='none')+
      labs(x=NULL,y=switch(spending_factor_data()$measure[1],pct='Contribution (percentage points of GDP)',bn='Contribution (nominal $bn)',real_bn='Contribution ($2030 bn)'))+theme_e61()
  })
  output$spending_contribution_table<-renderTable({
    d<-aggregate(contribution~factor,spending_factor_data(),sum)
    d<-d[match(unname(spending_factor_labels()),d$factor),]
    data.frame(Factor=d$factor,Contribution=round(d$contribution,3),check.names=FALSE)
  },striped=TRUE)
  output$spending_subfunctions<-renderTable({
    d<-scenario()$subfunctions;group<-input$spending_group
    if(!is.null(group)&&!group%in%c('primary','all'))d<-d[d$programme==group,]
    first<-d[d$year==2030,];last<-d[d$year==2066,];last<-last[match(first$id,last$id),]
    data.frame(Category=first$programme,Subfunction=first$subfunction,
      '2030 ($bn)'=round(first$spending_bn,1),'2066 ($bn)'=round(last$spending_bn,1),
      '2030 (% GDP)'=round(first$share_gdp,2),'2066 (% GDP)'=round(last$share_gdp,2),check.names=FALSE)
  },striped=TRUE)
  output$spending_factor_download<-downloadHandler(filename=function()'e61_spending_factor_contributions.csv',
    content=function(file)write.csv(spending_factor_data(),file,row.names=FALSE))
  output$spending_detail_download<-downloadHandler(filename=function()'e61_spending_subfunctions.csv',
    content=function(file){
      d<-merge(scenario()$subfunctions,calibration$subfunctions[,c('id','profile','source','locator','status','limits')],by='id',sort=FALSE)
      write.csv(d,file,row.names=FALSE)
    })
  output$spending_background<-downloadHandler(filename=function()'e61_spending_background.pdf',
    content=function(file)file.copy('documentation/spending_background.pdf',file,overwrite=TRUE))
  health_education_evidence<-reactive({service_cost_comparison(scenario(),inputs)})
  output$health_education_costs<-renderTable({
    d<-health_education_evidence();group<-input$spending_group
    if(!is.null(group)&&!group%in%c('primary','all'))d<-d[d$programme==group,]
    validate(need(nrow(d)>0,'Select a service function to inspect its cost assumptions.'))
    format_service_cost_comparison(d)
  },striped=TRUE,na='Not available')
  output$health_education_download<-downloadHandler(filename=function()'e61_service_cost_parameters_and_benchmarks.csv',
    content=function(file)write.csv(health_education_evidence(),file,row.names=FALSE))
  output$spending_driver_period <- renderText({
    d<-spending_factor_data()
    sprintf('Factor changes from %s to %s, for the selected spending coverage and measure. Scroll horizontally to inspect all nine factors.',d$start_year[1],d$end_year[1])
  })
  output$spending_driver_download<-downloadHandler(filename=function()'e61_spending_factor_levels.csv',
    content=function(file){
      d<-spending_factor_data();d$factor_ratio<-ifelse(d$factor_start==0,NA_real_,d$factor_end/d$factor_start)
      write.csv(d,file,row.names=FALSE)
    })
  output$components <- renderPlot({
    extract <- function(z,label) {
      x <- subset(z$components,year==2066)[,c('programme','share_gdp')]
      end <- tail(z$path,1)
      rbind(transform(x,case=label),data.frame(programme=c('Interest','Net capital investment'),
        share_gdp=c(end$interest_pct,end$net_capital_investment_pct),case=label))
    }
    d <- rbind(extract(scenario(),'Your scenario'),extract(reference(),'Reference'))
    ggplot(d,aes(share_gdp,reorder(programme,share_gdp),fill=case))+
      geom_col(position='dodge',width=.7)+scale_fill_manual(values=colours)+
      labs(x='% of GDP',y=NULL,fill=NULL)+theme_e61()
  })
  output$ndis_growth_readout <- renderText({
    d<-subset(scenario()$components,programme=='NDIS')
    cost<-function(y)d$spending_bn[d$year==y]
    early<-100*((cost(2035)/cost(2030))^(1/5)-1)
    later<-100*((cost(2066)/cost(2035))^(1/31)-1)
    sprintf('Implied total NDIS spending growth: %.1f%% a year in 2030-35 and %.1f%% a year in 2035-66. Spending in 2034-35: $%.1fbn.',early,later,cost(2035))
  })
  output$ndis_summary <- renderText({
    d<-subset(scenario()$components,programme=='NDIS')
    growth<-100*((d$spending_bn[d$year==2035]/d$spending_bn[d$year==2030])^(1/5)-1)
    sprintf('Your scenario: $%.1fbn in 2030 and $%.1fbn in 2035; average nominal growth %.1f%% a year over those five years. Additional service growth multiplies participant demand and explicit unit costs.',
      d$spending_bn[d$year==2030],d$spending_bn[d$year==2035],growth)
  })
  igr_comparisons<-reactive({
    setNames(lapply(names(igr_notes),function(programme)
      igr_spending_comparison(scenario(),reference(),igr_spending_official,programme)),names(igr_notes))
  })
  for(programme in setdiff(names(igr_notes),'NDIS'))local({
    category<-programme;id<-make.names(category)
    output[[paste0('igr_summary_',id)]]<-renderText(igr_spending_summary(scenario(),igr_spending_official,category))
    output[[paste0('igr_table_',id)]]<-renderTable({
      o<-igr_spending_official[igr_spending_official$programme==category,]
      label<-if(nrow(o))o$igr_label[1] else 'No matching aggregate'
      format_igr_spending_table(igr_comparisons()[[category]],label)
    },striped=TRUE,na='Not available')
  })
  output$igr_spending_download<-downloadHandler(filename=function()'e61_igr_spending_comparisons.csv',
    content=function(file){
      d<-do.call(rbind,lapply(names(igr_notes),function(programme){
        z<-igr_comparisons()[[programme]];z$programme<-programme
        o<-igr_spending_official[igr_spending_official$programme==programme,]
        z$igr_label<-if(nrow(o))o$igr_label[1] else NA_character_
        z$coverage_note<-igr_notes[[programme]]$coverage
        z$igr_source<-igr_notes[[programme]]$source
        z$source_url<-'https://treasury.gov.au/publication/2026-intergenerational-report'
        z
      }))
      write.csv(d,file,row.names=FALSE)
    })
  output$ndis_benchmark <- renderTable({
    format_igr_spending_table(igr_comparisons()[['NDIS']],'Total NDIS Scheme')
  },striped=TRUE)
  revenue_long <- reactive({
    z<-scenario()$revenue
    labels<-c(personal_income='Personal income tax',company='Company tax',superannuation='Superannuation tax',
      medicare_fbt='Medicare and FBT',gst='GST',excises_customs='Excises and customs',payroll='Payroll tax',
      property='Property taxes',stamp_duties='Stamp duties',other_taxes='Other taxes',royalties='Royalties',
      fees_other='Fees and other revenue',interest_receipts='Interest receipts')
    do.call(rbind,lapply(setdiff(names(z),'year'),function(n)data.frame(year=z$year,
      head=unname(labels[n]),revenue_bn=z[[n]],share_gdp=100*z[[n]]/scenario()$path$nominal_gdp)))
  })
  output$revenue_path<-renderPlot({
    z<-both();d<-data.frame(year=z$year,case=z$case,value=z$revenue_pct)
    s<-scenario();w<-s$path
    d<-rbind(d,data.frame(year=w$year,case='Wage-indexed thresholds',
      value=w$revenue_pct-100*s$revenue_diagnostics$bracket_creep_revenue_bn/w$nominal_gdp))
    ggplot(d,aes(year,value,colour=case,linetype=case))+geom_line(linewidth=1)+
      scale_colour_manual(values=c(colours,'Wage-indexed thresholds'='#cb983e'))+
      labs(x='Financial year ending',y='Revenue (% GDP)',colour=NULL,linetype=NULL)+theme_e61()
  })
  output$revenue_summary<-renderText({
    s<-tail(scenario()$path,1);d<-tail(scenario()$revenue_diagnostics,1)
    sprintf('2066 revenue: $%.0f billion (%.1f%% of GDP). Threshold policy adds $%.0f billion relative to wage indexation. This isolates the direct income-tax effect; a full financing counterfactual would also change interest receipts.',
      s$revenue_bn,s$revenue_pct,d$bracket_creep_revenue_bn)
  })
  output$company_tax_summary<-renderText({
    s<-scenario();m<-tail(s$path,1)
    base<-100*calibration$revenue_anchor['company']/calibration$gdp0
    share<-100*tail(s$revenue$company,1)/m$nominal_gdp
    sprintf('Structural starting share: %.2f%% of GDP. In 2066 your scenario has %.2f%% of GDP ($%.1fbn), with the terms-of-trade index at %.1f (2030 = 100) and a share elasticity of %.1f.',
      base,share,tail(s$revenue$company,1),100*m$tot_index,a()$company_tot_elasticity)
  })
  output$company_tax_table<-renderTable({
    s<-scenario();k<-which(s$path$year%in%c(2030,2040,2066));m<-s$path[k,]
    data.frame(Year=m$year,'Terms of trade (2030=100)'=round(100*m$tot_index,1),
      'Company tax (% GDP)'=round(100*s$revenue$company[k]/m$nominal_gdp,2),
      'Company tax ($bn)'=round(s$revenue$company[k],1),check.names=FALSE)
  },striped=TRUE)
  output$revenue_mix<-renderPlot({
    ggplot(revenue_long(),aes(year,share_gdp,fill=head))+geom_area()+
      scale_fill_manual(values=setNames(grDevices::hcl.colors(13,'Dark 3'),unique(revenue_long()$head)))+
      labs(x='Financial year ending',y='% GDP',fill=NULL)+
      theme_e61()+theme(legend.text=element_text(size=9))+guides(fill=guide_legend(nrow=4))
  })
  output$tax_rules<-renderTable({
    d<-scenario()$revenue_diagnostics;d<-d[d$year%in%c(2030,2040,2066),]
    data.frame(Year=d$year,'Mean taxable income ($)'=round(d$mean_taxable_income),
      'Threshold index (2030=1)'=round(d$threshold_index,2),
      'Model average tax rate (%)'=round(100*d$effective_income_tax_rate,1),
      'Extra tax vs wage indexation ($bn)'=round(d$bracket_creep_revenue_bn,1),check.names=FALSE)
  },striped=TRUE)
  output$revenue_mechanisms<-renderTable(data.frame(
    Head=c('Personal income','Company','Other taxes','GST','Excises / customs','Payroll / Medicare-FBT / super',
      'Property / stamp duty','Royalties','Fees and other','Interest receipts'),
    Base=c('Observed income percentiles; statutory schedule; threshold rule','Structural share of GDP; level responds to terms of trade','Nominal GDP proxy for other tax bases',
      'Consumption proxy proportional to nominal GDP','Taxable units proportional to population; CPI-indexed rates','Employment times average nominal earnings',
      'Property values proportional to GDP; constant turnover and yield','Domestic output and terms of trade',
      'Domestic nominal output','Opening interest-bearing financial assets'),
    Evidence=c('ATO Table 16 + ABS tax totals; chosen threshold policy','Calibrated 2030 share; illustrative ToT sensitivity','Observed anchor; constant GDP-share assumption',
      'Observed anchor; constant consumption/GDP and coverage assumptions','Observed anchor; constant units/person and indexed-rate assumptions','Observed anchors; constant effective coverage/yield; combined-head proxies',
      'Observed anchors; property-base and turnover assumptions','Observed GFS level; ToT elasticity 2 is an assumption',
      'Observed GFS non-tax level; unit elasticity','Starting receipts calibrated to GFS/PBO; constant asset yield')),
    striped=TRUE)
  output$revenue_endpoints<-renderTable({
    d<-revenue_long();years<-range(d$year)
    first<-d[d$year==years[1],];last<-d[d$year==years[2],]
    last<-last[match(first$head,last$head),]
    out<-data.frame(first$head,first$revenue_bn,first$share_gdp,
      last$revenue_bn,last$share_gdp,check.names=FALSE)
    out[nrow(out)+1,]<-list('Total revenue',sum(first$revenue_bn),sum(first$share_gdp),
      sum(last$revenue_bn),sum(last$share_gdp))
    names(out)<-c('Revenue type',paste(years[1],'revenue ($bn)'),paste(years[1],'(% GDP)'),
      paste(years[2],'revenue ($bn)'),paste(years[2],'(% GDP)'))
    for(j in 2:5)out[[j]]<-formatC(out[[j]],format='f',digits=if(j%in%c(2,4))1 else 2,big.mark=',')
    out
  },striped=TRUE,rownames=FALSE)
  output$coverage_bridge<-renderTable({
    d<-read.csv('data/processed/consolidation_bridge.csv');d<-d[d$year==2030,]
    data.frame(Measure=c('Commonwealth','States','National consolidated','Net adjustment'),
      'Revenue ($bn)'=round(c(d$revenue_billion_dollars,d$states_revenue_bn,d$national_revenue_bn,d$revenue_consolidation_adjustment_bn),1),
      'Expenses ($bn)'=round(c(d$expenses_billion_dollars,d$states_expenses_bn,d$national_expenses_bn,d$expense_consolidation_adjustment_bn),1),check.names=FALSE)
  },striped=TRUE)
  output$assumption_register<-renderTable({
    d<-read.csv('data/assumption_register.csv',check.names=FALSE)
    if(!is.null(input$register_domain)&&input$register_domain!='All')d<-d[d$domain==input$register_domain,]
    data.frame(Item=d$item,Status=d$classification,'Value / rule'=d$value_rule,
      Source=d$source,'Our addition'=d$model_addition,Limit=d$limitation,check.names=FALSE)
  },striped=TRUE)
  output$register_download<-downloadHandler(filename=function()'e61_evidence_and_assumptions.csv',
    content=function(file)file.copy('data/assumption_register.csv',file,overwrite=TRUE))
  output$parameters <- renderTable({
    pp<-calibration$programmes
    data.frame(Programme=pp$programme,'2030 allocation ($bn)'=round(pp$anchor_2030_bn,1),
      'Service productivity (% economy)'=vapply(pp$programme,function(n)if(n%in%names(service_productivity_parameters()))100*a()[[service_productivity_parameters()[[n]]]] else NA_real_,numeric(1)),
      'Additional growth adjustment (pp/yr)'=vapply(pp$programme,function(n)if(n%in%names(cost_pass_through_parameters()))100*a()[[cost_pass_through_parameters()[[n]]]] else NA_real_,numeric(1)),check.names=FALSE)
  },striped=TRUE,na='Own policy rule')
  output$sources <- renderTable(data.frame(
    Input=c('Consolidated PBO anchor','Population and participation','Observed employment rates and hours','Spending and tax composition','Health and aged care profiles','NDIS','Income-tax distribution','Pension rules'),
    Source=c('2026-27 National Fiscal Outlook','2026 IGR + Centre for Population sex ratios',
      'ABS April 2025 to March 2026 age/sex averages','ABS GFS and Taxation Revenue 2024-25',
      '2026 IGR C12.10 and C6.6; Commonwealth health profile used as a national proxy',
      'NDIA 2024-25 accounts and quarterly age participation rates','ATO Taxation Statistics 2023-24 Table 16',
      'Services Australia / DSS parameters at September 2026')),
    striped=TRUE)
  output$download <- downloadHandler(filename=function() 'e61_fiscal_scenario.csv',content=function(file) {
    d <- both()
    for(n in names(a())) d[[paste0('scenario_assumption_',n)]] <- a()[[n]]
    write.csv(d,file,row.names=FALSE)
  })
  output$audit <- downloadHandler(filename=function() 'e61_fiscal_assumptions.txt',content=function(file) {
    capture.output(list(scenario=a(),reference=ref_a(),programme_calibration=calibration$programmes,
      tax_calibration=calibration$revenue_anchor,historical_elasticities_not_used=inputs$tax_elasticities,
      evidence_register=read.csv('data/assumption_register.csv'),
      source_manifest=read.csv('data/raw/v2/download_manifest.csv')),file=file)
  })
  output$revenue_download<-downloadHandler(filename=function()'e61_revenue_detail.csv',content=function(file){
    d<-merge(revenue_long(),scenario()$revenue_diagnostics,by='year');write.csv(d,file,row.names=FALSE)
  })
}
shinyApp(ui,server)
