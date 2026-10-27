dir.create(file.path('outputs','cache'),recursive=TRUE,showWarnings=FALSE)
Sys.setenv(R_USER_CACHE_DIR=normalizePath(file.path('outputs','cache')))
library(shiny)
library(ggplot2)
source(file.path('R','model.R'))
inputs <- load_inputs()
calibration <- calibrate(inputs)

theme_e61 <- function() theme_minimal(base_size=13) + theme(
  panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(),
  plot.title=element_text(face='bold',colour='#122b3f'),
  axis.title=element_text(colour='#41515d'), legend.position='bottom',
  plot.margin=margin(12,18,8,8))
colours <- c('Your scenario'='#007e80','Reference'='#9b6171')
reference_text <- 'Fixed reference: productivity 1.2%; linked NGDP; service price premium 0.3%; unchanged terms of trade; CPI-indexed tax thresholds and default policy/financing settings. Only your scenario changes when you adjust a control.'

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
      div(class='model-badge','Research prototype · 25 October 2026')),
  h1('What changes Australia’s fiscal future?'),
  p(class='subhead','Explore how economic assumptions flow through services, revenue and debt. Consolidated general government · financial years ending 2030–2066.'),
  p(class='note','Illustrative scenarios: the reference is a modelling starting point, not an e61 forecast. Results depend on the programme and policy assumptions in Method & Data.'),
  sidebarLayout(
    sidebarPanel(width=3,
      h3('Build a scenario'),
      sliderInput('productivity','Labour productivity growth (% a year)',min=-0.5,max=3,value=1.2,step=0.1),
      helpText('Output per hour. Employment uses observed age/sex rates and 2026 IGR participation targets; average hours follow the IGR trend.'),
      checkboxInput('ngdp_override','Set NGDP growth separately',FALSE),
      conditionalPanel('input.ngdp_override',
        sliderInput('ngdp','NGDP growth (% a year)',min=1,max=8,value=4.4,step=0.1)),
      helpText('When linked, NGDP follows real output, 2.5% domestic inflation and the terms-of-trade income effect. An NGDP override instead determines the implied price path.'),
      sliderInput('relative_price','Extra government service price growth (% a year)',min=-0.5,max=2,value=0.3,step=0.1),
      helpText('Added to the service-specific price path, which already includes wage pass-through. Does not apply to Age Pension cash payments.'),
      sliderInput('tot','Terms-of-trade level change by 2040 (%)',min=-30,max=30,value=0,step=5),
      helpText('A permanent export/import price shift, phased in over ten years. Affects nominal income and resource revenue; leaves real output unchanged.'),
      tags$details(tags$summary('Policy and financing assumptions'),
        selectInput('policy','Service and benefit convention',
          c('Indexed rules (illustrative)'='indexed','Constant relative standards'='standards')),
        helpText('Indexed rules apply pension means tests and cohort superannuation accumulation. Relative standards preserve pension entitlements per eligible person and fully wage-link other social protection.'),
        selectInput('tax_policy','Personal tax thresholds',
          c('CPI indexation (default policy assumption)'='cpi','Wage indexation (policy alternative)'='wage_indexed',
            'Fixed nominal thresholds (legislated convention)'='fixed')),
        sliderInput('super_return','Real superannuation return (%)',min=0,max=7,value=4,step=0.5),
        helpText('Affects private retirement balances and Age Pension means tests under indexed rules. Higher returns reduce pensions and debt. Does not change GDP, superannuation tax receipts or government asset returns; inactive under constant relative standards.'),
        sliderInput('ndis_intensity','NDIS extra cost per participant (% a year)',min=0,max=2,value=0.5,step=0.1),
        div(class='help-block',textOutput('ndis_growth_readout')),
        helpText('The slider adds to wages and prices. Total spending growth also includes participant numbers, productivity and service prices, so the implied growth updates when those assumptions change. Figures above are nominal compound annual growth rates. See Spending for official comparisons.'),
        sliderInput('interest','Long-run effective borrowing rate (%)',min=2,max=7,value=4.4,step=0.1)),
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
          div(class='panel-card',h3('What drives the change in debt?'),
              plotOutput('attribution',height=240),
              p(class='note','An exact Shapley allocation of economic controls and policy/financing changes, including their interactions, relative to the fixed default reference. This explains model arithmetic, not estimated causal effects.'))),
        tabPanel('Economic channels',value='channels',
          div(class='panel-card',h3('Real output, nominal output and prices'),
              plotOutput('macro',height=390),textOutput('price_message')),
          div(class='panel-card',h3('The productivity mechanism'),
            p('Higher productivity raises real output and wages. Wage exposure differs by spending programme. Revenue responds to its tax base; interest responds to the debt stock.'),
            p('The NGDP override fixes nominal output growth. At a given NGDP path, higher real growth means lower implied inflation. This is a different experiment from raising productivity while holding inflation fixed.'),
            p('Terms of trade affect nominal income with an illustrative elasticity of 0.20 and resource revenue with an elasticity of 2. Under a fixed NGDP override, the domestic price path adjusts to preserve that NGDP target.'))),
        tabPanel('Debt & interest',value='debt',
          div(class='panel-card',h3('Gross debt, financial assets and net debt'),
            conditionalPanel('input.ngdp_override',checkboxInput('debt_linked','Also compare the same scenario with linked NGDP',TRUE)),
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
            plotOutput('spending_paths',height=620),
            p(class='note','Each panel has its own vertical scale. Both paths include demographic ageing and programme cost growth; overlapping lines mean the scenario matches the reference, not that spending is constant over time. Years identify financial years by ending year.')),
          div(class='panel-card',h3('Number, unit cost and intensity: inspect the mechanism'),
            div(style='overflow-x:auto',tableOutput('spending_drivers')),textOutput('spending_denominator'),
            p(class='note','Factors are multiples of 2030 levels. For services: spending/GDP equals its starting share times number, real unit cost and intensity, divided by GDP after removing domestic price growth. Number means age-weighted demand or participants, not always headcount. Real unit cost includes productivity-linked wages and extra service prices. Age Pension uses eligible population, recipient coverage and mean payment; its real unit cost also reflects recipient composition. Defence instead follows a GDP-share target.'),
            p('Demographic assumptions are shared by your scenario and the reference and currently have no sidebar control. Changing NGDP growth alone changes implied inflation: domestic prices cancel from these spending/GDP ratios. Productivity, relative service costs and NDIS intensity can change the ratios. Interest still responds to nominal debt and the borrowing rate.')),
          div(class='panel-card',h3('Spending components in 2066'),plotOutput('components',height=330),
            p(class='note','NDIS and aged care are now separate modules, removed from residual social protection. Total expenditure also includes conventional interest and net capital investment.')),
          div(class='panel-card',h3('NDIS: checking the projection'),textOutput('ndis_summary'),
            tableOutput('ndis_benchmark'),
            p(class='note','Whole-Scheme spending as a share of each projection\'s own GDP; IGR figures are rounded to one decimal. These are not Commonwealth net funding shares. Our allocation starts at the common PBO aggregate anchor, with fixed under-65 age-specific participation rates and 0.5% extra intensity growth. It does not reproduce the Scheme Actuary\'s entrant, exit, disability or support-cost model, or explicitly cost the 2026 reforms.'),
            p('The NDIA 2024-25 actuarial report projected $95.8bn in 2034-35 and 7.2% growth that year. That older projection predates the 2026 reforms. The PBO July 2026 outlook describes reform targets below 2% growth over the forward estimates and approximately 5% subsequently. Our default is lower than the older path and broadly close to the newer long-run GDP shares, but is not a validated replication.'),
            p(tags$a(href='https://ndis.gov.au/media/8183/download?attachment=',target='_blank','NDIA 2024-25 actuarial report'), ' | ',
              tags$a(href='https://www.pbo.gov.au/publications-and-data/publications/2026-27-medium-term-budget-outlook',target='_blank','PBO July 2026 outlook'), ' | ',
              tags$a(href='https://treasury.gov.au/publication/2026-intergenerational-report',target='_blank','2026 IGR chart C12.17'))),
          div(class='panel-card',h3('Revenue in 2066'),tableOutput('revenue_table'),
              p(class='note','Observed ABS tax-head allocations and an ATO taxable-income distribution replace fixed tax shares. The Revenue tab explains mechanisms, indexation and uncertainty.'))),
        tabPanel('Revenue',value='revenue',
          div(class='panel-card',h3('How revenue responds'),
            plotOutput('revenue_path',height=310),textOutput('revenue_summary'),
            p(class='note','The reference stays at the default assumptions and CPI-indexed thresholds. CPI indexation still allows bracket creep from real earnings growth. The additional wage-indexed counterfactual holds your economic scenario fixed and changes only personal tax thresholds. No revenue/GDP cap or automatic future tax cut is imposed.')),
          div(class='panel-card',h3('Revenue composition over time'),
            plotOutput('revenue_mix',height=350),
            p(class='note','GST appears once at Commonwealth collection. State GST grants and NDIS funding contributions are internal flows, not extra national revenue. Fees and other revenue include dividends; royalties and interest receipts are separate.')),
          div(class='panel-card',h3('Income tax and bracket creep'),
            tableOutput('tax_rules'),
            p('Personal income tax applies the legislated resident schedule and Low Income Tax Offset to 100 observed ATO taxable-income percentile cells. Income grows with earnings; taxpayer numbers scale with employment. This approximates the income distribution and is not a household tax-benefit microsimulation.'),
            p('Choose fixed nominal, CPI-indexed or wage-indexed thresholds in the sidebar. Indexation is annual from the common 2030 starting point. Bracket creep is measured against wage-indexed thresholds under the same economic scenario.'),
            p(class='note','The 2027-28 14% lower rate is included. Medicare/FBT are a separate observed revenue block. WATO eligibility, non-resident schedules, capital-gains realisations and individual deductions are not simulated; starting aggregate differences are absorbed by calibration.')),
          div(class='panel-card',h3('Revenue mechanisms and evidence'),tableOutput('revenue_mechanisms'),
            h3('Historical elasticity evidence'),tableOutput('elasticities'),
            p(class='note','Nine annual changes (2017-2025). Estimates are shrunk toward one and applied to scenario deviations from a unit-elastic reference path. They are associations, not causal estimates. The historical intercept is not compounded into the long-run baseline. Personal income tax uses the schedule rather than the regression elasticity.'),
            downloadButton('revenue_download','Download annual revenue detail'))),
        tabPanel('Method & data',value='method',
          div(class='panel-card',h3('How to interpret this version'),
            p('This is an independent, transparent scenario prototype. It is not a validated forecast, a full current-policy simulation or a replication of the PBO or IGR.'),
            tags$ul(tags$li('The aggregate starting point now uses the 2026-27 PBO National Fiscal Outlook. National totals are already consolidated. The source bridge reports Commonwealth and state totals and the net local-government/consolidation adjustment.'),
              tags$li('Programme allocations use 2024–25 ABS GFS and an Age Pension outlay of $62.2bn from the ANAO. A single calibration reconciles their 2030 projection to PBO primary expenses. These are e61 allocations, not published PBO programme forecasts.'),
              tags$li('Population totals, age distributions and structural participation use the 2026 IGR. ABS observed age/sex rates and hours replace fixed labour weights. Annual age distributions interpolate published 2025, 2036 and 2066 profiles; this is not a full cohort population model.'),
              tags$li('Health and aged care use sourced IGR age-cost profiles. NDIS uses NDIA participation rates below 65 and retains existing older cohorts. Service-cost pass-through and intensity growth remain scenario assumptions.'),
              tags$li('Pension age 67, income/assets tests, deeming and statutory tax brackets are explicit. Pension wealth archetypes and superannuation accumulation are stylised: household wealth distributions and full current-policy reforms remain to be validated.'),
              tags$li('Debt is simulated from the common 2030 stock. Interest is charged on gross debt; receipts on financial assets. Financial asset acquisitions are accounted for separately. Surpluses after gross debt repayment accumulate in assets.'),
              tags$li('No statistical uncertainty bands are supplied: model structure and policy uncertainty dominate the precision of this prototype.')),
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
  a <- reactive({
    z <- default_assumptions()
    z$productivity <- input$productivity/100
    z$ngdp_override <- isTRUE(input$ngdp_override)
    z$ngdp_growth <- input$ngdp/100
    z$relative_price <- input$relative_price/100
    z$tot_change <- input$tot/100
    z$policy <- input$policy
    z$tax_policy <- input$tax_policy
    z$super_real_return <- input$super_return/100
    z$ndis_intensity_growth <- input$ndis_intensity/100
    z$interest_rate <- input$interest/100
    z
  })
  # Fix every reference assumption. Reusing selected policies here masks their
  # fiscal effect by moving both comparison paths together.
  ref_a <- reactive(default_assumptions())
  scenario <- reactive(project_model(inputs,a(),calibration))
  reference <- reactive(project_model(inputs,ref_a(),calibration))
  both <- reactive(rbind(transform(scenario()$path,case='Your scenario'),
                         transform(reference()$path,case='Reference')))
  observeEvent(input$reset,{
    updateSliderInput(session,'productivity',value=1.2)
    updateCheckboxInput(session,'ngdp_override',value=FALSE)
    updateSliderInput(session,'ngdp',value=4.4)
    updateSliderInput(session,'relative_price',value=0.3)
    updateSliderInput(session,'tot',value=0)
    updateSelectInput(session,'policy',selected='indexed')
    updateSelectInput(session,'tax_policy',selected='cpi')
    updateSliderInput(session,'super_return',value=4)
    updateSliderInput(session,'ndis_intensity',value=0.5)
    updateSliderInput(session,'interest',value=4.4)
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
    sprintf('Implied domestic inflation: %.2f%% in 2031 and %.2f%% in 2066. The GDP deflator also includes the terms-of-trade income effect. Participation follows IGR targets; average hours per worker decline 0.1%% annually.',
            100*m$domestic_inflation[2],100*tail(m$domestic_inflation,1))
  })
  debt_both <- reactive({
    d<-rbind(transform(debt_diagnostics(scenario()),case='Your scenario'),
      transform(debt_diagnostics(reference()),case='Reference'))
    if(isTRUE(input$ngdp_override) && isTRUE(input$debt_linked)) {
      linked_a<-a();linked_a$ngdp_override<-FALSE
      linked<-project_model(inputs,linked_a,calibration)
      d<-rbind(d,transform(debt_diagnostics(linked),case='Same scenario, linked NGDP'))
    }
    d
  })
  debt_plot <- function(d,fields,labels,unit) {
    long<-do.call(rbind,lapply(seq_along(fields),function(i)
      data.frame(year=d$year,case=d$case,measure=labels[i],value=d[[paste0(fields[i],'_',unit)]])))
    long$measure<-factor(long$measure,levels=labels)
    ggplot(long,aes(year,value,colour=case,linetype=case))+geom_line(linewidth=.9)+
      facet_wrap(~measure,ncol=3)+scale_colour_manual(values=c(colours,'Same scenario, linked NGDP'='#cb983e'))+
      scale_linetype_manual(values=c('Reference'='dashed','Your scenario'='solid','Same scenario, linked NGDP'='dotdash'))+
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
    linked<-subset(d,case=='Same scenario, linked NGDP' & year==s$year)
    if(nrow(linked))message<-paste(message,sprintf('With the same assumptions and linked NGDP, interest is $%.1fbn / $%.1fbn GDP = %.2f%%. The nominal borrowing rate is %.2f%% in both cases.',
      linked$interest_bn,linked$nominal_gdp,linked$interest_pct,100*s$effective_interest_rate))
    message
  })
  output$debt_download <- downloadHandler(filename=function()'e61_debt_interest_detail.csv',content=function(file){
    d<-debt_both()
    fields<-c('year','case','nominal_gdp','gross_debt_bn','financial_assets_bn','net_debt_bn',
      'gross_debt_pct','financial_assets_pct','net_debt_pct','opening_gross_debt_bn','opening_financial_assets_bn',
      'effective_interest_rate','asset_yield_pct','domestic_inflation','real_borrowing_rate_pct',
      'interest_bn','interest_pct','interest_receipts_bn','interest_receipts_pct','net_interest_bn','net_interest_pct')
    write.csv(d[,fields],file,row.names=FALSE)
  })
  output$spending_paths <- renderPlot({
    d<-rbind(transform(scenario()$components,case='Your scenario'),
      transform(reference()$components,case='Reference'))
    ggplot(d,aes(year,share_gdp,colour=case,linetype=case))+geom_line(linewidth=.9)+
      facet_wrap(~programme,scales='free_y',ncol=3)+scale_colour_manual(values=colours)+
      scale_linetype_manual(values=c('Reference'='dashed','Your scenario'='solid'))+
      labs(x='Financial year ending',y='Spending (% GDP)',colour=NULL,linetype=NULL)+theme_e61()
  })
  output$spending_drivers <- renderTable({
    d<-subset(spending_driver_audit(scenario()),year==2066)
    initial<-subset(scenario()$components,year==2030)
    base<-initial$share_gdp[match(d$programme,initial$programme)]
    number<-d$exposure_index;number[d$programme=='Defence']<-NA_real_
    data.frame(Programme=d$programme,'2030 (% GDP)'=round(base,2),
      '2066 (% GDP)'=round(d$share_gdp,2),'Change (pp)'=round(d$share_gdp-base,2),
      'Number (x)'=round(number,2),'Real unit cost (x)'=round(d$real_unit_cost_index,2),
      'Intensity / coverage (x)'=round(d$intensity_coverage_index,2),check.names=FALSE)
  },striped=TRUE,na='Target')
  output$spending_denominator <- renderText({
    d<-spending_driver_audit(scenario())
    sprintf('Over 2030-66, GDP after removing domestic price growth increases %.2f times. Divide the product of the spending factors by this denominator to obtain the change in the spending share. Terms-of-trade income effects remain in this GDP measure.',tail(d$gdp_after_domestic_prices_index,1))
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
    sprintf('Your scenario: $%.1fbn in 2030 and $%.1fbn in 2035; average nominal growth %.1f%% a year over those five years. The intensity slider adds to participant and unit-cost growth.',
      d$spending_bn[d$year==2030],d$spending_bn[d$year==2035],growth)
  })
  output$ndis_benchmark <- renderTable({
    d<-subset(scenario()$components,programme=='NDIS' & year%in%c(2030,2035,2050,2066))
    official<-read.csv('data/processed/ndis_igr_benchmark.csv')
    data.frame(Year=d$year,'Scenario (% GDP)'=round(d$share_gdp,2),
      '2026 IGR total Scheme (% GDP)'=official$igr_2026_pct[match(d$year,official$year)],check.names=FALSE)
  },striped=TRUE)
  output$revenue_table <- renderTable({
    s <- tail(scenario()$revenue,1); r <- tail(reference()$revenue,1)
    data.frame(Source=gsub('_',' ',names(s)[-1]),'Scenario ($bn)'=round(as.numeric(s[1,-1]),1),
      'Reference ($bn)'=round(as.numeric(r[1,-1]),1),check.names=FALSE)
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
    Head=c('Personal income','Company / other taxes','GST / excises','Payroll / Medicare-FBT',
      'Property / stamp duty','Royalties','Fees and other','Interest receipts'),
    Base=c('Observed income percentiles; statutory schedule; threshold rule','Nominal GDP proxy for profits and other bases',
      'Domestic nominal output proxy for consumption','Employment and average nominal earnings',
      'Nominal GDP proxy; no separate property market model','Domestic output and terms of trade',
      'Domestic nominal output','Opening interest-bearing financial assets'),
    Evidence=c('ATO Table 16 + ABS tax totals','ABS tax history; shrunk short-sample elasticity',
      'ABS tax history; shrunk short-sample elasticity','ABS tax history; shrunk short-sample elasticity',
      'ABS tax history; shrunk short-sample elasticity','Observed GFS level; ToT elasticity 2 is an assumption',
      'Observed GFS non-tax level; unit elasticity','Starting receipts calibrated to GFS/PBO; constant asset yield')),
    striped=TRUE)
  output$elasticities<-renderTable({
    d<-inputs$tax_elasticities
    data.frame(Head=d$head,'Raw estimate'=round(d$raw_elasticity,2),
      'Standard error'=round(d$standard_error,2),'Scenario elasticity'=round(d$applied_elasticity,2),check.names=FALSE)
  },striped=TRUE)
  output$coverage_bridge<-renderTable({
    d<-read.csv('data/processed/consolidation_bridge.csv');d<-d[d$year==2030,]
    data.frame(Measure=c('Commonwealth','States','National consolidated','Net adjustment'),
      'Revenue ($bn)'=round(c(d$revenue_billion_dollars,d$states_revenue_bn,d$national_revenue_bn,d$revenue_consolidation_adjustment_bn),1),
      'Expenses ($bn)'=round(c(d$expenses_billion_dollars,d$states_expenses_bn,d$national_expenses_bn,d$expense_consolidation_adjustment_bn),1),check.names=FALSE)
  },striped=TRUE)
  output$parameters <- renderTable({
    pp <- calibration$programmes
    data.frame(Programme=pp$programme,'2030 allocation ($bn)'=round(pp$anchor_2030_bn,1),
      'Wage pass-through (%)'=100*pp$wage_pass_through,
      'Excess intensity growth (%)'=100*pp$excess_cost_growth,check.names=FALSE)
  },striped=TRUE)
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
      tax_calibration=calibration$revenue_anchor,elasticities=inputs$tax_elasticities,
      source_manifest=read.csv('data/raw/v2/download_manifest.csv')),file=file)
  })
  output$revenue_download<-downloadHandler(filename=function()'e61_revenue_detail.csv',content=function(file){
    d<-merge(revenue_long(),scenario()$revenue_diagnostics,by='year');write.csv(d,file,row.names=FALSE)
  })
}
shinyApp(ui,server)
