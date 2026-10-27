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
reference_text <- 'Reference: productivity 1.2%; linked NGDP; service price premium 0.3%; unchanged terms of trade. Policy settings match your scenario.'

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
      helpText('Output per unit of labour. Employment and hours follow a fixed age-weighted labour-volume proxy.'),
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
        helpText('Indexed rules use wage-linked pensions with declining coverage. Relative standards hold pension coverage fixed and fully wage-link other social protection.'),
        selectInput('tax_policy','Personal tax thresholds',
          c('Wage indexation (no bracket creep)'='wage_indexed','Partial bracket creep (proxy)'='creep')),
        sliderInput('coverage','Pension coverage decline (% a year)',min=0,max=1.5,value=0.5,step=0.1),
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
              p(class='note','An exact Shapley allocation of the four economic controls, including their interactions, relative to a reference with the same policy settings. This explains model arithmetic, not estimated causal effects.'))),
        tabPanel('Economic channels',value='channels',
          div(class='panel-card',h3('Real output, nominal output and prices'),
              plotOutput('macro',height=390),textOutput('price_message')),
          div(class='panel-card',h3('The productivity mechanism'),
            p('Higher productivity raises real output and wages. Wage exposure differs by spending programme. Revenue responds to its tax base; interest responds to the debt stock.'),
            p('The NGDP override fixes nominal output growth. At a given NGDP path, higher real growth means lower implied inflation. This is a different experiment from raising productivity while holding inflation fixed.'),
            p('Terms of trade affect nominal income with an illustrative elasticity of 0.20 and resource revenue with an elasticity of 2. Under a fixed NGDP override, the domestic price path adjusts to preserve that NGDP target.'))),
        tabPanel('Spending & revenue',value='components',
          div(class='panel-card',h3('Spending components in 2066'),plotOutput('components',height=330),
            p(class='note','Total expenditure includes conventional interest and net capital investment. The remaining social protection block contains aged care and NDIS; they are not separately modelled yet.')),
          div(class='panel-card',h3('Revenue in 2066'),tableOutput('revenue_table'),
              p(class='note','Tax-head allocations and elasticities are illustrative; only the aggregate is calibrated to PBO. This is not a tax microsimulation.'))),
        tabPanel('Method & data',value='method',
          div(class='panel-card',h3('How to interpret this version'),
            p('This is an independent, transparent scenario prototype. It is not a validated forecast, a full current-policy simulation or a replication of the PBO or IGR.'),
            tags$ul(tags$li('The aggregate starting point uses the project’s cached 2025–26 PBO National Fiscal Outlook, through 2029–30. It has not been updated to the 2026–27 edition.'),
              tags$li('Programme allocations use 2024–25 ABS GFS and an Age Pension outlay of $62.2bn from the ANAO. A single calibration reconciles their 2030 projection to PBO primary expenses. These are e61 allocations, not published PBO programme forecasts.'),
              tags$li('Age profiles, wage pass-through, pension coverage, excess intensity growth and tax elasticities are exposed modelling judgements. Future participation, means tests and superannuation are represented only by simple proxies.'),
              tags$li('Debt is simulated from the common 2030 stock. Interest is charged on gross debt; receipts on financial assets. Financial asset acquisitions are accounted for separately. Surpluses after gross debt repayment accumulate in assets.'),
              tags$li('No statistical uncertainty bands are supplied: model structure and policy uncertainty dominate the precision of this prototype.')),
            h3('Programme parameters'),tableOutput('parameters'),
            h3('Source files and provenance'),tableOutput('sources'),
            p(tags$a(href='https://www.anao.gov.au/work/performance-audit/administration-of-the-age-pension',target='_blank','ANAO Age Pension anchor'),
              ' · ',tags$a(href='https://www.pbo.gov.au/publications-and-data/publications/2025-26-National-Fiscal-Outlook',target='_blank','PBO anchor edition')),
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
    z$pension_coverage_decline <- input$coverage/100
    z$interest_rate <- input$interest/100
    z
  })
  ref_a <- reactive(modifyList(default_assumptions(),a()[c('policy','tax_policy','pension_coverage_decline','interest_rate')]))
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
    updateSelectInput(session,'tax_policy',selected='wage_indexed')
    updateSliderInput(session,'coverage',value=0.5)
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
      ggplot()+annotate('text',x=0,y=0,label='Reference settings selected\nMove an economic slider to see its contribution',
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
    sprintf('Implied domestic inflation: %.2f%% in 2031 and %.2f%% in 2066. The GDP deflator also includes the terms-of-trade income effect. Labour participation and hours propensities are fixed.',
            100*m$domestic_inflation[2],100*tail(m$domestic_inflation,1))
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
  output$revenue_table <- renderTable({
    s <- tail(scenario()$revenue,1); r <- tail(reference()$revenue,1)
    data.frame(Source=gsub('_',' ',names(s)[-1]),'Scenario ($bn)'=round(as.numeric(s[1,-1]),1),
      'Reference ($bn)'=round(as.numeric(r[1,-1]),1),check.names=FALSE)
  },striped=TRUE)
  output$parameters <- renderTable({
    pp <- calibration$programmes
    data.frame(Programme=pp$programme,'2030 allocation ($bn)'=round(pp$anchor_2030_bn,1),
      'Wage pass-through (%)'=100*pp$wage_pass_through,
      'Excess intensity growth (%)'=100*pp$excess_cost_growth,check.names=FALSE)
  },striped=TRUE)
  output$sources <- renderTable(data.frame(
    Input=c('Consolidated PBO anchor','Population and age shares','Spending composition and operating items','Age Pension allocation'),
    Source=c('Cached 2025–26 National Fiscal Outlook','Existing project population projection; held common across scenarios',
             'Cached ABS GFS 2024–25','ANAO: 2024–25 outlay $62.2bn; proxy allocation within social protection')),
    striped=TRUE)
  output$download <- downloadHandler(filename=function() 'e61_fiscal_scenario.csv',content=function(file) {
    d <- both()
    for(n in names(a())) d[[paste0('scenario_assumption_',n)]] <- a()[[n]]
    write.csv(d,file,row.names=FALSE)
  })
  output$audit <- downloadHandler(filename=function() 'e61_fiscal_assumptions.txt',content=function(file) {
    capture.output(list(scenario=a(),reference=ref_a(),programme_calibration=calibration$programmes,
      source_manifest=read.csv('data/snapshot_manifest.csv')),file=file)
  })
}
shinyApp(ui,server)
