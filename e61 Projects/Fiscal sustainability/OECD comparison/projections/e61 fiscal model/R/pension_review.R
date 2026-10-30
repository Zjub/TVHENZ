pension_age_bands <- function(d) {
 d$age_group<-cut(d$age,c(66,69,74,79,84,89,Inf),labels=c('67-69','70-74','75-79','80-84','85-89','90+'))
 z<-aggregate(cbind(people,recipients,pension_dollars)~year+age_group,d,sum)
 z$coverage<-z$recipients/z$people;z
}
pension_review_ui <- function() {
 ns<-shiny::NS('pension_review')
 shiny::tabPanel('Age Pension',value='age_pension',
  shiny::div(class='panel-card',shiny::h3('Age Pension: age, cohorts and retirement wealth'),
   shiny::p('The public-data cohort model starts with existing wealth for retirees and workers. It applies income and asset tests as cohorts age, accumulate super and consume retirement wealth. Pension age and retirement from work are separate.'),
   shiny::textOutput(ns('summary')),
   shiny::p(class='note','Public tables calibrate the starting position. Retirement transitions, consumption, saving and the joint household wealth distribution are provisional assumptions. These need future estimation and validation using HILDA; no HILDA parameters have been estimated in this version.')),
  shiny::div(class='panel-card',shiny::h3('How pension receipt by age changes over time'),
   shiny::selectInput(ns('years'),'Projection years',choices=2030:2066,selected=c(2030,2040,2050,2066),multiple=TRUE),
   shiny::plotOutput(ns('age_profile'),height=350),
   shiny::p(class='note','Each line shows the share of the resident population in each age band receiving a positive modelled payment under your current assumptions. Dots show the 2026 public-data calibration targets, not observed 2030 outcomes. Downloads retain single-age detail, with age 100 pooling people aged 100+. Receipt is not a full legal eligibility assessment.'),
   shiny::tableOutput(ns('age_table'))),
  shiny::div(class='panel-card',shiny::h3('Receipt when each cohort reaches pension age'),
   shiny::plotOutput(ns('entry'),height=290),
   shiny::p('These are people reaching age 67 in each year. Some have already retired; others are still working. Entry-age receipt depends on the wealth accumulated before that date and assessed income.')),
  shiny::div(class='panel-card',shiny::h3('Following a birth cohort as it ages'),
   shiny::selectInput(ns('cohort'),'Birth year',choices=1930:1999,selected=1963),
   shiny::plotOutput(ns('cohort_path'),height=290),
   shiny::p(class='note','Shows receipt within a surviving cohort from age 67. Public cross-sectional age gradients are not imposed as individual drawdown paths. Household type and wealth rank are fixed; wealth-specific mortality, partnering and widowhood transitions are not estimated. Ages 100+ depend on a pooled-population allocation assumption.')),
  shiny::div(class='panel-card',shiny::h3('Spending, recipients and mean payments'),
   shiny::tableOutput(ns('spending_table')),
   shiny::p(class='note','The common 2030 fiscal allocation is preserved. A single fixed multiplier reconciles raw means-tested entitlements to that allocation. Counts and receipt rates are not scaled; reported fiscal mean payments include that multiplier. A fixed-GDP-share spending override changes fiscal spending and these fiscal mean payments, while the receipt profiles continue to show the underlying means-tested rule. No refitting occurs when economic or return controls move.'),
   shiny::downloadButton(ns('age_download'),'Download annual receipt by age'),
   shiny::downloadButton(ns('cohort_download'),'Download cohort and entry paths')),
  shiny::div(class='panel-card',shiny::h3('Methods, evidence and assumptions'),
   shiny::p('DSS June 2026 identified-state recipient counts are divided by the cached 2026 population by age. Geographic Unknown records are excluded; a national adjustment removes zero-rate, suspended and unclassified payments. That adjustment and the national full/part mix are applied uniformly across ages and sexes because the downloaded tables do not identify their joint distribution.'),
   shiny::p('ATO 2024 positive-balance super medians are combined with ABS SIH 2019-20 dispersion and coverage proxies. ABS age-specific household wealth, homeownership and family composition supply marginal asset priors. Synthetic ranked cells connect these marginals; they are not observed joint households. Existing retirees are reweighted once to match age-specific receipt and the national full/part marginal. This can move wealth moments away from the priors; the calibration audit reports the resulting values.'),
   shiny::p('Only existing pension-age groups have a direct receipt calibration. Workers retain public wealth priors, so future entrants are less tightly identified. Differences across age bands and cohorts can reflect source boundaries and calibration as well as accumulated wealth. These should be reviewed with longitudinal data; the age gradient is not an estimated drawdown effect. June 2026 recipient targets are applied under the available September 2026 policy settings.'),
   shiny::p('Source wealth is expressed in 2026-price dollars using a provisional 2.5% annual uprating, not reconstructed asset-specific returns. A fixed four-year bridge to 2030 uses 4% real super returns and 1.2% real earnings growth. Scenario changes begin after 2030 and do not rewrite existing wealth.'),
   shiny::p('Retirement ages 60, 65 and 75 have weights informed by broad ABS age/sex employment rates; interpreting those cross-sectional rates as retirement transitions is an assumption. Earnings use the ATO income-cell mean as a wage proxy, correlated with synthetic wealth rank. After 60, conditional hours use provisional age factors. Spouses are assumed the same age and wealth rank; household types stay fixed.'),
   shiny::p('Super returns act on existing super and contributions and are treated as net of investment fees and earnings tax; those deductions are not separately simulated. Cash has a separate 1% real return. Retired account holders withdraw the age-specific minimum into liquid assets; consumption removes 5% of opening super plus liquid wealth annually. Withdrawals are not themselves consumption. Other income is assumed consumed. Non-super investment-property/business values are held constant in real terms with a 3% income yield. Principal homes are excluded from assessed wealth; residency, benefit take-up, grandfathering and the banked Work Bonus are not explicitly simulated. Only the regular $300-per-fortnight Work Bonus is included.'),
   shiny::h4('What HILDA estimation needs to establish'),
   shiny::p('The joint wealth/income distribution; retirement and employment transitions; consumption versus reinvestment of withdrawals; partnering/widowhood and wealth-related survival; changes in pension receipt and payment amounts within cohorts. Public age profiles cannot independently distinguish cohort history from wealth depletion.'),
   shiny::p(shiny::tags$a(href='https://data.gov.au/data/dataset/dss-payment-demographic-data',target='_blank','DSS pension demographics'),' | ',
     shiny::tags$a(href='https://data.gov.au/data/dataset/taxation-statistics-2023-24',target='_blank','ATO super balances'),' | ',
     shiny::tags$a(href='https://www.abs.gov.au/statistics/economy/finance/household-income-and-wealth-australia/2019-20',target='_blank','ABS household wealth'),' | ',
     shiny::tags$a(href='https://moneysmart.gov.au/retirement-income-sources/account-based-pensions',target='_blank','Minimum withdrawals')),
   shiny::tableOutput(ns('sources')),shiny::tableOutput(ns('assumptions')),
   shiny::h4('2026 calibration: wealth priors and reweighted outcomes'),
   shiny::p(class='note','Average dollars per adult-equivalent, including people with zero balances. These are synthetic distributions constrained by public marginals. Reweighting to pension receipt does not preserve every wealth moment.'),
   shiny::tableOutput(ns('calibration_table')),
   shiny::downloadButton(ns('source_download'),'Download source register')))
}
pension_review_server <- function(inputs,scenario,reference) {
 shiny::moduleServer('pension_review',function(input,output,session){
  profiles<-shiny::reactive(attr(scenario()$pension,'age_profiles'))
  years<-shiny::reactive({v<-as.integer(input$years);if(!length(v))2030L else v})
  output$summary<-shiny::renderText({p<-scenario()$pension
   sprintf('Receipt among people aged 67+: %.1f%% in 2030 and %.1f%% in 2066. Means-tested cohort stocks are fixed at the shared 2030 start; real super return %.1f%% annually.',100*p$coverage[1],100*tail(p$coverage,1),100*scenario()$assumptions$super_real_return)})
  output$age_profile<-shiny::renderPlot({d<-pension_age_bands(profiles());d<-d[d$year%in%years(),];o<-inputs$pension_public_age;o$age_group<-factor(c('67-69','70-74','75-79','80-84','85-89','90+'),levels=levels(d$age_group))
   ggplot(d,aes(age_group,100*coverage,colour=factor(year),group=year))+geom_line(linewidth=1)+geom_point()+
    geom_point(data=o,aes(age_group,100*coverage),inherit.aes=FALSE,shape=4,size=3,colour='#122b3f')+
    labs(x='Age group',y='Receiving Age Pension (%)',colour='Projection year')+coord_cartesian(ylim=c(0,100))+theme_e61()})
  output$age_table<-shiny::renderTable({d<-pension_age_bands(profiles());d<-d[d$year%in%years(),]
   reshape(data.frame(Age=d$age_group,Year=d$year,Receipt=round(100*d$coverage,1)),idvar='Age',timevar='Year',direction='wide')},striped=TRUE)
  output$entry<-shiny::renderPlot({d<-rbind(transform(attr(scenario()$pension,'entry_profiles'),case='Your scenario'),transform(attr(reference()$pension,'entry_profiles'),case='Reference'))
   ggplot(d,aes(year,100*coverage,colour=case,linetype=case))+geom_line(linewidth=1)+scale_colour_manual(values=colours)+
    coord_cartesian(ylim=c(0,100))+labs(x='Year reaching age 67',y='Receiving Age Pension at 67 (%)',colour=NULL,linetype=NULL)+theme_e61()})
  output$cohort_path<-shiny::renderPlot({birth<-as.integer(input$cohort);if(!length(birth))birth<-1963L
   d<-rbind(transform(attr(scenario()$pension,'cohort_profiles'),case='Your scenario'),transform(attr(reference()$pension,'cohort_profiles'),case='Reference'));d<-d[d$birth_year==birth,]
   ggplot(d,aes(age,100*coverage,colour=case,linetype=case))+geom_line(linewidth=1)+scale_colour_manual(values=colours)+
    coord_cartesian(ylim=c(0,100))+labs(x=paste('Age: cohort born',birth),y='Receiving Age Pension (%)',colour=NULL,linetype=NULL)+theme_e61()})
  output$spending_table<-shiny::renderTable({s<-scenario();p<-s$pension;k<-p$year%in%sort(unique(c(2030,years(),2066)));p<-p[k,];d<-subset(s$components,programme=='Age Pension');sp<-d$spending_bn[match(p$year,d$year)]
   data.frame(Year=p$year,'People aged 67+ (million)'=round(p$eligible_population/1e6,3),'Recipients (million)'=round(p$recipients/1e6,3),
    'Receipt (%)'=round(100*p$coverage,1),'Spending ($bn)'=round(sp,2),'Mean payment ($/recipient)'=round(sp*1e9/p$recipients),check.names=FALSE)},striped=TRUE)
  output$sources<-shiny::renderTable(inputs$pension_sources[,c('file','locator','classification')],striped=TRUE)
  output$assumptions<-shiny::renderTable(inputs$pension_cohort_parameters,striped=TRUE)
  output$calibration_table<-shiny::renderTable({d<-inputs$pension_initial_calibration;d<-d[d$age%in%c(67,70,75,80,85,90,95),]
    data.frame(Age=d$age,'Receipt (%)'=round(100*d$coverage,1),
      'Super prior ($)'=round(d$prior_mean_super_2026),'Super after calibration ($)'=round(d$mean_super_2026),
      'Assessable assets prior ($)'=round(d$prior_mean_assessable_assets_2026),'Assets after calibration ($)'=round(d$mean_assessable_assets_2026),check.names=FALSE)},striped=TRUE)
  output$age_download<-shiny::downloadHandler(filename=function()'age_pension_by_age.csv',content=function(file){s<-scenario();d<-profiles();factor<-subset(s$calibration$programmes,programme=='Age Pension')$anchor_2030_bn/s$pension$raw_spending_bn[1]
   actual<-subset(s$components,programme=='Age Pension');k<-match(d$year,s$pension$year)
   scale<-actual$spending_bn[match(d$year,actual$year)]/s$pension$raw_spending_bn[k]
   d$fiscal_calibration_factor<-factor;d$fiscal_policy_factor<-scale/factor
   d$calibrated_spending_bn<-d$pension_dollars/1e9*scale;write.csv(d,file,row.names=FALSE)})
  output$cohort_download<-shiny::downloadHandler(filename=function()'age_pension_cohorts.csv',content=function(file)write.csv(attr(scenario()$pension,'cohort_profiles'),file,row.names=FALSE))
  output$source_download<-shiny::downloadHandler(filename=function()'age_pension_sources.csv',content=function(file)write.csv(inputs$pension_sources,file,row.names=FALSE))
 })
}
