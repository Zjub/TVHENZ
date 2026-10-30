# Sector review uses the projection's authoritative subfunction factors.
# It adds presentation and provenance; it does not change spending equations.
sector_review_data <- function(projection,programme,id='all',end_year=2066L,measure='pct') {
 stopifnot(programme%in%c('NDIS','Health','Education'),end_year>2030,end_year<=2066,
   measure%in%c('pct','bn','real_bn'))
 d<-projection$subfunctions
 d<-d[d$programme==programme & (id=='all'|d$id==id),]
 stopifnot(nrow(d)>0)
 m<-projection$path;idx<-match(d$year,m$year)
 d$value<-switch(measure,pct=d$share_gdp,bn=d$spending_bn,
   real_bn=d$spending_bn/m$domestic_price_index[idx])
 d<-d[d$year<=end_year,]
 c<-spending_contributions(projection,2030L,end_year,measure)
 c<-c[c$programme==programme & c$id%in%d$id,]
 list(annual=d,contributions=c,totals=aggregate(value~year,d,sum),
   unit=switch(measure,pct='% GDP',bn='nominal AUD bn',real_bn='2030-dollar bn'))
}

health_per_person_data <- function(projection,id='all',end_year=2066L,relative=FALSE) {
 stopifnot(end_year>2030,end_year<=projection$assumptions$end_year,length(relative)==1,!is.na(relative))
 m<-projection$path;d<-projection$subfunctions
 d<-d[d$programme=='Health' & (id=='all'|d$id==id) & d$year<=end_year,]
 stopifnot(nrow(d)>0)
 k<-match(d$year,m$year);pop0<-m$population[1];gdp0<-projection$calibration$gdp0
 d$real_spending_per_person<-d$spending_bn*1e9/(m$domestic_price_index[k]*m$population[k])
 d$real_gdp_per_person<-gdp0*1e9*m$real_output_index[k]/m$population[k]
 d$value<-if(relative)100*d$real_spending_per_person/d$real_gdp_per_person else d$real_spending_per_person
 # Cancel total population and the common price index before allocating interactions.
 # The optional denominator is real output per person, excluding the ToT income effect.
 d$population_factor<-d$population_factor/m$population_index[k]
 d$price_factor<-d$price_factor/m$domestic_price_index[k]
 d$gdp_factor<-if(relative)m$population_index[k]/m$real_output_index[k] else 1
 labels<-spending_factor_labels();labels['population_factor']<-'Population size (removed per person)'
 labels['price_factor']<-'Relative health prices';labels['gdp_factor']<-'Real GDP per person denominator'
 factors<-names(labels)
 contributions<-do.call(rbind,lapply(unique(d$id),function(key){
  z<-d[d$id==key & d$year%in%c(2030,end_year),];z<-z[order(z$year),]
  f0<-as.numeric(z[1,factors]);f1<-as.numeric(z[2,factors])
  anchor<-z$anchor_2030_bn[1]*if(relative)100/gdp0 else 1e9/pop0
  values<-anchor*c(prod(f0),prod(f1))
  data.frame(programme='Health',id=key,subfunction=z$subfunction[1],factor=unname(labels),
    contribution=product_shapley(f0,f1,anchor),factor_start=f0,factor_end=f1,
    start_value=values[1],end_value=values[2],change=values[2]-values[1],start_year=2030,end_year=end_year,
    measure=if(relative)'real_spending_relative_real_gdp_per_person' else 'real_spending_per_person')
 }))
 list(annual=d,contributions=contributions,totals=aggregate(value~year,d,sum),labels=labels,
  unit=if(relative)'% of real GDP per person' else '2030 dollars per person',
  contribution_unit=if(relative)'percentage points of real GDP per person' else '2030 dollars per person')
}

sector_review_questions <- function(programme) switch(programme,
 NDIS=c('Participant demand: are fixed under-65 age-specific rates credible? What entrant, exit, disability and reform assumptions should replace them?',
 'Older participants: is population survival a reasonable retention proxy? How should exits, mortality differences and transfers to aged care be treated?',
 'Cost per participant: what shares reflect worker pay, non-labour inputs, plan utilisation, support hours and participant severity? How can we estimate them separately?',
 'Overlap: how much additional growth remains after explicit wage costs and service productivity? Which observed series can distinguish these?',
 'Boundary and starting point: should we use participant plan expenses, cash payments, total Scheme costs or Commonwealth funding? How should administration and the 2026 reforms be represented?'),
 Health=c('Hospitals: can admitted-acute labour shares describe the full public/private hospital funding envelope? What separate demand, staffing, capital and productivity measures are needed?',
 'Primary/community and referred care: should rebates, provider costs and utilisation be separate? What replaces the unestimated 70% wage weights and shared medical-benefits age profile?',
 'Medicines: is CPI-only unit cost plus additional service growth credible after innovation, price disclosure, rebates and treatment-mix changes?',
 'Age and morbidity: should age-specific costs change with healthy ageing, disease prevalence and end-of-life care? Can coverage and service intensity be measured separately?',
 'Residual and overlap: what services sit in other health? Is additional service growth distinct from input costs and service productivity? Which historical series can identify each driver?'),
 Education=c('Schools: should pupil numbers replace ages 5-17, with separate government/non-government coverage, class sizes, staffing ratios and funding rules?',
 'Higher education: how should domestic/mature-age/international enrolments, government subsidies, student contributions and research be separated? Is the current tertiary spending split defensible?',
 'VET: should training hours and government-funded enrolments replace ages 15-64? Can the Commonwealth flexible-funding index represent all public VET spending?',
 'Other education: how should preschool eligibility, participation and policy expansion replace the total-population proxy?',
 'Unit costs and productivity: are observed labour shares representative of government-funded services? Which quality-adjusted output measures support an efficiency trend, and how should wages and funding standards change?'))

sector_review_ui <- function(programme,subfunctions) {
 id<-tolower(programme);ns<-shiny::NS(paste0('sector_',id))
 s<-subfunctions[subfunctions$programme==programme,]
 shiny::tabPanel(programme,value=paste0('sector_',id),
   shiny::div(class='panel-card',shiny::h3(paste(programme,'projection: a sector review')),
     shiny::p('Trace the calculation, inspect the evidence and identify assumptions to discuss with sector experts. All results below use your sidebar scenario. The fixed reference retains the default assumptions.'),
     if(programme=='Health')shiny::p(class='note','Health categories follow AIHW service areas, combining Commonwealth and state/territory government funding and reconciling to the ABS COFOG/GFS health total. They differ from the IGR Commonwealth programme categories. IGR age-cost profiles are used as demographic proxies; using those profiles does not make the spending categories equivalent.'),
     shiny::fluidRow(shiny::column(5,shiny::selectInput(ns('scope'),'Spending to inspect',
       c('Whole function'='all',setNames(s$id,s$subfunction)))),
       shiny::column(3,shiny::selectInput(ns('end'),'End year',2031:2066,selected=2066)),
       shiny::column(4,shiny::selectInput(ns('measure'),'Measure',
         c('Share of GDP'='pct','Nominal AUD billions'='bn','2030-dollar billions'='real_bn')))),
     shiny::textOutput(ns('summary'))),
   shiny::tabsetPanel(id=ns('section'),
     shiny::tabPanel('Projection & decomposition',value='projection',
       shiny::div(class='panel-card',shiny::h3('Projected spending'),shiny::plotOutput(ns('path'),height=290),
         shiny::tableOutput(ns('endpoints'))),
       shiny::div(class='panel-card',shiny::h3('What explains the change from 2030?'),
         shiny::plotOutput(ns('contributions'),height=350),shiny::tableOutput(ns('contribution_table')),
         shiny::p(class='note','Contributions sum exactly before rounding. Shapley accounting shares interactions symmetrically across drivers. It explains model arithmetic, not causal effects. The GDP denominator appears only for shares; general prices are removed in 2030-dollar results. The age/cohort driver includes NDIS retention above age 65.'),
         shiny::downloadButton(ns('contribution_download'),'Download driver contributions')),
       if(programme=='Health')shiny::div(class='panel-card',
         shiny::h3('What explains the change in real spending per person?'),
         shiny::selectInput(ns('per_person_measure'),'Show contributions to',
           c('Real health spending per person'='real_per_person',
             'Real health spending per person relative to real GDP per person'='relative_real_gdp')),
         shiny::textOutput(ns('per_person_summary')),
         shiny::plotOutput(ns('per_person_contributions'),height=350),
         shiny::tableOutput(ns('per_person_table')),
         shiny::p(class='note','Uses the spending scope and end year above, independently of the main chart measure. Real spending uses the common domestic-price deflator and total resident population. Population size cancels before decomposition; ageing and income-related demand remain. Relative health-price changes remain when the health price wedge is nonzero. The relative option divides by real GDP per person, excluding the terms-of-trade income effect. Its contributions are percentage points of that ratio. Shapley contributions sum exactly before rounding; they describe model arithmetic.'),
         shiny::downloadButton(ns('per_person_download'),'Download per-person driver contributions'),
         shiny::downloadButton(ns('per_person_annual_download'),'Download annual per-person spending')),
       shiny::div(class='panel-card',shiny::h3('Demand: population and age/cohort composition'),
         shiny::plotOutput(ns('demand'),height=280),shiny::uiOutput(ns('demand_notes')),
         shiny::tableOutput(ns('demand_inputs')))),
     shiny::tabPanel('Construction & evidence',value='construction',
       shiny::div(class='panel-card',shiny::h3('Cost assumptions and IGR discipline'),
         shiny::p('Combined real non-demographic growth compounds income-related demand, real input costs net of efficiency and remaining pressures over 2030-2066. Income demand applies only in the new health specification. Comparators use fixed reference macro assumptions and their own funding scopes; they are not targets for consolidated spending.'),
         shiny::div(style='overflow-x:auto',shiny::tableOutput(ns('cost_comparison')))),
       shiny::div(class='panel-card',shiny::h3('1. Establish the starting spending'),
         shiny::p('Observed 2025 allocations are reconciled to category controls, advanced to 2030 and calibrated to national PBO primary spending. PBO supplies the aggregate, not these sector or subfunction allocations. This bridge always shows the whole function; the inspection selector controls the subsequent driver walkthrough.'),
         shiny::div(style='overflow-x:auto',shiny::tableOutput(ns('bridge'))),
         shiny::uiOutput(ns('anchor_notes'))),
       shiny::div(class='panel-card',shiny::h3('2. Build each subfunction from separate drivers'),
         shiny::p('Spending = 2030 anchor x population x age/cohort demand x coverage x general prices x income-related demand x real wages/funding index x service-productivity efficiency x remaining/additional growth x policy rule. Income demand is 1 outside the new health specification. Every factor is 1 in 2030. Sum subfunctions and divide by nominal GDP for a GDP share.'),
         shiny::uiOutput(ns('mechanism')),
         shiny::p(class='note','Indices below are nominal-spending factors at the selected endpoint. They do not remove prices or include the GDP denominator, regardless of the chart measure. Multiply the factors and the 2030 anchor to recover the displayed nominal endpoint. Function-wide factors cannot generally be averaged to reproduce the aggregate.'),
         shiny::div(style='overflow-x:auto',shiny::tableOutput(ns('factor_check')))),
       shiny::div(class='panel-card',shiny::h3('3. Evidence behind the inputs'),shiny::uiOutput(ns('evidence')),
         shiny::downloadButton(ns('annual_download'),'Download annual calculation and sources'),
         shiny::downloadButton(ns('evidence_download'),'Download source and parameter register'))),
     shiny::tabPanel('Expert discussion',value='discussion',
       shiny::div(class='panel-card',shiny::h3('Where expert evidence could change the projection'),
         shiny::tags$ol(lapply(sector_review_questions(programme),shiny::tags$li)),
         shiny::h4('Keep the decomposition consistent'),
         shiny::p('For any proposed change, identify whether it changes the starting allocation, eligible population, coverage, input price, real unit cost, service intensity or policy rule. Specify the affected subfunctions, evidence, annual rate or path, and plausible range. Identify overlap with other drivers before adding a new allowance.'),
         shiny::p('The sidebar changes economic assumptions, the service-productivity response and additional service growth. Labour weights, demographic profiles and coverage are specified in the evidence register. These discussion assumptions are not fitted automatically to official projections.'),
         shiny::p(class='note','A sourced amount or input share is not an estimated long-run elasticity. Proxies extend evidence beyond its observed scope. Judgments are scenario choices. Identities are arithmetic consequences conditional on those choices.')))))
}

sector_review_server <- function(programme,inputs,scenario,reference) {
 shiny::moduleServer(paste0('sector_',tolower(programme)),function(input,output,session){
   selected<-shiny::reactive(if(is.null(input$scope))'all' else input$scope)
   end<-shiny::reactive(if(is.null(input$end))2066L else as.integer(input$end))
   measure<-shiny::reactive(if(is.null(input$measure))'pct' else input$measure)
   d<-shiny::reactive(sector_review_data(scenario(),programme,selected(),end(),measure()))
   ref<-shiny::reactive(sector_review_data(reference(),programme,selected(),end(),measure()))
   output$cost_comparison<-shiny::renderTable({z<-service_cost_comparison(scenario(),inputs);format_service_cost_comparison(z[z$programme==programme & (selected()=='all'|z$id==selected()),])},striped=TRUE,na='Not available')
   spec<-shiny::reactive({s<-scenario()$calibration$subfunctions;s[s$programme==programme & (selected()=='all'|s$id==selected()),]})
   output$summary<-shiny::renderText({z<-d();v<-z$totals$value
     text<-sprintf('2030 to %s: %.2f to %.2f %s; change %+.2f. Nominal annual growth: %.2f%%. All years label financial years by ending year.',
       end(),v[1],tail(v,1),z$unit,tail(v,1)-v[1],100*((sum(subset(z$annual,year==end())$spending_bn)/sum(subset(z$annual,year==2030)$spending_bn))^(1/(end()-2030))-1))
     target<-scenario()$assumptions$spending_gdp_targets[programme]
     if(length(target)&&!is.na(target))text<-paste(text,sprintf('FIXED-SHARE POLICY: the whole %s function is set to %.2f%% of GDP from 2031. Cost assumptions describe underlying pressure; the policy factor reconciles it to the target.',programme,target))
     text})
   output$path<-shiny::renderPlot({z<-rbind(transform(d()$totals,case='Your scenario'),transform(ref()$totals,case='Reference'))
     ggplot(z,aes(year,value,colour=case,linetype=case))+geom_line(linewidth=1)+scale_colour_manual(values=colours)+
       scale_linetype_manual(values=c('Your scenario'='solid','Reference'='dashed'))+
       natural_y(z)+labs(x='Financial year ending',y=d()$unit,colour=NULL,linetype=NULL)+theme_e61()})
   output$endpoints<-shiny::renderTable({z<-d()$annual;s<-z[z$year==2030,];e<-z[z$year==end(),];e<-e[match(s$id,e$id),]
     data.frame(Subfunction=s$subfunction,'2030 ($bn)'=round(s$spending_bn,2),
       'End year ($bn)'=round(e$spending_bn,2),'2030 (% GDP)'=round(s$share_gdp,3),
       'End year (% GDP)'=round(e$share_gdp,3),check.names=FALSE)},striped=TRUE)
   contributions<-shiny::reactive({z<-aggregate(contribution~factor,d()$contributions,sum);z[match(unname(spending_factor_labels()),z$factor),]})
   output$contributions<-shiny::renderPlot({z<-contributions();z$factor<-factor(z$factor,levels=rev(unname(spending_factor_labels())))
     ggplot(z,aes(factor,contribution,fill=contribution>=0))+geom_col()+coord_flip()+natural_contribution_axis(ref()$totals$value[1])+
       scale_fill_manual(values=c('TRUE'='#007e80','FALSE'='#9b6171'),guide='none')+
       labs(x=NULL,y=if(measure()=='pct')'Contribution (pp GDP)' else paste('Contribution (',d()$unit,')'))+theme_e61()})
   output$contribution_table<-shiny::renderTable({z<-contributions();data.frame(Driver=z$factor,Contribution=round(z$contribution,4))},striped=TRUE)
   if(programme=='Health'){
     per_person<-shiny::reactive(health_per_person_data(scenario(),selected(),end(),
       identical(input$per_person_measure,'relative_real_gdp')))
     person_contributions<-shiny::reactive({v<-per_person();z<-aggregate(contribution~factor,v$contributions,sum)
       z[match(unname(v$labels),z$factor),]})
     output$per_person_summary<-shiny::renderText({z<-per_person();v<-z$totals$value
       sprintf('2030 to %s: %.2f to %.2f %s; change %+.2f. Contributions below sum to this change.',
         end(),v[1],tail(v,1),z$unit,tail(v,1)-v[1])})
     output$per_person_contributions<-shiny::renderPlot({v<-per_person();z<-person_contributions()
       z$factor<-factor(z$factor,levels=rev(unname(v$labels)))
       ggplot(z,aes(factor,contribution,fill=contribution>=0))+geom_col()+coord_flip()+natural_contribution_axis(v$totals$value[1])+
         scale_fill_manual(values=c('TRUE'='#007e80','FALSE'='#9b6171'),guide='none')+
         labs(x=NULL,y=paste('Contribution (',v$contribution_unit,')',sep=''))+theme_e61()})
     output$per_person_table<-shiny::renderTable({z<-person_contributions()
       data.frame(Driver=z$factor,Contribution=round(z$contribution,4))},striped=TRUE)
     output$per_person_download<-shiny::downloadHandler(filename=function()'health_per_person_contributions.csv',
       content=function(file)write.csv(per_person()$contributions,file,row.names=FALSE))
     output$per_person_annual_download<-shiny::downloadHandler(filename=function()'health_per_person_annual.csv',
       content=function(file){z<-per_person();d<-z$annual;d$measure<-input$per_person_measure;d$unit<-z$unit
         write.csv(d,file,row.names=FALSE)})
   }
   output$demand<-shiny::renderPlot({z<-d()$annual;z<-z[!duplicated(z$year),]
     # Population and total demand are common only within each subfunction;
     # use anchor weights to build an explicit total-demand index for aggregates.
     u<-d()$annual;total<-aggregate(I(anchor_2030_bn*exposure_index)~year,u,sum)
     total$value<-total[[2]]/sum(spec()$anchor_2030_bn)
     p<-data.frame(year=z$year,value=z$population_factor,Driver='Population size')
     q<-data.frame(year=total$year,value=total$value,Driver='Age/cohort-weighted demand')
     ggplot(rbind(p,q),aes(year,value,colour=Driver))+geom_line(linewidth=1)+natural_y(rbind(p,q))+
       scale_colour_manual(values=c('Population size'='#9b6171','Age/cohort-weighted demand'='#007e80'))+
       labs(x='Financial year ending',y='Demand index (2030 = 1)',colour=NULL)+theme_e61()})
   output$demand_notes<-shiny::renderUI({shiny::tagList(
       shiny::p('Population uses the official annual 2026 IGR age/sex projections, with all age/sex groups scaled equally within each year to your population-growth setting. This is not a new migration or fertility simulation.'),
       if(programme=='NDIS'){
         m<-scenario()$path;z<-economic_inputs(inputs,scenario()$assumptions)$ndis
         p<-z$participants[match(c(2030,end()),z$year)];old<-z$participants_65plus[match(c(2030,end()),z$year)]
         shiny::p(sprintf('Implied participants: %.0f in 2030 and %.0f in %s. Participants aged 65+: %.0f and %.0f respectively. These are model counts, not an NDIA forecast.',p[1],p[2],end(),old[1],old[2]))
       },
     if(programme=='NDIS')shiny::p('Below 65: people in each age group x fixed observed NDIA participation rates. From 65: existing cohorts age forward using population survival ratios, with no new older entrants and a 100+ terminal group. The population ratios are a retention proxy, not observed NDIS-specific survival. The implied eligible-participant count is not multiplied by a second coverage assumption.') else
       shiny::p('Health demand sums population x fixed IGR age-cost weights for hospital, medical and pharmaceutical profiles. These weights already embody age differences in observed use and cost; they are not a pure patient count. Education uses age-band populations rather than observed enrolment projections. Aggregate demand is an anchor-weighted index, not a count of interchangeable services.'))})
   output$demand_inputs<-shiny::renderTable({
     if(programme=='NDIS')return(data.frame('Age from'=inputs$ndis_incidence$age_min,'Age to'=inputs$ndis_incidence$age_max,
       'Observed participation (%)'=100*inputs$ndis_incidence$participation_rate,check.names=FALSE))
     s<-spec()
     if(programme=='Health'){
       p<-inputs$subfunction_age_profiles;p<-p[p$profile%in%s$profile,]
       if(nrow(p))return(data.frame(Profile=p$profile,'Age from'=p$age_min,'Age to'=p$age_max,'Fixed age-cost weight'=p$weight,Source=p$source,check.names=FALSE))
     }
     data.frame(Subfunction=s$subfunction,Profile=s$profile,
       'Demand in 2025 (profile units)'=round(s$demand_2025,1),'Demand in 2030 (profile units)'=round(s$demand_2030,1),check.names=FALSE)
   },striped=TRUE)
   output$bridge<-shiny::renderTable({s<-scenario()$calibration$subfunctions;s<-s[s$programme==programme,]
     data.frame(Subfunction=s$subfunction,'2025 source ($bn)'=round(s$source_2025_bn,3),
       'Category reconciliation (x)'=round(s$parent_reconciliation_factor,4),
       '2025 reconciled ($bn)'=round(s$base_2025_bn,3),'2030 before calibration ($bn)'=round(s$raw_2030_bn,3),
       '2030 reconciliation (x)'=round(s$anchor_reconciliation_factor,4),'2030 anchor ($bn)'=round(s$anchor_2030_bn,3),check.names=FALSE)},striped=TRUE)
   output$anchor_notes<-shiny::renderUI({q<-scenario()$calibration$programmes;q<-q[q$programme==programme,]
     shiny::tagList(shiny::p(sprintf('Before calibration, each reconciled 2025 amount is multiplied by its 2030/2025 demand ratio x 1.025^5 x 1.012^(5 x %.2f) x 1.003^5 x (1 + %.4f)^5. These are the allocation assumptions: 2.5%% general prices, 1.2%% productivity, %.0f%% category pass-through, 0.3%% service-price premium and %.2f%% intensity per year. Subfunction shares are then reconciled to the category anchor; category anchors together match PBO primary spending. These are e61 allocations, not published PBO sector projections. Current sliders do not rebuild this anchor.',q$wage_pass_through,q$excess_cost_growth,100*q$wage_pass_through,100*q$excess_cost_growth)),
     if(programme=='Education')shiny::p('The ABS tertiary total is split into higher education and VET using a cross-source expenses proxy. University provider expenses include private funding and research, so this is an allocation judgment, not an observed general-government split.') else if(programme=='NDIS')shiny::p('The observed base is $46.352178bn of participant plan expenses, not total Scheme administration or the Commonwealth net contribution. Administration remains in broader residual spending. The 2030 calibration can differ from the Scheme Actuary projection.') else shiny::p('Named AIHW funding blocks are reconciled to ABS GFS health. Other health includes the residual classification/accounting difference; it is not a single measured service. Transfers between governments are not added as extra national spending.'))})
   output$mechanism<-shiny::renderUI({shiny::tagList(
     if(programme=='Health' && scenario()$assumptions$health_specification=='income_costs')shiny::tagList(
       shiny::p('Income demand = (real GDP per person / anchor)^elasticity. Real GDP per person includes employment, participation, average hours and labour productivity; it is not nominal income per person or productivity alone. This is accommodated spending pressure, not a forecast that governments automatically fund it.'),
       shiny::p('Health CPI defaults to the existing domestic-price proxy. The advanced wedge adds to its annual inflation rate and leaves nominal wages unchanged. The GDP deflator differs when terms of trade move. This price experiment affects health only. Remaining pressures are additional to the explicit channels; historical residuals have been re-estimated after removing income demand.')),
     shiny::p(if(programme=='NDIS')
       'NDIS plan spending = participant/cohort demand x general prices x original plan-cost index x additional cost/use growth. The plan-cost index is (1 - 0.85) + 0.85 x real wage index. Additional cost/use growth defaults to 0.5% annually plus the policy adjustment. No service-efficiency deduction is applied to participant payments.' else
       'Nominal unit cost = general prices x [(1 - labour share) + labour share x real wage index / service-productivity index]. Service productivity grows at the chosen share of economy-wide productivity growth. The efficiency offset applies to labour requirements, not to non-labour inputs.'),
     shiny::p('Real wages follow economy-wide productivity. If service productivity grows more slowly, labour cost per service rises relative to general prices: this is the Baumol mechanism. Additional service growth is a separate allowance for intensity, quality and mix. It is not another Baumol uplift.'),
     shiny::p('Medicines follow a CPI unit-cost proxy. VET follows a wage/CPI funding-index proxy. These funding rules have no service-efficiency deduction. Cash-indexed benefits follow their stated earnings weight. Each additional-growth adjustment adds to the subfunction parameter; no residual is re-estimated when the macro scenario changes.'))})
   output$factor_check<-shiny::renderTable({z<-d()$annual;z<-z[z$year==end(),];r<-data.frame(Subfunction=z$subfunction,'2030 anchor ($bn)'=round(z$anchor_2030_bn,3),check.names=FALSE)
     for(f in setdiff(names(spending_factor_labels()),'gdp_factor'))r[[unname(spending_factor_labels()[f])]]<-round(z[[f]],4)
     r[['End nominal spending ($bn)']]<-round(z$spending_bn,3);r},striped=TRUE)
   evidence<-shiny::reactive({
     z<-service_cost_comparison(scenario(),inputs);z<-z[z$id%in%spec()$id,]
     merge(spec(),z[,setdiff(names(z),c('programme','subfunction'))],by='id',sort=FALSE)
   })
   link<-function(url,label){
     if(is.na(url)||!nzchar(url))return(shiny::span('No empirical estimate cited'))
     if(grepl('^https://',url))return(shiny::tags$a(href=strsplit(url,'[;[:space:]]')[[1]][1],target='_blank',label))
     shiny::span(url)
   }
   output$evidence<-shiny::renderUI({s<-spec();hp<-service_cost_comparison(scenario(),inputs)
     shiny::tagList(
       shiny::h4('Common drivers'),
       shiny::p(link('https://population.gov.au/publications/statements/2025-population-statement','Population Statement 2025'),': sourced age/sex structure; selected population total is a scenario. ',
         link('https://treasury.gov.au/publication/2026-intergenerational-report','2026 IGR'),': fixed hospital, medical and pharmaceutical age weights (C12.10); using them for every corresponding funding block is a proxy.'),
       shiny::p('General prices: the model reconciles your nominal GDP per person, population, productivity, hours and terms-of-trade assumptions. Price growth is an accounting consequence conditional on that macro structure, not an independent health, education or NDIS price forecast. GDP-share results also divide by that nominal GDP path.'),
       shiny::p('Health income elasticity defaults to 0.8 as an international-evidence-informed spending scenario, not an Australian causal estimate. Efficiency at 25% is an arbitrary modelling choice. New health remaining pressures default to zero beyond the explicit channels. Matched 2016-2025 health residuals remove income demand; the 1999-2025 national-accounts alternative is an aggregate scope proxy, not matched GFS subfunction data. Education retains its existing model. Historical estimates never refit when a slider moves.'),
       if(programme=='NDIS')shiny::p(link('https://www.ndis.gov.au/media/7891/download','NDIA Q4 2024-25 Table D.13'),': sourced rounded participation rates; fixed future age rates and cohort retention are proxies. The participant/cohort projection is retained. The original 85% wage-linked plan-cost index and 0.5% additional cost/use growth are modelling judgements, not estimated efficiency parameters. Participant payments receive no service-productivity deduction. Coverage remains 1. Administration and reform-specific behavioural effects are not separately simulated.'),
       lapply(seq_len(nrow(s)),function(i){v<-s[i,];p<-hp[hp$id==v$id,]
         shiny::tags$details(open=TRUE,shiny::tags$summary(v$subfunction),
           shiny::p(shiny::strong('Starting allocation: '),v$status,'. ',v$locator,'. ',link(v$source,'Source'),'. ',v$limits),
           shiny::p(shiny::strong('Demand: '),switch(v$profile,hospital='IGR hospital age-cost profile (C12.10).',medical='IGR medical-benefits age-cost profile (C12.10).',pharma='IGR pharmaceutical age-cost profile (C12.10).',school_age='Population ages 5-17; fixed enrolment/coverage proxy.',tertiary_age='Population ages 18-24; fixed participation proxy.',vet_age='Population ages 15-64; fixed training participation/hours proxy.',ndis='NDIA age participation plus retained older cohorts.',population='Total population; heterogeneous residual proxy.')),
           if(nrow(p))shiny::tagList(shiny::p(shiny::strong('Unit-cost evidence: '),p$evidence_class,'. Wage weight ',sprintf('%.2f%%',100*p$wage_weight),'. ',p$locator,'. ',link(p$source_url,'Cost source')),
             shiny::p(sprintf('Selected assumptions: coverage growth %.2f%%/yr; service productivity %.1f%% of economy-wide growth (%.2f%%/yr); additional service growth %.2f%%/yr.',100*p$coverage_growth,100*p$selected_service_productivity_share,100*p$selected_service_productivity_growth,100*p$selected_additional_service_growth)), shiny::p(shiny::strong('Comparator: '),p$benchmark_description,'. ',p$benchmark_scope),
             shiny::p(shiny::strong('Limits: '),p$limitation)))}) )})
   output$contribution_download<-shiny::downloadHandler(filename=function()paste0(tolower(programme),'_driver_contributions.csv'),content=function(file)write.csv(d()$contributions,file,row.names=FALSE))
   output$annual_download<-shiny::downloadHandler(filename=function()paste0(tolower(programme),'_annual_calculation.csv'),content=function(file){
     z<-merge(d()$annual,spec()[,c('id','profile','source','locator','status','limits')],by='id',sort=FALSE)
     z$nominal_gdp_bn<-scenario()$path$nominal_gdp[match(z$year,scenario()$path$year)]
       z$nominal_gdp_per_capita_growth<-scenario()$path$nominal_gdp_per_capita_growth[match(z$year,scenario()$path$year)]
     z$domestic_price_index<-scenario()$path$domestic_price_index[match(z$year,scenario()$path$year)]
       z$general_price_index<-z$price_factor
       z$nominal_hourly_wage_index<-scenario()$path$nominal_hourly_wage_index[match(z$year,scenario()$path$year)]
       z$gdp_deflator_index<-scenario()$path$gdp_deflator_index[match(z$year,scenario()$path$year)]
     write.csv(z,file,row.names=FALSE)})
   output$evidence_download<-shiny::downloadHandler(filename=function()paste0(tolower(programme),'_evidence_register.csv'),content=function(file){
     z<-evidence();a<-scenario()$assumptions;z$selected_productivity<-a$productivity
     z$selected_growth_adjustment<-a[[cost_pass_through_parameters()[[programme]]]]
     z$selected_macro_mode<-a$macro_mode;z$selected_domestic_price_growth<-if(a$macro_mode=='prices')a$domestic_price_growth else NA_real_;z$selected_ngdp_per_capita_growth<-if(a$macro_mode=='ngdp_per_capita')a$ngdp_per_capita_growth else NA_real_;z$selected_population_growth<-a$population_growth
     z$selected_hours_growth<-a$hours_growth;z$selected_tot_change<-a$tot_change
     write.csv(z,file,row.names=FALSE)})
 })
}
