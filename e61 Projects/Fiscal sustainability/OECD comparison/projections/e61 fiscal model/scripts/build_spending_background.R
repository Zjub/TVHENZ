# Reproducible background report, figures and walk-through tables. Offline.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
source('R/model.R');library(htmltools);library(ggplot2)
dir.create('documentation',showWarnings=FALSE);dir.create('outputs/spending_background',showWarnings=FALSE)
x<-load_inputs();cal<-calibrate(x);a<-default_assumptions();z<-project_model(x,a,cal);m<-z$path;s<-cal$subfunctions
aihw<-'https://www.aihw.gov.au/reports/health-welfare-expenditure/health-expenditure-australia-2024-25/contents/spending-trends-by-source/'
legacy<-new.env(parent=globalenv());previous_root<-'archive/pre_health_education_2026_10_27'
sys.source(file.path(previous_root,'model_v2.R'),envir=legacy);sys.source(file.path(previous_root,'spending_bottom_up.R'),envir=legacy)
old_x<-legacy$load_inputs();old_x$subfunctions<-read.csv(file.path(previous_root,'spending_subfunctions.csv'))
old<-legacy$project_model(old_x,legacy$default_assumptions(),legacy$calibrate(old_x))
fmt<-function(v,d=2)formatC(v,format='f',digits=d,big.mark=',')
table_html<-function(d,digits=2){
 d<-as.data.frame(d,check.names=FALSE)
 for(n in names(d))if(is.numeric(d[[n]]))d[[n]]<-fmt(d[[n]],digits)
 tags$table(tags$thead(tags$tr(lapply(names(d),tags$th))),tags$tbody(lapply(seq_len(nrow(d)),function(i)tags$tr(lapply(d[i,],function(v)tags$td(as.character(v)))))))
}
para<-function(...)tags$p(...);h2<-function(t)tags$h2(t);h3<-function(t)tags$h3(t)
link<-function(url,label)tags$a(href=url,label)
figure<-function(plot,name,caption,width=8,height=4.5){
 file<-file.path('outputs/spending_background',paste0(name,'.png'));ggsave(file,plot,width=width,height=height,dpi=170)
 tags$figure(tags$img(src=base64enc::dataURI(file=file,mime='image/png')),tags$figcaption(caption))
}
contrib<-setNames(lapply(c('bn','real_bn','pct'),function(u)spending_contributions(z,measure=u,include_financing=TRUE)),c('bn','real_bn','pct'))
for(n in names(contrib))write.csv(contrib[[n]],paste0('outputs/spending_background/contributions_',n,'.csv'),row.names=FALSE)
write.csv(s,'outputs/spending_background/calibration.csv',row.names=FALSE)
write.csv(z$subfunctions,'outputs/spending_background/annual_subfunctions.csv',row.names=FALSE)
endpoints<-merge(subset(z$components,year==2030,c('programme','spending_bn','share_gdp')),
 subset(z$components,year==2066,c('programme','spending_bn','share_gdp')),by='programme',suffixes=c('_2030','_2066'))
endpoints$change_pp<-endpoints$share_gdp_2066-endpoints$share_gdp_2030
write.csv(endpoints,'outputs/spending_background/category_endpoints.csv',row.names=FALSE)
previous<-subset(old$components,year==2066,c('programme','share_gdp'));names(previous)[2]<-'previous_pct'
comparison<-merge(endpoints[,c('programme','share_gdp_2066')],previous,by='programme');comparison$revision_pp<-comparison$share_gdp_2066-comparison$previous_pct
write.csv(comparison,'outputs/spending_background/revision_vs_previous.csv',row.names=FALSE)
all_sourced<-data.frame(Category=s$programme,Subfunction=s$subfunction,'2025 source/reconciled ($bn)'=s$source_2025_bn,
 '2030 allocation ($bn)'=s$anchor_2030_bn,'Share of parent in 2030 (%)'=100*s$parent_share_2030,check.names=FALSE)
factors<-data.frame(Factor=unname(spending_factor_labels()),Definition=c(
 'Total population relative to 2030; one for policy-target and interest blocks.',
 'Age-weighted exposure divided by total population. NDIS includes cohort retention, not a new disability-incidence forecast.',
 'Pension recipient coverage relative to 2030. One for other blocks: constant eligibility/uptake is a maintained assumption.',
 'Domestic price index inferred from nominal GDP, real output and ToT; one for target/interest rules. In real-dollar analysis this factor includes deflation.',
 'Health/education: separate input baskets or funding proxies divided by service productivity. Other services: productivity exponents. Pension: mean real payment. Investment: exponent 0.5.',
 'Additional service price factor, 0.3% a year by default; excluded from medicine CPI and VET funding proxies.',
 'Additional programme intensity growth: Health 0.5%, aged care 0.3%, NDIS 0.5%, others zero. Guesses, not fitted residuals.',
 'Defence spending follows its GDP-share target. Interest follows gross-debt financing. This factor captures the whole rule, not an identified policy shock.',
 'Health/education common annual cost adjustment compounded after 2030; zero by default, preserving subfunction differences.',
 '2030 GDP divided by current GDP. Used only for GDP-share contributions; the nominal income denominator must be accounted for.'),check.names=FALSE)
sum_factors<-function(d,scope=NULL){if(!is.null(scope))d<-d[d$programme%in%scope,];v<-aggregate(contribution~factor,d,sum);v[match(unname(spending_factor_labels()),v$factor),]}
primary<-cal$programmes$programme
nom<-sum_factors(contrib$bn,primary);real<-sum_factors(contrib$real_bn,primary);pct<-sum_factors(contrib$pct,primary)
factor_summary<-data.frame(Factor=nom$factor,'Nominal change ($bn)'=nom$contribution,
 'Real change ($2030 bn)'=real$contribution,'GDP-share change (pp)'=pct$contribution,check.names=FALSE)
write.csv(factor_summary,'outputs/spending_background/primary_factor_summary.csv',row.names=FALSE)
all_nom<-sum_factors(contrib$bn);all_real<-sum_factors(contrib$real_bn);all_pct<-sum_factors(contrib$pct)
all_factor_summary<-data.frame(Factor=all_nom$factor,'Nominal change ($bn)'=all_nom$contribution,
 'Real change ($2030 bn)'=all_real$contribution,'GDP-share change (pp)'=all_pct$contribution,check.names=FALSE)
write.csv(all_factor_summary,'outputs/spending_background/total_factor_summary.csv',row.names=FALSE)
economy<-data.frame(Assumption=c('Projection window','Population growth','NGDP per person growth','Productivity growth','Average hours growth','Relative service prices','Tax thresholds','Real super return','Long-run nominal borrowing rate'),
 Value=c('Financial years ending 2030 to 2066','1.2% a year','3.2% a year','1.2% a year','0% a year','0.3% a year','Wage indexed','4% a year','4.4%'))
plot_theme<-theme_minimal(base_size=11)+theme(panel.grid.major.y=element_blank(),legend.position='bottom')
graph_data<-rbind(transform(endpoints[,c('programme','share_gdp_2030')],Year='2030',value=share_gdp_2030)[,c('programme','Year','value')],
 transform(endpoints[,c('programme','share_gdp_2066')],Year='2066',value=share_gdp_2066)[,c('programme','Year','value')])
chart_categories<-ggplot(graph_data,aes(reorder(programme,value),value,fill=Year))+geom_col(position='dodge')+coord_flip()+
 scale_fill_manual(values=c('2030'='#9b6171','2066'='#007e80'))+labs(x=NULL,y='Primary expenses (% GDP)')+plot_theme
chart_factor<-ggplot(pct,aes(factor(factor,levels=rev(unname(spending_factor_labels()))),contribution,fill=contribution>=0))+
 geom_col()+coord_flip()+scale_fill_manual(values=c('TRUE'='#007e80','FALSE'='#9b6171'),guide='none')+
 labs(x=NULL,y='Contribution to change in primary expenses (percentage points of GDP)')+plot_theme
hp<-x$subfunction_age_profiles
chart_age<-ggplot(hp,aes(age_min,weight/ave(weight,profile,FUN=max),colour=profile))+geom_line(linewidth=.9)+
 labs(x='Age (lower bound of group)',y='Profile divided by its own maximum',colour='Health profile')+plot_theme
worked<-subset(z$subfunctions,id=='h_hospital' & year==2066)
walk<-data.frame(Step=c('2025 hospital government funding','2030 calibrated hospital allocation','Population factor','Age/cohort factor','General price factor','Real unit-cost factor','Relative service-price factor','Intensity factor','Aggregate cost multiplier','2066 hospital spending'),
 Value=c(paste0('$',fmt(subset(s,id=='h_hospital')$source_2025_bn),'bn'),paste0('$',fmt(worked$anchor_2030_bn),'bn'),
  fmt(worked$population_factor,4),fmt(worked$age_factor,4),fmt(worked$price_factor,4),fmt(worked$real_unit_factor,4),fmt(worked$relative_price_factor,4),fmt(worked$intensity_factor,4),fmt(worked$aggregate_cost_factor,4),paste0('$',fmt(worked$spending_bn),'bn')))
baseline<-data.frame(Measure=c('Nominal GDP ($bn)','Primary expenses ($bn)','Interest expenses ($bn)','Net capital investment ($bn)','Total spending including net investment ($bn)','Primary expenses (% GDP)','Total spending including net investment (% GDP)','Net debt (% GDP)'),
 '2030'=as.numeric(m[1,c('nominal_gdp','primary_expense_bn','interest_bn','net_capital_investment_bn','total_spending_bn','primary_expense_pct','total_spending_pct','net_debt_pct')]),
 '2066'=as.numeric(m[nrow(m),c('nominal_gdp','primary_expense_bn','interest_bn','net_capital_investment_bn','total_spending_bn','primary_expense_pct','total_spending_pct','net_debt_pct')]),check.names=FALSE)
programme_sections<-lapply(c(primary,'Interest','Net capital investment'),function(n){
   d0<-sum_factors(contrib$bn,n);d1<-sum_factors(contrib$real_bn,n);d2<-sum_factors(contrib$pct,n)
   d<-data.frame(Factor=d0$factor,'Nominal $bn'=d0$contribution,'Real $2030 bn'=d1$contribution,'GDP share pp'=d2$contribution,check.names=FALSE)
   tagList(h3(n),table_html(d,3))
})
source_sections<-lapply(seq_len(nrow(s)),function(i){q<-s[i,];tagList(
 h3(paste(q$id,q$subfunction,sep=': ')),para(tags$strong('Source and extraction: '),if(startsWith(q$source,'http')&&!grepl(';',q$source,fixed=TRUE))link(q$source,q$source) else q$source,'; ',q$locator,'.'),
 para(tags$strong('Allocation and demand: '),paste0('2025 $',fmt(q$source_2025_bn),'bn; reconciled $',fmt(q$base_2025_bn),'bn; raw 2030 $',fmt(q$raw_2030_bn),'bn; final 2030 $',fmt(q$anchor_2030_bn),'bn. Demand profile: ',q$profile,'. Parent reconciliation factor: ',fmt(q$parent_reconciliation_factor,6),'; 2030 anchor factor: ',fmt(q$anchor_reconciliation_factor,6),'.')),
 para(tags$strong('Evidence status: '),q$status,'. ',q$limits,'.'),
 if(q$id%in%x$health_education_parameters$id){
   p<-x$health_education_parameters[match(q$id,x$health_education_parameters$id),]
   tagList(para(tags$strong('Cost rule: '),p$cost_rule,'; wage weight ',fmt(100*p$wage_weight,3),'%; ',p$evidence_class,'. ',p$locator,'.'),
     para(tags$strong('Cost source and limitation: '),if(nzchar(p$source_url))link(p$source_url,p$source_url) else 'Retained explicit scenario judgment','; ',p$limitation,'.'))
 } else NULL)})
manifest<-read.csv('data/raw/bottom_up/source_manifest.csv')
extra_manifest<-read.csv('data/raw/health_education/source_manifest.csv')
manifest<-rbind(manifest[,c('file','snapshot_date','bytes','md5')],data.frame(file=extra_manifest$file,snapshot_date=extra_manifest$accessed,bytes=extra_manifest$bytes,md5=extra_manifest$md5))
cost_spec<-merge(s[,c('id','subfunction')],x$health_education_parameters,by='id')
coverage<-data.frame(Item=c('Health named government-funding blocks','Health residual and GFS reconciliation','Other social protection, still unallocated'),
 '2025 ($bn)'=c(sum(subset(s,programme=='Health' & id!='h_other')$source_2025_bn),subset(s,id=='h_other')$source_2025_bn,subset(s,id=='s_other')$source_2025_bn),check.names=FALSE)
body<-tagList(
 tags$header(tags$div(class='eyebrow','e61 fiscal model | background paper | 27 October 2026'),
 tags$h1('Constructing bottom-up government spending projections'),
 para(class='lead','A walk-through of allocations, demographic demand, cost assumptions and factor contributions for Australian consolidated general government.')),
 para(tags$strong('Status: '),'research scenario model. The expansion creates 24 separately projected blocks, including separate higher education and VET, with explicit proxies and residuals. Health/education now use distinct cost rules and a common aggregate multiplier. It does not establish causal productivity elasticities or reproduce official programme forecasts. The report uses default assumptions; the Shiny recalculates the same decomposition for selected scenarios.'),
 h2('1. What has changed, and what remains an assumption'),
 para('The initial prototype used one demand index per broad category. The first expansion introduced separate health age profiles and named spending blocks. This update adds distinct health/education input-cost rules, separates higher education and VET, and replaces their aggregate productivity-exponent overrides with common annual cost multipliers. Existing NDIS and pension paths, the fiscal engine and all 2030 category totals are retained.'),
 para('This is a hybrid bottom-up model. Published allocations and age patterns anchor the estimates, while coverage, cost response, intensity and some demand profiles remain assumptions. A detailed name is not evidence of a detailed behavioural model. Residential/home care still share an age profile; other social protection remains a working-age residual. These are priorities for subsequent refinement.'),
 table_html(coverage),
 h2('2. Accounting perimeter and construction sequence'),
 para('The perimeter is consolidated Australian general government. ABS national totals are the accounting control. Commonwealth grants to states, GST transfers and contributions to NDIS funding are internal transfers, not extra national spending. AIHW government-funder amounts are used only to allocate named health blocks; state own-source funding is combined with Commonwealth funding without adding grants again. Private household and insurance funding are excluded from allocations.'),
 table_html(data.frame(Stage=as.character(1:6),Construction=c('2025 ABS functional primary expense controls, removing conventional interest once',
 'Allocate named subfunctions using ABS education, AIHW government health funding and GEN care amounts; retain explicit residuals',
 'Project subfunction allocation weights to 2030 using demographic exposures and the existing bridge assumptions',
 'Reconcile within each category to its unchanged 2030 model anchor; all categories still match PBO aggregate primary expenses',
 'Project each block from its own 2030 allocation using consistent multiplicative factors',
 'Sum blocks to categories; add modelled interest and net capital investment in the financing engine'))),
 para('ABS primary expenses include depreciation and capital transfers where classified as expenses. Gross capital formation is not added again. The model adds net capital investment from the PBO anchor separately; expenses plus net investment is the fiscal-balance spending measure. Source concepts differ: AIHW recurrent funding, programme cash outlays and GFS accrued expenses do not perfectly align. Reconciliation is an allocation device, not proof of conceptual equivalence.'),
 h2('3. The source-to-anchor bridge'),
 para('The aggregate 2030 anchor comes from the PBO 2026-27 National Fiscal Outlook. Individual category anchors are inherited e61 allocations, not published PBO category forecasts. The prior category bridge used 2025 GFS/programme outlays, exposure growth, 2.5% price growth, 1.2% productivity growth, programme pass-through, 0.3% extra service prices where applicable and programme intensity; one common factor matched PBO primary expenses. This bridge is retained to isolate the effects of the subfunction expansion.'),
 para('Within a category, the new 2030 allocation is proportional to its 2025 subfunction amount times its own 2025-30 demographic exposure growth and the bridge cost factors. A category-specific reconciliation factor then makes the sum equal the unchanged parent anchor. Changing a scenario does not recalibrate any 2030 amount.'),
 tags$pre('raw block[2030] = block[2025] x demand[2030]/demand[2025] x bridge price/cost factors\nblock anchor[2030] = raw block[2030] / sum(parent raw blocks[2030]) x parent anchor[2030]'),
 table_html(all_sourced),
 h2('4. A common decomposition'),
 tags$pre('block spending[t] = anchor[2030] x population x age/cohort demand x coverage\n                    x general prices x real unit cost/payment\n                    x relative service prices x service intensity x policy/financing rule\n                    x aggregate cost multiplier\nspending share[t] = 100 x block spending[t] / nominal GDP[t]'),
 para('Factors equal one in 2030. For ordinary services, population times age/cohort demand equals the age-weighted service exposure. Fixed age weights assume unchanged age-specific relative need, utilisation and service mix. They do not model healthy ageing, migration composition or disability severity. Population controls uniformly scale the sourced age/sex shares; employment uses the same total-population path.'),
 table_html(factors),
 para('Domestic prices are inferred from GDP = real output x domestic prices x the ToT income factor. Higher NGDP/person at unchanged real assumptions scales nominal service spending and GDP together. Hospital, school and university input weights are observed shares applied as scope proxies; VET uses a published funding mix as a proxy. Remaining weights, future coverage, service productivity and intensity are disclosed judgments. None is a causal productivity elasticity.'),
 h3('Health/education cost rules and evidence'),
 table_html(data.frame(Subfunction=cost_spec$subfunction,Rule=cost_spec$cost_rule,'Wage weight (%)'=100*cost_spec$wage_weight,Status=cost_spec$evidence_class,check.names=FALSE)),
 tags$pre('Input basket: real unit cost = [(1-L) + L x real hourly wage index] / service-productivity index\nVET: real unit cost = (1 + 0.6975 x annual productivity growth)^years / service-productivity index\nAggregate multiplier = (1 + selected annual cost adjustment)^years; default adjustment = 0'),
 para('Hospital labour share is 67.253% from IHACPA public admitted-acute national costs; school labour share is 75.993% from ACARA recurrent expenditure excluding notional user cost of capital; university employee share is 55.473% from Finance 2024. VET weight 69.75% derives from the National Skills Agreement flexible-funding formula; its productivity discount is assumed zero. These applications extend beyond exact source coverage. The active nine-row file data/health_education_parameters.csv records each rule, source locator and limitation. The README provides full arithmetic, linked references and reproduction commands.'),
 h2('5. Health: separate profiles and a reconciliation block'),
 para('Hospital allocation uses Commonwealth public/private hospital funding plus state hospital funding. Primary/community care combines Commonwealth and state primary care, subtracting PBS medicines once because those medicines are separated. Referred medical services and outside-hospital PBS medicines are separate blocks. Other health and GFS reconciliation is the ABS Health total less these named blocks; research, administration and conceptual differences are not silently assigned a hospital profile.'),
 para('Hospital services use the IGR C12.10 public-hospital age profile; applying it to private hospitals is a proxy. Primary and referred care use the medical-benefits profile, including a proxy application to community/public-health services. Medicines use the pharmaceutical-benefits profile. Other health follows total population. The three published shapes are used as relative weights, not as independently validated national unit-cost levels.'),
 figure(chart_age,'health_age_profiles','Distinct published age profiles, each normalised to its own maximum for visual comparison. Source: 2026 IGR C12.10. The 85+ source group is carried to age 100+.'),
 h3('Worked example: hospital services'),table_html(walk),
 para('Multiply the 2030 hospital allocation by the seven displayed growth factors to recover the 2066 amount. Coverage is one. The admitted-acute labour share is applied as a proxy for all hospital funding. Primary, referred and residual health retain a 70% judgmental wage weight; medicines use CPI-only unit costs with no service-price premium. Health intensity remains 0.5% throughout, unestimated. The aggregate cost slider multiplies every unit-cost path equally and preserves their differences.'),
 h2('6. Education: school, tertiary and other demand'),
 para('ABS Education Table_1 supplies consolidated operating expenses for school, tertiary and other education. These sum to the GFS Education control; school grants are not added separately. School demand is population aged 5-17, higher education ages 18-24 and VET ages 15-64. These are proxies with fixed enrolment/coverage, not fitted participation models. Other education, including preschool and residual services, follows total population. Mature-age and international enrolment changes are not separately projected.'),
 para('Higher education and VET now split the ABS tertiary total using 2024 university expenses ($43.068392bn) and VET recurrent expenditure excluding user cost of capital ($7.6172bn) as allocation weights. Concepts differ: this is a proxy, not a directly observed GFS split. Higher-education demand remains ages 18-24; VET uses ages 15-64. Schools and universities use separate observed input baskets as scope proxies; VET uses the funding proxy. Other education retains an 80% judgmental wage weight. Coverage and service productivity are constant; education intensity is zero. The inherited category bridge remains unchanged.'),
 h2('7. Care, transfers and other services'),
 para('GEN 2024-25 amounts split aged care into residential, home/support, flexible and other care. Each currently uses the same IGR aged-care age profile and parent cost rule. The split makes allocations and coverage transparent but does not yet create separate care-uptake forecasts. The existing 0.3% aged-care intensity and 80% pass-through remain in place.'),
 para('NDIS remains one participant-plan block, with the existing below-65 participation rates and retention of older cohorts. The user-selected 85% pass-through and 0.5% intensity are unchanged. Population versus age/cohort contributions separate overall scale from the participant/population ratio. The latter includes cohort retention; it should not be interpreted as pure demographic ageing or new eligibility-policy change.'),
 para('Age Pension retains the representative means-test and private-wealth model. Its factors are eligible age demand, recipient coverage, domestic prices and mean real payment per recipient. Changes in means tests, retirement assets and recipient composition can affect both coverage and mean payment. These are accounting channels, not separate causal estimates of indexation or super returns.'),
 para('Other social protection remains the GFS residual after removing pension, care and NDIS. Its demand is ages 15-66 and its existing 65% pass-through is retained; constant-relative-standards policy overrides that to 100%. It is not a simulation of the statutory indexation of individual benefits. The residual is material and should be split into disability, unemployment, family/childcare and other programmes before making stronger policy claims.'),
 para('Other public services now sum seven named GFS functions, including general public services net of conventional interest, public order/safety, economic affairs, environment, housing/community amenities, recreation/culture/religion and transport. All retain total-population demand and the common parent cost rule. Separate names identify budget weights; they do not establish workload forecasts. Interest on defined-benefit superannuation remains inside the GFS residual, consistent with the prior model.'),
 para('Defence is a target rule reaching 2.5% GDP by 2035. Its demographic and price factors are set to one and the policy factor reproduces the target path. Interest is represented by a financing factor reproducing the actual model bill; it combines gross debt and the nominal borrowing rate. Investment uses population, prices and a 0.5 productivity exponent. These explicit exceptions avoid mislabelling financing and targets as demographic service demand.'),
 h2('8. Contributions to the change: method and interpretation'),
 para('A percentage change in a factor is not its dollar contribution because factors interact. We use exact Shapley accounting: average each factor\'s marginal contribution across all orders of switching factors from the start-year to end-year values. This allocates interactions symmetrically, handles zero-valued factors and sums exactly to each block\'s observed model change. Contributions are summed from blocks to categories and national totals. They are not regression coefficients, causal effects or confidence intervals.'),
 para('For example, a $100 starting amount with population increasing 10% and prices 20% ends at $132. The method assigns $11 to population and $21 to prices, sharing the $2 interaction. Factors which do not change receive zero. Contributions to a GDP-share change include the GDP denominator as a separate factor. This is why population and prices can have positive numerator contributions alongside a negative denominator contribution.'),
 para('Nominal-dollar contributions describe the actual projected dollar increase. Real-dollar contributions divide by the domestic price index, in 2030 prices; the general-price factor cancels for ordinary price-indexed services. This is a model deflator, not a separately measured sector output-price index. GDP-share contributions describe percentage-point changes in fiscal burden. Always choose the measure that answers the question being considered.'),
 h3('Primary spending contributions'),table_html(factor_summary,3),
 h3('All spending, including interest and net capital investment'),table_html(all_factor_summary,3),
 figure(chart_factor,'primary_share_contributions','Exact factor accounting for the change in primary spending/GDP, 2030-2066. Positive numerator effects and the denominator effect should be read together.'),
 h2('9. Default projection and revision from the previous model'),table_html(economy),table_html(baseline),
 figure(chart_categories,'category_shares','The category totals are sums of separately projected subfunctions; the starting category anchors are fixed.'),
 table_html(data.frame(Category=endpoints$programme,'2030 ($bn)'=endpoints$spending_bn_2030,'2066 ($bn)'=endpoints$spending_bn_2066,'2030 (% GDP)'=endpoints$share_gdp_2030,'2066 (% GDP)'=endpoints$share_gdp_2066,'Change (pp)'=endpoints$change_pp,check.names=FALSE)),
 para('The next table compares this update with the frozen 23-block model immediately before the health/education cost changes, using identical macro and tax-policy defaults and neutral cost controls in each version. Distinct input-cost assumptions and the separate higher/VET allocation and demand revise spending. The other six primary category paths are unchanged. Debt and interest change as a consequence. The revision does not establish forecast accuracy.'),
 table_html(data.frame(Category=comparison$programme,'Previous 2066 (% GDP)'=comparison$previous_pct,'Expanded 2066 (% GDP)'=comparison$share_gdp_2066,'Revision (pp)'=comparison$revision_pp,check.names=FALSE),3),
 h2('10. Category-by-category factor contributions'),programme_sections,
 h2('11. Source and assumption audit by subfunction'),
 para('Sourced means a published amount or shape. Calibrated means an allocation reconciled to a control total. Proxy means an assumed relationship to an available population/base. Judgment means an unestimated future response. Identity means an accounting consequence. The following entries disclose the role of each source and the additional assumptions; citing a source for an amount does not validate its future growth rule.'),source_sections,
 h2('12. Reproduction, checks and the next assumption discussion'),
 para('Run scripts/acquire_bottom_up_sources.R once to cache additional official sources, then scripts/prepare_bottom_up_inputs.R offline. Run tests/validate_bottom_up.R and scripts/build_spending_background.R from the project or via Rscript from its parent directory. The report generator writes its HTML, PNG figures and CSV walk-through tables as well as this PDF. Annual subfunction and factor exports are available from Shiny.'),
 para('The 64 bottom-up checks cover controls, unchanged anchors, NDIS/pension preservation, aggregation, factor identities and financing stress. A further 57 health/education checks verify observed-share arithmetic, separate rules, higher/VET demand, uniform scaling, zero-productivity scenarios and unchanged other categories. These establish implementation consistency, not forecast accuracy.'),
 table_html(data.frame(File=c('data/spending_subfunctions.csv','data/processed/subfunction_age_profiles.csv','R/spending_bottom_up.R','outputs/spending_background/calibration.csv','outputs/spending_background/annual_subfunctions.csv','outputs/spending_background/contributions_*.csv','archive/pre_bottom_up_2026_10_27'),Purpose=c('Source allocations, profile mapping, locators and limitations','Separate health age profiles extracted from IGR C12.10','Projection, aggregation and exact contribution functions','All source-to-GFS-to-2030 reconciliation calculations','Annual spending and multiplicative factors for all 24 blocks','Factor contributions by block, category and measure','Frozen prior spending implementation and report'))),
 para('The next discussion should prioritise: age-specific enrolments and tertiary participation; separate residential/home care utilisation; splitting residual social protection and other health; measured wage/non-wage exposure; service-efficiency trends; and residual intensity with policy breaks removed. The present pass-through and intensity controls remain scenario judgments while that evidence is assembled. No new statistical uncertainty bands are claimed.'),
 h3('Primary source links'),tags$ul(
 tags$li(link('https://www.pbo.gov.au/publications-and-data/publications/2026-27-National-Fiscal-Outlook','PBO National Fiscal Outlook 2026-27'),': consolidated anchor; workbook Table 1/2.'),
 tags$li(link('https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/2024-25','ABS GFS 2024-25'),': national Table 4 controls; Education workbook Table_1.'),
 tags$li(link('https://treasury.gov.au/publication/2026-intergenerational-report','2026 IGR chart data'),': C8.6 population shares, C9.13 participation, C12.10 health profiles, C6.6 aged-care profile, C12.19 pension starting status.'),
 tags$li(link(paste0(aihw,'australian-government-spending'),'AIHW Commonwealth government health spending 2024-25'),'; ',link(paste0(aihw,'state-and-territory-government-spending'),'state own-source spending'),': rounded area allocations, Figures 11 and 16; PDF report and spreadsheets not required for the manually recorded figures.'),
 tags$li(link('https://www.gen-agedcaredata.gov.au/topics/spending-on-aged-care','GEN aged-care expenditure'),': Figure 1, 2024-25, updated 28 August 2026.'),
 tags$li(link('https://www.ndis.gov.au/media/8108/download','NDIA Annual Report 2024-25'),'; ',link('https://www.ndis.gov.au/media/7891/download','Q4 participation report'),': Note 1.1G and Table D.13.'),
 tags$li(link('https://www.anao.gov.au/work/performance-audit/administration-of-the-age-pension','ANAO pension outlay'),'; individual statutory parameters and dated URLs remain in data/policy_parameters.csv.')),
 h3('Additional source snapshot manifest'),table_html(manifest[,c('file','snapshot_date','bytes','md5')],0),
 para(class='footer','Prepared from the local e61 fiscal model, 27 October 2026. This background paper is a reproducible explanation of a research scenario model; it is not an official PBO, IGR, AIHW or NDIA forecast.'))
css<-'@page{size:A4;margin:17mm}body{font-family:Arial,sans-serif;font-size:9.5pt;line-height:1.45;color:#183343}h1{font-size:25pt;color:#007e80;line-height:1.15}h2{font-size:15pt;color:#007e80;margin-top:22pt;break-after:avoid}h3{font-size:11pt;break-after:avoid}.eyebrow{font-size:9pt;text-transform:uppercase;letter-spacing:1px}.lead{font-size:12pt}p{orphans:3;widows:3}table{border-collapse:collapse;width:100%;font-size:8pt;margin:12pt 0;table-layout:auto}th,td{border:1px solid #d5dfdc;padding:5pt;text-align:left;vertical-align:top;overflow-wrap:anywhere}th{background:#eaf2ef}thead{display:table-header-group}tr{break-inside:avoid}a{color:#007e80;overflow-wrap:anywhere}pre{white-space:pre-wrap;background:#f1f5f4;padding:9pt;font-size:8pt;break-inside:avoid}figure{margin:14pt 0;break-inside:avoid}img{width:100%}figcaption,.footer{font-size:8pt;color:#526570}'
html<-paste0('<!doctype html><html><head><meta charset="utf-8"><title>e61 spending background</title><style>',css,'</style></head><body>',as.character(body),'</body></html>')
writeLines(enc2utf8(html),'documentation/spending_background.html',useBytes=TRUE)
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
browser<-chromote::ChromoteSession$new();frame<-browser$Page$getFrameTree()$frameTree$frame$id
browser$Page$setDocumentContent(frameId=frame,html=html)
browser$Runtime$evaluate('Promise.all([document.fonts.ready,...Array.from(document.images).map(i=>i.decode())]).then(()=>true)',awaitPromise=TRUE)
pdf<-browser$Page$printToPDF(printBackground=TRUE,preferCSSPageSize=TRUE,displayHeaderFooter=TRUE,
 headerTemplate='<span></span>',footerTemplate='<div style="font-size:8px;width:100%;text-align:center;color:#60717b">e61 spending background | <span class="pageNumber"></span> / <span class="totalPages"></span></div>')
writeBin(jsonlite::base64_dec(pdf$data),'documentation/spending_background.pdf');browser$close()
text<-paste(pdftools::pdf_text('documentation/spending_background.pdf'),collapse='\n')
stopifnot(grepl('Source and assumption audit',text,fixed=TRUE),grepl('Other social protection',text,fixed=TRUE),grepl('57 health/education checks',text,fixed=TRUE))
cat('Created documentation/spending_background.pdf:',pdftools::pdf_info('documentation/spending_background.pdf')$pages,'pages.\n')
