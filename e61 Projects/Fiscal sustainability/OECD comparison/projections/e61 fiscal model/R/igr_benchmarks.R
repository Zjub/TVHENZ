# Comparisons preserve each projection's own coverage and nominal-GDP denominator.
igr_spending_notes<-function()list(
 Health=list(coverage='Model: consolidated government health. IGR: Commonwealth health, including transfers to states. Levels are not like-for-like; changes in GDP shares are also affected by this coverage difference.',
   mechanism='Our five health subfunctions retain separate age profiles. Hospitals use an observed admitted-acute labour-cost mix as a proxy; medicines use CPI-only unit costs; primary, referred and residual health retain explicit cost-weight judgments. Coverage and service productivity are fixed and health intensity retains a 0.5% assumption. The aggregate adjustment multiplies every unit-cost path. The IGR instead estimates programme-specific non-demographic growth from historical age/sex-adjusted spending; our input-cost decomposition is not a replication of those estimates.',source='Charts C12.9 and C12.11; IGR section 12.2 and Appendix A3 (printed page 307).'),
 Education=list(coverage='Model: consolidated government education. IGR: Commonwealth education, including school funding transfers, higher education and VET. State school spending is included in our model, so levels should not be expected to match.',
   mechanism='Our school and university cost baskets use observed employee-cost shares as proxies; higher education and VET have separate demand and cost rules. VET uses a Commonwealth flexible-funding wage/CPI mix as a proxy for the aggregate. The IGR instead indexes long-run Commonwealth school funding per student by 25% CPI and 75% wages, higher-education funding by CPI and VET funding by a wage cost index. Government delivery costs and Commonwealth grants are different concepts. We do not separately model mature-age or international enrolments.',source='Chart C12.21; IGR section 12.6 and Appendix A3 education methods (printed pages 311-312).'),
 `Age Pension`=list(coverage='Model: Age Pension. IGR benchmark: Age and Service Pensions, a slightly broader category. Both are predominantly Commonwealth programmes, but the starting allocations and GDP denominators differ.',
   mechanism='Our pension module combines people aged 67+, synthetic means-tested eligibility and coverage, payment indexation and superannuation wealth. The IGR uses its MARIA retirement-income microsimulation and projects declining pension coverage as superannuation matures. Our synthetic entitlement model is not a replication of MARIA; returns and means-test assumptions can generate different declines.',source='Chart C12.20 (Age and Service Pensions column), C12.19; IGR section 12.5 and Appendix A3.'),
 `Aged care`=list(coverage='Model: government aged care allocated from the observed national spending total. IGR: Commonwealth aged-care payments. Coverage, accounting and the common PBO calibration can produce different starting shares.',
   mechanism='Our residential, home/support, flexible and other care blocks currently share the same IGR age-cost profile and category cost rule. The IGR projects aged-care demand and policy costs; our four allocations do not yet have independently estimated utilisation, eligibility or cost paths. A common age shape therefore does not imply matching the IGR spending trajectory.',source='Chart C12.15; IGR section 12.3 and Appendix A3 aged-care methods.'),
 NDIS=list(coverage='Whole-Scheme spending as a share of each projection\'s own GDP; IGR figures are rounded to one decimal. These are not Commonwealth net funding shares.',
   mechanism='Our allocation starts at the common PBO aggregate anchor, with fixed under-65 age-specific participation rates and retention of older cohorts. The intensity slider adds to participant and unit-cost growth. We do not reproduce the Scheme Actuary\'s entrant, exit, disability or support-cost model, or explicitly cost the 2026 reforms.',source='Chart C12.17, total Scheme series; IGR section 12.4.'),
 `Other social protection`=list(coverage='Model: residual consolidated social protection after removing Age Pension, aged care and NDIS. IGR: Commonwealth income support and family assistance excluding Age Pension. This is a partial comparator, not a matching residual; childcare, service spending and classification boundaries differ.',
   mechanism='Our residual uses population aged 15-66, a selected real cost pass-through and general/relative prices. It does not model individual benefit eligibility, family composition or separate CPI/wage indexation rules. The IGR projects payments such as JobSeeker, Family Tax Benefit, disability and carer payments separately. Its declining share cannot be interpreted as validating our aggregate rule.',source='Chart C12.23; IGR section 12.7 and Table A2.3.'),
 `Other public services`=list(coverage='No directly comparable published IGR aggregate. Our category combines general public services excluding conventional interest, public order, economic affairs, environment, housing, recreation and transport across governments. The IGR Commonwealth infrastructure series covers only part of this category.',
   mechanism='Our seven functions use total population and a common category cost rule. We show scenario and reference values but leave the IGR column unavailable. Subtracting selected Commonwealth programmes from total payments would create a different residual and would not supply a valid benchmark.',source='No matching aggregate in IGR chart C12.5 or Table A2.3.'),
 Defence=list(coverage='Model: consolidated GFS defence allocation moving to a 2.5% GDP target by 2035. Comparator: IGR budget-accounting defence payments in Table A2.3. Chart C12.26 uses the broader NATO definition (3% GDP long run) and is deliberately not used here.',
   mechanism='Our 2.5% target is a scenario setting rather than a bottom-up defence cost forecast. The IGR describes defence portfolio spending reaching 2.5% by 2034, with its budget-accounting table reporting selected years. The published 2036, 2046, 2056 and 2066 observations are shown without interpolation; target timing and accounting need not match.',source='Table A2.3, printed page 299; IGR section 12.8. Chart C12.5 verifies the final budget-accounting share.'))

igr_spending_comparison<-function(projection,reference,official,programme){
 years<-sort(unique(c(2030L,2035L,2050L,2066L,if(programme=='Defence')official$year[official$programme==programme])))
 d<-projection$components[projection$components$programme==programme,]
 r<-reference$components[reference$components$programme==programme,]
 o<-official[official$programme==programme,]
 data.frame(year=years,scenario_bn=d$spending_bn[match(years,d$year)],
   scenario_pct=d$share_gdp[match(years,d$year)],reference_pct=r$share_gdp[match(years,r$year)],
   igr_pct=o$igr_pct[match(years,o$year)])
}

format_igr_spending_table<-function(d,label){
 data.frame(Year=d$year,'Scenario ($bn)'=round(d$scenario_bn,1),
   'Scenario (% GDP)'=round(d$scenario_pct,2),'Fixed reference (% GDP)'=round(d$reference_pct,2),
   setNames(list(d$igr_pct),paste0('2026 IGR: ',label,' (% GDP)')),check.names=FALSE)
}

igr_spending_summary<-function(projection,official,programme){
 d<-projection$components[projection$components$programme==programme,]
 first<-d[d$year==2030,];last<-d[d$year==2066,];early<-d[d$year==2035,]
 text<-sprintf('Your scenario: $%.1fbn in 2030 and $%.1fbn in 2066; GDP share %.2f%% to %.2f%% (%+.2f pp). Average nominal growth in 2030-35: %.1f%% a year.',
   first$spending_bn,last$spending_bn,first$share_gdp,last$share_gdp,last$share_gdp-first$share_gdp,
   100*((early$spending_bn/first$spending_bn)^(1/5)-1))
 o<-official[official$programme==programme,]
 common<-sort(intersect(d$year,o$year))
 if(length(common)>=2){
   years<-range(common);v<-o$igr_pct[match(years,o$year)];s<-d$share_gdp[match(years,d$year)]
   text<-paste(text,sprintf('Over the common published period %s-%s, IGR %s changes from %.1f%% to %.1f%% of GDP (%+.1f pp); our scenario changes %+.2f pp over those same years. These changes retain the coverage differences described below.',
     years[1],years[2],o$igr_label[1],v[1],v[2],diff(v),diff(s)))
 }else text<-paste(text,'No matching published IGR aggregate is available.')
 text
}
