# e61 fiscal model: a framework for discussion

28 October 2026. This model projects Australian consolidated general-government spending, revenue and debt from a common 2030 starting point to 2066. It is a scenario model for examining economic and policy assumptions, not an e61 forecast. Years denote financial years by ending year; monetary amounts are AUD billions.

Open <http://127.0.0.1:7461> while the local server is running. From the parent projections directory, run:

    & 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'e61 fiscal model/run_app.R'

The app runs offline from cached sources. It uses shiny, bslib and ggplot2; the engine uses base R. A deployment is made only when requested.

## Starting data and calibration

The 2026-27 PBO National Fiscal Outlook supplies consolidated national fiscal totals and gross/net debt in FY2029-30. The primary-expense anchor is $1,171.9bn and nominal GDP is $3,507.4bn. All scenarios share that starting point; changes begin in FY2030-31. No automatic tax increase or expenditure cut closes the budget.

Eight functions sum 24 subfunctions. ABS GFS 2024-25 supplies national functional controls. AIHW funding allocations split health; ABS controls and a cross-source allocation split schools, higher education and VET. NDIA participant plan expenses supply the NDIS base ($46.352178bn). Government aged care allocations and the Age Pension outlay are identified separately.

The allocation bridge reconciles observed amounts to category controls, advances them using stated demand and cost assumptions, and reconciles the resulting shares to PBO primary spending. PBO does not supply the sector allocations. The bridge and factors are visible in each sector tab; scenario controls do not recalibrate it. Internal government grants and funding contributions are not added as extra consolidated spending.

## Cost assumption sets and IGR-inspired rules

The sidebar offers **Current assumptions** and **Historical trend estimates (2016-2025)**. Current assumptions remain the fixed reference and initial selection. The historical alternative replaces health and education additional-growth baselines using the separate estimation pack, conditional on the arbitrary 25% service-productivity share: hospitals 2.84%, primary/community care 1.08%, referred medical services 0.74%, medicines 0.79%, health reconciliation 2.99%, schools 2.34%, tertiary 1.26% and other education 1.24% annually. Higher education and VET share the tertiary estimate. The reconciliation estimate is a heterogeneous accounting residual. These estimates include policy, coverage, quality and mix; they are not pure technical costs or separate efficiency estimates. They are a scenario alternative requiring expert review.

`data/historical_service_cost_parameters.csv` is the explicit runtime snapshot of the 25% productivity specification in `estimation/outputs/window_and_structure_sensitivity.csv`, prepared by `scripts/prepare_cost_defaults.R`. Running the estimation does not automatically update that snapshot. Neither aggregate health/education estimates nor provisional baseline rates are added on top of the selected subfunction rates. Each aggregate cost slider adds percentage points to the selected baseline in every subfunction. The construction tables show baseline, adjustment, selected rate and evidence. The Spending tab compares net debt and endpoint primary spending under both cost sets with all other selected economic/policy assumptions held fixed. Scenario attribution identifies the cost-set contribution separately. Estimates are never refitted when productivity, NGDP or service-efficiency controls move.

NDIS participant-plan payments follow the existing participant/cohort projection, general prices, the original plan-cost index (15% CPI-only and 85% real-wage-linked) and 0.5% additional cost/use growth plus the selected policy adjustment. They receive no service-efficiency deduction. The population age-incidence and retained-cohort model is unchanged; it does not introduce new future take-up reforms or actuarial estimates. Wage indexation can respond to economy-wide productivity without creating a productivity saving on transfers.

Other social protection uses the original working-age demand and cash-indexation proxy, with real wages raised to the 0.65 exposure under indexed rules and full wage linkage under relative standards. That exposure is an arbitrary proxy, not a sourced programme allocation. Cash transfers receive no service-efficiency deduction. Age Pension retains its explicit wealth accumulation, means tests and payment indexation. NDIS and transfer cost/use adjustments appear under Policy and financing, separately from health and education efficiency controls.

Other public services can retain input-cost projections or hold its **2030 GDP share constant** from 2031. An explicit target in Fixed spending shares takes precedence. The policy factor records the override, and underlying projected costs remain exported. Applying the Commonwealth IGR residual rule to the consolidated residual is an e61 scope assumption. Aged care retains its provisional 0.3% additional-growth baseline; the reviewed IGR appendix does not publish the residential-care trend coefficient. Age Pension retains its explicit means-testing/wealth model. Sources: [IGR Appendix A3, printed pp 308-312](https://treasury.gov.au/sites/default/files/2026-09/p2026-797917.pdf#page=332) and the [independent estimation report](estimation/outputs/estimation_report.html).

## Macro controls

The reference has productivity growth of 1.2%, NGDP per person growth of 3.2%, population growth of 1.2%, constant average hours and unchanged terms of trade. Nominal GDP equals population times nominal GDP per person. At fixed values of those two inputs, productivity and hours change real output and implied domestic inflation, while leaving nominal GDP unchanged.

Real hourly wages follow economy-wide productivity. Demographic employment and participation affect total hours; average hours have their own assumption. Terms of trade change the wedge between domestic output prices and nominal income and affect structural company-tax and royalty shares. The implied domestic price index is used consistently in spending; it is an accounting consequence of the macro structure, not an independent sector price forecast.

## Spending: demographic demand, unit cost and additional service growth

For each service block:

**Spending = 2030 allocation x demographic demand x coverage x general prices x real unit cost x additional service growth.**

Demand separates total population from age/cohort composition. Coverage represents participation or eligibility beyond the demand measure. Health age-cost weights embody age differences in utilisation and cost, so demand is not a pure patient count. NDIS participation is already in demand; there is no second participation multiplier.

The resource-cost equation is:

**Real unit cost = (1 - L) + L x W / Q**

L is the base-period labour share, W is the real wage index, and Q is the service labour-productivity index. Non-labour inputs follow general inflation. Service productivity grows at a proportion, theta, of economy-wide productivity: Q = (1 + theta x productivity growth)^years. Efficiency reduces labour required per service; it does not reduce non-labour inputs in this basket. Fixed input weights do not model future input substitution.

Real wages rise with economy-wide productivity. If service productivity lags, labour cost per service rises relative to general prices. This is the **Baumol mechanism**, generated inside the cost equation. Rising relative service prices need not imply a rising spending/GDP ratio: demand and the GDP denominator also matter.

Additional service growth multiplies spending by (1 + r)^years. It represents intensity, quality, treatment mix and other pressures beyond demographics and explicit unit costs. It is not a second Baumol allowance or a separately identified causal intensity estimate. Persistent rates compound; a one-off policy level change needs its own path.

### Two uncertain cost assumptions

Each resource-cost block has a service-productivity response and an additional-growth parameter. The service-productivity default is **25% of economy-wide productivity growth** for resource-cost blocks in health and education. This value is **arbitrary**: a modelling choice for discussion, not an empirical estimate, a neutral value or an official recommendation. At 1.2% economy-wide growth it implies 0.3% service efficiency growth annually. CPI, funding-index and cash-payment rules do not receive efficiency deductions.

| Function | Additional service growth baseline | Evidence status |
| --- | ---: | --- |
| Health | 0.5% a year | Arbitrary modelling choice; not estimated |
| Education | 0% | Fixed quality/coverage baseline |
| NDIS | 0.5% additional plan cost/use growth; no efficiency deduction | Arbitrary modelling choice; not estimated |
| Aged care | 0.3% a year | Arbitrary modelling choice; not estimated |
| Other public services | 0% | Fixed additional-service baseline |
| Other social protection | 0% | Residual cash/indexation proxy |

Health and education each have an efficiency-growth control, default 25% of economy-wide growth (0.3% annually at 1.2%), and an additional-cost-growth control, default 0.5% annually above CPI and standard wage pass-through. Both are arbitrary discussion assumptions, not empirical estimates. The additional-cost control shows the actual rate, not a zero adjustment. Under the historical alternative it shows the rounded anchor-spending-weighted mean of subfunction residuals; changes add a common percentage-point shift to all subfunctions, preserving differences. The displayed mean is rounded to 0.01 percentage points; selecting an assumption set retains its original residuals at that rounded centre. Medicine CPI and VET funding-index rules have no efficiency deduction. NDIS, cash transfers, aged care and other public services have no new efficiency default; their cost/use or indexation rules remain explicit.

Funding-index and CPI rules have no resource-efficiency deduction. The mixed social-protection residual uses earnings indexation rather than claiming to measure service production; its service-productivity control is inactive. Pension payments and defence have their own policy rules.

### Subfunction evidence

Hospital labour weight is approximately 67.25%, from IHACPA admitted-acute costs, applied as a scope proxy. School weight is 75.99%, from ACARA salaries/recurrent expenses. University weight is 55.47%, from provider employee benefits/expenses. These shares are not causal productivity elasticities; their source populations differ from consolidated spending.

Primary, referred and residual health use a 70% labour-weight judgment; residual education 80%; NDIS 85%; aged care 80%; other public services 70%. Other social protection uses a 65% earnings-indexation proxy. Medicines use CPI unit costs, with additional growth separately covering utilisation, innovation and mix. VET uses the National Skills Agreement flexible-funding index as a scope proxy: wage weighting 85% x 75% + 15% x 40% = 69.75%, compounded annually.

The 22 service/indexation blocks, rules, parameter classifications, URLs and locators are recorded in data/service_cost_parameters.csv. Allocations and demand profiles are in data/spending_subfunctions.csv. Both are inspectable through the app and its downloads.

## IGR discipline and the broader literature

The comparator table reports labour exposure, service productivity, additional growth, reference/scenario combined non-demographic growth and available rule comparators. Combined growth is the 2030-2066 compound annual growth in real unit cost times additional service growth; it excludes demographics and inflation.

The 2026 IGR estimates programme-specific non-demographic health growth from historical real age/sex-adjusted spending, using a chained index and trend regression. The method is published; coefficients are not tabulated in the reviewed appendix. Numerical health trend comparators are therefore unavailable. The 0.5% assumption is not labelled as an IGR estimate.

NDIS average costs by age/disability transition to average weekly earnings growth after the medium term. That rule implies 1.2% above inflation at reference macro assumptions, before participant-mix effects. This is a cost-index comparator, not a reproduction of the actuarial/reform transition. Commonwealth school funding uses 75% wages/25% CPI (0.9% above inflation at reference assumptions). Commonwealth university grants use CPI (zero real growth), a partial comparator for consolidated university resource costs. The VET numerical comparator is the 69.75% funding proxy; the IGR specifies a wage-cost index without that numerical weight.

Comparators and reference implied growth use fixed reference macro assumptions. Moving a scenario does not refit a residual or change an official coefficient. A gap from a fixed comparator is not automatically an error. Commonwealth grants, state service costs and consolidated expenses have different boundaries.

The OECD separates demographics, income, productivity constraints and other pressures. The NSW IGR estimates other-growth residuals after economic/demographic effects, including health and schools in its baseline. Those residuals include quality and policy effects and cannot be added without checking overlap with explicit wage/productivity costs. These sources discipline assumptions; they do not establish every coefficient in this model.

## Sector review and exact decomposition

NDIS, Health and Education each have projections, driver contributions, allocation bridges, equations, linked evidence, benchmark comparisons and expert discussion questions. Select a subfunction, endpoint and units (GDP share, nominal dollars or 2030 dollars).

Health uses fixed IGR hospital/medical/pharmaceutical age-cost profiles. Education uses ages 5-17 for schools, 18-24 for university, 15-64 for VET and total population for other education. These are demand proxies, not enrolment models. NDIS fixes under-65 age participation and retains older cohorts using population survival. It does not separately simulate disability severity, age-specific plan costs, reform-driven entry/exit or scheme mortality. Administration remains in broader residual spending.

Nine-factor Shapley accounting separates population, age/cohort demand, coverage, general prices, real wages/funding indexation, service productivity, additional service growth, policy/financing and the GDP denominator. Interactions are shared symmetrically and reconcile exactly before rounding. These are accounting contributions, not causal estimates. Real-dollar results remove general prices; GDP shares include the denominator. Wage and efficiency factors multiply to the real unit-cost basket. Factors, sources and annual calculations are downloadable.

Read the [spending background PDF](documentation/spending_background.pdf) for a standalone guide with allocations, assumptions and sources.

## Fixed spending shares and debt-sensitive borrowing rates

The **Fixed spending shares of GDP** section follows Policy and financing assumptions. Each of the eight functions can optionally be held at a user-selected percentage of nominal GDP. These overrides are off by default. They take effect immediately in 2031 and apply through 2066; the common 2030 allocation is preserved. A target replaces the function total rather than adding expenditure. Subfunctions receive the total in proportion to their unconstrained projected spending in each year, so their mix can evolve. A zero target is supported. If unconstrained spending is zero, anchor shares provide the allocation fallback.

Annual downloads show both unconstrained spending and actual spending, the active target and its policy factor. Sector summaries identify an active override. Cost assumptions still describe underlying resource pressures; the policy factor offsets those pressures to achieve the selected total. Exact decomposition remains valid, including zero targets. A constant GDP share does not imply constant nominal dollars. Overrides flow through the fiscal balance, gross debt, interest and net debt without recalibrating revenue or the fixed reference.

Under **Cost pass-through and financing sensitivity**, the interest/debt control is a semi-elasticity measured in **basis points per percentage point of net debt/GDP**, not a percentage elasticity. Its default is **zero**, which disables feedback. The available range is 0-5. The research benchmark is **3 bps per +1 percentage point of net debt/GDP**, the coefficient used in the debt-scenario work and linked there to Yan and Brittle, Treasury Working Paper 2010-04. The paper estimates around 3 bps for the long-run Australian-US real ten-year interest margin, with a plausible range of 0-5. This is historical Treasury research, not a current official Treasury borrowing-rate forecast.

The effective-rate scenario rule is:

    rate[t] = max(0, base_rate[t] + sensitivity_bps / 10000
                  x (net_debt_GDP_pct[t-1] - net_debt_GDP_pct[2030]))

At 3 bps, net debt 10 percentage points of GDP above its 2030 share adds 0.30 percentage points to the base rate. Below-anchor debt reduces it, subject to a zero floor. The first projection year has no debt feedback because its opening ratio is the anchor. The base rate retains its transition to the selected long-run borrowing-rate assumption. The signed adjustment is recalculated each year; it is not added cumulatively to the preceding year's rate.

Interest is charged on opening **gross debt**, while the rate sensitivity responds to lagged **net debt/GDP**. Asset yields are not repriced. Applying a long-run bond-yield-margin coefficient immediately to the effective rate on the whole debt stock is an explicit scenario approximation; debt maturity and refinancing lags are not separately simulated. The Debt & interest tab and download distinguish base rate, debt adjustment, lagged net-debt gap and actual borrowing rate.

Source: [Yan and Brittle (2010), Reconsidering the Link Between Fiscal Policy and Interest Rates in Australia, printed pages 13 and 17](https://treasury.gov.au/sites/default/files/2019-03/Tsy_Working_Paper_2010_04.pdf#page=16). The archived scenario coefficient is in Fiscal sustainability/Debt scenario/0 Initial scenario.R and 2 Updated scenario.R. Its scenario implementation used a debt ratio; this model explicitly uses the requested net-debt ratio.

The numerical productivity and service-cost defaults remain discussion assumptions. Calculator labels identify **service productivity cost savings** and **additional service cost growth** to make their cost interpretation explicit.

## Revenue, pensions and financing

Personal income tax applies statutory schedules/offsets to observed ATO income cells. Thresholds are wage-indexed by default, with CPI and nominal alternatives. Other receipts use structural tax-base proxies. Company tax/GDP responds to terms of trade and is invariant to NGDP growth. Royalties and interest receipts are separate. Effective yields, coverage and transaction propensities are explicit assumptions.

Age Pension spending combines eligibility, means tests and synthetic wealth/household archetypes. The 4% reference real superannuation return changes private balances and pension eligibility, not GDP or public asset returns. Constant-relative-standards policy has a stated pension convention.

Gross debt finances deficits and asset acquisitions. Interest is paid on opening gross debt; financial assets earn a calibrated yield. Net debt subtracts modelled financial assets. This is a simplified balance sheet. International comparisons distinguish gross debt, model net debt and OECD net financial liabilities. IMF long-term chart readings are approximate; both OECD Survey gross-debt scenarios end in 2050. No source series is extended. Short-run international forecasts are excluded.

## Sources

* [PBO National Fiscal Outlook](https://www.pbo.gov.au/publications-and-data/publications/2026-27-National-Fiscal-Outlook): consolidated fiscal anchor.
* [ABS GFS](https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/2024-25): national spending controls.
* [Population Statement 2025](https://population.gov.au/publications/statements/2025-population-statement): age/sex structure, rescaled to the selected total path.
* [Commonwealth IGR](https://treasury.gov.au/sites/default/files/2026-09/p2026-797917.pdf): health profiles and Appendix A3, printed pages 307-312, cost/funding methods.
* [NSW IGR technical note](https://www.nsw.gov.au/sites/default/files/noindex/2026-08/2026-27-nsw-igr-technical-note.pdf): printed pages 12-15, economic/demographic drivers and residuals.
* [OECD health fiscal-sustainability methods](https://www.oecd.org/en/publications/fiscal-sustainability-of-health-systems_880f3195-en/full-report/component-6.html): income, productivity constraints and other pressures.
* [IHACPA hospital costs](https://www.ihacpa.gov.au/resources/national-hospital-cost-data-collection-public-sector-2023-24), [ACARA school finances](https://dataandreporting.blob.core.windows.net/anrdataportal/ANR-Documents/ANR2024/nationalreportonschoolinginaustralia_2024_Chapter9.pdf), [university finances](https://www.education.gov.au/download/19832/finance-2024-financial-reports-higher-education-providers/43005/2024-university-finance-summary-information/pdf) and [National Skills Agreement](https://federalfinancialrelations.gov.au/sites/federalfinancialrelations.gov.au/files/2023-12/national-skills-agreement.pdf): cost-share and funding proxies.
* [NDIA Annual Report](https://www.ndis.gov.au/media/8108/download) and [NDIA participation report](https://www.ndis.gov.au/media/7891/download): plan expense and age participation.

Citations support the identified input or method, not all assumptions. The register distinguishes sourced values, calibrated allocations, proxies, judgments and identities.

## Discussion questions

Which service-productivity response is defensible for quality-adjusted output? How much additional growth remains after explicit costs? Which grant rules warrant separate blocks? How should participant mix, enrolment, morbidity and treatment changes affect demand? Do data justify a point estimate, or should a parameter remain a scenario range?

## Reproduction and validation

The independent historical estimation workflow is in `estimation/`. Run `Rscript estimation/run_estimation.R --acquire` once to cache its sources, then `Rscript estimation/run_estimation.R` for offline rebuilds. Its [report](estimation/outputs/estimation_report.html) and [PDF](estimation/outputs/estimation_report.pdf) explain the health and education estimates, period and wage-measure sensitivity, and provisional IGR rules for other functions. `estimation/outputs/candidate_service_cost_parameters.csv` contains review candidates; this workflow does not install them as model defaults. See `estimation/outputs/source_register.csv` and `estimation/data/source_manifest.csv` for provenance and source hashes.

R/model.R loads the engine. R/service_costs.R implements cost baskets, funding rules and fixed-reference comparisons. R/spending_bottom_up.R contains allocation and Shapley accounting. R/sector_review.R presents sector evidence. scripts/build_spending_background.R builds the spending PDF; scripts/render_readme_pdf.R prints this README. scripts/deploy_fiscal_tester.R checks the runtime bundle without uploading unless --deploy is specified.

Tests cover labour-only efficiency, zero/full productivity responses, GDP invariance, fixed anchors/comparators, exact factor and Shapley reconciliation, scenario attribution, sector controls, source panels and downloads. Source inputs and selected assumptions remain inspectable to support sector review.
