# e61 fiscal model: a framework for discussion

30 October 2026. This model projects Australian consolidated general-government spending, revenue and debt from a common 2030 starting point to 2066. It is a scenario model for examining economic and policy assumptions, not an e61 forecast. Years denote financial years by ending year; monetary amounts are AUD billions.

Open <http://127.0.0.1:7461> while the local server is running. From the parent projections directory, run:

    & 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'e61 fiscal model/run_app.R'

The app runs offline from cached sources. It uses shiny, bslib and ggplot2; the engine uses base R. A deployment is made only when requested.

## Starting data and calibration

The 2026-27 PBO National Fiscal Outlook supplies consolidated national fiscal totals and gross/net debt in FY2029-30. The primary-expense anchor is $1,171.9bn and nominal GDP is $3,507.4bn. All scenarios share that starting point; changes begin in FY2030-31. No automatic tax increase or expenditure cut closes the budget.

Eight functions sum 24 subfunctions. ABS GFS 2024-25 supplies national functional controls. Health categories follow AIHW service areas, combining Commonwealth and state/territory government funding and reconciling to the ABS COFOG/GFS health total. They differ from the IGR Commonwealth programme categories. IGR age-cost profiles are used as demographic proxies; using those profiles does not make the spending categories equivalent. AIHW funding allocations split health; ABS controls and a cross-source allocation split schools, higher education and VET. NDIA participant plan expenses supply the NDIS base ($46.352178bn). Government aged care allocations and the Age Pension outlay are identified separately.

The allocation bridge reconciles observed amounts to category controls, advances them using stated demand and cost assumptions, and reconciles the resulting shares to PBO primary spending. PBO does not supply the sector allocations. The bridge and factors are visible in each sector tab; scenario controls do not recalibrate it. Internal government grants and funding contributions are not added as extra consolidated spending.

## Cost assumption sets and IGR-inspired rules

For education and the legacy health benchmark, the sidebar offers **Current assumptions** and **Historical trend estimates (2016-2025)**. The explicit health specification uses its separate income-adjusted residual choices described below. Current assumptions remain the fixed reference and initial selection. This legacy cost-only alternative replaces education and legacy health additional-growth baselines using the separate estimation pack, conditional on the arbitrary 25% service-productivity share: hospitals 2.84%, primary/community care 1.08%, referred medical services 0.74%, medicines 0.79%, health reconciliation 2.99%, schools 2.34%, tertiary 1.26% and other education 1.24% annually. Higher education and VET share the tertiary estimate. The reconciliation estimate is a heterogeneous accounting residual. These estimates include policy, coverage, quality and mix; they are not pure technical costs or separate efficiency estimates. They are a scenario alternative requiring expert review.

`data/historical_service_cost_parameters.csv` is the explicit runtime snapshot of the 25% productivity specification in `estimation/outputs/window_and_structure_sensitivity.csv`, prepared by `scripts/prepare_cost_defaults.R`. Running the estimation does not automatically update that snapshot. Neither aggregate health/education estimates nor provisional baseline rates are added on top of the selected subfunction rates. Each aggregate cost slider adds percentage points to the selected baseline in every subfunction. The construction tables show baseline, adjustment, selected rate and evidence. The Spending tab compares net debt and endpoint primary spending under both cost sets with all other selected economic/policy assumptions held fixed. Scenario attribution identifies the cost-set contribution separately. Estimates are never refitted when productivity, NGDP or service-efficiency controls move.

NDIS participant-plan payments follow the existing participant/cohort projection, general prices, the original plan-cost index (15% CPI-only and 85% real-wage-linked) and 0.5% additional cost/use growth plus the selected policy adjustment. They receive no service-efficiency deduction. The population age-incidence and retained-cohort model is unchanged; it does not introduce new future take-up reforms or actuarial estimates. Wage indexation can respond to economy-wide productivity without creating a productivity saving on transfers.

Other social protection uses the original working-age demand and cash-indexation proxy, with real wages raised to the 0.65 exposure under indexed rules and full wage linkage under relative standards. That exposure is an arbitrary proxy, not a sourced programme allocation. Cash transfers receive no service-efficiency deduction. Age Pension retains its explicit wealth accumulation, means tests and payment indexation. NDIS and transfer cost/use adjustments appear under Policy and financing, separately from health and education efficiency controls.

Other public services can retain input-cost projections or hold its **2030 GDP share constant** from 2031. An explicit target in Fixed spending shares takes precedence. The policy factor records the override, and underlying projected costs remain exported. Applying the Commonwealth IGR residual rule to the consolidated residual is an e61 scope assumption. Aged care retains its provisional 0.3% additional-growth baseline; the reviewed IGR appendix does not publish the residential-care trend coefficient. Age Pension retains its explicit means-testing/wealth model. Sources: [IGR Appendix A3, printed pp 308-312](https://treasury.gov.au/sites/default/files/2026-09/p2026-797917.pdf#page=332) and the [independent estimation report](estimation/outputs/estimation_report.html).

## Macro controls

The reference fixes **domestic price growth at 2.5% a year**, with productivity growth of 1.2%, population growth of 1.2%, constant average hours and unchanged terms of trade. Nominal GDP equals real output x domestic prices x the terms-of-trade income factor. NGDP per person is derived by dividing by population; its first-year, final-year and compound annual growth rates are displayed. This rate can vary with employment per resident and the ten-year terms-of-trade transition. Alternatively select **NGDP per capita growth** (slider initially 3.2%): nominal GDP then follows population times the selected nominal per-person path, while domestic inflation adjusts to productivity, hours and terms of trade. Domestic prices are the baseline CPI proxy; the GDP deflator also includes the terms-of-trade effect. Inactive slider values have no effect. Reset restores price-growth mode at 2.5%.

Real hourly wages follow economy-wide productivity. Demographic employment and participation affect total hours; average hours have their own assumption. Terms of trade change the wedge between domestic output prices and nominal income and affect structural company-tax and royalty shares. The implied domestic price index is the baseline general-price proxy; health has an optional CPI wedge. It it is selected directly in price-growth mode or implied by the accounting identity in NGDP mode, rather than an independent sector price forecast.

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
| Health | 0% remaining pressures after income and explicit costs | Judgement; income elasticity 0.8 is a separate channel |
| Education | 0.5% a year | Arbitrary modelling choice; not estimated |
| NDIS | 0.5% additional plan cost/use growth; no efficiency deduction | Arbitrary modelling choice; not estimated |
| Aged care | 0.3% a year | Arbitrary modelling choice; not estimated |
| Other public services | 0% | Fixed additional-service baseline |
| Other social protection | 0% | Residual cash/indexation proxy |

Health has a dedicated **Health demand and costs** panel. Income elasticity defaults to 0.8 (range 0-1.5), service efficiency to 25% of economy-wide productivity (range 0-150%), and remaining real spending pressures to 0% annually (range -1% to +2%). Advanced controls allow a health wage premium and a health CPI-minus-domestic-inflation wedge, each initially zero. These prospective scenario changes do not refit historical residuals or recalibrate the common anchor.

Education retains 25% service efficiency and 0.5% additional annual cost growth. Both are arbitrary discussion assumptions. Its historical option retains fitted 2016-2025 residuals. Its additional-cost control shifts all education residuals by a common percentage-point amount and preserves differences. Medicine CPI and VET funding rules have no efficiency deduction. NDIS and transfers have no service-efficiency deduction.

### Explicit health equation and evidence

For each health subfunction j:

    H[j,t] = H[j,2030] x demographic demand x coverage x P_health
             x (real GDP per person index)^epsilon
             x ((1-L[j]) + L[j] x nominal hourly wage index / P_health
                x health wage premium index / service efficiency index)
             x (1 + remaining growth[j])^years x policy factor

Medicines use the CPI cost proxy (unit basket one) rather than labour-efficiency savings. Income-related demand and remaining pressures still apply. Income is real GDP **per person**, so population is not counted twice. This is an OBR/OECD-inspired framework, not an exact reproduction of either organisation. The OECD conditional income coefficient is approximately 0.77; the 0.8 reference is a rounded international scenario assumption, not an Australian causal estimate or a statutory commitment. The labour shares and arbitrary efficiency response are not OECD regression coefficients.

P_health defaults to the model domestic price index. It is a CPI **proxy**, not observed future CPI. With a nonzero terms-of-trade path the GDP deflator differs from this index. The advanced price wedge adds percentage points to annual domestic inflation, compounded into P_health; it changes health costs, not nominal GDP or nominal wages. Wage premiums act only on labour costs. No second Baumol allowance is added: lagging service efficiency relative to economy-wide wages generates the relative-cost pressure within the basket. Real-dollar outputs use the common domestic-price deflator, so an additional health price wedge remains a relative-cost effect in those outputs.

Remaining pressures include technology, morbidity, intensity, coverage, policy and measurement effects beyond explicit channels. Zero is a transparent reference choice, not evidence that those pressures disappear. Historical alternatives remove income-related demand with elasticity 0.8 and recompute costs with the 25% efficiency response before fitting a log-linear trend. The matched health-subfunction series covers 2016-2025. The long aggregate health-consumption option covers 1999-2025 with WPI and applies one aggregate residual to all health subfunctions; that allocation is a proxy. The remaining-pressure slider is centred on the rounded anchor-weighted residual (0.1 percentage points) and shifts rates together, preserving unrounded source rates at the displayed centre. Residuals remain fixed when productivity or other scenario assumptions change.

The longer ABS series contains **63 annual observations, FY1962-63 to FY2024-25**, for aggregate health and education government consumption. Complete decompositions use **27 observations, 1999-2025 with WPI**, and **39 observations, 1987-2025 with compensation per hour**. This is not a 63-year complete decomposition. GFS expenses and government final consumption have different boundaries; the overlap CSV documents levels and ratios without splicing them. Aggregate education history is diagnostic and does not change its projection rules.

Read the [health framework background PDF](documentation/health_framework_background.pdf) for equations, parameter classifications, estimates, accounting reconciliation and projection comparisons. The independent health framework report retains the legacy comparison. The fixed reference always uses the new health specification.

Funding-index and CPI rules have no resource-efficiency deduction. The mixed social-protection residual uses earnings indexation rather than claiming to measure service production; its service-productivity control is inactive. Pension payments and defence have their own policy rules.

### Subfunction evidence

Hospital labour weight is approximately 67.25%, from IHACPA admitted-acute costs, applied as a scope proxy. School weight is 75.99%, from ACARA salaries/recurrent expenses. University weight is 55.47%, from provider employee benefits/expenses. These shares are not causal productivity elasticities; their source populations differ from consolidated spending.

Primary, referred and residual health use a 70% labour-weight judgment; residual education 80%; NDIS 85%; aged care 80%; other public services 70%. Other social protection uses a 65% earnings-indexation proxy. Medicines use CPI unit costs, with additional growth separately covering utilisation, innovation and mix. VET uses the National Skills Agreement flexible-funding index as a scope proxy: wage weighting 85% x 75% + 15% x 40% = 69.75%, compounded annually.

The 22 service/indexation blocks, rules, parameter classifications, URLs and locators are recorded in data/service_cost_parameters.csv. Allocations and demand profiles are in data/spending_subfunctions.csv. Both are inspectable through the app and its downloads.

## IGR discipline and the broader literature

The comparator table reports labour exposure, service productivity, additional growth, reference/scenario combined non-demographic growth and available rule comparators. Combined growth is the 2030-2066 compound annual growth in income-related demand times real unit cost times remaining/additional service growth; it excludes demographics and inflation.

The 2026 IGR estimates programme-specific non-demographic health growth from historical real age/sex-adjusted spending, using a chained index and trend regression. The method is published; coefficients are not tabulated in the reviewed appendix. Numerical health trend comparators are therefore unavailable. The health income coefficient, efficiency response and remaining-pressure reference are not labelled as IGR estimates.

NDIS average costs by age/disability transition to average weekly earnings growth after the medium term. That rule implies 1.2% above inflation at reference macro assumptions, before participant-mix effects. This is a cost-index comparator, not a reproduction of the actuarial/reform transition. Commonwealth school funding uses 75% wages/25% CPI (0.9% above inflation at reference assumptions). Commonwealth university grants use CPI (zero real growth), a partial comparator for consolidated university resource costs. The VET numerical comparator is the 69.75% funding proxy; the IGR specifies a wage-cost index without that numerical weight.

Comparators and reference implied growth use fixed reference macro assumptions. Moving a scenario does not refit a residual or change an official coefficient. A gap from a fixed comparator is not automatically an error. Commonwealth grants, state service costs and consolidated expenses have different boundaries.

The OECD separates demographics, income, productivity constraints and other pressures. The NSW IGR estimates other-growth residuals after economic/demographic effects, including health and schools in its baseline. Those residuals include quality and policy effects and cannot be added without checking overlap with explicit wage/productivity costs. These sources discipline assumptions; they do not establish every coefficient in this model.

## Sector review and exact decomposition

NDIS, Health and Education each have projections, driver contributions, allocation bridges, equations, linked evidence, benchmark comparisons and expert discussion questions. Select a subfunction, endpoint and units (GDP share, nominal dollars or 2030 dollars).

Health uses fixed IGR hospital/medical/pharmaceutical age-cost profiles. Education uses ages 5-17 for schools, 18-24 for university, 15-64 for VET and total population for other education. These are demand proxies, not enrolment models. NDIS fixes under-65 age participation and retains older cohorts using population survival. It does not separately simulate disability severity, age-specific plan costs, reform-driven entry/exit or scheme mortality. Administration remains in broader residual spending.

Ten-factor Shapley accounting separates income-related demand, population, age/cohort demand, coverage, general prices, real wages/funding indexation, service productivity, additional service growth, policy/financing and the GDP denominator. Interactions are shared symmetrically and reconcile exactly before rounding. These are accounting contributions, not causal estimates. Real-dollar results remove general prices; GDP shares include the denominator. Wage and efficiency factors multiply to the real unit-cost basket. Factors, sources and annual calculations are downloadable.

Health also has a second **What explains the change in real spending per person?** panel. It follows the selected subfunction and end year independently of the main chart units. Its first view allocates the change in 2030-dollar spending per resident. Population size and the common domestic-price index are removed before Shapley attribution; ageing, income-related demand, real input costs and any health-specific price wedge remain. The alternative divides real health spending per person by real GDP per person and reports contributions in percentage points of that ratio. Real GDP uses real output, excluding the terms-of-trade income effect, and its per-person denominator has a separate contribution. Each view reconciles exactly before rounding and has driver and annual-data downloads.

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

Age Pension spending follows public-data calibrated birth cohorts, existing household wealth, retirement transitions and means tests. The joint wealth distribution and transition/consumption assumptions need future HILDA estimation; no HILDA parameters have been estimated. The 4% reference real superannuation return changes private balances and pension eligibility, not GDP or public asset returns. Constant-relative-standards policy has a stated pension convention.

Gross debt finances deficits and asset acquisitions. Interest is paid on opening gross debt; financial assets earn a calibrated yield. Net debt subtracts modelled financial assets. This is a simplified balance sheet. International comparisons distinguish gross debt, model net debt and OECD net financial liabilities. IMF long-term chart readings are approximate; both OECD Survey gross-debt scenarios end in 2050. No source series is extended. Short-run international forecasts are excluded.

## Sources

* [PBO National Fiscal Outlook](https://www.pbo.gov.au/publications-and-data/publications/2026-27-National-Fiscal-Outlook): consolidated fiscal anchor.
* [ABS GFS](https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/2024-25): national spending controls.
* [Centre for Population: 2026 IGR annual national population projections](https://population.gov.au/data-and-forecasts/projections): annual population by single-year age and sex, 2026-2066. Population Statement 2025 supplies only the 2025 pre-projection bridge.
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


## Extended historical health workflow

Run `Rscript estimation/04_extend_health_history.R --acquire` once to cache the archived Table 35 source, or omit `--acquire` for an offline rebuild. Run `Rscript estimation/05_health_framework_report.R` to rebuild the current background PDF. Extended observations, annual factors, window sensitivities, accounting overlap and the source manifest are in `estimation/outputs/extended/`. The runtime optional health residuals are in `data/health_income_residuals.csv`. The earlier estimation report describes the legacy cost-only health equation and unchanged education residuals.

The archived 2024-25 ABS Table 35 snapshot is used because that table was discontinued in October 2026. See the current health framework report for source links and the accounting reconciliation.


## Age Pension: public-data cohort model

The Age Pension tab shows receipt by age in selected projection years, entry at age 67, the trajectory of a selected birth cohort, recipients, spending and mean payments. Its methods distinguish public inputs, calibrated quantities and provisional assumptions. Receipt means a positive modelled payment; full legal residency and take-up eligibility are not independently simulated.

The starting distribution covers existing retirees and workers. DSS June 2026 identified-state counts by pension age band provide receipt targets, using the cached demographic population as denominator. Geographic Unknown records, including overseas addresses, are excluded. National payment-rate data remove zero, suspended and unclassified cases and provide the full/part marginal. Applying those adjustments uniformly by age and sex is an assumption because their joint distribution is not supplied in these tables. These targets should not be labelled observed 2030 rates.

ATO Taxation Statistics 2023-24, Snapshot Table 5 sheet 12, supplies positive-balance super medians by age and sex. ABS SIH 2019-20 Tables 12.1/12.3 supply super coverage and dispersion proxies. Coverage includes some recent lump-sum/income-stream recipients and is not exactly account ownership; using it as a zero-balance proxy is explicit. Tables 10.2/10.3 supply household non-super assets, property liabilities, family composition and homeownership by reference-person age. Household age and individual age are imperfectly aligned. Positive super balances use a lognormal approximation; other asset dispersion is proxied from net-worth mean/median ratios. All asset ranks are perfectly associated in the synthetic cells. Same-age/rank spouses and conversion from household to person margins are allocation assumptions. Homeownership and partner type remain fixed.

There are 38,880 cells: source ages 20-100, sex, partner/homeownership status, 20 wealth ranks and retirement ages 60/65/75. Retirement weights use broad cross-sectional ABS employment rates; treating those rates as retirement transitions is provisional. Below 60, employment exposure scales earnings and contributions. Conditional hours factors at older ages and a common earnings distribution derived from the existing ATO income cells are judgement proxies. This is not an estimated retirement-choice model.

Retiree cells are reweighted once to match age-specific receipt and the national full/part marginal. This changes some source asset moments: the ABS/ATO values are priors, not jointly matched constraints. The calibration audit reports the resulting wealth means. Source asset values are uprated to 2026-price dollars at a provisional 2.5% annually, rather than reconstructing asset-specific historical returns. The fixed 2026-2030 bridge uses 4% real super returns and 1.2% real earnings growth. The shared 2030 stocks are then fixed: slider changes cannot retrospectively alter existing wealth.

Each cohort retains its assets as it ages. Existing worker super earns the selected return and receives contributions; economic retirement is separate from pension age. After retirement, age-based minimum super withdrawals go into liquid assets. Assumed consumption removes 5% of opening super plus liquid wealth annually; withdrawals themselves are not consumption. Liquid assets have a separate 1% real return. Net investment-property/business values are held constant in real terms with a 3% income yield. Other income is assumed consumed rather than separately saved. The principal home is excluded from assessable wealth. Minimum withdrawals are applied to retired account holders as a proxy; transfer-balance caps, defined benefits, grandfathering, tax detail and the banked Work Bonus are not simulated. The regular $300/fortnight Work Bonus is included.

Means-tested receipt and average payments emerge from those stocks and income. The 2030 fiscal allocation remains unchanged; one fixed multiplier reconciles raw payments to that allocation, without changing recipient counts. Population counts use the shared age/sex path; migration and mortality are not wealth-selective. The sourced 100+ population is distributed across surviving cohorts using an assumed 65% annual relative survival weight; it does not add a second population mortality adjustment. Constant-relative-standards policy holds 2030 receipt and payment entitlements by age fixed relative to wages; changing age composition can still change aggregate receipt.

**Future HILDA work is required** to estimate the joint distribution of wealth and income, retirement and employment transitions, consumption versus reinvestment, partnering/widowhood, wealth-related survival and within-cohort changes in pension receipt and amounts. Public cross-sections cannot identify how much of an age gradient is cohort history versus asset depletion. The current parameters are discussion assumptions, not estimated causal effects or validated central forecasts.

Reproduce with `Rscript scripts/acquire_pension_sources.R`, then `Rscript scripts/prepare_pension_cohorts.R`. The first command caches public workbooks without refreshing existing files; the second rebuilds the distribution offline. Runtime data are the pension CSVs in `data/`; calibration audits are in `outputs/pension_cohort_preparation/`. Sources and hashes are in `data/pension_source_register.csv` and `data/raw/pension/source_downloads.csv`. `Rscript tests/validate_pension_cohorts.R` checks calibration, stock conservation, fiscal reconciliation and return scenarios.

Sources: [DSS demographic data](https://data.gov.au/data/dataset/dss-payment-demographic-data), [ATO Taxation Statistics 2023-24](https://data.gov.au/data/dataset/taxation-statistics-2023-24), [ABS SIH 2019-20](https://www.abs.gov.au/statistics/economy/finance/household-income-and-wealth-australia/2019-20), [Moneysmart account-based pension minimum withdrawals](https://moneysmart.gov.au/retirement-income-sources/account-based-pensions), and [Services Australia Work Bonus](https://www.servicesaustralia.gov.au/work-bonus-if-you-work).

The super return slider is interpreted as a net real return after investment fees and earnings tax; these deductions are not separately simulated. Liquid returns are also net real assumptions.

The main receipt chart groups ages into the DSS reporting bands; downloads retain single-age detail. Existing retirees have direct receipt calibration while workers retain public wealth priors, so future entrant projections are less tightly identified. Source-band and calibration boundaries may affect cohort profiles. June 2026 targets are applied under the available September 2026 policy settings. These limits require review with longitudinal data.

## Annual demographic foundation

All projection years use the official 2026 IGR annual national population cells by sex and single year of age (0-99, plus 100+), published by the Centre for Population on 27 October 2026. Financial-year labels are mapped to their ending year. The 2025 starting year for NDIS uses Population Statement 2025. Published male/female counts are independently rounded; the model sums those cells consistently rather than mixing them with separately rounded persons totals. The reconciliation is saved in `data/processed/population_source_reconciliation.csv`.

The same annual age/sex path feeds employment, hours composition before the anchor, health, education, aged care, NDIS, Age Pension and other age-based spending demand. Revenue linked to employment therefore also reflects this demographic structure. The population-growth control scales all age/sex groups equally in each year from the common 2030 population anchor: it retains each year's official projected age distribution. It is demographically neutral and does not model age-selective migration or changes in fertility/mortality. Total-population spending proxies remain total-population proxies; health/aged-care age-cost weights, education age windows and employment-rate bands are not made more detailed merely by richer population inputs. NDIS population cohort ratios include net migration as well as mortality and are not pure survival estimates. Pension wealth/receipt inputs retain their stated public-data and synthetic-distribution limitations.

Reproduce offline extraction with `Rscript scripts/prepare_annual_population.R` after caching the source using `Rscript scripts/acquire_annual_population.R`. Rebuild the dependent public-data pension calibration with `Rscript scripts/prepare_pension_cohorts.R`, then rebuild the assumption register and spending background. No interpolation between distant IGR age-distribution snapshots is used in projection years.

## Plot scales

Positive level paths use a minimum displayed range from 50% to 150% of the starting level, calculated separately for each facet and shared across the displayed scenarios. Larger changes expand the range automatically; no observations are removed or clipped. Debt includes zero. Fiscal balances use a symmetric range of at least +/-5 percentage points of GDP; international debt changes use at least +/-10 points. Pension receipt charts use 0-100%. Contribution charts use a zero-centred minimum range of half the starting spending level; net-debt scenario attribution uses at least +/-10 percentage points. Stacked revenue starts at zero and has room for 150% of starting total revenue. Plot scales are presentation choices and do not affect calculations or downloads.

## Participation and employment channels

Economic channels plots the participation rate (labour force / population aged 15+), employment rate (employed / population aged 15+) and workers per resident (employed / total population). Participation uses the IGR male/female aggregate paths (chart C9.13): a common odds adjustment to observed ABS age/sex participation profiles matches each sex-specific target every year. Age-specific unemployment rates remain fixed at observed April 2025-March 2026 averages. Employment and aggregate unemployment therefore depend on the projected age/sex mix; they are model-derived and are not separate IGR forecasts. The total participation rate weights the IGR sex-specific targets by each year's population aged 15+. No fixed-participation counterfactual is introduced.

The accounting identity is real GDP per person = output per hour x annual hours per worker x workers per resident. Employment already scales personal income tax and wage-linked revenue bases. Demographically neutral population scaling changes worker counts without changing these rates. The displayed scenario and reference rates normally overlap. The labour-channel CSV provides population denominators, labour force, employment, hours and rates; rate columns are fractions (0-1).
