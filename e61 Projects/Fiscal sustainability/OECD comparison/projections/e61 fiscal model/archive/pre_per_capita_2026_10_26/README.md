# e61 fiscal model - updated research prototype

Updated 26 October 2026. This version implements the first four development
priorities from the original prototype and adds a dedicated Revenue tab.
The previous code, inputs, README and PDF are preserved in `archive/v1`.

Open <http://127.0.0.1:7461> while the local server is running. To restart from
the parent `projections` directory:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'e61 fiscal model\run_app.R'
```

The app uses shiny, bslib and ggplot2. The projection engine itself uses base R.
It runs offline from cached sources. Scenario changes begin in FY2030-31, after
the common FY2029-30 PBO anchor. Years label financial years by ending year.
All monetary model amounts are AUD billions unless otherwise stated.

**This remains a research scenario model, not a validated e61 forecast or a full
household microsimulation.** New evidence replaces several arbitrary inputs,
but programme cost responses and representative pension wealth remain important
assumptions. Steps 5-6, historical validation and external review, remain open.

## What changed in steps 1-4

| Step | Implemented | Remaining qualification |
| --- | --- | --- |
| 1. Source vintages and coverage | 2026-27 PBO workbook; 2026 IGR population, participation and cost profiles; ABS tax and GFS reconciliation; Commonwealth/state/national bridge | No separate long-run budgets for each jurisdiction; local coverage and transfer elimination appear together in the net bridge |
| 2. Labour and service exposures | Observed ABS age/sex participation, unemployment and hours; IGR participation and average-hours projections; sourced health and aged-care age-cost profiles | Annual age distributions are interpolated; participation uses an aggregate-target bridge, not Treasury's full cohort model; education remains a proxy |
| 3. Care and pensions | Separate NDIS and aged-care modules; NDIS cohorts retained after 65; pension age 67, income/assets tests, deeming and super accumulation | Pension wealth archetypes and care intensity remain stylised; not all 2026 care reforms or eligibility decisions are simulated |
| 4. Tax shares and elasticities | Observed ABS tax heads, ATO income percentiles, actual resident tax schedule and LITO; explicit nominal/CPI/wage threshold rules; historical elasticity estimates | Short-sample elasticities are associations; taxable-income cells are not individual records; some offsets and state tax rules remain outside scope |

The new Revenue tab explains the annual revenue path, composition, personal tax
thresholds, bracket creep, calibration and estimated elasticities. Annual revenue
detail can be downloaded separately from the full fiscal projection.

## Deploying Fiscal_tester

`scripts/deploy_fiscal_tester.R` follows the calculator deployment scripts and
uses the locally configured `e61-institute` account on `shinyapps.io`. It does
not contain credentials. From the parent `projections` directory, check the
runtime bundle without uploading:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'e61 fiscal model\scripts\deploy_fiscal_tester.R'
```

Publish or update the app:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'e61 fiscal model\scripts\deploy_fiscal_tester.R' --deploy
```

The defaults are `--account=e61-institute --server=shinyapps.io
--app-name=Fiscal_tester --app-title="e61 Fiscal Tester"`; each can be overridden.
The URL is <https://e61-institute.shinyapps.io/Fiscal_tester/>. From an R console
in `e61 fiscal model`, a check can also be run with
`source('scripts/deploy_fiscal_tester.R')`.

The explicit file list includes model code, the required CSV extracts and the
source URL manifest used by the assumptions download. Raw workbooks, reports,
archives, test code, logs and local output files are excluded. The script stages
and starts that exact file set to catch missing inputs before uploading, checks
UTF-8 source and finite default projections, and records the file/package lists
in `outputs/deployment_files.txt` and `outputs/deployment_dependencies.csv`.
Deployment records are kept in `rsconnect/shinyapps.io/e61-institute/` for future
updates. Remote package resolution and deployment require internet access.

## Starting data and calibration

`scripts/acquire_update_sources.R` caches primary sources and records URLs,
checksums and the snapshot date in `data/raw/v2/download_manifest.csv`.
`scripts/prepare_update_inputs.R` extracts the model inputs offline into
`data/processed`. Acquisition retains existing cache files; archive a source
before intentionally refreshing its vintage. No parent project files are edited.

Principal source vintages:

* [PBO 2026-27 National Fiscal Outlook](https://www.pbo.gov.au/publications-and-data/publications/2026-27-National-Fiscal-Outlook),
  August 2026: consolidated national, Commonwealth and state fiscal series to 2030.
* [Treasury 2026 IGR](https://treasury.gov.au/publication/2026-intergenerational-report),
  September 2026: public chart workbooks, including population, structural
  participation, average-hours assumptions and age-cost profiles. IGR fiscal
  benchmarks cover the Commonwealth, not all Australian governments.
* Centre for Population 2025 Population Statement age/sex workbook, published
  January 2026: sex ratios within each age, used alongside IGR population totals
  and age shares.
* ABS Government Finance Statistics 2024-25 and Taxation Revenue 2024-25,
  released in 2026: observed consolidated expenses, taxes and non-tax revenue.
  The GFS workbook already existed in the project; it is copied into this snapshot.
* ABS Labour Force Detailed March 2026 Table 1 and EM1a: a matched 12-month
  April 2025-March 2026 window for participation, unemployment and hours by age
  and sex. This is a matched detailed-data vintage, not a claim that March is the
  latest headline labour release.
* ATO Taxation Statistics 2023-24, Individuals Table 16A, downloaded from the
  [ATO dataset on data.gov.au](https://data.gov.au/data/dataset/faea4485-f407-457d-97f8-3f0822ccd654):
  actual taxable-income percentile counts and income sums.
* NDIA Annual Report 2024-25 Note 1.1G: $46.352178bn participant plan expenses,
  excluding administration; Q4 2024-25 Table D.13: participation rates by age.
* [AIHW GEN spending on aged care](https://www.gen-agedcaredata.gov.au/topics/spending-on-aged-care),
  August 2026: $39.8bn all-government expenditure in 2024-25.
* [ANAO Age Pension audit](https://www.anao.gov.au/work/performance-audit/administration-of-the-age-pension):
  $62.2bn 2024-25 outlay, used as a cash-to-GFS allocation approximation.
* Services Australia, DSS and the legislated September 2026 deeming determination:
  pension rates and means-test settings, recorded with URLs and effective dates
  in `data/policy_parameters.csv`.

### Fiscal coverage and reconciliation

PBO national totals already consolidate Commonwealth, state/territory and local
general government. The model uses these totals directly. It does not sum gross
Commonwealth and state budgets or count GST grants and NDIS funding contributions
as extra national revenue or expenses.

`consolidation_bridge.csv` reports Commonwealth, state and national revenue and
expenses. Its residual combines transfer elimination, local-government coverage
and accounting adjustments; **it is not a separately identified transfer total**.
The app displays this bridge. Independent long-run jurisdictional forecasts have
not been constructed.

Detailed tax tables have a small revision/consolidation difference from national
GFS. Their tax heads are reconciled proportionately to observed consolidated GFS
taxation revenue. `tax_consolidation_bridge.csv` preserves both reported
intergovernmental taxes and the actual net adjustment. GST is collected once in
the Commonwealth tax data, with no state GST grant added.

PBO dollar amounts control the 2030 fiscal anchor. Nominal GDP is inferred from
published gross debt dollars and its GDP ratio. Rounded reported ratios can
therefore differ slightly from dollar-derived spending and revenue shares. The
model preserves the dollar accounting identities rather than forcing all rounded
ratios to agree simultaneously.

### Spending allocation

2025 GFS primary expenses are allocated to eight modules: Health, Education,
Age Pension, Aged Care, NDIS, Other Social Protection, Other Public Services and
Defence. Conventional interest is removed once. The pension, aged-care and NDIS
amounts are **subtracted from Social Protection**, not added above the GFS total.
The broad functional mapping remains an approximation across programme and GFS
reporting concepts; the reconciliation does not establish exact classification
equivalence. NDIA administration stays in the residual public-service envelope.

Source exposures and default cost assumptions carry these allocations to 2030;
one common factor reconciles them to PBO primary expenses. These are model
allocations, not published PBO programme forecasts. Scenario changes never
recalibrate this starting point. `outputs/programme_calibration.csv` exposes the
2025 values, raw calibration factor and 2030 amounts.

## Demographics, participation and hours

IGR C8.6 publishes single-year age shares at 2025, 2036 and 2066. The model
linearly interpolates those shares annually and normalises the published rounded
shares. It chains annual IGR C8.1 population growth from the 2025 IGR population
level. Centre for Population male/female ratios allocate each age to sex. The
result is a reconciled age/sex input, **not an independently estimated population
cohort-component forecast**. Rounded annual growth can produce a small difference
from the IGR's rounded population-level chart.

ABS observed five-year age/sex participation and unemployment rates replace the
old employment propensities. Annual participation probabilities shift in common
log odds within each sex to reproduce IGR C9.13 structural participation targets.
This is a transparent projection bridge; it does not reproduce Treasury's cohort
participation model or independently identify future retirement behaviour.

Employment equals population times participation times one minus unemployment.
Observed unemployment rates are held by age and sex. EM1a supplies actual hours
per employed person, including non-working respondents. Detailed source rates
and hours are available to 65+ as a combined group, so the model does not invent
75+ employment weights. Average hours per employed person follow the IGR C7.2
aggregate trend of -0.1% per year; the age/sex hour mix is rescaled to that target,
avoiding a second composition effect. Participation and hours are common across
the four main economic scenarios in this version.

```
total hours = employed people * average weekly hours * 52
real output index = total hours index * productivity index
productivity index = (1 + selected productivity growth)^years
```

## Nominal output and economic controls

With linked NGDP, domestic prices grow at 2.5% annually:

```
NGDP = starting NGDP * real output index * domestic price index * ToT index^0.20
```

The terms-of-trade setting is a permanent level shift phased over ten years to
2040. The 0.20 nominal-income elasticity is a scenario assumption, not an estimated
trade model. It changes nominal income without changing real production.

With an NGDP override, nominal GDP instead follows the selected growth rate and
domestic prices are the residual implied by the same identity. Higher productivity
at fixed NGDP means lower inflation. This is a different experiment from higher
productivity with fixed inflation. Implied inflation is displayed, including
deflation under extreme combinations. Negative productivity is permitted.

The domestic price index remains a common CPI/wage-deflator proxy. The GDP
deflator additionally includes the ToT income effect. Hourly wages track domestic
prices and productivity; average earnings also reflect average hours per worker.

## Programme spending mechanisms

### Audit of spending shares and the three drivers (26 October 2026)

The number, unit-cost and intensity framework is applied annually. The previous
Spending chart only compared the scenario and reference in 2066, concealing
changes already present in both paths. The Spending tab now adds annual paths
and a driver table showing starting and ending GDP shares and the multiplicative
factors behind them. No spending equations or projection parameters were changed
in this audit.

At current defaults, the shares are:

| Programme | 2029-30 (% GDP) | 2065-66 (% GDP) |
|---|---:|---:|
| Health | 7.35 | 10.40 |
| Aged care | 1.58 | 3.11 |
| NDIS | 1.61 | 2.08 |
| Education | 4.76 | 4.14 |
| Age Pension | 2.24 | 1.52 |
| Other social protection | 5.43 | 5.18 |
| Other public services | 8.87 | 9.02 |
| Defence | 1.59 | 2.50 |

For service programmes the exact identity is:

```
spending/GDP relative to 2030 =
  number index * real unit-cost index * intensity index
  / (nominal GDP index / domestic price index)
```

Number is age-weighted demand for health and aged care, participants for NDIS,
an education-age proxy for education, or the relevant population for other
services. Health's age-weighted demand rises 1.56 times, compared with total
population rising 1.34 times. Its real unit-cost factor is about 1.50 and intensity
about 1.20, against the denominator rising about 1.98 times. The resulting share
increases about 41%. Ageing and costs therefore already raise spending relative
to GDP in the baseline, before changing a sidebar control.

Age Pension instead decomposes into eligible population, recipient coverage and
mean payment per recipient. Mean payment also reflects means-test composition,
not just statutory benefit indexation. At the default 4% real super return,
reduced coverage outweighs demographic pressure. Defence is a separate GDP-share
target, and is explicitly excluded from the three-driver identity.

There are two reasons the 2066 scenario/reference comparison can move little:

* Demography is the same in both paths; there is currently no scenario control
  for population ageing, migration, disability incidence or healthy ageing.
* Changing NGDP growth alone, with real output unchanged, changes implied
  inflation. All these spending modules immediately pass through domestic price
  growth. That same price factor appears in GDP and cancels from their GDP
  shares. This is an explicit model restriction, not evidence that real costs
  or ageing are irrelevant. Nominal spending still changes, and debt/interest
  can respond. Spending indexation lags and different programme price indices
  are not currently simulated.

Changing productivity, extra service prices or NDIS intensity can change service
GDP shares. Baseline intensity trends and wage pass-through differ by programme,
but are assumptions rather than estimated sector productivity relationships.
The next substantive work is to validate these parameters and permit separate
demographic and programme-cost scenarios, rather than force shares to respond
to a purely nominal change.

`spending_driver_audit()` exposes the annual factors and reconstructs each
non-target programme's nominal spending and GDP share. Validation writes
`outputs/spending_driver_audit.csv` and checks both identities, faster aged
health/care exposure growth, the service-price response and cancellation of a
purely nominal GDP change.

```
service spending = calibrated base * demographic exposure * unit cost * intensity
unit cost = domestic prices * productivity^wage_pass_through * extra service prices
```

Health uses the sum of major-component age-cost ratios in IGR C12.10. These are
Commonwealth health profiles applied as a proxy to consolidated GFS Health; they
do not capture every state-funded service. Aged care uses the separate age-cost
profile in IGR C6.6, whose underlying fiscal incidence data are 2023-24. Age-cost
profiles are held fixed; healthy ageing and changing morbidity are not estimated.

Education uses IGR C6.6's combined Education and Other profile after subtracting
its minimum per-person value as an approximation to age-neutral other spending.
This is explicitly an education **proxy**, not observed enrolment by programme.
Other Social Protection follows the population aged 15-66 after removing the
three separately allocated programmes. Remaining services follow total population.

NDIS uses the observed age-specific participation rates from NDIA Table D.13
below 65. Starting recipients aged 65+ are retained and aged annually; there are
no new entrants at 65 or older. Population cohort ratios supply demographic
retention, including migration effects rather than pure survival probabilities.
The top-coded 100+ group retains the previous 99 and 100+ cohorts. Costs use the
calibrated programme base, with wage/price exposure and an adjustable real service
intensity trend. This is not the Scheme Actuary's disability-specific entrant/exit
model, and does not implement every 2026 access reform or distinguish SIL costs.

### NDIS benchmark check (25 October 2026)

At default economic settings, nominal Scheme spending rises from $56.3bn in
2029-30 to $73.7bn in 2034-35, averaging 5.5% annual growth. Growth in 2034-35
itself is 5.5%. The 0.5% intensity setting is extra cost per participant above
the wage/price path, not total Scheme growth. Under-65 participation rates are
held fixed by age; there is no independent rollout or rising-incidence trend.
The sidebar displays implied total nominal spending growth next to the
extra-cost slider: compound annual growth over 2030-35 and 2035-66, plus the
2034-35 spending level. This updates with the full economic scenario. At
otherwise default assumptions, 0.5% extra cost per participant implies about
5.5% annual growth in 2030-35; a 2% setting implies about 7.1%.
The 2030 programme allocation is scaled with every other programme to reconcile
the PBO aggregate; it is not a published programme forecast.

There are materially different official policy vintages:

* The [NDIA 2024-25 actuarial report](https://ndis.gov.au/media/8183/download?attachment=),
  table 1 and section 4, projects total accrual expenses of $95.755bn in 2034-35
  and 7.2% growth that year. Our $73.7bn is $22.1bn (23%) lower. This older
  projection predates the 2026 reforms.
* The [PBO July 2026 outlook](https://www.pbo.gov.au/publications-and-data/publications/2026-27-medium-term-budget-outlook),
  section 5.3, describes reform targets below 2% annual growth over the forward
  estimates and approximately 5% subsequently. These are policy-dependent targets,
  not an assurance the savings will be achieved.
* [2026 IGR chart C12.17](https://treasury.gov.au/publication/2026-intergenerational-report)
  reports **total Scheme** spending of 1.5% GDP in 2029-30, 1.6% in 2034-35,
  1.8% in 2049-50 and 2.0% in 2065-66. Our corresponding shares are 1.61%,
  1.64%, 1.84% and 2.08%. Each uses its own GDP denominator and IGR values are
  rounded to one decimal. The Commonwealth net contribution in C12.5 has
  different coverage and must not be substituted for total Scheme costs.

Our growth is low against the older actuarial path, and broadly close to newer
long-run GDP shares. That numerical proximity does not validate the mechanism:
we do not explicitly simulate the reform savings, disability mix or support-cost
pressures. The baseline has therefore not been retuned to force a match to an
older projection. Before using it as a forecast, replace the common programme
allocation with a reconciled, dated Scheme anchor and validate participant
entrants/exits and cost per participant separately. Preserve a reform-delivery
scenario and a higher-pressure scenario rather than assuming either is certain.

`scripts/check_ndis_and_super.R` reproduces the IGR extraction and comparisons
offline from the cached workbook. It writes annual results and an explicitly
dated older-vintage comparison to `outputs/ndis_benchmark_check.csv` and
`outputs/ndis_2025_vintage_check.csv`. The Spending tab shows the same coverage
and vintage qualifications alongside a live scenario comparison.

Wage pass-through remains a net service-cost assumption: Health .70, Education
.80, Aged Care .80, NDIS .85, residual Social Protection .65 and other services
.70. The extra service-price slider is above that wage-linked path. Default
excess intensity growth is 0.5% for Health, 0.3% for Aged Care, 0.5% for NDIS and
zero elsewhere. These parameters are **not newly estimated causal coefficients**.
Defence converges to 2.5% GDP by 2035. Net capital investment grows with population,
domestic prices and half of productivity growth. Depreciation is not added again.

## Age Pension, means tests and retirement incomes

The fixed annual coverage-decline parameter has been removed. The replacement
module uses age 67 eligibility and calculates annual entitlements from explicit
income and assets tests, taking the lower payment after those tests. Singles,
couples, homeowners and non-homeowners have separate published free areas and
maximum rates. Deeming uses the September 2026 1.75%/3.75% rates and thresholds.
The combined couple income taper is 50 cents per dollar, or 25 cents per person;
the asset taper is $3 per $1,000 per fortnight for the household.

Four wealth/income archetypes represent full-rate, assets-tested, income-tested
and non-recipient cases. Their initial status weights come from the **starting
2026** IGR C12.19 distribution. Future IGR coverage is retained as a benchmark,
not fitted or imposed on this model. Representative assets and private incomes
are constructed around the statutory thresholds; half-rate part-pension cases
are assumed. Person weights of 35% single/65% couple members and 75% homeowner/
25% non-homeowner are modelling judgements, not extracted household estimates.

For cohorts retiring after 2030, additional superannuation accumulates from 12%
SG contributions, less a simplified 15% contributions tax, using the ATO mean
taxable income as an earnings proxy and the selected real return (default 4%,
selectable from 0% to 7% in half-percentage-point steps, including 6.5%).
Balances earn returns and use a stylised 5% drawdown after retirement. This is not
a reconstruction of full past work histories, contribution caps, housing wealth,
residence eligibility, Work Bonus, partner-age differences or observed household
wealth distributions. It improves the mechanism while leaving substantial pension
calibration work before publication.

The real-return control operates **only through private retirement assets and
Age Pension means tests**. Higher returns increase accumulated contributions and
slow asset depletion after retirement. Larger assessable assets and deemed
income reduce pension eligibility/payments; smaller deficits then reduce debt
and subsequent interest costs. Deeming rates do not automatically follow the
chosen return. GDP, government borrowing rates, public asset yields and the
superannuation-tax revenue block do not change directly. In particular, the
model does not capture extra earnings-tax receipts from higher fund returns.

With CPI-indexed tax scales and all other defaults, illustrative sensitivity is:

| Real super return | Age Pension in 2065-66 (% GDP) | Net debt in 2065-66 (% GDP) |
|---|---:|---:|
| 2% | 2.62 | 111.5 |
| 3% | 2.07 | 102.2 |
| 4% (default) | 1.52 | 92.7 |
| 6.5% | 0.55 | 74.8 |
| 7% | 0.42 | 71.9 |

This large sensitivity depends on synthetic wealth archetypes, the 5% drawdown
and simplified accumulation. It is a model sensitivity, not a measured fiscal
effect or a validated pension forecast. Under Constant Relative Standards,
means-test changes are bypassed and the return control has no fiscal effect.
Annual mechanisms are checked in `tests/validate_update.R`; the reproducible
sensitivity table is `outputs/super_return_sensitivity.csv`.

Maximum payments use a non-decreasing benchmark of prices and average earnings;
means-test free areas follow prices. This approximates legislated indexation,
without separately projecting PBLCI or the exact MTAWE benchmark. September 2026
settings are uprated to the 2030 starting point; 2030 programme expenditure is
then controlled by the common fiscal calibration, not the synthetic population's
unadjusted dollar total.

Under Constant Relative Standards, starting pension entitlements per eligible
person are maintained relative to the payment benchmark instead of applying
future wealth tests; residual Social Protection becomes fully wage-linked.
This policy alternative is distinct from the indexed-rules scenario.

## Revenue projections

### Observed allocations and the common anchor

Fixed 42/20/25 percent revenue weights are removed. ABS observations identify
personal income tax, company tax, superannuation tax, Medicare/FBT, GST, excises
and customs, payroll, property, stamp duties and other taxes. GFS separately
identifies royalties, fees/other non-tax revenue and interest receipts.

Observed 2025 heads are carried to 2030 under the default nominal cost path;
personal income tax also reflects the change in the statutory schedule. One
common reconciliation makes their sum equal PBO 2030 revenue. The calibration is
frozen across scenarios. The app's detailed revenue table and downloads expose
each head rather than treating all receipts as a fixed GDP share.

### Personal income tax and threshold policy

ATO Table 16A supplies counts and taxable income by percentile, sex and state.
The model aggregates mutually exclusive sex/state cells into 100 percentile cells
and applies the resident schedule to each cell's mean taxable income. It does
not sum published subtotal rows. The distribution is uprated from 2023-24 to 2030;
future per-taxpayer income follows average nominal earnings, and taxpayer numbers
scale with employment. This assumes a stable relative income distribution and
does not independently model pensioner, non-resident or capital-gains tax bases.

The 2030 schedule uses thresholds $18,200/$45,000/$135,000/$190,000 and marginal
rates 0/14/30/37/45%, including the legislated 2027-28 lower rate. LITO is explicit.
Medicare and FBT are modelled in their separate observed tax block; income-tax
outputs are not incorrectly labelled as including those levies. The WATO offset,
whose wage-only eligibility cannot be identified from Table 16, is not simulated.
Its starting aggregate effect and other differences enter the fiscal calibration.

Three annual threshold conventions are selectable from 2030:

* **Fixed nominal:** unchanged nominal thresholds and offset amounts. This
  corresponds to the statutory threshold convention, without assumed future cuts.
* **CPI indexed:** thresholds and LITO amounts increase with domestic prices.
* **Wage indexed:** thresholds and LITO amounts increase with average earnings.

CPI indexation is the default policy assumption, including on reset and in the
fixed reference. It removes inflation-driven bracket creep while retaining
bracket creep from real earnings growth. It is not current legislation; fixed
nominal thresholds remain a selectable statutory-convention alternative. The tax
schedule itself generates the changing effective rate; there is no 1.1 proxy
income-tax elasticity. The app shows the direct revenue difference relative to
wage indexation under the same economic scenario. That comparison does not
re-simulate debt and interest receipts, which would require a complete financing
counterfactual. No revenue/GDP cap or hidden tax relief is applied.

### Other tax bases and estimated elasticities

Nine annual changes, 2017-2025, estimate each tax head's association with nominal
GDP growth, using a regression with an intercept. Estimates and standard errors
are retained. A Normal(1, 0.5 squared) prior shrinks noisy elasticities toward one.
These are short-sample descriptive estimates, affected by policy, COVID and base
measurement. They are not causal responses identified from productivity shocks.

For long-run projections the reference assumes unit-elastic growth with the
relevant economic base. Estimated elasticities apply only to **scenario deviations**:

```
scenario receipts = anchor receipts * reference base * (scenario base / reference base)^elasticity
```

This avoids compounding a short-sample policy trend into baseline tax/GDP for
40 years. Neither the estimated intercept nor a regression time trend is projected.
Personal income tax uses its explicit schedule instead of this regression rule.

Company, property, stamp duties and other taxes use NGDP as a base proxy. GST and
excises use domestic nominal output as a consumption proxy. Payroll, Medicare/FBT
and superannuation taxes follow employment and average earnings. Royalties follow
domestic nominal output plus a ToT elasticity of 2, which remains an assumption.
Fees and other receipts use unit-elastic domestic nominal output; dividends are
inside that residual rather than modelled from separate public-corporation assets.
State-specific payroll thresholds, property prices, consumption propensities,
company profits, excise erosion and energy-transition effects are not separately
estimated yet. These limitations remain visible in the Revenue tab.

## Debt and financial assets

All spending and tax cases use the same financing engine. Interest is charged on
opening gross debt. Its effective rate starts at the PBO 2030 bill divided by
2029 gross debt and converges by 2040 to the selected rate, 4.4% by default.
Interest receipts use opening financial assets and the anchor-calibrated yield.

Assets maintain the initial stock per person in domestic purchasing power;
acquisition is explicitly financed. After gross debt is repaid, further surpluses
accumulate in interest-bearing assets. Net debt may be negative.

```
fiscal balance = revenue - expenses - net capital investment
gross debt change = -fiscal balance + financial asset acquisition
net debt = gross debt - financial assets
```

There is no automatic fiscal repair. Revaluation, maturity buckets, indexed bonds
and a full public-sector balance sheet remain outside scope. Recursive identities
apply from 2031; published historical/PBO stocks are not reconstructed from rounded
historical flow ratios.

### Interpreting the productivity and fixed-NGDP example

The Debt & interest tab compares gross debt, modelled financial assets and net
debt, plus interest paid, interest received and net interest. Charts switch
between dollars and GDP shares. Tables expose the selected year's opening gross
debt, borrowing rate, GDP denominator, asset yield and implied real borrowing
rate. The annual download includes the same diagnostics. With an NGDP override,
an optional third path holds every other scenario assumption fixed and restores
linked NGDP. This isolates the checkbox's effect; the fixed reference still uses
1.2% productivity and all other defaults.

With 1.7% productivity and otherwise default assumptions, in 2065-66:

| Measure | Linked NGDP | NGDP fixed at 4.4% |
|---|---:|---:|
| Nominal GDP ($tn) | 20.16 | 16.53 |
| Gross debt ($tn) | 10.22 | 10.00 |
| Financial assets ($tn) | 2.46 | 2.01 |
| Net debt ($tn) | 7.76 | 7.99 |
| Net debt (% GDP) | 38.49 | 48.34 |
| Interest payments ($bn) | 433.37 | 423.06 |
| Interest payments (% GDP) | 2.15 | 2.56 |

Interest payments fall in dollars, while their GDP share increases because GDP
is proportionately lower. Gross debt also falls in dollars, but financial assets
fall by more, so dollar net debt rises slightly. The asset path depends on the
domestic price level, which changes with the NGDP override. Both scenarios still
have lower debt and interest shares than the fixed default reference (92.67%
net debt/GDP and 4.43% interest/GDP). Comparisons to that reference must not be
confused with comparisons before and after ticking the checkbox.

The interest equation is `interest[t] = nominal rate[t] * gross debt[t-1]`.
Dividing by current GDP does not make it a function of current net debt/GDP.
The nominal borrowing-rate assumption is independent of inflation. Lower implied
inflation therefore raises the real borrowing rate; there is no automatic Fisher
adjustment, maturity structure or monetary-policy reaction. This is a modelling
restriction to expose and test, not an accounting discrepancy. Likewise the
model treats its financial asset stock as earning one calibrated yield; it is
not a full decomposition of public-sector financial assets.

`tests/validate_debt.R` reproduces this comparison and passes 21 checks on gross
and net debt, opening-stock interest, asset income, the GDP denominator and the
NGDP toggle. It writes `outputs/productivity_ngdp_debt_comparison.csv` and
`outputs/debt_validation_checks.csv`. `debt_diagnostics()` adds reporting fields
without changing the financing or projection equations. Opening-stock fields
are blank at the published 2030 anchor; recursive identities apply from 2031.

## Comparisons, validation and outputs

The reference is fixed at all default assumptions: productivity 1.2%, linked NGDP,
0.3% extra service-price growth, unchanged ToT, CPI-indexed tax thresholds and
default pension, care-intensity, 4% real super return and interest settings. Changing a control
updates only the scenario. This corrects the earlier comparison behaviour in
which selected policy settings also moved the reference and could leave the
reported net-debt difference at zero.

The Revenue tab additionally isolates personal tax threshold policy within the
selected economic scenario. Shapley attribution now includes the four economic
groups plus a Policy and Financing group, reconciling their interactions to the
total 2066 net-debt difference from the fixed reference. It is model accounting,
not causal inference. Groups unchanged from defaults receive zero contribution.

Run validation from `projections`:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'e61 fiscal model\tests\validate_update.R'
```

All **88 updated engine checks** passed on 26 October 2026. Checks cover source
dimensions, labour identities, care separation, fiscal calibration, observed tax
totals, actual tax brackets/LITO, indexation, pension tests, controlled economic
changes, debt/asset identities under stress, and attribution interactions. They
establish implementation consistency, not historical predictive performance.
`tests/browser_smoke.R` checks live controls, tabs, reset and downloads.
`tests/validate_indexation.R` additionally verifies that wage-indexed and fixed nominal
thresholds change scenario net debt while the reference and its exported series
stay fixed. It checks the policy contribution and mixed-control attribution.
The browser regression checks both the net-debt headline and its reference gap.

Main files:

* `R/model.R`: public entry point; `R/model_v2.R`: projection and mechanism code.
* `scripts/acquire_update_sources.R`: cache sources and archive the first version.
* `scripts/prepare_update_inputs.R`: reproducible data extraction and reconciliation.
* `data/raw/v2`: raw workbooks, reports, chart data and request manifest.
* `data/processed`: model inputs, tax elasticity estimates, coverage bridges,
  IGR Commonwealth benchmarks and source checksums.
* `data/policy_parameters.csv` and `data/ndis_incidence_2025.csv`: explicit manually
  transcribed anchors/rules with provenance. Old top-level CSVs not loaded by v2
  are retained for traceability; the active inputs are named in `load_inputs()`.
* `outputs/reference_projection.csv`, `reference_components.csv`,
  `reference_revenue.csv`, `reference_revenue_diagnostics.csv`: default results.
* `outputs/programme_calibration.csv`, `labour_projection.csv`,
  `ndis_participant_projection.csv`, `pension_projection.csv`: mechanism diagnostics.
* `outputs/validation_checks.csv`, `browser_smoke_results.txt` and screenshots:
  verification evidence. Logs and PID identify the local Shiny process.
* `README.pdf`: printable version, regenerated by `scripts/render_readme_pdf.R`.

## Next development priorities

The first four priorities now have working implementations, with the remaining
qualifications above. The next priorities are:

1. Backcast programme spending, taxes and balances and assess historical forecast
   vintages. Use the existing top-down model as an independent challenger.
2. Replace synthetic pension wealth and family/tenure weights with household or
   administrative distributions; improve contribution histories, drawdowns and
   exact indexation before calling the pension module a current-policy forecast.
3. Add separately estimated service productivity, disability-specific NDIS costs,
   residential/home-care demand, education enrolment and current-policy reforms.
4. Improve tax bases, offsets, state thresholds, excise erosion and jurisdictional
   budgets; quantify structural and policy uncertainty before public release.
