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
| 4. Structural revenue bases | Observed ABS tax heads, ATO income percentiles, resident schedule and LITO; explicit threshold rules; proportional structural base proxies | No historical regression elasticity enters projections. Consumption, property, combined super taxes and some other bases remain explicit interim proxies |

The new Revenue tab explains the annual revenue path, composition, personal tax
thresholds, bracket creep, calibration and structural tax-base assumptions. Annual revenue
detail can be downloaded separately from the full fiscal projection.

## Deploying Fiscal_tester

**Current workflow: local launch only. Deployment requires a new explicit user
request.** The manual commands below are retained for when deployment is requested.

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
avoiding a second composition effect. This constructs the source labour path. Future average hours use the separate control, constant by default, and population follows the chosen growth rate. Age-specific employment profiles and participation targets remain shared.

```
total hours = employed people * average weekly hours * 52
real output index = total hours index * productivity index
productivity index = (1 + selected productivity growth)^years
```

## Nominal output and economic controls

The linked/override NGDP switch has been removed. The four explicit controls are:

| Assumption | Default |
|---|---:|
| Nominal GDP per capita growth | 3.2% a year |
| Population growth | 1.2% a year |
| Productivity growth per hour | 1.2% a year |
| Average hours growth per worker | 0% a year (constant hours) |

These are user-selected scenario settings, not a new economic forecast.
The reference, initial controls and reset use these same defaults. Other defaults
are wage tax indexation, 4% real super returns and a 0.3% service price premium.

```
population[t] = population[2030] * (1 + population growth)^t
NGDP per capita index[t] = (1 + selected per-capita growth)^t
NGDP[t] = NGDP[2030] * population index[t] * NGDP per capita index[t]
average hours[t] = average hours[2030] * (1 + hours growth)^t
real output index = employment index * average hours index * productivity index
domestic price index = NGDP index / (real output index * ToT income index)
```

Thus total nominal GDP grows at `(1.032 * 1.012) - 1 = 4.4384%` per year.
Changing productivity or hours alone leaves **every year's nominal GDP exactly
unchanged**. It changes real output and implied prices instead. Inflation is not
merely per-capita nominal growth minus productivity: hours per resident also
reflect participation, employment, ageing and average hours per worker.

The sidebar displays total NGDP growth, weekly hours at the anchor and endpoint,
and implied inflation. Default average weekly hours remain about 31.66 in both
2030 and 2066. The common GDP anchor is unchanged; default 2066 GDP is $16.748tn.
A 1.7% productivity scenario has precisely that same GDP when the other inputs
are held fixed. The old mode switch and its hidden growth target are absent.

Population growth scales the sourced age/sex population cells uniformly within
each year after 2030. The same cells feed employment, age-weighted service demand,
NDIS cohorts and pension eligibility. Age shares continue to evolve along the
sourced path; a different population growth setting does not simulate a different
migration, fertility or mortality pattern. Observed employment rates and the
IGR participation bridge are retained. A growing population raises both GDP and
service demand; it need not raise spending/GDP proportionately.

The terms-of-trade setting remains a permanent level shift phased to 2040.
Its 0.20 income elasticity is an illustrative assumption. At fixed nominal GDP
per capita and population, domestic prices adjust to preserve nominal GDP;
royalty exposure remains separate. The domestic price index is still a common
CPI/wage-deflator proxy; the GDP deflator additionally includes the ToT effect.
Hourly wages track prices and productivity. Annual earnings also include hours.

The common 2030 fiscal calibration and its historical 2025-30 carry-forward are
unchanged. Only future paths use the new controls. Legacy `ngdp_override` and
`ngdp_growth` arguments now fail explicitly, preventing silent use of an obsolete
control. The preceding implementation, README and tests are preserved in
`archive/pre_per_capita_2026_10_26/`.

## Programme spending mechanisms

### Audit of spending shares and the three drivers (26 October 2026)

The Spending tab shows annual paths and a driver table. Services follow number
(age-weighted demand or participants), unit cost and intensity. For each non-target
programme the driver audit reconstructs both nominal spending and the GDP share.

| Programme | 2029-30 (% GDP) | 2065-66 (% GDP) |
|---|---:|---:|
| Health | 7.35 | 10.03 |
| Aged care | 1.58 | 3.00 |
| NDIS | 1.61 | 2.01 |
| Education | 4.76 | 3.99 |
| Age Pension | 2.24 | 1.57 |
| Other social protection | 5.43 | 5.00 |
| Other public services | 8.87 | 8.70 |
| Defence | 1.59 | 2.50 |

```
relative spending/GDP = number index * real unit-cost index * intensity index
                       / (NGDP index / domestic price index)
```

At current defaults health's age-weighted demand grows 1.79 times, versus total
population growing 1.54 times. Its real unit cost grows 1.50 times and intensity
1.20 times, against the denominator growing 2.36 times. Health's GDP share rises
about 36%. Thus ageing and programme cost pressures are already in the baseline.

Population scenarios change demand but preserve the age shares. Age Pension uses
eligible people, recipient coverage and average payment; its average payment also
reflects means-test composition. Defence instead follows an explicit GDP-share
target and is excluded from the three-driver reconstruction. The cost pass-through
and intensity coefficients remain assumptions requiring validation. Hours affect
production and implied prices; no separate service-capacity response is fitted.

`spending_driver_audit()` writes annual diagnostics through the validation script
to `outputs/spending_driver_audit.csv`. Overlapping reference/scenario curves do
not mean the spending share is constant over time.

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

### NDIS benchmark check (26 October 2026)

At the new defaults, nominal Scheme spending rises from $56.3bn in 2029-30 to
$71.25bn in 2034-35, averaging 4.8% annual growth. The 0.5% intensity setting
is extra cost per participant above the wage/price path, not total Scheme growth.
The live readout shows compound annual growth for 2030-35 and 2035-66 and the
2034-35 spending level. An intensity setting of 2% implies about 6.4% annual
spending growth in 2030-35 at otherwise default assumptions. The readout responds
to the entire economic scenario. Holding age-specific incidence fixed omits an
independent rollout or rising-incidence trend.

* The [NDIA 2024-25 actuarial report](https://ndis.gov.au/media/8183/download?attachment=),
  table 1, projects $95.755bn total accrual expenses in 2034-35 and 7.2% growth
  that year. The new default is $24.5bn (25.6%) lower. That older forecast predates
  the 2026 reforms; matching it is not the current baseline calibration target.
* The [PBO July 2026 outlook](https://www.pbo.gov.au/publications-and-data/publications/2026-27-medium-term-budget-outlook),
  section 5.3, describes reform targets below 2% annual growth over the forward
  estimates and approximately 5% subsequently. These depend on savings delivery.
* [2026 IGR chart C12.17](https://treasury.gov.au/publication/2026-intergenerational-report)
  reports **total Scheme** spending of 1.5% GDP in 2029-30, 1.6% in 2034-35,
  1.8% in 2049-50 and 2.0% in 2065-66. Current model shares are 1.61%, 1.63%,
  1.80% and 2.01%. GDP denominators differ and official values are rounded.
  Commonwealth net funding in C12.5 has different coverage.

This proximity does not validate the model's mechanism. Its 2030 allocation is
scaled to the PBO aggregate, not a sourced Scheme forecast, and it does not
explicitly cost reforms or disability-specific entrant, exit and support-cost
pressures. Validate the Scheme anchor and participant/unit-cost paths before
using it as a forecast. `scripts/check_ndis_and_super.R` reproduces the comparison
from cached data and writes the annual benchmark and older-vintage checks.

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

With wage-indexed tax scales and all other defaults, illustrative sensitivity is:

| Real super return | Age Pension in 2065-66 (% GDP) | Net debt in 2065-66 (% GDP) |
|---|---:|---:|
| 2% | 2.69 | 137.0 |
| 3% | 2.13 | 128.0 |
| 4% (default) | 1.57 | 118.7 |
| 6.5% | 0.57 | 101.0 |
| 7% | 0.44 | 98.1 |

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

Wage indexation is the default policy assumption, including on reset and in the
fixed reference. It removes bracket creep from growth in average earnings.
CPI indexation remains selectable and retains bracket creep from real earnings
growth. Wage indexation is not current legislation; fixed
nominal thresholds remain a selectable statutory-convention alternative. The tax
schedule itself generates the changing effective rate; there is no 1.1 proxy
income-tax elasticity. The app shows the direct revenue difference relative to
wage indexation under the same economic scenario. That comparison does not
re-simulate debt and interest receipts, which would require a complete financing
counterfactual. No revenue/GDP cap or hidden tax relief is applied.

### Structural company tax

Company tax is projected as a structural share of GDP, with no business-cycle
mechanism and no historical nominal-GDP elasticity:

```
company tax/GDP = calibrated 2030 company-tax/GDP share * ToT index^eta
company tax dollars = nominal GDP * company tax/GDP
```

The starting share is 4.9026% of GDP, taken from the existing calibrated 2030
allocation. This is a modelling anchor, not a freshly estimated normal-profit
share or a separately published PBO company-tax forecast. `eta`, the elasticity
of the GDP share to the ToT level, defaults to 1 and can be varied from 0 to 2
under advanced assumptions. Its magnitude is an explicit illustrative assumption,
not a fitted coefficient. At eta=1:

| Terms-of-trade level relative to 2030 | Structural company tax (% GDP) |
|---|---:|
| -20% | 3.92 |
| Unchanged | 4.90 |
| +10% | 5.39 |
| +20% | 5.88 |

The existing ToT control phases in the level change by 2040. Once the ToT level
stabilises, the company-tax share stabilises; the effect is not compounded as
extra annual tax growth. Increasing NGDP per capita changes dollar collections
proportionately and cannot change this share. Productivity, population and hours
also leave the share unchanged when ToT is fixed.

The former 1.60 historical GDP elasticity is no longer applied to company tax.
It remains in the historical evidence extract and assumptions download for
provenance, labelled as not used. The zero-productivity, 8% nominal per-capita-growth example
now has company tax of 4.90% of GDP in every projected year, rather than reaching
13.16%. Other revenue heads retain their documented separate rules.

The Revenue tab shows a live company-tax reconciliation and annual detail. The
assumptions download includes `company_tot_elasticity`. There is no firm-level
profit or loss-offset model here: the ToT response is a reduced-form structural
scenario assumption. Royalties are a separate revenue head and retain their
existing rule. `tests/validate_company_tax.R` checks 38 structural relationships
and writes `outputs/structural_company_tax_scenarios.csv` and
`outputs/company_tax_validation_checks.csv`.

### Structural revenue benchmark (26 October update)

**No historical tax/GDP regression coefficient is used in any projection.**
The old nine-observation estimates remain descriptive evidence only. Both the
reference and alternative scenarios now use the same structural rules:

```
receipts = calibrated anchor * nominal base index
```

This assumes constant effective yield and taxable coverage, except for the
explicit personal-tax schedule and company/royalty ToT rules. A unit response
to a nominal base is a modelling assumption about unchanged coverage and policy,
not an estimated elasticity or an accounting law.

| Head | Active structural rule | What we assume rather than observe |
| --- | --- | --- |
| Personal income tax | ATO percentile incomes, employment and explicit schedule/indexation | Fixed distribution shape; average taxable income follows earnings; wage-indexation default is a chosen policy |
| Company tax | GDP * calibrated share * ToT^eta | Structural anchor allocation and eta=1; no firm-profit model |
| GST | Nominal consumption proxy proportional to GDP | Consumption/GDP, taxable fraction and effective yield constant; no estimated consumption-level series is loaded |
| Payroll tax | Nominal earnings * employed people | Constant effective coverage/yield; no employer distribution or state thresholds |
| Medicare/FBT | Nominal wage-bill proxy | Combined head; neither levy thresholds nor fringe benefits separately modelled |
| Superannuation taxes | Nominal wage-bill proxy | Total contributions/earnings tax remains combined; returns do not affect tax receipts yet |
| Excises/customs | Population * domestic price index | Constant taxable units per resident and CPI-indexed effective unit rates across a mixed aggregate |
| Property taxes | Nominal GDP proxy for property values | Constant property-value/GDP ratio and effective yield; no local rates/land threshold model |
| Stamp duties | Nominal GDP proxy for transaction values | Constant property-value/GDP ratio, turnover and effective yield; not a housing forecast |
| Other taxes | Nominal GDP | Constant residual share; heterogeneous bases not separately modelled |
| Royalties | Domestic nominal output * ToT^2 | Exponent 2 is illustrative; net GDP-share response is ToT^(2-0.20) under the price identity |
| Fees/other | Domestic nominal output | Constant effective yield; includes dividends without a public-corporation model |
| Interest receipts | Opening modelled financial assets * calibrated yield | Constant yield; balance-sheet asset proxy described below |

The method is informed by [IGR Appendix A3.3](https://treasury.gov.au/publication/2026-intergenerational-report),
which links taxes to macroeconomic bases. It does **not** supply our effective
yields, constant ratios, pass-through coefficients or proxy choices. The
[PBO revenue discussion](https://www.pbo.gov.au/publications-and-data/publications/2026-27-medium-term-budget-outlook)
supports treating excise erosion explicitly. We have not copied its projected
decline: the current benchmark has no additional EV, tobacco or alcohol volume
erosion. Nor do we adopt the IGR aggregate tax ceiling.

Excise now falls relative to GDP when real output per resident grows: indexed
tax per unit does not capture growing real incomes if units/person are constant.
This is a consequence of the chosen quantity/rate structure. Customs contains
ad valorem taxes, so applying the same rule to the combined head is a provisional
approximation. Fuel, tobacco, alcohol and customs need separate data before a
credible transition forecast can replace it.

Higher nominal growth at unchanged real assumptions should not itself change
these tax/GDP paths under indexed policy. In the zero-productivity case, most
tax shares are constant; excise can still move with employment/participation
changes because those alter real output per resident. Asset income/GDP can move
through financing and asset accumulation. Fixed nominal PIT thresholds deliberately
break inflation neutrality. These are testable conditional implications, not a
claim that every revenue/GDP ratio must be constant.

### Evidence, judgments and structural implications

`data/assumption_register.csv` is the full, downloadable audit register, shown
under **Method & data** in the app. Rebuild it offline with
`Rscript scripts/build_assumption_register.R`. It records each choice's value or
rule, classification, named source and URL, our addition and its limitation.
The register is a snapshot of the default model; scenario selections are exported
separately in the assumptions download.

* **Sourced:** an observed figure, published forecast or policy parameter directly
  transcribed. It may still be applied to a broader population than its source.
* **Calibrated:** a model-derived allocation reconciled to published totals. A
  PBO aggregate anchor does not make individual programme or tax-head allocations
  official PBO forecasts.
* **Proxy:** an unavailable economic base represented by another series, with an
  explicit maintained relationship. For example, constant consumption/GDP.
* **Judgment / user choice:** a scenario convention or guess without an estimated
  supporting coefficient. A source cited for the starting value does not support
  the guessed future response.
* **Identity:** a result that follows algebraically from equations. Its economic
  realism still depends on the chosen equations and their assumptions.

Several entries combine these classes: a sourced age-cost profile and an assumed
cost-growth rate play different roles within the same spending module.

### Spending choices and their evidence

For service programmes the implemented identity is:

```
spending = 2030 allocation * age-weighted exposure index
           * domestic price index * productivity index^pass-through
           * (1 + relative service-price growth)^years
           * (1 + programme intensity growth)^years
```

| Programme | Number/exposure evidence | Productivity pass-through | Additional annual intensity |
| --- | --- | ---: | ---: |
| Health | IGR C12.10 Commonwealth health age-cost profile used nationally | 70% | 0.5% |
| Education | Transformed IGR C6.6 net Education and other age profile | 80% | 0% |
| Aged care | Absolute IGR C6.6 aged-care age profile | 80% | 0.3% |
| NDIS | NDIA Q4 2024-25 D.13 incidence; older cohorts retained | 85% | 0.5%, adjustable |
| Other social protection | Population aged 15-66 | 65% | 0% |
| Other public services | Total population | 70% | 0% |

**Every pass-through and intensity coefficient in this table is a prototype
judgment, not an estimated coefficient or copied IGR parameter.** Zero intensity
is also an assumption. The common additional relative-price default, 0.3% per
year, is another judgment. Their combined effect can overlap wage, technology
and service-quality changes; they have not been jointly identified from data.
Pass-through means how much economy-wide productivity growth raises real unit
costs; it is not an independently measured government-sector productivity rate.

The [IGR chart workbooks](https://treasury.gov.au/publication/2026-intergenerational-report)
supply the age patterns, not our spending trajectories. The education proxy is
particularly weak: it is a shifted net category, not observed enrolment or gross
education costs. National health/care application is also an extrapolation from
Commonwealth profiles. Ageing raises spending mechanically where older people
have higher weights; the magnitude depends on the sourced weights remaining
applicable in future.

Age Pension overrides the generic equation with population aged 67+, eligibility
coverage and mean entitlements from the representative pension module. Published
rules and IGR starting status shares are inputs. The 65% couple share, 75%
homeowner share, 80% financial-asset allocation, constructed wealth archetypes,
5% drawdown and simplified contribution histories are **guesses**, not observed
household distributions. The user chose 4% real returns and a slider up to 7%.
Higher returns can reduce means-tested pension spending by construction; this
does not validate the magnitude, or project super-fund tax receipts. All current
numeric pension-rule inputs and their dated source URLs appear in the register.

Defence overrides the programme equation with a target reaching **2.5% of GDP
in 2035**. This is a retained prototype policy judgment, not a sourced national
defence projection. Net capital investment uses a **0.5 productivity exponent**,
also a judgment. Debt stocks and interest then follow the financing identities,
conditional on the independently selected nominal borrowing rate and assumed
asset accumulation policy.

The most consequential unsupported choices to validate next are programme
pass-through/intensity, synthetic pension wealth, the normal company-tax anchor
and ToT response, excise quantities and the property/consumption proxies.

## Cost pass-through controls and the NGDP link (27 October 2026)

The separate **Cost pass-through** sidebar section exposes Health (70%),
Education (80%), Aged care (80%), NDIS (85%), Other social protection (65%) and
Other public services (70%). Each can vary from 0% to 150%. They are unestimated
scenario judgments. The controls change the future productivity exponent in
real unit costs, leaving every 2030 allocation and the fixed reference unchanged.
The separate reset restores only these six values; the main reset restores all
assumptions. Constant relative standards overrides social protection to 100%.
Age Pension and Defence retain their separate entitlement and target rules.
Exports include the six rates, and Shapley attribution has a Cost pass-through
group. The driver audit reports the effective coefficient for each programme.

The Spending tab now switches between GDP shares and nominal dollars and shows
a counterfactual changing only NGDP/person growth from 3.2% to the selected rate,
with all other selected settings held fixed. There is no missing NGDP-to-spending
link for fully indexed services:

```
domestic prices = nominal GDP index / (real output index * ToT income factor)
service spending = anchor * exposure * domestic prices * productivity^pass-through
                   * relative-price factor * intensity
```

At fixed real assumptions and ToT, higher NGDP/person raises domestic prices,
nominal GDP and indexed service spending proportionately. Dividing spending by
GDP cancels the nominal change. ToT is different: at fixed GDP it changes the
real-income factor, hence domestic prices and spending/GDP. Raising NGDP does
not reverse that wedge merely by changing the overall nominal scale.

For example, in 2066 at default pass-through: Health is $1,680.6bn (10.035% GDP)
with unchanged ToT and NGDP/person growth 3.2%. With ToT 20% higher, it becomes
$1,620.4bn (9.676% GDP), while GDP remains $16,747.7bn. Raising NGDP/person growth
to 4% then raises Health to $2,139.8bn and GDP to $22,114.9bn; Health remains
9.676% of GDP. Thus the dollar link works even though the share is unchanged.
Nominal interest-rate assumptions and pension indexation floors can produce
different responses outside these fully indexed service relationships.

`tests/validate_cost_and_ngdp.R` checks 48 restrictions, including all six cost
controls, common anchors, isolated programme effects, ToT/NGDP combinations,
nominal price scaling and attribution. Annual comparison cases are saved in
`outputs/cost_and_ngdp_scenarios.csv`. These tests establish equation consistency,
not the empirical validity of the cost or ToT coefficients.

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

### Understanding debt and interest under the new controls

The Debt & interest tab shows gross debt, modelled financial assets and net debt,
plus interest paid, interest received and net interest. Charts switch between
AUD billions and GDP shares. Tables expose opening debt, borrowing rate, asset
yield, GDP and the implied real borrowing rate. Downloads contain annual detail.

At the selected wage-indexed defaults, 2066 net debt is 118.67% GDP and interest
paid is 5.44% GDP. With productivity at 1.7% and all other inputs unchanged, net
debt is 95.30% and interest paid 4.42% GDP. Nominal GDP is **$16.748tn in both
scenarios**. The default debt level is higher than under CPI indexation because
the model no longer collects additional personal tax through real bracket creep.
These are conditional scenario outcomes, not an accounting correction or forecast.
These outcomes are illustrative and depend on the spending and revenue rules.

```
net debt = gross debt - modelled financial assets
interest paid[t] = nominal borrowing rate[t] * opening gross debt[t]
interest received[t] = calibrated asset yield * opening financial assets[t]
net interest = interest paid - interest received
interest paid/GDP = dollar interest paid / current nominal GDP
```

A lower net-debt figure alone does not determine the gross interest bill. The
opening gross stock, nominal borrowing rate and GDP denominator must be compared.
The nominal rate remains independent of implied inflation: higher productivity
at fixed nominal income lowers inflation and raises the real borrowing rate.
There is no automatic Fisher adjustment, maturity structure or monetary-policy
reaction. Financial assets earn a single calibrated yield; this is a simplified
balance sheet. The reference remains fixed at all default assumptions.

`tests/validate_debt.R` passes 16 financing checks and writes
`outputs/debt_comparison.csv` and `outputs/debt_validation_checks.csv`.
`tests/validate_structure.R` passes 20 macroeconomic/demographic checks, including
exact GDP invariance at 1.2% versus 1.7% productivity and when hours change,
population/NGDP consistency and NDIS cohort reconciliation. Annual scenarios are
in `outputs/structure_regression.csv`. The published 2030 anchor is not rebuilt;
opening-stock fields are blank there and recursive identities start in 2031.

## Comparisons, validation and outputs

The reference is fixed at productivity 1.2%, NGDP per capita growth 3.2%, population growth 1.2%, constant hours,
0.3% extra service-price growth, unchanged ToT, wage-indexed tax thresholds and
default pension, care-intensity, 4% real super return and interest settings. Changing a control
updates only the scenario. This corrects the earlier comparison behaviour in
which selected policy settings also moved the reference and could leave the
reported net-debt difference at zero.

The Revenue tab additionally isolates personal tax threshold policy within the
selected economic scenario. Shapley attribution includes six economic
groups, a Cost pass-through group and a Policy and Financing group, reconciling their interactions to the
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
An additional **45 structural revenue checks** in
`tests/validate_structural_revenue.R` verify inflation neutrality, explicit base
responses, complete removal of regression coefficients, the zero-productivity
stress case and evidence-register coverage. The source register has 64 entries.
`outputs/structural_revenue_scenarios.csv` contains annual revenue-head shares
for the reference and zero-productivity scenarios at 3.2% and 8% nominal growth.
The historical company, macro and debt checks continue to pass. These checks
test internal restrictions; they do not establish that the proxy assumptions
are good empirical forecasts.
`tests/browser_smoke.R` checks live controls, tabs, reset and downloads.
`tests/validate_indexation.R` additionally verifies that CPI-indexed and fixed nominal
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
* `data/assumption_register.csv`: source, value/rule, classification, model addition
  and limitation for each material input or rule. Rebuilt offline by
  `scripts/build_assumption_register.R`; available in the app and its download.
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
