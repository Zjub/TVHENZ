# e61 fiscal model - updated research prototype

Updated 27 October 2026. This version implements the first four development
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

### International debt comparisons: long-term IMF and OECD scenarios

The Fiscal outlook comparison starts in **2030**. We use the PBO for our common
FY2029-30 anchor and begin scenario changes afterwards. Short-run international
forecasts and the previous near-term display option have been removed. No
projection or fiscal parameter has been recalibrated to an international source.
A display control switches between levels and changes from each series' own
2030 value; rebasing does not harmonise their accounting or policy assumptions.

**The IMF does publish Australian long-term debt scenarios.** These are in
[Country Report 26/38, Annex III, Figure 7, printed page 53 (PDF page 58)](https://www.imf.org/en/-/media/files/publications/cr/2026/english/1ausea2026001-source-pdf.pdf#page=58).
The report covers the 2025 Article IV consultation; its staff report was completed
on 21 January 2026. This is a different source from the October 2026 Fiscal
Monitor series used in the first version of this comparison, which ends in 2031.

The IMF's long-term **large-amortisation risk module** contains three gross-debt
paths. We now show all three in the gross-debt panel, using its original labels
in the CSV and explanatory labels in the app:

| IMF source label | Interpretation | Approximate gross debt in 2054 (% GDP) |
| --- | --- | ---: |
| Baseline with t+5 | Extend the medium-term assumptions into the long run | 24.2 |
| Baseline with t+5 and DSPB | Use a debt-stabilising primary balance | 49.2 |
| Historical 10-year average | Use the module's historical-average assumptions | 63.1 |

These are conditional debt-risk scenarios, **not a complete bottom-up projection
of future ageing-related spending**. The pensions and health modules are greyed
out on that figure. The stabilising case assumes the primary balance that
stabilises debt: its flat debt path is not evidence that present tax and spending
policy automatically remains sustainable. Differences from our model reflect
policy, spending and macro assumptions as well as starting levels. The baseline's
falling debt must not be interpreted as an independently validated estimate of
our unchanged-policy model. The module extends medium-term assumptions, whereas
our model explicitly accumulates demographic and service-cost pressures.

**Extraction accuracy:** the published IMF source is a chart, not the official
annual numerical data behind it. `scripts/prepare_imf_long_term.R` renders the
cached PDF page at 600 dpi, identifies the three coloured debt lines and maps
pixel positions to the chart axes. It bridges gaps in dashed lines, rounds to
0.1 percentage point, and saves **approximate chart readings**. Allow at least
+/-0.5 percentage points of GDP reading uncertainty; decimal presentation is not
an assertion of official precision. The chart labels every third year through
2053 and includes a final annual category in 2054. The extracted 2030 baseline is
49.2%, matching the report's numerical baseline table to the displayed precision.
These approximations retain their own starting levels; they are not shifted to
our PBO anchor, converted to net debt, or extended to 2066.

The upper panel compares model **net debt** with OECD **net financial
liabilities** (`GNFLQ`, `BAU1`). This comes from the dataflow identifying itself as
**Economic Outlook 117 long-term scenarios**, published through 2100 and shown
only to 2066. Its broader balance sheet and financial-asset netting make it
materially different from PBO-style net debt. It also has a technical fiscal
reaction rule that stabilises debt. It is a separate scenario and vintage from
the Survey, not a net conversion of either Survey scenario or the IMF cases.
No IMF long-term net-debt path is inferred from its gross-debt chart.

The lower panel compares model **gross debt** with the three IMF cases and
**both OECD Australia Survey 2026 scenarios**:

| OECD Figure 1.14 panel B | Published measure | Last year | Last value (% GDP) |
| --- | --- | ---: | ---: |
| Current tax/spending structure with future ageing and health costs | Gross public debt | 2050 | 113.2277 |
| Path with fiscal adjustment | Gross public debt | 2050 | 57.2830 |

Both OECD Survey lines come from the official StatLink workbook, sheet `fig1-14`,
columns J, L and M (updated 21 January 2026). They share a vintage and definition.
The adjustment case includes spending savings or additional revenue. No path is
extended beyond its source horizon or given an assumed financial-asset path to
manufacture a net-debt series.

All ratios retain their provider's GDP denominator, coverage and valuation.
Model dates are financial years ending in June; source years remain unchanged,
without a fiscal/calendar conversion. The table reports each series' last
available year, which is not a common comparison endpoint. The change-from-2030
view helps inspect slopes despite different initial levels, but does not remove
these substantive differences.

Additional sources:

* [OECD Survey workbook](https://stat.link/gdxr47) and
  [Australia Survey 2026, Figure 1.14](https://www.oecd.org/content/dam/oecd/en/publications/reports/2026/01/oecd-economic-surveys-australia-2026_1b6f84bc/d22a1efd-en.pdf).
* [OECD long-term dataflow](https://data-explorer.oecd.org/vis?df[ag]=OECD.ECO.MAD&df[id]=DSD_EO_LTB@DF_EO_LTB)
  and [long-term fiscal model methods](https://www.oecd.org/en/publications/illustrating-long-run-fiscal-challenges-using-the-long-term-model_421bf86f-en.html).
* [IMF Fiscal Monitor indicator metadata](https://www.imf.org/external/datamapper/api/v1/indicators)
  documents the archived medium-term series, which is excluded from this chart.

Separate CSVs and reproducibility:

* `data/processed/imf_long_term_debt_projections.csv`: **75 approximate chart
  observations**, three gross-debt cases, 2030-2054; downloaded by the app's IMF
  button. Each row identifies its source, scenario and approximation limitation.
* `data/processed/oecd_debt_projections.csv`: **169 official numerical
  observations**, both Survey gross-debt cases (2022-2050) and the separate EO117
  net-financial-liabilities series (1990-2100). Only 2030 onward is displayed.
* `data/processed/imf_debt_projections.csv`: the earlier **84 medium-term and
  historical** net/gross observations (1990-2031), retained for provenance only;
  neither displayed nor downloaded by the app's IMF long-term button.

`scripts/acquire_international_debt.R` and
`scripts/prepare_international_debt.R` reproduce the initial API/workbook data.
`scripts/prepare_imf_long_term.R` separately reproduces the IMF long-term
extraction from its cached official PDF. Source manifests record URLs, dates and
hashes in `data/raw/international_debt`. The app runs offline using the two active
CSV files. Source paths remain fixed when users change model assumptions.
`R/international_debt.R` explicitly rejects short-run windows and excludes the
medium-term IMF series even if supplied by a caller. The comparison download
includes raw levels and any rebased plot values, with source and accuracy notes.
The validation and browser checks cover the three IMF cases, both OECD Survey
cases, horizons, accounting panels, rebasing, downloads, and benchmark invariance.

### Bottom-up construction

Spending now sums **24 subfunctions** to the eight headline categories. Separate
health age profiles and school/higher-education/VET demand replace the previous single
future demand index for Health and Education. All 2030 category anchors and the
NDIS and pension paths are preserved. Some blocks retain explicit simple proxies:
residential/home care share an age profile and other social protection remains
a working-age residual. No new causal pass-through estimate is claimed.

Read [the spending background PDF](documentation/spending_background.pdf) for the
full source-to-anchor bridge, assumptions, worked hospital example, revisions and
factor contributions. Its editable/reproducible HTML and all audit tables are
generated by `scripts/build_spending_background.R`. New raw sources are cached by
`scripts/acquire_bottom_up_sources.R`; offline extraction is in
`scripts/prepare_bottom_up_inputs.R`. The prior implementation is frozen in
`archive/pre_bottom_up_2026_10_27`.

The Spending tab shows contributions from population, age/cohort demand,
coverage, general prices, real unit cost/payment, relative service prices,
intensity, policy/financing rules, aggregate cost scaling and (for shares) the GDP denominator. Exact
Shapley accounting reconciles the interactions to the change, in nominal dollars,
2030 dollars or percentage points of GDP. This is model accounting, not causal
attribution. Detailed annual blocks and contributions can be downloaded. The
64 checks in `tests/validate_bottom_up.R` verify anchors, aggregation, unchanged
NDIS/pensions and decomposition identities, including zero-valued factors.

### Health and education: differentiated mechanisms and evidence

The 27 October update replaces the common future Health and Education cost
exponents with nine explicit subfunction specifications. It separates higher
education from VET, bringing the model to 24 spending blocks. It does **not** fit
nine causal elasticities or claim that the complete spending identity is observed.
The purpose is to distinguish what can be measured from the assumptions needed
to project it. The previous engine and allocations are preserved in
`archive/pre_health_education_2026_10_27`.

#### Identity, units and interpretation

For these subfunctions the annual equation is:

```
spending[i,t] = anchor[i,2030]
               * population_index[t] * age_demand_index[i,t]
               * coverage_index[i,t] * intensity_index[i,t]
               * domestic_price_index[t] * real_unit_cost_index[i,t]
               * relative_service_price_index[i,t]
               * aggregate_cost_multiplier[function,t]
```

All indices equal one in 2030. The common domestic-price index serves as the
model's CPI proxy, rather than a separately estimated Australian CPI path.
Population times age demand equals the existing demographic exposure index.
Coverage means access, eligibility or participation; intensity means services
or inputs per covered person. Unit cost means government
expense per modelled service unit, rather than the entire provider's costs in
every case. In practice, the health age profiles are **age-spending profiles**:
they already embed baseline differences in use, complexity, coverage and prices.
They are not separate observations of each component. We hold that embedded
pattern fixed and apply explicit changes to coverage and intensity afterwards.
Consequently, the model is an anchored expenditure-index model; it does not yet
forecast absolute numbers of hospital episodes or prescriptions.

Where an observed input share is available, the real unit-cost index is a
base-period input basket:

```
real_unit_cost[i,t] = ((1 - labour_share[i])
                      + labour_share[i] * real_hourly_wage_index[t])
                     / (1 + service_productivity_growth[i])^(t-2030)
real_hourly_wage_index[t] = economy_wide_productivity_index[t]
```

The non-labour basket follows general domestic prices. Labour follows those
prices plus economy-wide hourly productivity. Input quantities per service are
fixed unless the service-productivity assumption changes. As relative wages
rise, the labour share of the resulting cost basket rises: the formula is a
fixed-quantity basket, **not** a constant-share Cobb-Douglas index. Linking wages
to economy-wide productivity remains a structural assumption. An observed labour
share alone says nothing about future service efficiency or the fiscal saving
from better outcomes. Constant average hours does not prevent hourly wages from
growing; the service-cost wage index does not use hours growth as a wage increase.

For VET the model instead compounds the annual funding proxy:

```
real_unit_cost[VET,t] = (1 + 0.6975 * annual_productivity_growth)^(t-2030)
                      / service_productivity_index[VET,t]
```

This is a weighted annual wage/CPI index under the model's wage assumption,
not the fixed-quantity basket above. Medicines have a zero wage weight and a
CPI-only unit-cost proxy. A common aggregate cost adjustment still applies to
both VET and medicines.

#### Active specifications and their evidence

| Subfunction | Demand base | Real unit-cost specification | Evidence status |
| --- | --- | --- | --- |
| Hospital services | IGR public-hospital age-spending profile | Input basket, labour weight 67.253% | Observed public admitted-acute cost mix, applied as a scope proxy |
| Primary/community care excluding PBS | IGR medical-benefits profile | Input basket, wage weight 70% | Retained judgment; neither a measured labour share nor an MBS indexation estimate |
| Referred medical services | IGR medical-benefits profile | Input basket, wage weight 70% | Retained judgment; government rebates and provider costs differ |
| PBS medicines outside hospitals | IGR pharmaceutical profile | CPI-only unit-cost proxy | Explicit judgment; no real medicine-price trend fitted |
| Other health/reconciliation | Total population | Input basket, wage weight 70% | Retained judgment for a heterogeneous residual |
| Schools | Population aged 5-17 | Input basket, labour weight 75.993% | Observed government-school cost mix, applied as a scope proxy |
| Higher education | Population aged 18-24 | Input basket, labour weight 55.473% | Observed university employee-expense share, applied as a scope proxy |
| VET | Population aged 15-64 | Annual wage/CPI funding mix, wage weight 69.750% | Published Commonwealth flexible-funding formula, extended as a proxy |
| Other education | Total population | Input basket, wage weight 80% | Retained judgment; preschool and other functions remain combined |

**Hospital calculation.** IHACPA's NHCDC 2023-24 appendix workbook, sheet
`5 Acute Overview`, Appendix 5.2, national 2023-24 column, reports line-item costs
per admitted-acute separation. The extractor sums the five salary/wage categories
(nursing, medical non-VMO, medical VMO, allied health and other staff) and oncosts,
then divides by the total line-item cost, including depreciation. The result is
0.6725298. The source covers public admitted-acute activity; our hospital block
also includes other activity and government funding of private hospitals. This
extension is a **proxy**, not a national hospital labour-share estimate. No recent
one-year cost increase is extrapolated as long-run growth.
[IHACPA report and appendix downloads](https://www.ihacpa.gov.au/resources/national-hospital-cost-data-collection-public-sector-2023-24).

**School calculation.** ACARA's 2024 report, Table 9.3, supplies government-school
recurrent spending for 2023/24. In $000, teaching salaries 31,447,043 plus
non-teaching salaries 11,301,892 plus redundancies 19,259, divided by total
recurrent expenditure **excluding notional user cost of capital**, 56,279,069,
gives 0.7599307. Using the report's headline share including user cost of capital
would give a different weight. We exclude that notional charge to improve
alignment with the model's expense concept; this does not make the two accounting
boundaries identical. Applying the government-school basket to all government
school funding, including non-government-school grants, remains a proxy.
[ACARA Chapter 9, Table 9.3](https://dataandreporting.blob.core.windows.net/anrdataportal/ANR-Documents/ANR2024/nationalreportonschoolinginaustralia_2024_Chapter9.pdf).

**University calculation.** Finance 2024 Table 4 reports employee benefits of
$23,891.495 million and total expenses of $43,068.392 million. Their ratio is
0.5547339. These are whole-provider accounts, including research and activities
funded by international fees and other private sources. We use the ratio as a
cost-structure proxy for the higher-education block, not as a direct measurement
of government-funded teaching costs or a rule for Commonwealth grants.
[Department of Education, Finance 2024, Table 4](https://www.education.gov.au/download/19832/finance-2024-financial-reports-higher-education-providers/43005/2024-university-finance-summary-information/pdf).

**VET calculation.** The National Skills Agreement specifies flexible-funding
indexation using 85% WCI-1 and 15% WCI-6. Their wage weights are 75% and 40%,
so the implied wage weight is `0.85*0.75 + 0.15*0.40 = 0.6975`; the remaining
30.25% is CPI exposure. The agreement's wage component includes a productivity
adjustment. We set that offset to zero in this prototype and disclose that choice;
we do not claim to reproduce the complete index. Extending this Commonwealth
funding formula to the whole VET allocation is also a proxy, not an observed
provider input share or a consolidated current-policy projection.
[National Skills Agreement, flexible-funding indexation provisions](https://federalfinancialrelations.gov.au/sites/federalfinancialrelations.gov.au/files/2023-12/national-skills-agreement.pdf).

#### Splitting the tertiary allocation without inventing a new total

ABS Education Table_1 reports consolidated tertiary operating expenses of
$51.439bn in 2024-25, without the higher-education/VET split needed here. We
allocate that control using two 2024 expenditure magnitudes: university total
expenses of $43.068392bn and RoGS VET government recurrent expenditure excluding
user cost of capital of $7.6172bn. The resulting VET allocation weight is
15.0283%; higher education receives the remainder. These are **allocation
weights**, not additional spending. They sum back exactly to the existing ABS
tertiary total. University accounts and VET recurrent expenditure differ in
coverage and accounting; calendar 2024 also differs from financial 2024-25.
The split is therefore explicitly classified as a cross-source proxy, pending
a direct COFOG-compatible allocation.
[ABS Education workbook](https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/2024-25/55120DO095_202425.xlsx),
[RoGS 2026 VET, Table 5A.1](https://www.pc.gov.au/ongoing/report-on-government-services/child-care-education-and-training/vocational-education-and-training/).

The separate blocks use different age bases: 18-24 for higher education and
15-64 for VET. The VET age range follows the RoGS reporting scope, but uniform
participation within that range is our assumption. The higher-education range
remains a simple proxy, not a use of observed age-specific enrolment rates.
Both omit changes in mature-age participation, international enrolments, course
mix and study intensity. Other education still includes preschool within a
population-based residual. These limitations are not resolved by having more rows.

The new cost specifications operate after 2030. The inherited 2025-to-2030 bridge
still uses the earlier category cost assumptions to construct allocation weights,
then reconciles to the unchanged eight category anchors. The separate VET age
base changes within-education allocation weights slightly during that bridge;
we do not claim every former subfunction anchor is identical. This is separate
from the future input-cost mechanism and is visible in the calibration export.

#### Assumptions deliberately left unestimated

The parameter file sets annual coverage growth to zero and service-productivity
growth to zero in all nine blocks. Health intensity remains 0.5% a year and
education intensity remains zero. These are maintained scenario judgments, not
new empirical findings. Baseline coverage and use are already embodied in the
anchors and age profiles. We do not add another baseline take-up level on top.

The global 0.3% relative service-price premium remains an additional judgment.
It applies to hospital, medical, residual health, school, university and residual
education baskets. It is excluded from the CPI-only medicine proxy and the VET
funding proxy, to avoid silently adding a second price rule to them. The health
intensity assumption still applies to medicines: **CPI-only unit cost does not
mean CPI-only total spending**. Demography, coverage, intensity and the category
adjustment also affect the total. No generic substitution, new-listing or
confidential rebate model has been fitted.

Primary/community and referred medical care retain the previous 70% wage-exposure
judgment, interpreted now as an input-basket weight. We have not relabelled it as
observed MBS policy. The PBO identifies WCI-5 as the MBS index and notes that parts
of WCI construction are unpublished. Further separation of government rebates,
community services, incentives and actual provider costs is necessary before
claiming a faithful funding-rule model.
[PBO, Indexation and the budget: an introduction](https://www.pbo.gov.au/about-budgets/budget-insights/budget-explainers/indexation-budget-introduction).

The apparent precision of a calculated salary share reflects source arithmetic,
not certainty about its applicability for the next 36 years. Differences in
efficiency, coverage or intensity are only introduced when a source or a stated
scenario assumption supports them; no random subfunction growth rates are drawn.

#### Aggregate controls preserve the subfunction mechanisms

The sidebar retains its Cost pass-through section, but the first two controls
are now **Health: annual cost adjustment** and **Education: annual cost
adjustment**. Both default to 0%, range from -2% to +2%, and use:

```
aggregate_cost_multiplier[f,t] = (1 + annual_adjustment[f])^(t-2030)
```

For example, +1% gives a 2066 multiplier of `1.01^36 = 1.43077`. Every unit-cost
path in that function is multiplied by the same number, including medicines or
VET. Thus relative subfunction costs at each date are preserved; neither the
input weights nor demographic paths are overwritten. This is an annual growth
adjustment, **not** a one-off 1% spending-level increase and not a percentage
scaling of the productivity coefficient. It works even when productivity is
zero and preserves the common 2030 anchor. A -1% adjustment gives `0.99^36`.

The accounting decomposition adds **Aggregate cost scaling** as a separate
tenth factor. The exact Shapley calculation allocates its interactions with the
other factors and still sums to the spending change. Do not interpret its
contribution as a causal estimate. The other four category pass-through controls
retain their earlier definitions. References, resets and exports use the new
zero-adjustment defaults; the old health/education slider values are not reused.
The R engine rejects the obsolete `health_pass_through` and
`education_pass_through` fields rather than silently ignoring them. Saved
assumptions should use `health_cost_growth_adjustment` and
`education_cost_growth_adjustment`, expressed as annual fractions (0.01 means 1%).

#### Differences from the IGR and remaining evidence work

This is a consolidated service-cost scenario framework. The IGR's main health
programmes instead use programme-specific non-demographic growth estimated from
historical age/sex-adjusted spending. Its Commonwealth education projections
apply programme funding rules rather than our provider-cost baskets. A
Commonwealth school-grant index is therefore not automatically the right rule
for consolidated school delivery costs. The comparison panels retain those
coverage and method differences rather than forcing the paths to coincide.
[2026 IGR, Appendix A3, health and education methods](https://treasury.gov.au/publication/2026-intergenerational-report).

The next data work is to estimate age-specific activity and participation, then
separate changes in activity from expenditure per comparable service. Medicare
and PBS data are useful for this, but they have **not** been fitted in this
update. Their existence does not make the present 70% medical weight or CPI-only
medicine assumption an estimate. In particular, benefits per MBS service and
expenditure per prescription change with service/medicine mix as well as prices.
[Medicare demographic reports](https://medicarestatistics.humanservices.gov.au/VEA0032/SAS.Web/statistics/mbs_group.html),
[PBS expenditure and prescriptions](https://m3.pbs.gov.au/statistics/expenditure-prescriptions/2024-2025/Expenditure-prescriptions-report-tables-2024-25.PDF),
[higher-education student data](https://www.education.gov.au/higher-education-statistics/student-data/selected-higher-education-statistics-2024-student-data),
[NCVER government-funded activity](https://www.ncver.edu.au/research-and-statistics/publications/all-publications/government-funded-students-and-courses-2025).

#### Reproduction, review and validation

`data/health_education_parameters.csv` is the active nine-row specification. It
records the rule, wage weight, coverage/intensity/service-productivity growth,
relative-price loading, evidence class, source link, locator and limitation.
Editing these explicit assumptions requires restarting the local app. The
Spending tab displays them and downloads the complete evidence table, along
with the selected aggregate adjustment and resulting unit-cost indices.

The source cache is `data/raw/health_education`; its manifest records URLs,
download date, byte counts and MD5 hashes. Hospital weights are extracted from
spreadsheet cells. School and university published table values are explicitly
transcribed in the preparation script, with the source PDFs retained for audit.
The VET magnitude is extracted from Table 5A.1. Preparation performs structural
and numeric checks; it does not silently search for a convenient coefficient.

From this folder, the relevant commands are:

Network access is needed only for acquisition; existing cached files are retained.
The scripts use their own command-line paths, so run the following from a shell
with this folder as the working directory:

```powershell
Rscript --vanilla scripts/acquire_health_education_sources.R
Rscript --vanilla scripts/prepare_health_education_inputs.R
Rscript --vanilla tests/validate_health_education.R
Rscript --vanilla tests/validate_bottom_up.R
Rscript --vanilla tests/validate_cost_and_ngdp.R
Rscript --vanilla scripts/build_assumption_register.R
Rscript --vanilla scripts/build_spending_background.R
Rscript --vanilla scripts/render_readme_pdf.R
```

For a full source rebuild, run `prepare_bottom_up_inputs.R` **before**
`prepare_health_education_inputs.R`: the latter splits its tertiary allocation
and supplies the new cost parameters. Re-running the health/education preparation
is idempotent. There is no network call in the projection engine or Shiny.

Dedicated tests verify the observed-share calculations, separate higher/VET
demand, each cost formula, common scaling of every subfunction, operation at
zero productivity, negative adjustments, parameter bounds, no double counting
of coverage/intensity/efficiency, unchanged macro paths, unchanged other six
primary categories and unchanged 2030 category totals. Exact factor identities
are checked in nominal dollars, real dollars and GDP shares. The revision table
is written to `outputs/health_education_revisions.csv`; annual factor paths and
named checks are exported beside it. These checks establish implementation
consistency, not forecast accuracy or statistical identification.

At default assumptions, the new 2066 Health share is **9.646% GDP** versus
9.798% before this update; Education is **4.025%** versus 4.128%. Net debt is
**112.28% GDP**, versus 118.38% previously. That debt revision includes the
accumulated spending and interest effects. It is a consequence of the changed
cost assumptions and allocation proxy, not evidence that the new path is the
correct forecast. The PBO 2030 aggregate, GDP assumptions and tax rules are
unchanged; interest receipts can still respond through the financing engine.

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

### Checking aggregate spending against the IGR (27 October 2026)

The Spending tab now has comparison tables and method/coverage notes for all eight
high-level functions. Tables show scenario dollars, scenario and fixed-reference
GDP shares, and the published 2026 IGR shares. Summaries compare changes over a
common published period. The IGR is predominantly Commonwealth spending; health
and education are not like-for-like with our consolidated totals. Age and Service
Pensions is broader than our Age Pension, and other income support is only a
partial comparator for residual social protection. NDIS retains the total-Scheme
series. Defence uses budget-accounting Table A2.3, not the broader NATO chart.
Published defence observations are not interpolated. Other public services has no
matching IGR aggregate and its IGR cells remain unavailable. Each projection keeps
its own GDP denominator. The comparison does not recalibrate spending or validate
the model's assumptions.

`scripts/prepare_igr_spending_benchmarks.R` extracts the annual source series from
the cached Chapter 12 workbook and records the Table A2.3 defence transcription.
It writes `data/processed/igr_spending_benchmarks.csv`, with chart/table locators
and source links. The app download includes all comparisons and coverage notes.
`tests/validate_igr_benchmarks.R` checks the published endpoints, NDIS compatibility,
missing comparators and scenario/reference separation.

### Inspecting spending factors (27 October 2026)

The Spending tab shows annual paths and a subfunction factor table. The table
follows the contribution section's selected spending coverage, start/end years
and measure. Each row reports endpoint spending and ten end/start factor ratios:
population size, age/cohort demand, coverage/eligibility, general prices, real unit
cost/payment, relative service prices, intensity, policy/financing and the GDP
denominator, plus aggregate cost scaling. It uses the same factor levels as the exact contribution calculation.
Multiplying unrounded ratios by starting spending reconstructs ending spending.
The download includes both factor levels, ratios and contributions. A zero starting
factor has an undefined ratio, displayed as a dash; endpoint levels remain available.
General prices cancel in 2030 dollars; the inverse nominal-GDP factor applies only
to GDP shares. The older aggregate three-driver audit remains available in R for
compatibility, but is no longer the displayed mechanism table.

| Programme | 2029-30 (% GDP) | 2065-66 (% GDP) |
|---|---:|---:|
| Health | 7.35 | 9.65 |
| Aged care | 1.58 | 3.00 |
| NDIS | 1.61 | 2.01 |
| Education | 4.76 | 4.02 |
| Age Pension | 2.24 | 1.57 |
| Other social protection | 5.43 | 5.00 |
| Other public services | 8.87 | 8.70 |
| Defence | 1.59 | 2.50 |

```
relative spending/GDP = number index * real unit-cost index * intensity index
                       / (NGDP index / domestic price index)
```

Health has separate demographic and input-cost paths; its resulting GDP share
rises about 31% from 2030. The old aggregate three-factor identity remains a
reconciled compatibility diagnostic. The displayed ten-factor subfunction table
and detailed background report are the authoritative mechanism decomposition.

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

Health now uses separate hospital, medical and pharmaceutical age-cost ratios
from IGR C12.10, with residual health following population. These Commonwealth
profiles are proxies for the consolidated subfunctions. Aged care uses the age-cost
profile in IGR C6.6, whose underlying fiscal incidence data are 2023-24. Age-cost
profiles are held fixed; healthy ageing and changing morbidity are not estimated.

Education now sums schools (ages 5-17), higher education (ages 18-24), VET (ages 15-64) and other education
(total population), using consolidated ABS 2025 allocations. These are **proxies**,
not observed future enrolment. The earlier transformed IGR education profile
remains only in the inherited pre-2030 category calibration bridge.
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

Health and Education now use the differentiated input/funding specifications
documented above, with neutral aggregate cost adjustments. Wage pass-through
remains a net service-cost assumption for Aged Care .80, NDIS .85, residual
Social Protection .65 and other services .70. Default
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
| 2% | 2.69 | 130.6 |
| 3% | 2.13 | 121.6 |
| 4% (default) | 1.57 | 112.3 |
| 6.5% | 0.57 | 94.6 |
| 7% | 0.44 | 91.7 |

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
spending = 2030 allocation * age-weighted exposure index * coverage index
           * domestic price index * real unit-cost index
           * (1 + relative service-price growth)^years
           * (1 + programme intensity growth)^years
           * aggregate cost scaling factor
```

| Programme | Number/exposure evidence | Productivity pass-through | Additional annual intensity |
| --- | --- | ---: | ---: |
| Health | Separate C12.10 shapes; residual population | Separate input baskets; medicine CPI proxy | 0.5% |
| Education | Schools 5-17, higher education 18-24, VET 15-64, other population | Separate input baskets and VET funding proxy | 0% |
| Aged care | Absolute IGR C6.6 aged-care age profile | 80% | 0.3% |
| NDIS | NDIA Q4 2024-25 D.13 incidence; older cohorts retained | 85% | 0.5%, adjustable |
| Other social protection | Population aged 15-66 | 65% | 0% |
| Other public services | Total population | 70% | 0% |

**The remaining aggregate pass-through and all intensity coefficients are prototype
judgments.** The health/education input shares and funding-rule proxy have separate
sources and limitations described above; they are not causal elasticities. Zero intensity
is also an assumption. The common additional relative-price default, 0.3% per
year, is another judgment. Their combined effect can overlap wage, technology
and service-quality changes; they have not been jointly identified from data.
Pass-through means how much economy-wide productivity growth raises real unit
costs; it is not an independently measured government-sector productivity rate.

The [IGR chart workbooks](https://treasury.gov.au/publication/2026-intergenerational-report)
supply the age patterns, not our spending trajectories. The education proxy is
still a proxy: fixed age bands do not capture future enrolment, mature-age or
international student demand. National health/care application is also an extrapolation from
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

The separate **Cost pass-through** sidebar section exposes common Health and
Education annual cost adjustments (both 0%, range -2% to +2%), plus Aged care
(80%), NDIS (85%), Other social protection (65%) and Other public services (70%)
pass-through controls (range 0% to 150%). The first two multiply every unit-cost
path without replacing differences; the latter four change the productivity
exponent. Every 2030 category allocation and the fixed reference remain unchanged.
The separate reset restores only these six values; the main reset restores all
assumptions. Constant relative standards overrides social protection to 100%.
Age Pension and Defence retain their separate entitlement and target rules.
Exports include the six rates, and Shapley attribution has a Cost pass-through
group. The subfunction audit separately reports input weights and the common scaling factor.

The Spending tab now switches between GDP shares and nominal dollars and shows
a counterfactual changing only NGDP/person growth from 3.2% to the selected rate,
with all other selected settings held fixed. There is no missing NGDP-to-spending
link for fully indexed services:

```
domestic prices = nominal GDP index / (real output index * ToT income factor)
service spending = anchor * exposure * coverage * domestic prices * real unit cost
                   * relative-price factor * intensity * aggregate cost scaling
```

At fixed real assumptions and ToT, higher NGDP/person raises domestic prices,
nominal GDP and indexed service spending proportionately. Dividing spending by
GDP cancels the nominal change. ToT is different: at fixed GDP it changes the
real-income factor, hence domestic prices and spending/GDP. Raising NGDP does
not reverse that wedge merely by changing the overall nominal scale.

For example, in 2066 at differentiated default costs: Health is $1,615.4bn (9.646% GDP)
with unchanged ToT and NGDP/person growth 3.2%. With ToT 20% higher, it becomes
$1,557.6bn (9.300% GDP), while GDP remains $16,747.7bn. Raising NGDP/person growth
to 4% then raises Health to $2,056.8bn and GDP to $22,114.9bn; Health remains
9.300% of GDP. Thus the dollar link works even though the share is unchanged.
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

At the differentiated wage-indexed defaults, 2066 net debt is 112.28% GDP and interest
paid is 5.19% GDP. With productivity at 1.7% and all other inputs unchanged, net
debt is 88.97% and interest paid 4.16% GDP. Nominal GDP is **$16.748tn in both
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
