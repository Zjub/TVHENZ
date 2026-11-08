# e61 fiscal model — local research prototype

Run from `projections`:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'e61 fiscal model\run_app.R'
```

Open <http://127.0.0.1:7461>. The app binds only to this computer. Set
`E61_FISCAL_PORT` to choose another port. Required packages: shiny, bslib, ggplot2.
The engine itself uses base R only. No downloads occur when the app runs.

## Scope

This is a new, independent scenario engine, not a fitted aggregate regression or
an overlay of existing projection outputs. It implements all four requested
controls: labour productivity, NGDP growth, government service prices and terms
of trade. It also offers two illustrative benefit/service conventions, a simple
tax-indexation sensitivity, pension coverage attrition and an interest-rate setting.

It demonstrates the modelling architecture. It is **not yet a validated realistic
alternative to PBO/IGR**, a complete current-policy simulation, or an official e61
forecast. Programme eligibility, historical validation and an updated official
data vintage are still needed before public release.

## Starting data and calibration

`scripts/prepare_inputs.R` copies four existing project inputs into `data/`, with
a source-path and checksum manifest. Parent models and files are not changed.
The snapshot was prepared on 25 October 2026; this is not the release date of
the underlying sources.

* `official_pbo_nfo_wide.csv`: existing consolidated **2025–26** National Fiscal
  Outlook extract, with forecasts through FY2029–30. This intentionally preserves
  the existing comparison anchor; it is not an updated 2026–27 PBO extract.
* `population_projection_age_shares.csv`: existing annual population and age-group
  projection to 2066. Broad bands and interpolation are inherited from the parent
  preparation workflow; no new demographic forecast is estimated here.
* `historical_gfs_expenses_by_purpose.csv` and
  `historical_gfs_operating_statement.csv`: existing ABS GFS extracts, 2024–25
  composition and conventional interest, with source amounts converted from
  AUD millions to billions.
* `external_anchors.csv`: manually recorded Age Pension outlay of $62.2bn and
  2.67m June recipients from the
  [ANAO audit](https://www.anao.gov.au/work/performance-audit/administration-of-the-age-pension).
  Cash outlays are used as an approximate allocation within GFS Social Protection,
  not added on top of it. The recipient stock is documented but the model projects
  exposure indices, not recipient counts or means-test eligibility.

2025 primary GFS spending is split into Health, Education, Age Pension, Other
Social Protection, Other Public Services and Defence. Conventional interest is
removed exactly once. Programme exposure and cost paths carry those allocations
to 2030 under the default assumptions; one common factor then reconciles their
sum to the PBO's 2030 primary-expense total. These are **e61 allocation estimates**,
not official PBO programme forecasts. This calibration is frozen before scenarios
are evaluated. Scenario changes start in FY2030–31.

The first version retains aged care and NDIS inside residual Social Protection.
They require separate programme data and eligibility/usage mechanisms in the
next development stage. Pension means testing and superannuation maturation are
represented only by an explicit, adjustable coverage-decline proxy. The source
vintages and broad programme mapping must be reconciled before publication.

## Economic engine

Let t be years since 2030. Labour volume is population times age shares weighted
by fixed propensities: ages 15–34: .65; 35–54: .80; 55–64: .60; 65–74: .20;
75+: .03; children: zero. These are **illustrative employment/hour weights**, not
estimated participation projections. Real output is normalised to 2030:

```
productivity index A[t] = (1 + productivity growth)^t
real output Q[t] = (labour volume[t] / labour volume[2030]) * A[t]
terms of trade T[t] = (1 + chosen level change)^min(t/10, 1)
```

With linked NGDP, domestic prices P grow by 2.5% per year:

```
NGDP[t] = NGDP[2030] * Q[t] * P[t] * T[t]^0.20
```

The 0.20 terms-of-trade income elasticity is an exposed judgement. This is a
reduced-form nominal income effect, not a full trade model. A positive terms-of-
trade shock increases export prices/income without increasing real production.

With the NGDP override, NGDP instead grows at the chosen rate and domestic prices
are the residual `P = NGDP / (NGDP[2030] * Q * T^0.20)`. Thus productivity and
NGDP are jointly consistent. Higher productivity at fixed NGDP reduces implied
inflation; it is not the same experiment as higher productivity at fixed inflation.
The implied domestic inflation is reported. CPI, the wage deflator and domestic
prices share this price proxy initially. The GDP deflator includes the ToT income
effect and is distinct from that domestic proxy. Extreme joint settings can imply
deflation: it is shown rather than silently clipped. Productivity can be negative.

## Spending

```
spending = calibrated base * demographic exposure * unit cost * coverage * intensity
service unit cost = P * A^wage_pass_through * (1 + extra service price growth)^t
```

The wage pass-through parameter represents net exposure to economy-wide wage
growth after assumed service-delivery productivity; it is not separately
estimated. Different sector wage/pass-through and delivery-productivity channels
should be separated in the next version. The extra service price slider is an
additional premium above this baseline, not total relative-price growth.
`data/programme_parameters.csv` exposes each age profile, pass-through and excess
intensity assumption. Health has 0.5% annual excess intensity growth; Other Social
Protection 0.3%; the rest zero. These are illustrative assumptions, not estimates.

* Indexed rules: Age Pension payments follow wages; demographic coverage falls
  0.5% annually by default, as a proxy for retirement-income maturation. Other
  social protection uses a .65 wage pass-through. This is not a full simulation
  of legislated indexation, eligibility or means tests.
* Constant relative standards: pension coverage stays fixed, and Other Social
  Protection follows real wages fully. Service quantity/intensity assumptions
  otherwise match the indexed case; non-zero excess intensity still applies.
* Defence converges from its calibrated share to 2.5% GDP by 2035.
* Net capital investment grows with population, domestic prices and half of
  productivity growth. It is separate from expenses; depreciation already within
  GFS expenses is not added again.

## Revenue and balance sheet

2030 aggregate revenue exactly matches PBO. Interest receipts are allocated using
their share of 2025 GFS revenue. Remaining revenue is split into **illustrative**
labour 42%, company 20%, consumption 25%, other 10%, and resource-linked 3% blocks.
These weights are not claimed to be observed tax-head shares.

Labour revenue follows labour volume * domestic prices * productivity. Partial
bracket creep raises its productivity exponent from 1 to 1.1, with no unbounded
nominal-threshold compounding. Company and other revenue follow NGDP; consumption
follows domestic nominal output; resource revenue also responds to ToT with an
illustrative elasticity of 2. There is no tax microsimulation or revenue cap.

Interest uses opening **gross** debt. The effective rate starts at the rate
implied by the 2030 PBO interest bill and 2029 gross debt, converging by 2040 to
4.4% by default. Interest receipts use opening financial assets and an anchor-
calibrated yield. Asset purchases maintain the starting financial asset stock per
person in domestic purchasing power. Their financing is explicit:

```
fiscal balance = revenue - expenses - net capital investment
gross debt[t] = gross debt[t-1] - fiscal balance[t] + financial asset acquisition[t]
net debt = gross debt - financial assets
```

After gross debt is repaid, further surpluses accumulate in interest-bearing
assets. There is no hidden debt floor applied to net debt and no automatic fiscal
repair. No maturity buckets, asset valuation changes, inflation-indexed debt or
cyclical stabilisation are modelled. The common 2030 official anchor may contain
stock-flow adjustments not reconstructible from the rounded published ratios;
the new recursive stock-flow identities apply from 2031.

## Comparisons and outputs

The reference uses productivity 1.2%, linked NGDP, 0.3% extra service prices and
unchanged ToT. It shares the user's selected policy/financing assumptions, so the
economic-control comparison is conditional on those settings. Reset returns all
settings to defaults. An exact four-factor Shapley calculation allocates economic
control interactions in 2066 net debt; it is accounting attribution, not causality.

Download annual reference/scenario results and the assumptions/calibration from
the Method & Data tab. `outputs/` holds the default projection, programme
calibration, validation results and R session information. No probability bands
are shown: structural and policy uncertainty have not yet been quantified.

Run validation from `projections`:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'e61 fiscal model\tests\validate_model.R'
```

Checks cover aggregate/anchor reconciliation, no duplicate pension spending,
stock-flow identities, interest bases, surplus asset accumulation, controlled
productivity/NGDP/price/ToT changes and attribution interactions. They establish
implementation consistency, not predictive accuracy. Historical backtesting and
programme validation remain necessary.

On 25 October 2026 all 33 engine checks passed. An optional real-Chrome smoke
test (`tests/browser_smoke.R`, requires chromote) also verified each of the four
controls independently, all tabs, reset, the live Shiny connection and both
downloads. Browser screenshots are saved as `outputs/app_reference.png` and
`outputs/app_scenario.png`; detailed results are in
`outputs/browser_smoke_results.txt`. The running process ID is recorded in
`outputs/shiny_pid.txt`, with startup messages in `outputs/shiny_stderr.log`.

## Next development priorities

1. Update and reconcile PBO, ABS, population and 2026 IGR vintages; distinguish
   Commonwealth/state/local coverage and eliminate internal transfers.
2. Replace proxy age exposures with participation/hours projections and sourced
   service-use and age-cost profiles.
3. Separate NDIS and aged care; improve pension eligibility, indexation, means
   testing and retirement-income maturation rather than fitting coverage attrition.
4. Estimate tax-head shares and elasticities; add explicit threshold rules.
5. Backcast components and balances and compare historical forecast vintages;
   evaluate structural uncertainty against the existing top-down challenger.
6. Review assumptions and sensitivity ranges before any public deployment.
