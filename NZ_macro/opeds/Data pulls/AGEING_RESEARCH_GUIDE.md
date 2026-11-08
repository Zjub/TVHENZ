# Ageing, spending growth and New Zealand's fiscal outlook

Snapshot: **2 November 2026**. This extension adds **28 charts**, with PNG/SVG images, plotted CSVs, source links and interpretation notes. It investigates the role of ageing without treating every increase in health or pension spending as a demographic effect.

Start with the [searchable ageing gallery](figures/ageing/index.html). The [numeric research notes](processed/ageing/key_findings.csv) retain values, units, periods and observation/projection status for articles without charts. The [election and retirement pack](figures/election_2026/index.html) and [earlier evidence pack](figures/op_ed_topics/index.html) remain separate and available.

## Start with these eight charts

**A04 → A07 → A08 → A14 → A12 → A18 → A17 → A26** gives a compact route through population ageing, age-specific health costs, historical spending, pension accounting, future spending and pre-funding.

| Question | Charts | What the measurement can establish |
|---|---|---|
| How much is the population ageing? | A01–A05 | Published population structure, uncertainty, age ratios, and growth of the oldest age groups |
| Do older people all leave the workforce? | A06 | Historical labour-force participation by older age bracket |
| How does ageing change healthcare demand? | A07–A11 | Published age-cost profiles and a conditional fixed-profile demand benchmark |
| What explains historical pension and health growth? | A12–A14 | Exact pension accounting factors, expense categories and comparison with a demographic benchmark |
| What is changing in the latest near-term pension forecast? | A15 | PREFU recipient numbers versus expense per published recipient |
| Which long-term spending categories grow? | A16–A17 | Reconciling core Crown expenses and category contributions to the projected increase |
| How does ageing interact with economic capacity? | A18–A19 | An exact pension/GDP identity and a mechanical fixed-age-ratio comparison |
| What about age-related tax payments? | A20–A21 | Selected historical tax/transfer profiles and a fixed-profile age-mix benchmark |
| Which assumptions change the result? | A22–A25, A28 | Treasury cost, demographic, productivity and interest-rate scenarios |
| How does existing pre-funding help? | A26 | Net-of-tax NZS financing with NZ Super Fund contributions/withdrawals |
| What happened to earlier projections? | A27 | Published projection vintages alongside the actual series available in 2025 |

## The main distinctions for an article or blog

**Ageing has several channels.** More people aged 65+ raise pension eligibility under existing rules. More people at advanced ages change healthcare and care needs. A changing age mix also affects employment, earnings and tax capacity. These channels interact with total population growth, payment indexation, service standards, input prices, productivity and policy.

**An age-related spending category is not a measure of ageing's contribution.** Health expenses can grow because there are more older people, because each person receives more or different care, because staff costs rise, or because of shocks and policy choices. NZ Super expense combines recipient numbers with payment rates and recipient composition. A16–A17 are explicitly category decompositions, rather than claims that those category increases were all caused by ageing.

**A projection is conditional.** The 2025 Long-term Fiscal Model (LTFM) carries forward Budget 2025 forecasts through June 2029 and projects from 2029/30. It is not an extension of September 2026 PREFU. The near-term PREFU pension calculation is kept separate in A15. Treasury describes its long-run no-adjustment scenario as a warning about policy pressures, not a prediction of what governments will choose. See [LTFS 2025](https://www.treasury.govt.nz/publications/ltfp/he-tirohanga-mokopuna-2025) and the [2026 model methods paper](https://www.treasury.govt.nz/publications/an/an-26-01).

## Population structure: useful measures and their limits

A01–A03 use [Stats NZ's 2024-base national population projections](https://www.stats.govt.nz/information-releases/national-population-projections-2024base2078/), published June 2025. They extend to 2078. The median projected 65+ share rises from about **17% in the 2024 base** to **29% in 2078**; the 85+ share rises from about **2% to 7%** in the rounded summary table. The uncertainty bands in A02 use each measure's published 5th and 95th percentiles. They are not confidence intervals on realised outcomes or jointly additive low/high cases.

A04–A05 instead use the age-sex population stored in Treasury's LTFM, covering 2006–2065. Treasury calibrates these data to HLFS population concepts and its forecast totals. The raw Stats NZ resident-population projections and Treasury-calibrated series are **not spliced into one history**. The Treasury series is historical/calibrated through 2024, forecast for 2025–2029 and projected from 2030. Its 2025 demographic values should not be described as newly observed 2025 outcomes.

A03 shows why the old-age ratio alone is incomplete: a falling child share partly offsets a rising older share in the total age-dependency ratio. But neither ratio identifies the number of earners or taxpayers. People over 65 work and pay taxes; people aged 15–64 include students, caregivers and people unable to work. A06 supplies historical older-age participation, without assuming everyone can or should extend working life. This series ends with the 2025 statement's last observation, not the latest 2026 labour release.

## Health: measuring ageing without assuming it explains all spending

A07 uses the published LTFS health expenditure age profile, split into aged care, hospital/community services, mental health, other services and primary care. It shows why looking only at the number over 65 misses the growing importance of older groups within retirement ages.

The underlying model cost weights were updated in 2019. The profile is used as a **fixed weighting schedule**, not represented as a new 2026 cost survey. Applying it to the Treasury age distribution gives:

`fixed-profile cost per resident = sum(age population share × source cost per person at that age)`

A08 holds every age-specific cost and service mix constant. The changing age mix raises the benchmark per-person cost by about **9.7% from 2006 to the model's 2025 demographic input**, and by about **30.4% from 2025 to 2065**. The first interval includes a forecast demographic input at its endpoint; the second is a counterfactual projection, not observed spending growth.

A14 compares that benchmark with actual historical core Crown health expenditure, deflated by annual-average CPI and divided by resident population. It provides a test of the idea that ageing alone explains historical expenditure growth. **Do not label the difference “waste” or a measured non-ageing cost effect.** The two methods differ, and the gap includes input prices above CPI, changed services, morbidity, policy, pandemic expenditure and benchmark error. CPI-deflated spending measures purchasing power, not the volume or quality of clinical services.

A09 makes a similar comparison prospectively, from a common June 2029 base:

1. population growth with unchanged composition and per-person costs;
2. population growth plus changing composition under the fixed age-cost profile;
3. the full LTFM's CPI-deflated health expenditure projection.

The full model includes additional wage/productivity, residual spending-growth and longevity assumptions. Its demographic method also differs from the simple benchmark. The distance between the lines is therefore **not** an official decomposition of the causal effect of ageing. A10–A11 show which services and age groups drive the benchmark itself.

**What could change the result:** healthier ageing, changing disability and disease prevalence, care near death, changes in clinical technology, household care, workforce costs and service quality. A cross-sectional cost profile does not tell us how those relationships will evolve as cohorts age. Ages 95+ are pooled in the Treasury population data; the published cost profile pools ages 90+.

## NZ Super: exact accounting, carefully interpreted

A12 expresses real pension expense as a product of four positive factors:

`real expense = total population × share aged 65+ × recipients per person aged 65+ × real expense per recipient`

It uses actual Treasury NZS expenses and MSD fiscal-year means of quarterly recipient counts from the earlier benefit-history pack. The age distribution is Treasury's calibrated demographic benchmark. A log-mean Divisia (LMDI) allocation makes the four contributions add exactly to the change in real expense between 2009 and each later year.

In this allocation the rising 65+ share contributes about **$4.7 billion of the $11.7 billion increase in annual real expenditure between 2009 and 2025**, in 2025 purchasing power. This is a descriptive accounting allocation under the chosen demographic benchmark, not a causal estimate or a finding that the rest is discretionary generosity. Total population growth also increases the number of older people.

The recipient/population factor is deliberately labelled a **residual**. It absorbs eligibility, recipient mix, population-definition differences and timing. It must not be read as a take-up rate: MSD recipients and the HLFS-calibrated population do not have identical coverage, and the Treasury age data for 2025 are the old Budget forecast. The average-expense factor includes payment rates, composition and timing, rather than a single statutory rate.

A15 applies the same exact accounting method to the newer PREFU expense and published recipient-count series. It separates numbers from average expense over 2025/26–2030/31. It uses nominal amounts and an unaudited 2025/26 base; it does not silently insert the PREFU forecasts into the older long-run model.

A18 uses a different identity suited to spending relative to economic capacity:

`NZS / GDP = (population 65+ / population 15–64) × (real NZS per person 65+) / (real GDP per person 15–64)`

The model's age ratio raises the projected pension share, while growth of CPI-deflated GDP per person aged 15–64 offsets part of that pressure. The latter is **not** a measure of labour productivity: its denominator is an age group, not hours or employment. The real pension factor also incorporates coverage/mix; it is not simply the couple payment rate. Positive contributions can exceed the net increase when another contribution is negative.

A19 holds only the age ratio fixed mechanically. It preserves the modelled per-older-person expense and per-15–64-person GDP paths. It is not a rerun of Treasury's model and does not estimate a new total-government debt path. Changing population structure would also change GDP, revenue and other expenses in a complete model.

## Long-term spending and sensitivity

The LTFM's gross NZS share rises from about **5.3% of GDP in its 2025 forecast base to 7.8% in 2065**. Core Crown health rises from about **7.1% to 9.9%**. Those are conditional 2025-vintage paths. A16 separates other welfare, education, other primary expenses and finance costs and reconciles them to total core Crown expense. Finance costs reflect the accumulated consequences of spending, revenue, investment and interest assumptions, not direct services to older people.

A22–A25 use Treasury's separate **overlapping-generations (OLG) model**. Its coverage, tax responses and starting values differ from LTFM. Compare scenarios **within** a figure, not by subtracting one model's baseline from the other's.

- A22 varies health cost growth, illustrating pressures beyond population structure.
- A23–A24 compare the baseline with higher fertility and a higher-skilled migrant intake. The latter keeps the baseline number of migrants and changes skill composition; it is not a higher migration-volume case. Higher fertility also raises childhood-service demand before it increases the adult population.
- A25 shows why greater productivity can improve living standards without proportionately reducing spending/GDP when pensions and health inputs rise with wages.
- A28 uses LTFM interest-rate scenarios to show debt-service amplification. These are conditional no-adjustment paths, not debt predictions or evidence that ageing alone creates the projected debt stock.

A27 provides a useful check on certainty: older pension projections did not all match subsequent spending shares. Population, migration, economic growth, policy and statistical revisions matter. The actual series in this comparison is the series supplied with the 2025 publication, not an updated 2026 forecast-accuracy exercise.

## Connecting this to pre-funding an annuity

A26 distinguishes the **pension promise** from its **financing**. It plots the LTFS's net-of-tax NZS cost alongside the financing series including NZ Super Fund contributions and withdrawals. The fund can help meet the bill, but withdrawals do not extinguish pension entitlements. Contributions acquire financial assets; adding them to pension expense would confuse spending with financing.

This complements the earlier S-series retirement charts. A proper comparison of individual accounts with a collectively funded lifelong pension needs matched income indexation, tax treatment, risk guarantees, redistribution and transition costs. In particular, a CPI-level private annuity is not equivalent to NZ Super with its statutory wage/CPI protections.

## Reproduce and inspect

From `opeds`, in PowerShell:

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "Data pulls\13_run_ageing_pack.R"
```

Use saved sources or regenerate only charts:

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "Data pulls\13_run_ageing_pack.R" --cached
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "Data pulls\13_run_ageing_pack.R" --plots-only
```

The earlier topic and election packs must have been run once. This extension reads their documented benefit/CPI/PREFU outputs but does not overwrite them. Source releases are pinned; it does not automatically discover later long-term statements or population projection releases. No packages are installed.

- `11_pull_ageing_data.R`: downloads sources, preserves source rows, constructs benchmarks and accounting allocations, and validates results.
- `12_make_ageing_plots.R`: exports images, plotted data, catalogue and local searchable gallery.
- `13_run_ageing_pack.R`: runs the extension.
- `raw/ageing/`: downloaded original workbooks and the model methods paper.
- `processed/ageing/source_manifest.csv`: URLs, hashes, timestamps and reuse status.
- `processed/ageing/validation.csv`: source anchors, full-year coverage, population sums, expense reconciliation and exact decomposition checks.

The fiscal history parser reads only the first dollar block. It removes year footnote markers and excludes the repeated percentage-of-GDP block; otherwise histories can silently lose earlier years or treat ratios as extra dollars. Annual CPI approximately deflates June fiscal-year expense. Historical benefit vintages, recipient-count alignment and their coverage breaks remain documented in [BENEFIT_HISTORY.md](BENEFIT_HISTORY.md).

The remaining evidence gaps are causal historical attribution of health growth; a fully reconciled age-specific public-service expenditure dataset across years; cohort-specific health and longevity changes; actual tax payments and working hours at older ages; and a freshly recalibrated long-run fiscal model incorporating all 2026 decisions. The plots make those gaps visible rather than filling them with assumed statistics.
