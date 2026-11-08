# Public-data charts for five New Zealand op-ed topics

**Ageing and fiscal spending:** [28 additional charts](figures/ageing/index.html) cover demographic structure, historical pension/health growth and conditional spending projections. See the [ageing research guide](AGEING_RESEARCH_GUIDE.md); reproduce with `13_run_ageing_pack.R`.

**Election extension (2 November 2026):** [63 additional charts](figures/election_2026/index.html) cover the six election questions plus compulsory retirement saving and pre-funded annuities. See the [research guide](ELECTION_RESEARCH_GUIDE.md) for reading paths, factual notes for articles without plots, assumptions and reproduction commands. Run `10_run_election_pack.R` for that separate extension.

This is an evidence pack, not an op-ed or a policy argument. It adds a separate pipeline alongside the existing fiscal-risk scripts and leaves their outputs intact. The expanded pack contains **70 charts**, each in PNG and SVG, with the exact plotted data in a CSV. Sources and interpretation notes accompany every chart. The additional **19 long-history benefit charts** are described in [BENEFIT_HISTORY.md](BENEFIT_HISTORY.md).

**Start with [the searchable chart gallery](figures/op_ed_topics/index.html).** Filter by topic, measure type, or text. The gallery opens locally without a web server. Clicking a chart opens its PNG; links below it provide the SVG, plotting data and original public sources.

## Run

From the `opeds` directory, in PowerShell:

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "Data pulls\05_run_topic_pack.R"
```

To reproduce the saved snapshots without downloading again:

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "Data pulls\05_run_topic_pack.R" --cached
```

To regenerate only the charts:

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "Data pulls\05_run_topic_pack.R" --plots-only
```

The component scripts can also run separately:

- `03_pull_topic_data.R`: downloads, parses, checks and exports public data and labelled scenarios.
- `04_make_topic_plots.R`: renders the charts, plotting-data files, catalogue and gallery.
- `06_pull_benefit_history.R`: pulls the longer official recipient, spending and archival benefit histories.
- `07_benefit_history_plots.R`: adds B11–B29 to the gallery; loaded by the plot script.
- `helpers/topic_pdf_extract.py`: extracts the Treasury PDF tables using their column positions and preserves their source wording and page numbers.

The R scripts require `dplyr`, `ggplot2`, `httr2`, `jsonlite`, `purrr`, `readr`, `readxl`, `rvest`, `scales`, `svglite` and `tidyr`. PDF extraction needs Python with **PyMuPDF 1.23 or newer**, imported as `fitz`. All were available in the environment used to run the pack; no packages were installed and no user libraries were changed. The scripts stop if dependencies are missing. To select a Python executable:

```powershell
$env:NZ_OPED_PYTHON = "C:\Users\OEM\anaconda3\python.exe"
```

`NZ_DATA_REFRESH=false` also enables cached pulls. The pipeline uses an explicit 14 October 2026 snapshot cutoff and pinned statistical release URLs. It does not automatically discover new statistical releases. Changing the cutoff alone does not update those URLs. Live policy pages and API series can subsequently be revised, so use `--cached` to reproduce this pack.

## Chart coverage

| Topic | Charts | Measures |
|---|---:|---|
| Digital services taxes | D01–D07: 7 | ICT exports and export share, internet use, revenue-base sensitivity, profit margins, assumed incidence, scale of the historic official DST estimate |
| Tax streamlining and business support | T01–T14: 14 | Estimated versus unestimated provisions, provision types and larger cost estimates, measured credits, RDTI expense, R&D by sector/organisation size/industry, international R&D intensity, domestic/international screen support, game allocations, deduction timing and present value |
| Taxation of inflation | I01–I08: 8 | Observed CPI inflation, real bracket erosion, tax on constant real earnings, 2010 and 2021 indexation counterfactuals, nominal-interest taxation, real saving returns, inflation/tax-rate sensitivity |
| Public servants and service value | P01–P12: 12 | Departmental FTEs and FTEs per resident, wider workforce composition and employment shares, regional shares, contractors and consultants, agency staffing, OIA workload/timeliness/response time, separately sourced 2025 workforce totals |
| Benefit spending | B01–B29: 29 | Original ten charts plus quarterly histories from 2008, annual actual spending from 2001, selected archival observations from 1940, pension/population history, growth and caseload decompositions; see the history notes |

The gallery and `chart_catalogue.csv` identify which figures are direct statistics, derived measures, context proxies, estimates, forecasts or scenarios. Descriptive charts do not establish causal policy effects. In particular, the scenarios do not establish the revenue from a currently proposed DST, the fiscal cost of comprehensive inflation indexation, or the savings from any particular suggested benefit cut.

## What the sources actually cover

**Digital services.** The [IRD bill record](https://www.taxpolicy.ird.govt.nz/bills/53-dst-23) records withdrawal on 21 May 2025. The [2023 regulatory impact statement](https://www.regulation.govt.nz/assets/RIS-Documents/ria-ird-dst-aug23.pdf) supplies historical design and revenue context. The 3% tax, assumed eligible revenues and pass-through scenarios are labelled hypothetical. World Bank ICT service exports are outward transactions, not the eligible inbound platform revenue. Internet penetration is a reach measure, not a tax base. Remote-services GST is a different tax; this pack does not substitute GST receipts for DST receipts.

**Business support.** The [2026 tax expenditure statement](https://www.treasury.govt.nz/sites/default/files/2026-05/b26-tax-expenditure-statement.pdf) is parsed into a 52-provision register with estimates, review dates, confidence wording and PDF pages. Unavailable estimates remain missing. Table 3 separates regularly measured tax expenditures from appropriated spending through the tax system. Its income-tax-year amounts are not combined with June fiscal-year amounts. Costs reviewed in different years are not summed as a common-year total. There is also no sum of estimated tax expenditures represented as potential repeal savings: estimates hold behaviour fixed and can overlap.

The [Stats NZ R&D survey](https://www.stats.govt.nz/information-releases/research-and-development-survey-2025/) supplies expenditure, industry, organisation-size and sector data with suppression, revision indicators, footnotes and relative sampling errors retained. Business R&D is available in 2025; all-sector and organisation-size detail is latest available in 2024. Organisation-size spending includes business, government and higher education and uses rolling mean employment. Chart lines connect available observations without imputing unobserved annual values.

Domestic screen expenses come from the [BEFU 2026 expense tables](https://www.treasury.govt.nz/sites/default/files/2026-05/befu26-data-expense-tables.xlsx). The international screen figure is the **2026/27 authorised appropriation** in [Vote Business, Science and Innovation](https://www.treasury.govt.nz/sites/default/files/2026-05/est26-v1-buscin.pdf), not actual payments. The two concepts are labelled. NZ On Air game figures are announced allocation rounds, not fiscal-year expense. Screen and game rebates are spending programmes and are not all tax credits.

**Inflation.** Annual-average New Zealand CPI comes from the World Bank indicator API, with the official source metadata retained in the raw response. It is used for purchasing-power comparisons. Personal tax scales come from IRD. The October 2010 scale is used as an indexation benchmark; historical June snapshots begin in **2011**, after the scale took effect. The 39% top rate first appears in the 2021 June snapshot and the July 2024 threshold increases first appear in the 2025 June snapshot. Charts I03–I04 include both inflation and policy changes. I05 indexes the **2021** thresholds while retaining the 2021 rates, so it avoids attributing the introduction of 39% to inflation.

Tax illustrations exclude ACC, credits, transfers, behavioural changes and tax-year blending. Real saving returns use `(1 + nominal after-tax return) / (1 + inflation) - 1`. The nominal interest rate is an **assumed 5%**, not a measured deposit series: RBNZ automated spreadsheet downloads returned HTTP 403. The indexation example taxes positive nominal interest net of inflation compensation and does not refund negative real returns. It is a counterfactual, not an interpretation of current tax law.

**Employment.** [PSC workforce data](https://www.publicservice.govt.nz/data/workforce-data/public-sector-composition/workforce-size) provides departmental FTEs to March 2026. The published aggregate is used; the aggregate row is not added to departmental rows. The wider sector CSV linked from the 2025 page actually ends in **2023**. Its historical charts retain that end date. The page's **2025** broad headline totals are extracted separately for P12 and do not fill the missing detailed sector years. Wider workforce data are Business Demography employee headcounts, not HLFS employment. They include health, education, SOEs and local government; departmental FTE is a narrower population. Annual population and June employment snapshots do not align perfectly.

Contractor OPEX and CAPEX are separate, and partial fiscal-year 2026 contractor observations are omitted from full-year comparisons. [OIA statistics](https://www.publicservice.govt.nz/data/oia-statistics) provide a genuine service-output dimension, but neither output quality, case mix nor the cost of handling OIAs is observed. The main reporting table covers 96 agencies in this release; Police and NZDF are in separate sheets and are not included in these charts. An OIA scatter is not a Public Service productivity estimate.

**Benefits.** Expense charts use 2024/25 actuals from Treasury rather than the older 2025 forecast endpoints in the existing fiscal-risk pack. Components reconcile to the published total. [MSD June 2026 fact-sheet tables](https://www.msd.govt.nz/about-msd-and-our-work/publications-resources/statistics/benefit/index.html) supply quarterly working-age counts. Their random rounding permits small differences between subgroups and totals. Some recipients are in work; benefit receipt and HLFS unemployment are different concepts. Jobseeker work-ready and health-condition/disability groups are kept separate, while Treasury combines Jobseeker and Emergency Benefit expenses.

The cut scenarios use the 2025 actual gross expense. The work-ready allocation is a proxy from June 2025 recipient shares, with an assumed average-payment ratio of 0.75–1.25 relative to health-related recipients. It uses a stock to approximate annual composition and cannot separately allocate Emergency Benefit. Lost income tax, other assistance, implementation costs and behavioural effects are not deducted, so the figures are **gross fiscal arithmetic**, not policy-specific net savings. The additional B11–B29 histories retain their own vintage, coverage and definition notes in [BENEFIT_HISTORY.md](BENEFIT_HISTORY.md); archival and modern spending are not treated as a single constant-coverage series.

## Reproducibility and checking

- `raw/op_ed_topics/` contains downloaded statistical files, policy-page snapshots and raw API responses.
- `processed/op_ed_topics/source_manifest.csv` records URLs, file timestamps, hashes and whether each file was reused.
- `processed/op_ed_topics/validation.csv` records the reconciliation/anchor checks. The code also checks table schemas, source labels, published values, tax-function anchors, dates, rounding tolerances and lifetime deduction totals.
- `processed/op_ed_topics/measurement_notes.csv` and [the measurement guide](MEASUREMENT_GUIDE.md) explain what can be measured now and what is missing for stronger claims.
- `figures/op_ed_topics/` contains images, each chart's data, the catalogue and the gallery.

The fresh-download pull and cached pull were both run successfully. The chart script was executed and representative images inspected. The existing fiscal-risk scripts and figures were not rerun or modified.
