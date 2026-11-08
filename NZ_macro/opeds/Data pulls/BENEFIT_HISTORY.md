# Longer benefit histories

The evidence pack now adds **19 charts, B11–B29**, bringing it to **70 charts overall** and **29 benefit charts**. They appear in the same [searchable gallery](figures/op_ed_topics/index.html). Select **Benefit spending** or search for a chart code.

## Available time spans

| Dataset | Coverage | Interpretation |
|---|---|---|
| Main working-age benefits, Jobseeker subgroups, and NZ Super counts | 72 consecutive quarters, September 2008–June 2026 | Newest available downloaded release for each quarter/category; pre-July 2013 working-age categories are MSD backcast estimates |
| Detailed Treasury benefit expenses | Every fiscal year, 2001–2025 | Actual columns only, taken from five published vintages; original rows and source pages retained |
| Older benefit/pension counts | Selected years, 1940–2012 | Original categories and age coverage; five-year intervals in the earlier decades and annual observations from 1990 |
| Older benefit/pension expenses | Selected years, 1940–2002 | Original MSD coverage, including supplementary assistance attributed to benefit groups; annual observations from 1980 |
| Pension/NZ Super counts | Selected years from 1940, annually from 1990 through 2026 | Explicit splice: archival counts through 2008 and modern June NZ Super stocks from 2009; counting/source breaks remain visible |
| Broader social security/welfare/GSF expenses | Annual, 1972–2025 | Treasury functional series; broader than benefits, with cash, reporting-period and accounting breaks |

Annual CPI and population are available from 1960 to 2025 in the downloaded World Bank series. Earlier nominal archival data are retained, but no pre-1960 inflation or population values are invented. Sparse historical observations are shown with points; intervening annual values are not filled.

## Additional plots

- **B11–B16:** long main-benefit counts, Jobseeker work-ready/health-related counts, health-related share, indexed caseloads, receipt per resident, and quarterly seasonality of other main benefits.
- **B17–B22:** 2001–2025 spending composition, shares of GDP and the benefit budget, real expenses, real spending per resident, and contributions to the nominal/real expense increase.
- **B23:** NZ Super real spending decomposed into average recipient stock and real expense per average recipient, 2009–2025.
- **B24–B26:** older main-benefit counts, pension recipients from 1940 to 2026, and pension recipients per resident from 1960.
- **B27–B28:** archival nominal and real expense series, kept separate from modern Treasury expenses.
- **B29:** the broader social security, welfare and Government Superannuation Fund expense function relative to GDP since 1972, separated by accounting regime.

## Comparability choices

### Modern recipient history and the 2013 reform

The [September 2013 MSD workbook](https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources/statistics/benefit/2013/september/quarterly-benefit-fact-sheets-national-benefit-tables1.xlsx) explains that its pre-15 July 2013 data map previous benefits and client characteristics into the new categories. These are **official backcast estimates**, not observations of a programme named Jobseeker Support before that programme existed. The plots mark the reform and retain an `estimate_basis` column.

The quarterly history combines September 2013, December 2018, December 2021 and June 2026 releases. The first three supply overlapping five-year windows; the modern 2026 window completes the series. For each date/category the newest downloaded vintage is selected. The files ending `_overlap_audit.csv` show every overlapping observation's range. In this snapshot, differences between these releases are at most two people for working-age categories and one person for NZ Super, consistent with the scale of rounding; no duplicate quarters are summed.

The 2013 workbook's other-benefit sheet contains several different tables. Its first `Total` is used for working-age other benefits; its later non-working-age and pension totals are not added to that count. NZ Super is taken from the explicitly labelled all-age NZ Super row, not the combined NZ Super/Veterans Pension total.

### Annual expense history

Actuals are selected as follows:

| Published release | Actual expense years used |
|---|---|
| HYEFU 2005 | 2001–2005 |
| HYEFU 2010 | 2006–2010 |
| HYEFU 2015 | 2011–2015 |
| HYEFU 2020 | 2016–2020 |
| BEFU 2026 | 2021–2025 |

Every component sum reconciles to its source's published benefit total. The parser checks the **Actual/Forecast** headers and exports only actual columns. The original PDF row labels, cells, page numbers and publication vintages are saved. These are the actuals **as published in each selected vintage**, not a claim that all 25 years have been restated to a single 2026 accounting basis.

The 2001 table reports **Community Wage**, which incorporated predecessors of both unemployment and sickness support. The 2014 fiscal year contains both the last legacy-benefit expenses and the reformed-benefit expenses, and both are included in its total. Simply replacing the legacy line with the new line would omit part of that year's spending.

The **Main income-replacement basket** sums Community Wage, Unemployment Benefit, Sickness Benefit, Invalids Benefit, Domestic Purposes Benefit, Jobseeker Support and Emergency Benefit, Supported Living Payment and Sole Parent Support when present. It is an **approximate basket**, not a perfectly constant-eligibility working-age population. Some old Domestic Purposes and Widow's categories moved into different reformed programmes; legacy widow expenditure is not separately identified in these Treasury tables. It must not be interpreted as spending solely on unemployed or work-ready recipients.

The other broad baskets combine the listed family tax credits/payments, housing support, temporary COVID/cost-of-living programmes, and an exhaustive residual. Original unavailable/not-applicable cells remain missing. For the reconciled grouped decomposition, a category that has no programmes in a year contributes zero; the source rows are not rewritten as zero. For example, temporary COVID support was absent before its introduction. The full mapping is in `benefit_history_components.csv`.

GDP denominators use the Treasury historical workbook before 2016 and BEFU 2026 from 2016 onward. CPI and population are annual calendar measures; they approximate the deflation and population matching of June fiscal-year expenses. Category shares and real changes reflect programme/rate/recipient changes and classification changes as well as economic conditions.

### The archival tables

The [MSD Statistical Report 2012](https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources/statistics/statistical-report-2012.pdf), Table OT.3, provides counts under the old programme categories back to 1940. It states that:

- the reporting stock date is March before 1990 and June thereafter;
- most benefit counts exclude partners receiving a share of another person's benefit;
- pension/NZ Super counts include non-qualified spouses from 1996;
- the pension series covers predecessor schemes with changing eligibility ages and rules;
- Family Benefit figures have uncertain accuracy.

The charts do not use the uncertain Family Benefit counts, and do not add overlapping child-assistance counts to main-benefit counts. The pension splice preserves the 1996 counting break and the 2008/2009 source change. Changes in pension eligibility, including the rise from 60 towards 65 during 1992–2001, also affect the historical caseload.

The [MSD Statistical Report 2002](https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources/statistics/statistical-report-2002.pdf), Table 7.2, provides historical expenses. Its notes say those amounts include supplementary assistance within benefit categories, use March years before 1990, and move from cash through June 1994 to accrual subsequently. Independent Youth Benefit expenditure is included within the unemployment amount and is also displayed separately in that source; it is not added again in these plots. **These expense tables are not spliced into Treasury's modern main-benefit expense totals.** Full original notes are retained as text files beside the extracted data.

The 1972–2025 Treasury functional series is a separate broader measure. It includes Government Superannuation Fund expenses and is not an unemployment-benefit or main-benefit total. Its cash/March, cash/June, old-GAAP, IFRS and PBE regimes remain separate line segments. Early GST treatment and valuation changes are material to comparisons.

### Caseload versus spending per recipient

For B23, approximate fiscal-year caseload is the average of four quarter-end NZ Super stocks. Only complete four-quarter years are used. The identity is:

`real annual expense = average recipient stock × real annual expense per average recipient`.

The per-recipient component is a **flow/stock proxy**, not the statutory pension rate. It can change with recipient composition, partial-year receipt, eligibility, payment timing and source revisions. The comparison does not estimate the effect of any single policy.

## Reproduce and audit

The existing `05_run_topic_pack.R` now runs `03_pull_topic_data.R`, then the new `06_pull_benefit_history.R`, then `04_make_topic_plots.R`. The plot script loads `07_benefit_history_plots.R` before exporting the combined gallery. Use the same `--cached` and `--plots-only` flags documented in [README_OPED_TOPICS.md](README_OPED_TOPICS.md).

To pull only the longer histories, then regenerate the charts:

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "Data pulls\06_pull_benefit_history.R"
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "Data pulls\05_run_topic_pack.R" --plots-only
```

Historical PDF extraction requires **PyMuPDF 1.23 or newer**, with `Page.find_tables`; the installed 1.26.5 was used. No packages were installed. Set `NZ_OPED_PYTHON` if a different Python executable is needed.

The new sources have their own `benefit_history_source_manifest.csv`, which the plot script combines with the original manifest. `benefit_history_validation.csv` records the coverage, reconciliation and anchor checks. The raw/processed files, overlap audits, archival notes, PNG/SVG charts and chart-data CSVs are all retained. Both a fresh history download and cached extraction were checked.
