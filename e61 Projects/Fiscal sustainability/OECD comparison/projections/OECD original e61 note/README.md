# Original e61 versus OECD gross-debt scenarios

Prepared 28 October 2026 for a separate note. This folder is independent of the
Shiny application and is not part of its deployment bundle.

The chart compares Australian general-government gross debt over **2030-2050**:

| Series | 2030 (% GDP) | 2040 (% GDP) | 2050 (% GDP) |
| --- | ---: | ---: | ---: |
| OECD without fiscal adjustment | 65.29 | 82.39 | 113.23 |
| e61 projections | 54.24 | 59.10 | 71.17 |
| OECD with fiscal adjustment | 62.26 | 56.70 | 57.28 |

## Files for the note

`outputs/oecd_original_e61_gross_debt.png`, `.pdf` and `.svg` contain the same
chart. PDF and SVG are vector formats. The PNG is for easy insertion and preview.
Styling uses the installed **theme61 0.8.0** package, including `theme_e61`,
`palette_e61`, `labs_e61` and `save_e61` (18.6 cm width). Direct series labels
use `plab()` (the installed package's plot-label helper) in place of a legend.
The e61 source credit on the chart is **e61-McKinnon Fiscal Report**.

`outputs/oecd_original_e61_gross_debt_chart_data.csv` contains the 63 annual
observations actually plotted. `outputs/comparison_endpoints.csv` contains the
2030, 2040 and 2050 observations. `data/comparison_source_series.csv` retains
the full available source series with provenance.

## Which original e61 projection?

The source is **`OECD comparison/graph_proj_all.xlsx`, Sheet1, variable `debt`**,
outside the `projections` directory. Its exact values are used; the Shiny engine
is neither called nor substituted for this series.

`OECD comparison/Cleaned up debt proj.R` builds that table from `debt_no_rev`,
which calls `project_debt_path(..., ramp_revenue = FALSE,
g_nom_assump = 0.045)`. This holds revenue/GDP at its last historical level.
Spending is the original consolidated spending projection with its historical
OECD expense-level gap carried forward. The original recurrence is
`d_next = (d_t + deficit_t) / (1 + nominal_growth_t)`, where deficit is the
expense/revenue gap and nominal GDP growth is 4.5% annually. No new interest,
stock-flow adjustment or financial-asset model is added for this chart.

The original script also makes a revenue/GDP +2 percentage-point case, but the
saved `graph_proj_all` comparison selects the **flat-revenue case**. That is the
single original e61 line shown here. The copied source script is archival
documentation and is not executed: rebuilding it would require its other
original inputs and could replace the saved projection with a new run.

## OECD sources and comparison boundaries

Both OECD paths are the official numerical observations in
[Australia Economic Survey 2026 Figure 1.14, panel B](https://www.oecd.org/content/dam/oecd/en/publications/reports/2026/01/oecd-economic-surveys-australia-2026_1b6f84bc/d22a1efd-en.pdf),
downloaded from the [StatLink source workbook](https://stat.link/gdxr47).
Worksheet `fig1-14` has year in column J, without-adjustment debt in L and
with-adjustment debt in M; both stop in 2050. The workbook records an update date
of 21 January 2026. The with-adjustment case includes spending savings or
additional revenue; it is a conditional policy scenario.

The chart retains **each source's own levels, GDP denominator, vintage and
assumptions**. It does not rebase the paths to a common initial debt ratio,
convert source year labels between calendar/financial years, extrapolate the
OECD series, or infer net debt. The original e61 code itself flags its historical
year mapping as a convention needing further reconciliation for a report.
Consequently, this is a comparison of saved scenario paths, not a harmonised
decomposition of why the models differ. Short-run years are excluded from the
chart in keeping with the long-term projection focus.

## Reproduce

From the `projections` directory:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'OECD original e61 note/make_comparison.R'
```

The script reads only the source snapshots in this folder and writes only here.
It does not need the Shiny app, its model or a network data download. Packages:
`theme61`, `ggplot2`, `readxl`. Source file hashes and snapshot date are in
`data/source_manifest.csv`; package versions are in `outputs/software_versions.txt`.
Checks verify unique observations, complete shared plotting horizons, exact
preservation of the original e61 values and the published OECD endpoints.
