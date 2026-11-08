# OECD Long-Run Scenario Workflow

## Australian fiscal model in R

The new fiscal workflow implements the documented OECD spending equations,
conditional on the available published macro paths. It is separate from the
original download-and-chart workflow described below.

Run from the `projections` directory:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla OECD_long_run_scenario\run_fiscal_replication.R
```

The default run is offline and uses the cached source snapshots. Add `--download`
to acquire missing source files. Downloads never silently refresh existing
snapshots. To update a source, archive its existing cache file first. Rebuilding
from a new WPP workbook also requires archiving the processed Australian WPP
extract. Keep the old snapshots when comparing vintages.

Start with `outputs/fiscal/fiscal_replication_report.html`, or the Word version
at `outputs/fiscal/fiscal_replication_report.docx`. The HTML embeds its figures
and can be shared as a single file. Nine PNG/PDF figure pairs are also exported.

**Replication status:** equations implemented; numerical match incomplete.
Independent increases from 2024 to 2060 are 1.88 percentage points of GDP for
health, 1.29 for pensions and -0.70 for residual primary spending. The Australia
2026 survey chart reports 1.98, 1.85 and +0.53 respectively. Its exact fiscal input
vintage, older-age employment paths and transition adjustments have not been
recovered. Benchmark numbers are used for evaluation, never to fit the baseline.
Debt paths are conditional illustrations, not reproduced OECD forecasts.

### Code and inputs

- `run_fiscal_replication.R`: runs preparation, projections, validation and reports.
- `scripts/03_acquire_fiscal_sources.R`: caches public source data and methodology.
- `scripts/03b_download_imf_sources.R`: caches official IMF Fiscal Monitor API data.
- `scripts/04_prepare_fiscal_inputs.R`: extracts UN age shares, OECD health/pension
  and employment inputs, fiscal anchors, and the published validation benchmarks.
- `R/fiscal_functions.R`: documented spending equations and the fiscal/debt rule.
- `scripts/05_replicate_fiscal_model.R`: six climate scenarios, spending-rule
  sensitivities, a 2026 residual-rule sensitivity, growth-source counterfactuals,
  debt illustrations and comparisons with the saved Australian models.
- `tests/validate_fiscal_replication.R`: 16 economic and accounting checks.
- `scripts/05b_compare_imf_projections.R`: compares IMF spending projections with
  the saved project models, preserving the published horizons and units.
- `scripts/06_fiscal_report.R`: figures, data documentation and HTML/Word reports.

Data sources, units, definitions and approximation status are in:

- `data/processed/fiscal/data_sources_and_limitations.csv`
- `data/processed/fiscal/data_dictionary.csv`
- `data/processed/fiscal/fiscal_anchor.csv`
- `data/processed/fiscal/fiscal_model_parameters.csv`
- `data/raw/fiscal/download_manifest.csv`
- `data/processed/fiscal/source_file_checksums.csv`

The model uses OECD EO117 macro paths, UN WPP 2024 age shares, OECD SHA health
expenditure, the four public pension categories in the 2019 SOCX definition,
OECD older-worker employment rates, and a cached copy of the existing PBO
historical extract. Direct OECD SDMX requests returned HTTP 403 during research;
the OECD series are accessed through DBnomics. Exact requests and raw JSON are
cached. The existing processed EO117 extract supplies the other climate scenarios.

The WPP workbook contains annual observations in five-year age bands. The model
does not interpolate five-year-spaced observations. Health-related long-term care
is already in the health total. Pension TP112 has no Australian observations and
is explicitly treated as zero. Future age-specific employment rates use a stated
aggregate-odds approximation, with fixed-rate sensitivity; they do not recreate
the OECD cohort employment model. PBO fiscal-year expense data are an explicit
approximation to the required calendar-year current-expense anchor.

The September 2026 OECD WP1877 is also examined. Its residual-spending rule is
implemented as a separate sensitivity. This is not the complete EO119 model:
defence, pension coverage, capital rules and asset revaluation are not updated in
that sensitivity. The report explains these differences.

Required R packages are `data.table`, `jsonlite`, `readxl` and `ggplot2`; `curl`
is needed for downloads and `officer` for the optional Word report. Package
versions are written to `outputs/fiscal/session_info.txt`.

### Main outputs

- `spending_projection_paths.csv`: all drivers, component shares and cases.
- `spending_components.csv` / `spending_endpoints.csv`: tidy spending results.
- `published_spending_comparison.csv`: independent and published changes, with gaps.
- `benchmark_gap_diagnostics.csv`: inverse calculations that explain gaps; not inputs.
- `spending_driver_contributions.csv`: additive component-change contributions.
- `growth_source_counterfactuals.csv`: identical 10% output gains from different sources.
- `debt_and_revenue_paths.csv`: fixed-revenue and documented-rule debt illustrations.
- `existing_model_comparison.csv`: changes from 2030 in the saved Australian models.
- `existing_model_endpoint_comparison.csv`: published OECD spending-component
  changes alongside all models over 2024-2060; Australian starts use historical
  PBO anchors for each expenditure concept. No annual OECD path is interpolated.
- `replication_gap_bridge.csv`: component shortfalls and their shares of the total gap.
- `replication_driver_diagnostics.csv`: calculated versus benchmark-implied
  drivers; inverse diagnostics, not recovered OECD assumptions.
- `validation_checks.csv`: implementation checks, separate from benchmark accuracy.
- `imf_medium_term_comparison.csv` / `imf_medium_term_endpoints.csv`: total
  expenditure comparisons through 2031, with common 2025-2030 endpoints.
- `imf_component_comparison.csv`: published IMF health/pension flow changes and
  available model components over 2025-2030.

### IMF comparison

The live official Fiscal Monitor API snapshot was retrieved on 23 October 2026.
Its response does not embed a release label, so the report identifies it by
retrieval date. Australian general-government expenditure is available to 2031.
Comparators include interest and net capital investment, with calendar years and
Australian fiscal years paired by ending year. The enhanced bottom-up comparator
borrows the baseline interest path; this does not create a new enhanced debt model.

April 2026 Fiscal Monitor Table A23 supplies separate health and public-pension
changes for 2025-2030. Its 2025-2050 entries are discounted cumulative spending
changes, not annual spending shares or endpoint changes. They are preserved as
NPVs and excluded from the 2060 chart. No IMF 2060 path is extrapolated.

Raw API responses and a manually transcribed Australian Table A23 row with source
URL and page are in `data/raw/imf`. The IMF PDF was read via web access; direct
PDF downloads returned HTTP 403. Processed data, source limitations and checksums
are in `data/processed/imf`. The default offline run uses these cached inputs.

All new outputs stay under `outputs/fiscal`. The existing Australian model code,
input workbooks and saved estimates are read only. The report's comparison is not
a controlled forecast-accuracy comparison: macro paths and expenditure concepts
also differ.

## Original macro scenario workflow

This folder builds an Australia-focused version of the OECD Economic Outlook
117 long-run scenarios. It downloads the OECD long-run scenario data, extracts
Australia, and produces projection tables and plots that can be used alongside
the spending and debt projection work in the parent `projections` folder.

The workflow uses the OECD Data Explorer dataset:

- Dataset: `OECD.ECO.MAD / DSD_EO_LTB@DF_EO_LTB`
- DBnomics mirror used by the scripts:
  `https://api.db.nomics.world/v22/series/OECD/DSD_EO_LTB%40DF_EO_LTB`
- Australia series are requested with the pattern:
  `AUS.<MEASURE>.<SCENARIO>.A`

Run
---

From the repository root:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" --vanilla "projections\OECD_long_run_scenario\run_oecd_lts_australia.R"
```

From the `projections` project root:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" --vanilla "OECD_long_run_scenario\run_oecd_lts_australia.R"
```

Scripts
-------

- `scripts/00_config.R`: folder paths, source metadata, scenario labels, and
  the OECD measures to pull.
- `scripts/01_download_oecd_lts_data.R`: downloads OECD/DBnomics JSON, saves
  raw metadata, and writes a tidy Australia long file.
- `scripts/02_build_australia_projection_outputs.R`: builds projection tables,
  summary tables, growth-accounting decompositions, debt-bridge inputs, and
  figures.
- `run_oecd_lts_australia.R`: top-level runner.

Scenarios
---------

- `BAU1`: business-as-usual energy transition, median climate damage curve,
  no carbon mitigation costs.
- `BAU2`: business-as-usual energy transition, high climate damage curve,
  no carbon mitigation costs.
- `ET1`: accelerated energy transition, median climate damage curve, slow
  reduction in carbon mitigation costs.
- `ET2`: accelerated energy transition, median climate damage curve, fast
  reduction in carbon mitigation costs.
- `ET3`: accelerated energy transition, high climate damage curve, slow
  reduction in carbon mitigation costs.
- `ET4`: accelerated energy transition, high climate damage curve, fast
  reduction in carbon mitigation costs.

Main Outputs
------------

All outputs are written to `outputs/`.

- `oecd_lts_australia_projection_paths.csv`: tidy long table of all downloaded
  Australia scenario paths.
- `oecd_lts_australia_scenario_summary.csv`: values in selected years plus
  change and annualised growth from 2025.
- `oecd_lts_australia_differences_from_bau1.csv`: absolute and percentage
  differences from the `BAU1` path.
- `oecd_lts_australia_debt_bridge_inputs.csv`: compact macro assumptions for
  debt modelling, including potential GDP growth in percent and decimal form.
- `oecd_lts_australia_growth_accounting.csv`: log-change decompositions for
  potential output and potential employment.

Figures are written to `outputs/figures/` as PNG and SVG:

- potential GDP per capita
- potential GDP index
- potential output growth
- trend population index
- scenario differences from `BAU1`
- potential output accounting
- potential employment accounting
- emissions and primary energy supply
- selected-year primary energy mix

Interpretation
--------------

This workflow initially reproduces the published OECD scenario paths rather
than re-estimating the OECD production-function model from first principles.
Where the OECD dataset provides a level but not the corresponding ready-made
growth rate for Australia, the workflow derives the growth rate directly from
the published level path. At present this is done for potential output growth,
using `GDPVTRD`.

The accounting plots are arithmetic diagnostics over the published series:

```text
log potential GDP change
  ~= log trend labour efficiency change
   + log potential employment change
   + residual
```

and:

```text
log potential employment change
  ~= log working-age population change
   + log trend employment-rate change
   + residual
```

The residuals are kept explicitly because the published series may include
scaling, model adjustments, or accounting definitions that do not line up as
exact identities in the downloaded levels.
