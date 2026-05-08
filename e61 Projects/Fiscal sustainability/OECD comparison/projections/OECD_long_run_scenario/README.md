# OECD Long-Run Scenario Workflow

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
