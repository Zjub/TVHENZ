# 2026 fiscal projections

This folder contains a self-contained, modular refresh of the Australian fiscal
projection workflow. It keeps official forecasts, bottom-up projections,
top-down time-series projections, forecast checks, debt arithmetic, figures and
documentation separate.

Run the complete workflow from this folder with:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" --vanilla "run_all.R"
```

The workflow downloads current official inputs when they are unavailable
locally. Set the environment variable `REFRESH_DATA=true` to force a new
download.

## Script order

1. `scripts/01_download_data.R` downloads official ABS, Centre for Population,
   PBO and Budget inputs.
2. `scripts/02_clean_data.R` builds consistent annual historical and projection
   datasets and runs data checks.
3. `scripts/03_assumptions.R` creates central and sensitivity assumptions.
4. `scripts/04_official_forecast.R` extracts the consolidated official forecast
   anchor.
5. `scripts/10_bottom_up_projection.R` projects spending by GFS purpose.
6. `scripts/20_top_down_projection.R` estimates five aggregate alternatives.
7. `scripts/21_shapley_attribution.R` averages incremental fitted contributions
   over every ordering of the structural factors. It is an attribution diagnostic,
   not a sixth forecasting model.
8. `scripts/30_forecast_checks.R` performs rolling tests and compares model-only
   forecasts with the official forecast period.
9. `scripts/31_model_diagnostics.R` reports in-sample fit, residual tests,
   coefficient standard errors, conditional forecast intervals, estimation-window
   sensitivity and economic-driver sensitivity for every top-down model.
10. `scripts/40_revenue_and_debt.R` builds revenue scenarios and debt paths.
11. `scripts/50_graphs.R` writes the full figure suite, including direct
    bottom-up versus top-down comparisons.
12. `scripts/60_word_report.R` writes the Word methodology and results report.

## Output layout

- `data/raw/`: downloaded source files and a source manifest.
- `data/processed/`: cleaned and model-ready datasets.
- `outputs/tables/`: projections, diagnostics and comparison tables.
- `outputs/figures/`: grouped bottom-up, top-down, comparison and debt figures.
- `documentation/`: generated Word report.

The top-down national-accounts expenditure concept and the PBO/GFS official
expense concept are not identical. The workflow preserves this distinction,
reports the bridge at the join, and anchors model growth rates rather than
silently treating the levels as interchangeable.
