# New Zealand fiscal-risk data pulls

This folder supports the fiscal-risks op-ed with reproducible data and charts. The selection follows the themes in `NZ Macro chapter full.docx`: the structural deficit, reduced debt headroom for shocks, population ageing, rising health and superannuation costs, weak productivity/per-capita growth, the composition of government spending, and the debt consequences of leaving policy unchanged.

## Run

From the `opeds` folder:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "Data pulls\01_pull_data.R"
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "Data pulls\02_make_plots.R"
```

Required R packages are listed at the top of each script. The scripts stop with a clear message if any are missing; they do not install packages or alter the user's R library.

Set `NZ_DATA_REFRESH=false` before running the pull script to reuse the workbooks already in `raw/`.

## Outputs

- `raw/`: the source Treasury workbooks and Stats NZ GFS file.
- `processed/treasury_fiscal_history_long.csv`: historical spending, revenue, balances, debt, net worth and NZ Super Fund series in dollars and as shares of GDP.
- `processed/befu_2026_fiscal.csv`: Budget 2026 fiscal actuals and forecasts in dollars and as shares of GDP.
- `processed/befu_2026_economic.csv`: Budget 2026 economic actuals and forecasts.
- `processed/ltfs_2025_selected_figures.csv`: selected long-term fiscal, demographic, productivity and interest-rate scenarios.
- `processed/stats_nz_cofog_history.csv`: general-government expenditure by COFOG function for 2009-2025, split into operating expenses, net acquisition of non-financial assets, and their total.
- `processed/stats_nz_cofog_growth_contributions.csv`: each COFOG function's contribution to annual nominal growth in total general-government expenditure.
- `processed/stats_nz_cofog_2010_2025_decomposition.csv`: the ten COFOG divisions' contributions to the total nominal spending increase between 2010 and 2025, with endpoint spending shares and own growth rates.
- `processed/treasury_ltfm_functional_projections.csv`: Treasury functional expense classes through 2065 in dollars and as shares of GDP.
- `processed/treasury_functional_subcategories_endpoint_decomposition.csv`: 2010-versus-2025 subcategory changes and contributions to growth in nine related core-Crown functional groups, extracted from the Treasury's 2010 and 2025 expense tables.
- `processed/treasury_functional_subcategory_notes.csv`: coverage and comparability warnings, including the absence of a comparable 2010 environmental-protection breakdown.
- `processed/treasury_welfare_benefits_2010_2025_decomposition.csv`: harmonised benefit groups and their contributions to the increase in core-Crown welfare-benefit expenditure.
- `processed/treasury_welfare_recipient_counts_2010_2025.csv`: 2010 and 2025 recipient counts for NZ Superannuation and selected working-age or supplementary benefits, with comparability notes.
- `processed/stats_nz_economic_affairs_2010_2025_by_expense_type.csv`: strict COFOG economic-affairs growth split between operating expenses and net acquisition of non-financial assets.
- `processed/treasury_economic_affairs_subcategories_2010_2025.csv`: the detailed contribution of transport, industry-support and primary-service components to growth in the closest core-Crown functional classes.
- `processed/treasury_to_cofog_concordance.csv`: a cautious crosswalk explaining the closest COFOG division for each Treasury class.
- `processed/world_bank_macro_comparison.csv`: New Zealand, Australia and OECD macro indicators from 1990 onward.
- `processed/source_manifest.csv`: source URLs and retrieval time.
- `figures/`: nineteen charts in both PNG and SVG format, styled and saved with `theme61` (`save_e61`). Chart 11 gives the high-level 2010-versus-2025 COFOG decomposition; charts 12a-12c provide the detailed Treasury functions; charts 13-14 decompose welfare benefits; charts 15a-15c explain economic-affairs growth.

The Treasury's long-term paths are scenarios under stated assumptions, not forecasts. The chart footnotes retain this distinction. The Stats NZ historical series is true COFOG for general government (central plus local government), but the published New Zealand file stops at the ten first-level divisions. The subcategory endpoint output therefore uses related Treasury core-Crown functional classes and is deliberately not relabelled as second-level COFOG. Its 2010 values are actuals from HYEFU 2010; its 2025 values are Budget 2025 forecasts. Environmental protection has no comparable 2010 Treasury breakdown because ETS expenses were then classified within heritage, culture and recreation.

## Official sources

- [Fiscal Time Series Historical Fiscal Indicators 1972-2025](https://www.treasury.govt.nz/publications/information-release/data-fiscal-time-series-historical-fiscal-indicators)
- [Budget Economic and Fiscal Update 2026](https://www.treasury.govt.nz/publications/efu/budget-economic-and-fiscal-update-2026)
- [He Tirohanga Mokopuna 2025](https://www.treasury.govt.nz/publications/ltfp/he-tirohanga-mokopuna-2025)
- [Stats NZ Government Finance Statistics](https://www.stats.govt.nz/information-releases/government-finance-statistics-general-government-year-ended-june-2025/)
- [Treasury 2025 Long-term Fiscal Model](https://www.treasury.govt.nz/publications/ltfm/long-term-fiscal-model-he-tirohanga-mokopuna-2025)
- [World Bank Indicators API](https://datahelpdesk.worldbank.org/knowledgebase/articles/889392-about-the-indicators-api-documentation)
