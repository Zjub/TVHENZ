# ARIMAX Spending Projection Workflow

`spending_projection_arima_paths.R` mirrors the Shapley spending projection
workflow but uses a basic ARIMA time-series model with the same explanatory
variables as external regressors.

`spending_projection_hybrid_level_diff_paths.R` is a separate hybrid workflow.
It keeps demographics in levels and puts macro/cyclical drivers in annual
differences.

Run from the `projections` project root with:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" --vanilla "ARIMA\spending_projection_arima_paths.R"
```

or from the repository root with:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" --vanilla "projections\ARIMA\spending_projection_arima_paths.R"
```

Inputs:

- ABS historical data via `readabs`: 5206.0, 3101.0, 5302.0, 6202.0
- `For_shapely_projections.xlsx`: forward age-share, terms-of-trade,
  relative-price, population, and unemployment assumptions

Model:

- Outcome: government expenditure as a share of GDP
- Regressors by default: `0_14`, `15_34`, `55_64`, `65p`, `tot`, `rp_g`,
  `unemp`
- Time-series component: ARIMA errors, with a small `(p, d, q)` grid selected
  by AIC
- Outlier years excluded from estimation: 2020 and 2021

Model form option:

- Set `model_form <- "level"` for the original ARIMAX-in-levels model.
- Set `model_form <- "diff"` to estimate annual changes in spending/GDP using
  annual changes in the same drivers, then cumulate projected changes from the
  final historical spending/GDP value.
- Set `model_form <- "ECM"` to estimate annual changes with differenced drivers
  and a lagged long-run gap. This option should be treated cautiously unless the
  cointegration diagnostic supports a stationary long-run relationship.

The output file names include the selected model form, for example
`Expenditure_National_level_intercept_ref_35_54`.

Hybrid level/difference script:

- Run `spending_projection_hybrid_level_diff_paths.R` to estimate the hybrid
  model.
- The demographic component is estimated in levels:
  `gov_gdp ~ age shares`, with `35_54` omitted as the reference age group.
- The macro component is estimated in differences. The dependent variable is
  the part of annual spending/GDP change left after the demographic level
  component has moved.
- The macro difference ARIMAX has no drift by default, so there is no
  unexplained annual change that mechanically cumulates through the projection.
- The output file names include
  `Expenditure_National_hybrid_level_diff_demo_ref_35_54_macro_no_drift`.
- The hybrid script also writes a second demographic decomposition plot that
  splits the demographic component into the included age groups, with `35_54`
  treated as the omitted reference group.
- The hybrid script writes extra out-of-sample level forecast plots:
  rolling projection paths against actual spending/GDP, forecast errors by
  horizon, and actual-versus-forecast scatter plots.
- The hybrid script also writes leverage diagnostics. These identify ordinary
  non-outlier years that have unusual regressor values or that move the
  estimated parameters/projection when removed one at a time.

Age-share/intercept option:

- The default is `no_intercept <- FALSE`.
- With this setting, the model includes an ARIMA intercept/constant and drops
  `35_54` as the reference age group.
- To reproduce the Shapley-style specification, set `no_intercept <- TRUE`.
  This includes all five age groups and excludes the intercept.
- For `model_form <- "diff"` and `model_form <- "ECM"`, `no_intercept <- TRUE`
  means no annual drift term. The script still drops one differenced age-share
  column, using `reference_age_group`, because all five differenced age shares
  sum exactly to zero and cannot be estimated together.

Main output:

- `outputs/arimax_spending_path_for_debt_projection_Expenditure_National_level_intercept_ref_35_54.csv`

That output has the same structure as the Shapley debt-bridge file:
`year`, `series`, and `Consolidated`.

Decomposition outputs:

- `outputs/arimax_spending_change_decomposition_since_2000_Expenditure_National_level_intercept_ref_35_54.csv`
- `outputs/arimax_spending_change_decomposition_totals_since_2000_Expenditure_National_level_intercept_ref_35_54.csv`
- `outputs/arimax_spending_change_decomposition_since_2000_Expenditure_National_level_intercept_ref_35_54.png`
- `outputs/arimax_spending_change_decomposition_since_2000_Expenditure_National_level_intercept_ref_35_54.svg`

For `model_form <- "level"`, the decomposition uses the ARIMAX coefficients to
attribute the change in the model fitted/projection path since 2000 to each
external regressor:

```text
contribution_j,t = beta_j * (x_j,t - x_j,2000)
```

The remaining change is reported as `ARIMA dynamics`, which captures the
time-series/error component of the ARIMAX model. In the plot, stacked bars sum
to the model-implied change since 2000, the solid line shows that total model
change, and the dashed line shows the actual historical change.

For `model_form <- "diff"` and `model_form <- "ECM"`, the decomposition
cumulate annual contribution terms because those models are estimated on annual
changes rather than levels.

Diagnostics:

The script writes a diagnostics workbench to:

- `outputs/diagnostics/`

Start with:

- `diagnostic_index_Expenditure_National_level_intercept_ref_35_54.csv`
- `diagnostic_cointegration_test_Expenditure_National_level_intercept_ref_35_54.csv`

The index file explains each diagnostic output and what to check first. The
cointegration test file reports whether the estimated long-run level residual
looks stationary. The folder also includes:

- coefficient estimates and an uncertainty plot
- an Engle-Granger residual ADF diagnostic for a possible cointegrating
  relationship
- VIFs and pairwise regressor correlations
- AR and MA root checks
- residual summaries, residual ACFs, and Ljung-Box tests
- stationarity checks for the outcome and regressors, if `tseries` is installed
- historical versus projection driver-range checks
- rolling holdout forecasts and a forecast plot
- model-sensitivity results for alternative regressor sets

Hybrid leverage diagnostics:

- `hybrid_leverage_demographic_ols_*.csv` reports hat values, Cook's distance,
  studentized residuals, and flags for demographic-level regression influence.
- `hybrid_leverage_leave_one_year_out_summary_*.csv` re-estimates the full
  hybrid model after dropping each non-outlier year in turn, then reports the
  change in regressor parameters and the final projection.
- `hybrid_leverage_leave_one_year_out_coefficients_*.csv` gives the underlying
  coefficient-by-coefficient comparison for those refits.
- The corresponding plots show Cook's distance, leverage versus residual size,
  final-projection sensitivity, and parameter sensitivity.
