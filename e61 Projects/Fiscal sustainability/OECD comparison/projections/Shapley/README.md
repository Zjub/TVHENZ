# Spending Projection Workflow

`spending_projection_paths.R` is a focused extraction from
`2e Shapley outliers.R`.

It produces the spending path used as an input to debt projections:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" --vanilla "projections\Shapley\spending_projection_paths.R"
```

Inputs:

- ABS historical data via `readabs`: 5206.0, 3101.0, 5302.0, 6202.0
- `For_shapely_projections.xlsx`: forward age-share, terms-of-trade,
  relative-price, population, and unemployment assumptions

Main output:

- `outputs/spending_path_for_debt_projection_Expenditure_State.csv`

That output has the same conceptual structure as the old `Graph_data.xlsx`
input used by the debt projection script: `year`, `series`, and
`Consolidated`.
