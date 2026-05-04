# Debt Projections

This folder contains the inputs and script needed to reproduce the consolidated
debt projections from the end of `OECD_consolidated.R`.

Run from the project root with:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" --vanilla "projections\debt_projections.R"
```

The script reads from `data/` and writes charts and CSVs to `outputs/`.
