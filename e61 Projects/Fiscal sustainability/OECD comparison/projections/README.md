# Debt Projections

This folder contains the inputs and script needed to reproduce the consolidated
debt projections from the end of `OECD_consolidated.R`.

Run from the project root with:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" --vanilla "projections\debt_projections.R"
```

The script reads from `data/` and writes charts and CSVs to `outputs/`.

Subfolders:

- `Shapley/`: spending projection paths built from the Shapley-style
  demographic/economic exercise.
- `ARIMA/`: ARIMAX and hybrid level/difference spending projection approaches.
- `OECD_long_run_scenario/`: Australia workflow for the OECD Economic Outlook
  117 long-run economic scenarios.
