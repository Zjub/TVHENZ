# Australian monetary–fiscal coordination model (Julia)

This folder contains a Julia extension of the monetary–fiscal policy game in
`SI_game`. It combines a small Australian three-equation macro model with a
dynamic two-player policy game and a separate Leeper-style fiscal-dominance
experiment.

The distinction between **preferences** and **policy regimes** is deliberate:

- the central bank and fiscal authority each have one fixed loss function;
- those primitive weights are unchanged across Nash, cooperation, central-bank
  leadership and fiscal leadership;
- monetary and fiscal dominance differ only through policy-rule coefficients,
  especially the interest-rate response to inflation and the fiscal response
  to debt.

The full economic explanation is in
[THREE_EQUATION_MODEL.md](THREE_EQUATION_MODEL.md). More concise implementation
notes are in [MODEL_NOTES.md](MODEL_NOTES.md).

## Running the model

From this directory:

```powershell
julia --project=. -e "using Pkg; Pkg.instantiate()"
julia --project=. run_model.jl
julia --project=. test/runtests.jl
```

The first command installs the exact dependencies in `Manifest.toml`. The
second uses the checked-in RBA data snapshot, estimates the Australian macro
block, solves all policy games and regenerates the outputs. To download the
latest RBA tables first, use:

```powershell
julia --project=. run_model.jl --refresh-data
```

To regenerate the PDF documentation:

```powershell
julia --project=. build_pdfs.jl
```

## Julia code map

- `policy_game.jl` contains the commented state-space model, fixed preference
  primitives, quadratic game solver and active/passive policy-rule simulator.
- `data_pipeline.jl` downloads and parses the RBA tables, constructs gaps,
  implements the HP filter and estimates the reduced-form equations.
- `run_model.jl` defines the single calibration, shock experiments, explicit
  policy regimes, output tables and plots.
- `test/runtests.jl` tests the transitions, Nash first-order conditions,
  cooperative loss comparison, fixed-preference regime experiment and data fit.
- `src/AustralianMonetaryFiscalGame.jl` is the small package entry point needed
  for reproducible Julia project loading and precompilation.
- `build_pdfs.jl` converts the three Markdown notes to PDF locally.

## Data

All series are public and are downloaded from the [RBA Statistical
Tables](https://www.rba.gov.au/statistics/tables/).

| Table | Series ID | Use |
|---|---|---|
| H1 | `GGDPCVGDP` | Chain-volume real GDP and HP-filtered output gap |
| H2 | `GGDPECCVPD` | Chain-volume public demand and fiscal-policy proxy |
| G1 | `GCPIOCPMTMQP`, `GCPIOCPMTMYP` | Trimmed-mean inflation and ex-post real rate |
| F1.1 | `FIRMMCRT` | Monthly cash-rate target, averaged to quarters |

The estimation window is 1993 Q1–2019 Q4. Ending before the pandemic avoids
asking one constant-coefficient model to explain both the effective-lower-bound
and lockdown periods. The checked-in raw files, source URLs, byte counts and
SHA-256 hashes are recorded in `data/raw/manifest.toml`.

This is a **data-informed calibration**, not causal identification. The cash
rate and public demand are endogenous policy choices, and the HP-filtered gaps
are two-sided estimates.

## Fixed objective primitives

| Authority | Inflation | Output | Debt | Instrument adjustment | Discount |
|---|---:|---:|---:|---:|---:|
| Central bank | 1.00 | 0.25 | 0.00 | 0.12 | 0.99 |
| Fiscal authority | 0.25 | 1.00 | 0.45 | 0.08 | 0.99 |

These illustrative weights are defined once in `run_model.jl` and written to
`output/model_primitives.toml`. The cooperative planner's 50–50 aggregation of
the two fixed losses is part of that solution concept; it does not change either
authority's primitive loss function.

## Generated outputs

- `output/australian_inputs.png`: Australian data used by the model.
- `output/demand_shock_game.png`: four solution concepts after a demand shock.
- `output/supply_shock_game.png`: four solution concepts after an inflation shock.
- `output/fiscal_dominance_comparison.png`: active/passive policy-rule regimes,
  evaluated with the same objective functions.
- `output/game_summary.csv`: loss and peak-response comparison for the games.
- `output/rule_regime_summary.csv`: rule coefficients, losses and terminal debt.
- `output/model_primitives.toml`: the complete fixed calibration.
- `output/estimated_coefficients.csv` and `estimation_summary.toml`: model fit.
- `output/policy_sensitivities.csv`: first Nash action's response to each state.
- `data/processed/australia_quarterly.csv`: analysis-ready quarterly data.

## Main limitations

The strategic game is a finite-horizon, perfect-foresight open-loop game. The
fiscal-dominance experiment is a backward-looking active/passive rule
simulation. It illustrates the mechanism but is not a rational-expectations
FTPL determinacy proof. A literal FTPL extension requires a forward-looking IS
curve, New Keynesian Phillips curve, nominal government valuation equation,
surplus rule and equilibrium-selection conditions.
