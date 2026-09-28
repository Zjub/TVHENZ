# Australian monetary–fiscal coordination model

This project extends the conventional three-equation New Keynesian model to
include an optimizing fiscal authority, government debt, distinct policy
mandates and a separate social-welfare criterion. It is written entirely in
Julia and calibrated with public Reserve Bank of Australia data.

The model has three complementary closures:

1. a finite-horizon **open-loop policy game**, in which complete monetary and
   fiscal paths are derived from their loss functions;
2. a stationary **recursive feedback game**, with a Markov-perfect Nash
   equilibrium and a first-stage game in delegated adjustment costs; and
3. an explicit **five-equation rule system**, containing IS, Phillips, debt,
   monetary-policy and fiscal-policy equations for active/passive regime
   comparisons.

The main experiment changes only the agencies' relative inflation and output
weights. Targets, structural equations, debt concern and primitive policy costs
remain fixed. Cooperation minimizes a third, fixed social-welfare loss. In the
institutional game, delegated adjustment coefficients change future policy
rules without changing the true mandate or social-welfare primitives.

## Reproduction

From this directory, run:

```powershell
julia --project=. -e "using Pkg; Pkg.instantiate(; update_registry=false)"
julia --project=. run_model.jl
julia --project=. test\runtests.jl
julia --project=. build_tex.jl
```

Use `julia --project=. run_model.jl --refresh-data` to download the current RBA
CSV tables. By default, the cached, checksum-recorded files in the adjacent
`australian_three_equation_game/data/raw` folder are reused.

## Main files

- `coordination_model.jl`: open-loop model, objectives, strategic diagnostics
  and rule-system solver.
- `recursive_game.jl`: recursive LQ feedback solver, impulse responses and
  two-stage commitment-technology game.
- `run_model.jl`: Australian calibration, simulations, sensitivity analysis,
  tables and graphs.
- `paper.tex` and `PAPER.pdf`: native LaTeX academic-paper source and PDF.
- `build_tex.jl`: reproducible LaTeX build using Julia's `tectonic_jll`.
- `test/runtests.jl`: tests of the macro blocks, welfare rankings, mandate
  isolation, recursive equilibrium, commitment game and rule closure.
- `output/calibration.toml`: all fixed model and objective primitives.
- `output/game_summary.csv`: open-loop equilibrium losses and peak responses.
- `output/complementarity_sweep.csv`: mandate-divergence results.
- `output/coordination_grid.csv`: policy-cost and mandate-divergence map.
- `output/recursive_game_summary.csv`: recursive welfare and impulse summaries.
- `output/recursive_policy_rules.csv`: all feedback-rule coefficients.
- `output/commitment_game.csv`: complete first-stage institutional objective grid.

## Interpretation boundary

The Australian IS and Phillips coefficients are descriptive OLS estimates over
1993Q1–2019Q4. The debt block and social loss are transparent calibrations, not
estimated structural primitives. The rule-based fiscal-dominance comparison is
an active/passive analogue; the model does not contain the full nominal-bond
valuation mechanism required for a complete fiscal theory of the price level.
The recursive closure uses estimated output and inflation persistence and is
not yet the fully forward-looking rational-expectations version of the model.
