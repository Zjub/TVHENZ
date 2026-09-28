# Australian endogenous-leadership monetary--fiscal game

This Julia project treats the original one-period Stackelberg games as
benchmarks. In the dynamic model, monetary and fiscal policy are chosen in
every period under simultaneous Markov strategies. Own-instrument adjustment
costs give an authority commitment capacity, potentially moving the dynamic
equilibrium toward its static Stackelberg-leader allocation.

The experiments are:

- flexible Nash: no adjustment costs;
- monetary commitment: only monetary adjustment is costly;
- fiscal commitment: only fiscal adjustment is costly;
- bilateral commitment: both authorities can acquire leader-like positions;
- coordination under the fixed true social objective with flexible instruments.

The adjustment coefficients are exogenous primitives, never strategic choices.
The bilateral-cost game is sometimes described informally as a meta-Nash
leadership contest, but the code does not pretend that the authorities choose
their adjustment coefficients.  A literal meta-game would require an outer
institutional-choice stage.

The Australian transmission coefficients are the 1993Q1--2019Q4 descriptive
estimates produced by the companion RBA data pipeline.  Debt dynamics, mandate
weights and adjustment costs are transparent calibrations.

Before applying those values, the runner constructs two parameter maps.  The
first uses the analytical response product `Gamma` to show how transmission
and direct instrument penalties govern static amplification.  The second
solves the full recursive game over `Gamma` and the dimensionless persistence
share `p = lambda / (D + lambda)`, and decomposes each authority's continuation
first-order condition into strategic and non-strategic components.

The forward-looking extension uses a homotopy parameter `theta`.  At
`theta = 0` it exactly reproduces the backward-looking model; at `theta = 1`
it solves a New Keynesian IS curve and Phillips curve with rational private
expectations.  The solver finds a fixed point between private decision rules
and the monetary--fiscal Markov-perfect equilibrium.  Its diagnostics separate
the macro-state, inherited-policy, and private-expectations channels.

## Reproduce

```powershell
julia --project=. -e "using Pkg; Pkg.instantiate(; update_registry=false)"
julia --project=. run_model.jl
julia --project=. run_nk_extension.jl
julia --project=. runtests.jl
julia --project=. runtests_nk.jl
julia --project=. build_tex.jl
```

The paper is built as `STRATEGIC_INVESTMENT_POLICY_GAME.pdf`; numerical tables and plots
are written to `output/`.
