# Dynamic strategic-investment game

This folder contains the finite- and infinite-horizon extension of the
original two-stage monetary--fiscal game. Adjustment-cost weights are fixed
primitives. Current monetary and fiscal choices become lagged policy states,
so the authorities can use current policy to affect the other authority's
future feedback action.

Run from `australian_four_equation_game`:

```powershell
julia --project=. dynamic_two_stage/run_dynamic_game.jl
julia --project=. dynamic_two_stage/runtests.jl
julia --project=. dynamic_two_stage/build_tex.jl
```

The first command writes numerical tables and plots to `output/`. The second
builds `DYNAMIC_STRATEGIC_INVESTMENT.pdf` from `paper.tex`.

The code solves:

- the exact two-period subgame-perfect feedback Nash game;
- finite-horizon feedback Nash games by backward coupled Riccati recursion;
- complete-path open-loop Nash on the identical equations and losses;
- finite-horizon cooperation under a fixed social loss; and
- a multi-start stationary feedback-Nash calculation.

The recursive macro block is a transparent hybrid Australian calibration. It
is not yet a full rational-expectations New Keynesian solution; that limitation
is stated explicitly in the paper.
