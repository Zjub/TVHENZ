# Forward-looking Australian monetary--fiscal policy game

The paper and main Julia experiment use a forward-looking New Keynesian IS
curve and Phillips curve with rational private expectations. Monetary and
fiscal authorities choose their instruments simultaneously in every period
under stationary Markov strategies. Their mandate weights are fixed model
primitives, and coordination is evaluated under a separate, fixed true social
loss.

Own-instrument adjustment costs make current policy an inherited state. This
creates a strategic-investment channel: an authority can influence its future
self and, if the rival responds to that inherited position, future rival
policy. The model compares:

- flexible Nash with no adjustment costs;
- monetary persistence only;
- fiscal persistence only;
- bilateral persistence; and
- discretionary coordination under true social welfare.

`nk_forward_looking_game.jl` contains the model and rational-expectations
solver. `run_nk_extension.jl` produces results, expectation accounting, policy
rules, strategic-channel decompositions, and impulse responses.
`run_nk_parameter_regions.jl` solves the forward-looking parameter sweeps and
produces the strategic-region heat maps. The older
recursive implementation remains in the folder as a code-validation aid, but
it is not part of the paper's economic model or reported results.

## Reproduce

```powershell
julia --project=. -e "using Pkg; Pkg.instantiate(; update_registry=false)"
julia --project=. run_nk_extension.jl
julia --project=. run_nk_parameter_regions.jl
julia --project=. runtests_nk.jl
julia --project=. build_tex.jl
julia --project=. build_slides.jl
```

The TeX source is `paper.tex`; the compiled paper is
`FORWARD_LOOKING_NK_POLICY_GAME.pdf`. Machine-readable results and plots are
written to `output/`. The academic presentation source is `slides.tex`; it
builds as `ANALYTICAL_STRATEGIC_INVESTMENT_MPE_SLIDES.pdf`.
