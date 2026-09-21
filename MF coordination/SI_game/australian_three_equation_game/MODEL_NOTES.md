# Implementation notes for the Julia policy game

## What changed from the first example

The initial example was written in Python because the local data and plotting
libraries were already available. That was expedient, but inconsistent with the
language of the surrounding project and less useful for extending the original
Julia code. The substantive implementation is now entirely in Julia.

The other correction is economic rather than cosmetic. Previously, the
fiscal-dominance chart changed the weights in the authorities' objective
functions. That treated the regime as if it were a change in tastes. In the
corrected model, preferences are fixed primitives. Fiscal dominance is instead
represented by active/passive policy rules.

## Fixed primitives and changing solution concepts

The central bank's loss is

\[
L_M=\sum_{t=0}^{T-1}\beta^t\left[
\pi_{t+1}^2+0.25x_{t+1}^2+0.12(u_t-u_{t-1})^2+10^{-4}u_t^2
\right].
\]

The fiscal authority's loss is

\[
L_F=\sum_{t=0}^{T-1}\beta^t\left[
0.25\pi_{t+1}^2+x_{t+1}^2+0.45b_{t+1}^2
+0.08(g_t-g_{t-1})^2+10^{-4}g_t^2
\right],
\]

with `beta = 0.99`. These expressions are used unchanged in every case.

- **Nash:** each authority chooses its entire 20-quarter instrument path taking
  the other path as given.
- **Cooperation:** a planner minimises `0.5 L_M + 0.5 L_F`. The 50–50 number is a
  planner aggregation weight, not a change in either primitive objective.
- **Central-bank leadership:** the central bank commits to a path after
  substituting the fiscal authority's full best-response function.
- **Fiscal leadership:** the fiscal authority performs the symmetric exercise.

Because transitions are linear and losses quadratic, the paths are found by
solving linear first-order conditions. No numerical optimiser is required.

## Policy-rule regimes

The fiscal-dominance exercise is intentionally separate from the strategic
solution concepts. The same macro parameters and same two losses evaluate both
regimes.

| Rule parameter | Monetary dominance | Fiscal dominance |
|---|---:|---:|
| Nominal rate response to inflation | 1.50 | 0.80 |
| Fiscal tightening response to debt | 0.35 | 0.00 |
| Monetary debt accommodation | 0.00 | 0.10 |

The first column represents active monetary/passive fiscal policy: the nominal
rate satisfies the Taylor principle and fiscal policy stabilises debt. The
second represents passive monetary/active fiscal policy: the nominal inflation
coefficient is below one and fiscal policy does not adjust to debt.

Since the model's monetary instrument is the **real** rate gap, its inflation
coefficient is the nominal coefficient minus one. Consequently, passive
monetary policy can produce a declining real rate when inflation rises. The
additional debt-accommodation coefficient makes the servicing-cost channel
explicit.

## Current Australian estimates

| Parameter | Estimate | Meaning |
|---|---:|---|
| `rho_x` | 0.629 | Output-gap persistence |
| `sigma_i` | 0.0369 | Real-rate effect on the output gap |
| `chi_g` | 0.0514 | Public-demand effect on the output gap |
| `rho_pi` | 0.563 | Inflation-gap persistence |
| `kappa` | 0.155 | Phillips-curve output coefficient |
| Demand residual SD | 0.464 | Output-gap percentage points |
| Supply residual SD | 0.670 | Annualised inflation percentage points |

The IS regression has 107 usable quarterly observations and the Phillips curve
has 106. Ordinary least squares currently produces the required economic signs;
the Julia code checks those signs and stops if a future data vintage reverses
one. It does not silently clip coefficients.

## Reading the output

For the one-standard-deviation demand contraction, the fixed-preference Nash
joint loss is approximately `0.179`, compared with `0.168` under cooperation.
This is the free-riding mechanism in the original draft: both instruments can
support demand, so each authority has an incentive to leave more adjustment to
the other.

For an inflation shock, the fiscal authority initially supports activity while
the central bank tightens, so the two instruments partly offset. Leadership
changes which authority carries more of the adjustment, but the loss ranking is
conditional on the calibration and shock.

In the policy-rule fiscal stress experiment, monetary dominance reduces the
debt gap from `1.0` initially to about `0.445` after 20 quarters. Under fiscal
dominance it remains around `0.668`, because fiscal policy does not respond to
debt and monetary policy is passive. Both paths are evaluated with the same
loss functions.

## Scope

The implementation nests the strategic accommodation channel of fiscal
dominance, not the full fiscal theory of the price level. The detailed boundary
between the three-equation core, the strategic game and the FTPL extension is
set out in [THREE_EQUATION_MODEL.md](THREE_EQUATION_MODEL.md).
