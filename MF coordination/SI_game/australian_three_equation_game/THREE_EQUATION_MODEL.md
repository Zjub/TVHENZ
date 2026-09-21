# A three-equation macro model of monetary–fiscal coordination

## 1. Purpose

The original monetary–fiscal coordination model maps the two policy instruments
directly into output and inflation. It is effective for demonstrating strategic
substitution, but it does not say when policy affects activity, when activity
affects inflation, or how current choices constrain later choices.

A three-equation macro framework supplies that propagation mechanism. The
policy game then supplies the reaction functions that normally close the model.
To discuss fiscal dominance rather than ordinary policy conflict, one further
ingredient is unavoidable: the government's budget constraint and a fiscal
response to debt.

The resulting structure should therefore be read as:

1. a conventional three-equation core;
2. a two-authority strategic replacement for the usual single monetary rule;
3. a debt equation and fiscal rule for the fiscal-dominance extension.

## 2. The conventional three-equation core

A standard New Keynesian three-equation model contains an IS curve, a Phillips
curve and a monetary policy rule.

### 2.1 Forward-looking IS curve

\[
x_t=E_t x_{t+1}-\sigma\left(i_t-E_t\pi_{t+1}-r_t^*\right)
+\chi g_t+\varepsilon_t^d.
\]

The output gap `x_t` rises when the expected real interest rate falls, when
fiscal demand `g_t` rises, or when a positive demand shock occurs. This equation
places monetary and fiscal instruments in the same aggregate-demand channel,
which generates the strategic substitution emphasised in the original model.

### 2.2 New Keynesian Phillips curve

\[
\pi_t=\beta E_t\pi_{t+1}+\kappa x_t+\varepsilon_t^s.
\]

Inflation rises when expected future inflation rises, when activity exceeds
potential, or when an adverse supply shock occurs. Monetary and fiscal policy
affect inflation indirectly by changing the output gap.

### 2.3 Monetary policy rule

\[
i_t=\rho_i i_{t-1}+(1-\rho_i)\left[
r_t^*+E_t\pi_{t+1}+\phi_\pi\pi_t+\phi_xx_t
\right]+\varepsilon_t^m.
\]

This third equation ordinarily closes the model. Under active monetary policy,
the nominal interest rate rises more than one-for-one with inflation. The real
rate therefore rises and stabilises demand.

## 3. How the strategic game changes the third equation

The objective of this project is not to assume a monetary rule while treating
fiscal policy as exogenous. Both authorities choose policy. The third equation
is therefore replaced, in the game experiments, by the first-order conditions
of two fixed loss functions.

Let `u_t` be the real cash-rate gap, where a positive value is monetary
tightening, and let `g_t` be the public-demand gap, where a positive value is
fiscal expansion. The central bank minimises

\[
L_M=\sum_{t=0}^{T-1}\beta^t\left[
q_\pi^M\pi_{t+1}^2+q_x^Mx_{t+1}^2+q_b^Mb_{t+1}^2
+r_M(u_t-u_{t-1})^2+\epsilon u_t^2
\right],
\]

and the fiscal authority minimises

\[
L_F=\sum_{t=0}^{T-1}\beta^t\left[
q_\pi^F\pi_{t+1}^2+q_x^Fx_{t+1}^2+q_b^Fb_{t+1}^2
+r_F(g_t-g_{t-1})^2+\epsilon g_t^2
\right].
\]

All `q`, `r`, `beta` and target values are primitives. They remain fixed when
the timing or solution concept changes.

The instrument adjustment terms do two jobs. Economically, they represent the
cost of reversing a communicated interest-rate path or changing fiscal policy
outside the budget cycle. Strategically, they make today's instrument a state
for tomorrow, so current actions can influence the other authority's future
choices. This is the dynamic precommitment mechanism in the original write-up.

## 4. The Australian empirical counterpart

The runnable example uses a backward-looking version that can be estimated
transparently from a short quarterly dataset:

\[
x_{t+1}=\rho_xx_t-\sigma_i u_t+\chi_g g_t+\varepsilon_{t+1}^d,
\]

\[
\pi_{t+1}=\rho_\pi\pi_t+\kappa x_{t-1}+\varepsilon_{t+1}^s.
\]

The two-quarter output-to-inflation delay is selected because it gives the
expected positive Phillips-curve slope in the Australian data. The model is
written in gaps, so the estimation intercepts are omitted from impulse
responses around the sample-centred steady state.

The estimates use chain-volume GDP, public demand, trimmed-mean inflation and
the cash-rate target from the RBA's statistical tables. Output and public-demand
gaps are two-sided HP-filter deviations from trend. The ex-post real cash rate
is centred on its 1993–2019 mean.

This is deliberately a descriptive calibration. A causal estimate would need
instruments or identified policy shocks, real-time estimates of potential
output and a richer treatment of fiscal composition.

## 5. Strategic solution concepts

Stack the complete monetary path and fiscal path as

\[
w=(u_0,\ldots,u_{T-1},g_0,\ldots,g_{T-1})'.
\]

Because the transition equations are linear, the stacked state path is affine:

\[
s=a+Bw.
\]

Substituting this mapping into either loss gives a quadratic form

\[
L_j(w)=w'H_jw+2h_j'w+c_j.
\]

The simultaneous open-loop Nash equilibrium combines the central bank's
derivatives with respect to `u` and the fiscal authority's derivatives with
respect to `g`:

\[
\begin{bmatrix}
H^M_{uu} & H^M_{ug}\\
H^F_{gu} & H^F_{gg}
\end{bmatrix}
\begin{bmatrix}u\\g\end{bmatrix}
=-
\begin{bmatrix}h^M_u\\h^F_g\end{bmatrix}.
\]

Cooperation minimises an explicit social aggregation of the same two losses.
Leadership first derives the follower's affine best-response path and then
substitutes it into the leader's unchanged loss. This makes differences across
the four cases differences in strategic timing, not differences in preferences.

### Demand shocks

After a negative demand shock, lower real rates and higher public demand are
substitutable ways to close the output gap. Each independent authority prefers
the other to bear more adjustment cost. The Nash response can therefore provide
less combined stabilisation than cooperation—the draft's “GFC effect.”

### Supply shocks

After an inflationary supply shock, the output-oriented fiscal authority tends
to support demand while the inflation-oriented central bank tightens. Their
instruments then offset each other. Adjustment costs and leadership determine
which authority moves first and how persistent that offset becomes—the draft's
“post-COVID effect.”

## 6. Why fiscal dominance requires more than three equations

Neither the standard three-equation model nor the original coordination game
contains fiscal solvency. Fiscal dominance cannot be defined without public
liabilities and a rule describing whether fiscal policy adjusts to them.

The example therefore adds a linearised debt equation:

\[
b_{t+1}=\rho_bb_t+\psi_gg_t+\psi_iu_t-\psi_\pi\pi_t
+\varepsilon_{t+1}^b.
\]

Fiscal expansion raises debt. Monetary tightening raises servicing costs.
Inflation reduces the real value of nominal debt. This equation is the minimum
reduced-form bridge between the strategic game and fiscal-dominance mechanisms.

For the active/passive comparison, policies follow explicit rules:

\[
u_t=(\phi_\pi^{nom}-1)\pi_t+\phi_xx_t+\rho_u u_{t-1}
-\phi_bb_t,
\]

\[
g_t=-\gamma_bb_t-\gamma_xx_t+\rho_gg_{t-1}.
\]

The subtraction of one in the monetary rule converts a nominal interest-rate
coefficient into a real-rate response.

| Policy mix | Monetary rule | Fiscal rule | Interpretation |
|---|---|---|---|
| Monetary dominance | `phi_pi_nom = 1.5` | `gamma_b = 0.35` | Active money, passive fiscal policy |
| Fiscal dominance | `phi_pi_nom = 0.8` | `gamma_b = 0.00` | Passive money, active fiscal policy |

This is the Leeper classification: one authority actively pursues its target
while the other accepts the adjustment needed to stabilise the system. The
regime parameters are institutional or behavioural primitives distinct from
the weights in either loss function.

## 7. What is and is not nested

The Julia extension nests three ideas from the fiscal-dominance literature:

1. the active-monetary/passive-fiscal versus passive-monetary/active-fiscal
   policy mix;
2. the debt-service channel through which tight money worsens fiscal pressure;
3. monetary accommodation when fiscal policy does not stabilise debt.

It does not yet nest FTPL price-level determination. The current model is
backward-looking and its debt equation is a real reduced form. A literal FTPL
model needs the nominal consolidated-government valuation equation

\[
\frac{B_{t-1}}{P_t}=E_t\sum_{j=0}^{\infty}Q_{t,t+j}s_{t+j},
\]

model-consistent expectations, debt maturity, transversality conditions and a
determinacy analysis under all four active/passive combinations.

Accordingly, the fiscal-dominance figure is a mechanism experiment, not an
empirical claim that Australia is currently fiscally dominant. The correct next
step is to replace the backward-looking macro block with the forward-looking
equations in Section 2 and solve the rational-expectations model under the two
policy-rule configurations.

## 8. Relationship to the Australian literature

The model's objective-function-plus-macro-equations structure follows the
tradition of Debelle and Stevens' small Australian policy model. The RBA's
neutral-rate work provides an Australian three-equation empirical reference,
while MARTIN is the natural richer macroeconometric benchmark. The debt and
policy-rule extension is also directly comparable to the government budget,
debt-stabilising transfer rule and Taylor rule in the recent Australian HANK
model.

Useful starting points are:

- [Debelle and Stevens (1995), *Monetary Policy Goals for Inflation in
  Australia*](https://www.rba.gov.au/publications/rdp/1995/9503/framework.html)
- [McCririck and Rees (2017), *The Neutral Interest
  Rate*](https://www.rba.gov.au/publications/bulletin/2017/sep/2.html)
- [Ballantyne et al. (2019), *MARTIN Has Its
  Place*](https://www.rba.gov.au/publications/rdp/2019/2019-07/sections.html)
- [Chipeniuk, Nolan and Nolan (2025), *HANK and the Transmission of Shocks to
  Demand and Supply*](https://www.rba.gov.au/publications/rdp/2025/2025-04/model.html)
- [Dixit and Lambertini (2003), *Interactions of Commitment and Discretion in
  Monetary and Fiscal Policies*](https://www.aeaweb.org/articles?id=10.1257%2F000282803322655428)
- [Leeper (1991), *Equilibria under Active and Passive Monetary and Fiscal
  Policies*](https://doi.org/10.1016/0304-3932(91)90007-B)
