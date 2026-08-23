# Model 16: three-agent toy search-and-matching model

**Paper:** COVID-19 income support and labour-supply transitions in Australia  
**Model file:** `16_three_agent_toy_model.jl`  
**Comparison runner:** `compare_models_15_16.jl`  
**Prepared:** 11 August 2026  
**Purpose:** Explain the structure, empirical mapping, calibration and interpretation of model 16; document the changes from model 15; and assess whether a broader full-economy search-and-matching model is required.

## Executive summary

Model 16 is a deliberately parsimonious three-agent model designed to interpret the paper's matched difference-in-differences estimates. It is not a population model and does not attempt to explain selection into the empirical sample.

The three structural agents are:

1. an Australian who receives the base JobSeeker payment and the Coronavirus Supplement (`AB`);
2. an Australian who does not receive JobSeeker or the Supplement (`A0`); and
3. a New Zealander who does not receive JobSeeker or the Supplement (`NZ`).

All agents are homogeneous within these groups. The only structural group differences are benefit receipt and a possible post-pandemic New Zealand differential in the nonpecuniary cost of work. This differential is a reduced-form representation of health risk, family support, caring responsibilities, migration selection or preferences for risk. It is not interpreted literally as an identified health effect.

The empirical analysis contains four observed series because there are two separately matched New Zealand panels: New Zealanders matched to Australian JobSeeker recipients, and New Zealanders matched to Australian non-recipients. Model 16 does **not** turn these two measurements into two New Zealand agents. Instead, it gives them time-invariant measurement offsets while requiring them to share one structural New Zealand value function and one post-period response. Their slightly different estimated post changes are reported as overidentifying residuals.

This structure gives the two empirical comparisons distinct roles:

- the non-recipient Australian–New Zealand placebo difference-in-differences disciplines the possible New Zealand differential work-cost shock; and
- the recipient Australian–New Zealand difference-in-differences disciplines the benefit mechanism after accounting for common shocks and the differential New Zealand response.

The preferred combined model reproduces the main job-finding DiD at -1.576 percentage points, compared with -1.710 in the data, and the main separation DiD at 3.727 percentage points, compared with 3.720 in the data. The two empirical New Zealand job-finding Post coefficients are -1.79 and -1.38 percentage points; the model imposes one shared response of -1.66 percentage points.

The Shapley decomposition of the preferred model attributes -1.766 percentage points of the main job-finding DiD to the benefit change, +0.560 percentage points to common shocks and -0.370 percentage points to the New Zealand differential. For separations, the corresponding contributions are +3.739, -0.013 and +0.001 percentage points. These are model-based mechanism calculations, not separately identified causal estimates.

The central change from model 15 is conceptual. Model 15 fitted a recipient-specific differential health/work-disutility wedge. Model 16 instead uses the untreated Australian placebo comparison to discipline whether New Zealanders responded differently. Because the placebo estimates are close to zero and statistically imprecise, the model no longer assigns most of the main treatment effect to a freely fitted recipient-specific pandemic shock.

## 1. What model 16 is intended to answer

The model's question is:

> Can changes in the value of non-employment, together with a common pandemic labour market and a possible differential New Zealand nonpecuniary work-cost shock, account for the observed changes in job finding and job separation among the paper's selected empirical groups?

The model is a mechanism and accounting device. It is appropriate for answering:

- how much of the main recipient–New Zealand DiD can be rationalised by the Supplement;
- how much is associated with common job-opportunity and employer-separation shocks;
- whether the placebo comparison supports a large nationality-specific pandemic wedge;
- how benefits can affect both job finding and match continuation; and
- whether the results require search, offer acceptance, or both.

It is not designed to estimate:

- the aggregate employment effect of the Supplement;
- population unemployment or vacancy dynamics;
- general-equilibrium wage or vacancy responses;
- an optimal benefit level;
- aggregate fiscal costs or welfare; or
- selection into the study sample.

The model should consequently be described in the paper as conditional on the selected empirical population: people satisfying the citizenship, residence, matching and non-employment-spell restrictions used in the empirical analysis.

## 2. Three structural agents and four empirical series

### 2.1 Structural agents

| Agent | Citizenship | JobSeeker before the Supplement | Coronavirus Supplement | Post differential work-cost shock |
|---|---|---|---|---|
| `AB` | Australian | Yes | Yes | No |
| `A0` | Australian | No | No | No |
| `NZ` | New Zealand | No | No | Estimated from placebo comparison |

The Australian recipient receives a pre-period benefit normalised to 28 per cent of the model wage. During the high-Supplement period this is doubled. The two non-recipient agents receive no JobSeeker benefit in the model.

Australian non-recipients and New Zealanders are structurally identical before the post-period New Zealand differential is introduced. This is intentional: without benefit receipt or the differential work cost, nationality by itself does not change preferences or technology.

### 2.2 Why two matched NZ samples are not two agents

The empirical analysis constructs:

- `NZ-main`: New Zealanders matched on observables to Australian JobSeeker recipients; and
- `NZ-placebo`: New Zealanders matched on observables to Australian non-recipients.

These samples have different observable compositions because they are matched to different Australian populations. Their pre-period job-finding rates are 10.02 and 9.48 per cent respectively. Their pre-period separation rates are both 4.80 per cent.

These are two measurements of how a common New Zealand structural type appears after two different matching/reweighting exercises. Creating two value functions or separate post shocks would incorrectly interpret sample construction as economic preference heterogeneity.

For outcome `y`, panel `k` and period `t`, the model uses the observation equation:

`y_hat_NZ,k,t = y_NZ,t + alpha_k + epsilon_k,t`.

The panel offset `alpha_k` is time invariant. It preserves the different matched pre-period levels but cancels from the Post coefficient and DiD. Both panels are therefore required to share:

`Delta y_NZ = y_NZ,post - y_NZ,pre`.

The two empirical estimates of that change need not be numerically identical because they are estimated with sampling error and use different matched compositions. Model 16 fits a common response between them and reports the residual against each estimate.

This is a measurement structure, not a fourth agent.

### 2.3 Exact empirical cells

The cell levels implied by the paper's regressions are:

| Outcome | Empirical panel and group | Pre | Post | Change |
|---|---|---:|---:|---:|
| Job finding | NZ matched to recipients | 0.1002 | 0.0823 | -0.0179 |
| Job finding | Australian recipient | 0.0868 | 0.0518 | -0.0350 |
| Job finding | NZ matched to non-recipients | 0.0948 | 0.0810 | -0.0138 |
| Job finding | Australian non-recipient | 0.0981 | 0.0832 | -0.0149 |
| Separation | NZ matched to recipients | 0.0480 | 0.0550 | +0.0070 |
| Separation | Australian recipient | 0.0484 | 0.0926 | +0.0442 |
| Separation | NZ matched to non-recipients | 0.0480 | 0.0540 | +0.0060 |
| Separation | Australian non-recipient | 0.0505 | 0.0529 | +0.0024 |

The principal DiDs are:

| Comparison | Job-finding DiD | Separation DiD | Role in model 16 |
|---|---:|---:|---|
| Recipient Australian versus matched NZ | -0.0171 | +0.0372 | Benefit mechanism plus any NZ differential |
| Non-recipient Australian versus matched NZ | -0.0011 | -0.0036 | Placebo discipline for NZ differential shock |

The placebo interactions are statistically insignificant. This matters structurally: they provide little evidence for a large nationality-specific shock and should not be converted into an exactly fitted difference without acknowledging their uncertainty.

## 3. Economic environment

### 3.1 Timing

The model is weekly and contains 60 weeks. The high-Supplement/pandemic period runs from model week 12 through week 36. It is a finite anticipated episode: agents solve backward while anticipating a return to the pre-period environment.

This timing is designed to match the principal pre/post high-Supplement exercise. It does not yet model the later $250 and $150 Supplement phases. Those phases should be added only when corresponding empirical transition moments are brought into the calibration or used as validation.

### 3.2 Discounting

Model 15 used `beta = 0.99` in a weekly model, implying an annual factor of approximately 0.593. Model 16 starts from an annual factor of 0.96 and converts it to weekly frequency:

`beta_weekly = 0.96^(1/52) = 0.999215`.

This materially changes the scale of value surpluses and search-cost parameters. Consequently, the numerical `kappa` estimates in models 15 and 16 should not be compared as if they had the same scale.

### 3.3 Benefits and nonpecuniary work costs

Let `b_g,t` be out-of-work benefit income and `h_g,t` the nonpecuniary cost of working. Flow values are:

`outside_g,t = b_g,t + leisure_value`,

and

`work_flow_g,t = wage - h_g,t`.

For `AB`, the pre benefit is 0.28 times the wage and the post benefit is twice that value. For `A0` and `NZ`, the modelled JobSeeker benefit is zero.

The New Zealand differential is introduced only in the post period:

`h_NZ,post = h_A,post + Delta h_NZ`.

Australians share the same normalised work-cost baseline. `Delta h_NZ` can be positive or negative. A negative estimate means that working is relatively less costly for New Zealanders than for Australians during the post period. This could represent migration selection, differences in extended-family exposure, caring responsibilities or preferences toward workplace risk. It should not be labelled as a measured health effect.

## 4. Job finding

### 4.1 Labour-market contact

As in model 15, firms' free-entry condition determines market tightness. With productivity `y`, wage `w`, vacancy cost `c_v`, matching efficiency `mu_t` and match elasticity `alpha`, the model calculates a common contact probability from:

`theta_t = [(beta * mu_t * firm_value) / vacancy_cost]^(1/alpha)`,

and

`contact_t = mu_t * theta_t^(1-alpha)`.

`mu_pre` and `mu_post` are estimated from the selected-sample transition moments. Public vacancy evidence is not used to set their levels. The public vacancy collapse is instead an external test of whether the sign and relative movement are plausible.

### 4.2 Offer acceptance

The combined and acceptance-only models use:

`accept_g,t = logistic[(flow_surplus_g,t - offer_threshold) / offer_dispersion]`,

where:

`flow_surplus_g,t = work_flow_g,t - outside_g,t`.

A higher benefit reduces the flow surplus from employment and therefore reduces the fraction of potential offers treated as acceptable.

### 4.3 Search choice

In the combined and search-only specifications, non-employed workers choose search to balance the marginal gain from a job opportunity against a convex search cost:

`cost(s) = kappa * s^(1+eta) / (1+eta)`.

The first-order condition accounts for the probability of at least one opportunity:

`offer_probability = 1 - exp(-contact * search)`.

The job-finding probability is:

`finding_g,t = accept_g,t * [1 - exp(-contact_t * search_g,t)]`.

### 4.4 Three mechanism specifications

The code recalibrates three nested versions:

1. **Combined:** benefits can affect both optimal search and offer acceptance.
2. **Search-only:** all offers are accepted; benefits affect job finding through the value of search.
3. **Acceptance-only:** search is normalised to one; benefits affect job finding through offer acceptance.

The data do not directly measure search or rejected offers, so these models should be interpreted as mechanism comparisons. A lower calibration loss for one channel is suggestive under the chosen functional forms, not nonparametric identification of that channel.

## 5. Job separation

Version 16 replaces the additive separation equation with a competing-risks expression:

`separation_g,t = 1 - (1 - common_destruction_t) * (1 - endogenous_exit_g,t)`.

The endogenous component is:

`endogenous_exit_g,t = sep_base + sep_amp * logistic[(sep_threshold - flow_surplus_g,t) / sep_dispersion]`.

This gives two interpretations:

- `common_destruction_t` represents common employer/background separation risk; and
- the logistic component represents match continuation or separation that is sensitive to the value of employment relative to non-employment.

The competing-risks form ensures that common and endogenous probabilities interact coherently and remain bounded. It also makes the interaction explicit rather than treating it as an unexplained additive residual.

For Australian recipients, the Supplement reduces the flow surplus from continuing employment. If the surplus approaches the continuation threshold, the separation response can be nonlinear even when the pre-period separation difference is small.

## 6. Dynamic value functions

For each group, the employed and non-employed values satisfy:

`W_g,t = work_utility_g,t + beta * [(1-separation_g,t) W_g,t+1 + separation_g,t U_g,t+1]`,

and

`U_g,t = outside_utility_g,t - search_cost_g,t + beta * [finding_g,t W_g,t+1 + (1-finding_g,t) U_g,t+1]`.

The terminal value is the pre-period stationary value. During the finite shock, agents anticipate the end of the high-Supplement/common-shock environment. Before the shock, decisions use the pre-period stationary continuation value.

Because weekly discounting produces slowly converging conventional value iteration, version 16 solves the stationary value-surplus fixed point directly. This is a computational change, not a change in economic content.

## 7. Calibration and identification

### 7.1 Minimum-distance objective

The model fits all 16 regression coefficients from the main and placebo job-finding and separation regressions. It minimises:

`sum_m [(model_m - estimate_m) / standard_error_m]^2`.

The published standard errors are used as diagonal weights. The current objective therefore ignores covariance across coefficients and the serial correlation implicit in weekly aggregate outcomes. When a covariance matrix is available from the secure empirical work, the objective should be replaced with full minimum distance.

Unlike model 15, version 16 does not force exact fit to each cell. This is particularly important for the two New Zealand panels because a one-agent model must not create separate structural changes solely to hit both estimates exactly.

### 7.2 Mapping from moments to model objects

| Model object | Principal discipline | Interpretation |
|---|---|---|
| `mu_pre`, `kappa` | Pre job-finding levels and gaps | Baseline selected-sample opportunity/search scale |
| `mu_post` | Australian non-recipient and both NZ Post changes | Common selected-sample opportunity shock |
| `common_sep_shock` | Australian non-recipient and both NZ separation changes | Common employer/background separation shock |
| `nz_work_cost_diff` | Non-recipient Australian–NZ placebo DiDs | Differential health/family/risk/migration-selection wedge |
| `sep_base` | Pre-period separation levels | Baseline separation risk |
| `sep_amp` | Recipient separation response after common and NZ shocks | Benefit-sensitive match continuation |
| Benefit multiplier | Institutional normalisation | Doubling of modelled JobSeeker income |
| Search/acceptance/separation shapes | Fixed and compared across nested models | Functional-form sensitivity, not directly identified |

There is residual weak identification. In particular, matching efficiency and search-cost scale can trade off in producing the same job-finding probability, and the placebo estimates only weakly discipline the New Zealand differential. The nested channel results and parameter classifications are therefore part of the required output, not optional diagnostics.

### 7.3 Role of public whole-population evidence

Public vacancy, employer, job-mobility and JobKeeper evidence is deliberately absent from the level objective. Those sources describe a broader population than the selected empirical sample.

They should be used to ask:

- Is `mu_post < mu_pre` consistent with the timing of vacancy advertisements?
- Is the estimated common separation increase directionally consistent with employer disruption?
- Are model counterfactuals consistent with the broad timing of JobKeeper retention?
- Does the model imply offer/contact rates that are plainly outside population benchmarks?

They should not be used to force the selected cohort's weekly hazards to equal aggregate rates.

## 8. Preferred combined-model results

### 8.1 Parameter values

| Parameter | Version-16 value | Status |
|---|---:|---|
| Weekly discount factor | 0.999215 | Fixed from 0.96 annual |
| Pre matching shifter | 0.052450 | Estimated |
| Common search-cost scale | 0.552052 | Estimated, weakly separate from matching scale |
| Post matching shifter | 0.047768 | Estimated |
| Common separation shock | 0.006674 | Estimated |
| NZ differential work cost | -0.042270 | Estimated, weakly disciplined |
| Separation base | 0.047959 | Estimated |
| Separation amplitude | 0.376654 | Estimated |
| Search curvature | 7.0 | Fixed/sensitivity |
| Separation threshold | 0.10 | Fixed/sensitivity |
| Separation dispersion | 0.04 | Fixed/sensitivity |

The matching shifter falls by approximately 8.9 per cent. The common separation shock is approximately 0.67 percentage points before the competing-risks interaction.

The estimated New Zealand differential work cost is negative, equal to approximately 7.7 per cent of the model wage. It is selected largely because the job-finding placebo point estimate indicates a slightly larger fall among Australian non-recipients than their matched New Zealanders. The separation placebo point estimate points imperfectly in the other direction. Neither estimate is statistically significant, so the magnitude and sign should be treated as a sensitivity result rather than a substantive finding about New Zealanders.

### 8.2 Regression-coefficient fit

| Outcome and panel | Coefficient | Data | Model | Gap in standard errors |
|---|---|---:|---:|---:|
| Finding, main | NZ intercept | 0.1002 | 0.0999 | -0.06 |
| Finding, main | Australian pre gap | -0.0134 | -0.0187 | -0.84 |
| Finding, main | NZ Post | -0.0179 | -0.0166 | +0.22 |
| Finding, main | Recipient DiD | -0.0171 | -0.0158 | +0.16 |
| Finding, placebo | NZ intercept | 0.0948 | 0.0945 | -0.04 |
| Finding, placebo | Australian pre gap | 0.0033 | 0.0039 | +0.06 |
| Finding, placebo | NZ Post | -0.0138 | -0.0166 | -0.30 |
| Finding, placebo | Placebo DiD | -0.0011 | -0.0033 | -0.17 |
| Separation, main | NZ intercept | 0.0480 | 0.0480 | approximately 0 |
| Separation, main | Australian pre gap | 0.0004 | 0.0009 | +0.05 |
| Separation, main | NZ Post | 0.0070 | 0.0063 | -0.07 |
| Separation, main | Recipient DiD | 0.0372 | 0.0373 | +0.01 |
| Separation, placebo | NZ intercept | 0.0480 | 0.0480 | approximately 0 |
| Separation, placebo | Australian pre gap | 0.0025 | 0.0000 | -0.32 |
| Separation, placebo | NZ Post | 0.0060 | 0.0063 | +0.05 |
| Separation, placebo | Placebo DiD | -0.0036 | 0.0000 | +0.36 |

All coefficient residuals are below one reported standard error. That good fit does not establish that all parameters are precisely identified: several empirical standard errors are large, and the model uses fixed functional-form parameters.

### 8.3 The shared New Zealand response

The empirical New Zealand job-finding changes are:

- -1.79 percentage points in the panel matched to recipients; and
- -1.38 percentage points in the panel matched to non-recipients.

The single structural New Zealand agent predicts -1.66 percentage points in both panels. Panel offsets reproduce their different pre levels, but cannot create different post changes.

For separation, the two empirical changes are +0.70 and +0.60 percentage points. The model predicts a shared +0.63 percentage-point change.

This is an important feature rather than a lack of fit. It shows exactly how a three-agent homogeneous model reconciles the four empirical series without inventing a fourth agent.

### 8.4 Channel comparison

| Job-finding channel | Weighted loss | Full main finding DiD | Benefit-only main finding DiD | Full main separation DiD |
|---|---:|---:|---:|---:|
| Combined | 1.156 | -1.576 pp | approximately -1.96 pp before other interactions | 3.727 pp |
| Search only | 3.249 | -0.719 pp | smaller than combined | 3.870 pp |
| Acceptance only | 0.672 | -1.438 pp | acceptance-driven | 3.718 pp |

The acceptance-only model has the lowest weighted loss under the current fixed functional forms, while the search-only model generates a substantially smaller recipient job-finding DiD. This is consistent with model 15's difficulty generating a large search response, but it does not prove that observed behaviour operated mainly through rejected offers. Applications and offer acceptance are not observed in the current empirical data.

The preferred reported model remains the combined version because it displays both conventional mechanisms. The search-only and acceptance-only versions should be presented as nested sensitivity exercises.

### 8.5 Shapley decomposition

The code evaluates all eight combinations of:

- the Supplement benefit change;
- common job-finding and separation shocks; and
- the New Zealand differential work-cost shock.

It then averages marginal contributions over all six possible orderings. This produces an order-invariant Shapley decomposition.

| Outcome | Benefit | Common shocks | NZ differential | Full main DiD |
|---|---:|---:|---:|---:|
| Job finding | -1.766 pp | +0.560 pp | -0.370 pp | -1.576 pp |
| Separation | +3.739 pp | -0.013 pp | +0.001 pp | +3.727 pp |

The benefit contribution to job finding is larger in magnitude than the full DiD because common shocks partly offset the relative effect through nonlinearities. Shares therefore need not lie between zero and one. The decomposition should be reported in percentage points, not only as percentages.

The version-16 mechanism differs materially from model 15: under the three-agent/placebo-disciplined structure, benefits account for essentially all of the separation DiD and more than the full job-finding DiD before offsetting common-shock interactions. This is a result of restricting differential shocks with the placebo evidence; it should be presented as conditional on the toy model's structure rather than as a newly identified causal decomposition.

## 9. Changes from model 15

| Feature | Model 15 | Model 16 | Reason for change |
|---|---|---|---|
| Structural agents | Recipient Australian and matched NZ | Recipient Australian, non-recipient Australian and one NZ | Use the placebo comparison as structural discipline |
| Two matched NZ panels | Only main matched NZ panel used | Two measurements of one NZ type | Preserve matching design without inventing heterogeneity |
| Differential post wedge | Recipient-specific health/work-cost wedge fitted to recipient separation | NZ differential work cost disciplined by placebo DiDs | The placebo comparison is the relevant evidence on nationality-specific response |
| Common shock identification | Primarily fitted to main NZ cells | Uses non-recipient Australian and both NZ measurements | Broader untreated information |
| Discounting | 0.99 weekly | 0.96 annual converted to 0.999215 weekly | Frequency consistency |
| Separation aggregation | Additive common and surplus-sensitive components | Competing risks | Coherent probability accounting and explicit interaction |
| Search curvature | Selected at 100 in preferred restricted grid | Fixed at 7 in combined baseline; channels compared | Avoid selecting an extreme curvature solely for fit |
| Search versus acceptance | Combined model with acceptance dominating implicitly | Combined, search-only and acceptance-only models recalibrated and reported | Make mechanism dependence transparent |
| Calibration | Exact/near-exact cell targeting with one held-out recipient finding cell | Standard-error-weighted fit to 16 regression coefficients | Recognise sampling uncertainty and overidentifying restrictions |
| Public population moments | Not systematically separated from model targets | External validation only | Population and selected-sample hazards are not directly comparable |
| Decomposition | Selected scenarios with large nonlinear interaction | Complete factorial scenarios plus Shapley contributions | Order-invariant attribution |
| Parameter documentation | Mixed calibrated/fixed objects | Machine-readable status/evidence table | Clarify identification |

### 9.1 What has not changed

Model 16 preserves the central model-15 logic:

- employment and non-employment values are forward looking;
- higher benefits raise the value of non-employment;
- benefits can reduce job finding through search and offer selectivity;
- benefits can increase separation by reducing match continuation surplus;
- firms create vacancies under a matching/free-entry condition;
- the pandemic affects common labour-market opportunities; and
- agents anticipate that the shock is temporary.

The rewrite changes the empirical mapping and discipline, not the central economic mechanism.

## 10. Interpretation and limitations

### 10.1 Conditional study population

The model is calibrated to flows among people selected into the empirical study, including the requirement that they experience a sustained spell out of employment. It does not generate that selection condition.

Results should be described as applying to workers resembling the matched study groups, not to the entire Australian workforce. Population vacancy and separation levels should not be placed in the same moment table as if they shared a denominator.

### 10.2 Differential work cost is weakly identified

The placebo DiDs are close to zero and imprecise. They usefully prevent the model from freely assigning a large differential shock, but they do not precisely identify its sign or magnitude.

The preferred paper presentation should therefore include:

- the estimated value;
- a zero-differential specification;
- a range based on the placebo confidence region; and
- the effect of this range on benefit attribution.

### 10.3 Search and acceptance are mechanism scenarios

The current empirical outcomes observe transitions, not applications, offers or rejected offers. Search-only, acceptance-only and combined results should be treated as alternative structural interpretations.

A claim that most of the response operated through offer acceptance would require direct evidence on offers or rejections. A claim about “effective job-finding behaviour” is supportable with the current data.

### 10.4 Functional-form sensitivity

The large recipient separation response is generated by the nonlinear continuation threshold. The following should be varied before paper submission:

- separation threshold and dispersion;
- search curvature;
- offer threshold and dispersion;
- annual discount factor;
- benefit replacement rate; and
- shock duration and anticipation.

Results should be reported as ranges if these choices materially alter the decomposition while preserving fit.

### 10.5 Empirical covariance

The regressions report IID standard errors, and the version-16 objective treats coefficient errors as independent. Weekly outcome series are likely serially correlated, and coefficients within a regression are correlated.

The secure empirical work should provide a covariance matrix across the structural moments. Until then, the numerical weighted loss is a fit diagnostic rather than a formal statistical test.

## 11. Files and reproducibility

Running `16_three_agent_toy_model.jl` produces:

- `16_three_agent_regression_moments.csv`: all main and placebo regression estimates, standard errors, predictions and standardised residuals;
- `16_three_agent_cells.csv`: implied pre/post empirical cells and model predictions;
- `16_three_agent_path.csv`: weekly values, choices and transitions for all three agents;
- `16_three_agent_scenarios.csv`: the full eight-scenario factorial design;
- `16_three_agent_shapley.csv`: order-invariant contribution calculations;
- `16_three_agent_parameters.csv`: parameter values, status and evidence source;
- `16_three_agent_identification.csv`: mapping between agents, empirical panels and structural objects; and
- `16_channel_comparison.csv`: recalibrated combined, search-only and acceptance-only specifications.

Running `compare_models_15_16.jl` runs both model scripts in separate Julia processes and produces:

- `15_16_moment_comparison.csv`; and
- `15_16_parameter_comparison.csv`.

Model 15 remains unchanged.

## 12. Recommended next steps

### Immediate model work

1. Add a sensitivity grid around search, acceptance and separation shape parameters.
2. Recalculate parameter sets using the secure empirical covariance matrix when available.
3. Add a zero-NZ-differential benchmark and a placebo-confidence-region profile.
4. If later 2020/2021 moments are supplied, implement the $250 and $150 Supplement phases and test whether the model predicts the response to benefit reductions.
5. Compare common-shock timing with public vacancy and employer series without using their levels as selected-sample targets.

### Immediate empirical inputs

1. Confirm the precise week definitions behind the main and placebo regressions.
2. Export the covariance matrix of the intercept, Australian, Post and interaction coefficients for all four regressions.
3. Report whether the same individuals and calendar weeks are used in the main and placebo panels.
4. Provide actual mean JobSeeker payments or replacement rates for recipients before and during the Supplement.
5. If possible, provide JobKeeper exposure separately for the recipient, non-recipient and matched NZ samples.

## End-note: Is a broader full-economy SAM model required?

### The toy model is appropriate for the current paper's mechanism question

A broader full-economy search-and-matching model is **not required** to use the structural exercise as a device for assessing the relative contributions of benefits and other shocks.

The empirical contribution is a matched natural experiment on transition rates for a selected population. The structural role is to organise those results and demonstrate how benefit generosity can affect both entry into and exit from employment under a common pandemic shock. The three-agent toy model is well aligned with that objective because:

- its agents correspond directly to the treatment and placebo comparisons;
- its common and differential shocks have transparent empirical counterparts;
- its assumptions are visible;
- its counterfactuals remain close to the variation in the data; and
- its limitations can be stated clearly.

For this purpose, adding a representative firm sector, endogenous unemployment stocks, aggregate vacancy feedback, fiscal closure and worker distributions could make the mechanism harder to audit without creating additional identification. A larger model is not automatically more credible when the available empirical moments remain group-level transition rates.

### When a full-economy model would become necessary

A broader model would be required if the paper wished to make claims about:

- aggregate employment or unemployment effects;
- equilibrium vacancy creation and wage responses;
- spillovers from recipient behaviour to non-recipients;
- the fiscal cost of the Supplement;
- population-wide welfare or optimal benefit design;
- distributional welfare across workers, firms and taxpayers;
- congestion externalities in matching; or
- how JobKeeper and JobSeeker jointly affected economy-wide equilibrium.

Those questions require population shares, worker heterogeneity, firm heterogeneity, government budget accounting and a mapping from the selected empirical sample to the full labour market. The currently available empirical design does not identify those objects.

### Recommended paper strategy

Use the three-agent model in the main paper as a disciplined mechanism exercise. State explicitly that it is a partial-equilibrium model of the selected empirical groups and that its decomposition is conditional on the model's functional forms.

A broader SAM framework could be outlined briefly as a nesting interpretation or future extension: the three agents in model 16 could be embedded as worker categories inside a full model with endogenous vacancies and government finance. There is no need to build that larger framework for the present submission unless the paper expands its claims to aggregate equilibrium or welfare.

The relevant standard is not whether the model is “large enough.” It is whether the model is proportionate to the empirical variation and transparent about what is identified. For assessing the relative contribution of benefits, common labour-market shocks and a placebo-disciplined differential response, the version-16 toy model is the more appropriate primary device.
