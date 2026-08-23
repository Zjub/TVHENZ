# Public identifying moments audit and recommended next steps for model 16

**Paper:** COVID-19 income support and labour-supply transitions in Australia  
**Model audited:** `15_structural_separation_model.jl`  
**Prepared:** 11 August 2026  
**Purpose:** Determine whether additional identifying moments are already publicly available before redesigning the structural model, and set out a defensible sequence for model 16.

## Executive assessment

The public-data search changes the appropriate design of model 16, but it does not solve the central identification problem.

Several useful public series are available and should be used. In particular, Jobs and Skills Australia's Internet Vacancy Index (IVI) can discipline the time path of employment opportunities; its Recruitment Experiences and Outlook Survey (REOS) can discipline the timing and broad magnitude of employer-side disruption, recruitment difficulty and applicant congestion; ABS Participation, Job Search and Mobility (PJSM) tables can provide broad benchmarks for offer incidence and search methods; ABS Job Mobility can bound the mix of voluntary and involuntary job endings; and the Treasury and RBA JobKeeper studies can discipline the timing and plausible magnitude of employment retention attributable to JobKeeper.

These sources do **not** contain the paper's treatment contrast. They do not provide weekly search effort, applications, offers, acceptance decisions, or separation reasons for Australian JobSeeker recipients versus matched ineligible New Zealand citizens. Consequently, they cannot separately identify all of the search, acceptance and separation margins currently represented in model 15.

The most important conclusion is therefore:

> Model 16 should use public data to impose external restrictions on the opportunity and employer-destruction paths, but it should collapse search effort and offer acceptance into a single effective job-finding margin unless new secure-data moments directly distinguish them.

This is a more credible design than estimating a rich set of latent mechanisms from eight aggregate transition moments. It preserves the economic content needed for the paper while making transparent what the quasi-experiment identifies, what is externally calibrated, and what remains set-valued.

The recommended immediate sequence is:

1. Build an auditable public-data input layer for vacancies, employer disruption and JobKeeper retention.
2. Produce a short, prioritised set of additional moments in the secure environment, especially duration-specific hazards, actual JobKeeper exposure, local/occupation vacancy exposure and employer-driven versus idiosyncratic separations.
3. Agree on a parsimonious model-16 architecture in which public opportunity paths are inputs rather than parameters chosen to fit the same transition moments.
4. Only then code and compare models 15 and 16. The comparison should present identified sets and sensitivity ranges, not only a best-fitting point calibration.

## 1. What model 15 currently asks the data to identify

### 1.1 Existing empirical targets

Model 15 contains eight primary transition moments: weekly job-finding and separation rates for the recipient group (`R`, Australian JobSeeker recipients) and the comparison group (`N`, ineligible New Zealand citizens), before and after the policy/shock.

| Moment | Data target | Model-15 value | Difference |
|---|---:|---:|---:|
| NZ job-finding, pre | 0.1002 | 0.100211 | +0.000011 |
| Recipient job-finding, pre | 0.0868 | 0.086781 | -0.000019 |
| NZ job-finding, post | 0.0823 | 0.082300 | approximately 0 |
| Recipient job-finding, post | 0.0518 | 0.055811 | +0.004011 |
| NZ separation, pre | 0.0480 | 0.048000 | 0 |
| Recipient separation, pre | 0.0484 | 0.048400 | 0 |
| NZ separation, post | 0.0550 | 0.055000 | approximately 0 |
| Recipient separation, post | 0.0926 | 0.092600 | approximately 0 |

The preferred restricted calibration deliberately holds out the post-period recipient job-finding rate. It predicts 5.581 per cent against a data target of 5.180 per cent, an error of 0.401 percentage points. The model job-finding difference-in-differences is -1.306 percentage points against -1.710 in the data.

That held-out performance is useful evidence. It should remain a central comparison metric. However, it does not by itself show that the internal allocation across search effort, contact opportunities and offer acceptance is identified.

### 1.2 Latent components and calibrated wedges

The restricted model-15 calibration also selects or fits:

- pre- and post-period matching/contact shifters (`mu_pre = 0.07394`, `mu_post = 0.06772`);
- a common search-cost parameter (`kappa = 0.34735`);
- a search curvature at the upper grid value (`100`);
- a separation threshold of `0.10` and dispersion of `0.04`;
- a common post-period separation shock of `0.007`;
- a recipient-specific health/work-disutility shock of `0.03682`, or 6.69 per cent of the model wage;
- an offer-acceptance threshold and dispersion set outside the target moments; and
- a weekly discount factor of `0.99`.

The upper-bound search curvature is an identification warning. Search choices in the reported scenarios are close to one, so most of the recipient job-finding response is generated through the offer-acceptance channel rather than a quantitatively meaningful search-effort response. Without an independent moment on applications, offers or acceptance, the allocation between those margins is determined largely by functional form and calibration bounds.

The separation decomposition raises a parallel issue. Model 15 attributes a large benefit-only effect to separation (+1.688 percentage points), a small differential-COVID effect (+0.061 percentage points), and then reaches the full +3.720 percentage-point data effect through a large nonlinear interaction. A decomposition dominated by an interaction can be economically valid, but here it is especially sensitive to the fitted separation threshold, dispersion and health wedge. The empirical transition moments do not independently locate those objects.

### 1.3 Parameters that need temporal correction or stronger justification

The discount factor is currently `0.99` in a weekly model. This implies an annual discount factor of approximately `0.99^52 = 0.593`. Model 16 should start from an economically defensible annual factor and convert it to weekly frequency. For example, annual factors of 0.95 and 0.96 imply weekly factors of approximately 0.999014 and 0.999215 respectively. Sensitivity over a conventional annual range should be reported.

The model also uses a single rectangular post period. Before final calibration, the empirical and model timing must be reconciled exactly: announcement, implementation, JobKeeper introduction and eligibility, mutual-obligation changes, lockdown phases, and the September Supplement reduction are distinct events. Public series are monthly, whereas the paper's transitions are weekly, so the aggregation convention and weights must be documented rather than silently interpolated.

## 2. Source-by-source public-data audit

### 2.1 Summary rating

| Public source | What it measures | Frequency / coverage | Best role in model 16 | Key limitation | Priority |
|---|---|---|---|---|---|
| JSA Internet Vacancy Index | Online job advertisements | Monthly; national, state, occupation; 2006 onward | Exogenous relative opportunity/contact path | Online ads are not all vacancies; no treatment-group split | Essential |
| JSA REOS 2020 report | Recruitment, staffing reductions, stand-downs, hours, applicants per vacancy, difficulty | Mostly monthly in 2020; employer survey | Employer-side shock and congestion validation | Employer shares, not worker hazards; detailed recruitment mainly Aug-Dec | High |
| ABS PJSM February 2020/2021 | Search methods, offers, difficulties, successful job starters | Annual February snapshot / last-12-month questions | Offer-incidence and search-method admissibility checks | No benefit/citizenship split; stock-flow selection; no weekly acceptance rate | Medium |
| ABS Household Impacts September 2020 | Active search, Supplement and JobKeeper receipt in same questionnaire | Survey wave, 11-21 September | Possible custom cross-tab or microdata moment | Public tables do not cross search with payment receipt | Conditional |
| ABS Job Mobility February 2021 | Reasons for leaving or losing a job | Annual, year ending February 2021 | Bound voluntary/involuntary separation mixture | All workers; annual; not the paper sample | Medium-high |
| ABS Labour Force detailed / gross flows | Aggregate transitions and duration stocks | Monthly | External timing and aggregate consistency checks | Includes non-participation; public duration stocks are not hazards | Medium |
| Treasury JobKeeper evaluation | Causal employment-retention effects for eligible employee groups | Fortnightly dynamics around eligibility rules | External JobKeeper retention path/range | Effect is not a weekly separation hazard; groups differ from paper | Essential |
| RBA JobKeeper study | Early causal estimate of jobs retained | Pandemic period | Complementary sensitivity benchmark | Different comparison design and smaller migrant samples | High |

### 2.2 Jobs and Skills Australia Internet Vacancy Index

The IVI is the strongest immediately usable public series. It is a monthly count of online advertisements collected from SEEK, CareerOne and Workforce Australia, de-duplicated and published nationally, by state/territory and by two-digit ANZSCO occupation. The principal state/occupation series extends back to January 2006. A regional SA4 series is available from January 2019, although it is published as a three-month moving average and is therefore less suitable for sharp weekly timing.

The seasonally adjusted national series shows the scale and timing of the 2020 opportunity collapse:

| Month in 2020 | Seasonally adjusted advertisements | Index, January 2019 = 100 |
|---|---:|---:|
| January | 167,434 | 90.6 |
| February | 160,338 | 86.7 |
| March | 118,135 | 63.9 |
| April | 67,245 | 36.4 |
| May | 90,332 | 48.9 |
| June | 120,002 | 64.9 |
| July | 134,012 | 72.5 |
| August | 136,323 | 73.8 |
| September | 151,209 | 81.8 |
| October | 160,307 | 86.7 |
| November | 176,206 | 95.3 |
| December | 185,208 | 100.2 |

This pattern is far more informative about common job opportunities than fitting a single post-period matching shifter to the New Zealand transition rate. Model 16 should treat the **relative time path** of contact opportunities as observed or tightly calibrated:

`c_g,t / c_g,pre = sum_(r,o) omega_g,r,o^pre * IVI_r,o,t / sum_(r,o) omega_g,r,o^pre * IVI_r,o,pre`.

The weights should be fixed using pre-period region and occupation shares. Two versions should be constructed:

1. a pooled or common matched-sample exposure, using identical pre-period weights for both groups, as the preferred common-opportunity measure; and
2. group-specific pre-period exposure, as a composition sensitivity check.

Fixed weights avoid contaminating the opportunity measure with pandemic-induced migration across industries or places. If the secure data only contain current industry rather than the occupation of intended search, use the last pre-period occupation/industry and show both national and weighted series.

The IVI should not be interpreted as the level of the job-offer probability. It covers online advertisements rather than all vacancies, has a higher-skill bias, and one advertisement can contain more than one position. The model should therefore estimate or calibrate one baseline scale parameter from the pre period while importing the public relative path. The monthly series can be held constant within each calendar month in the weekly solution. Linear interpolation can be a sensitivity, but should not be the baseline because it invents within-month information.

Sources: [JSA Internet Vacancy Index landing page](https://www.jobsandskills.gov.au/data/internet-vacancy-index); [IVI methodology](https://www.jobsandskills.gov.au/data/internet-vacancy-index/methodology); [June 2026 state and occupation workbook](https://www.jobsandskills.gov.au/sites/default/files/2026-07/internet_vacancies_anzsco2_occupations_states_and_territories_-_june_2026.xlsx); [June 2026 regional workbook](https://www.jobsandskills.gov.au/sites/default/files/2026-07/internet_vacancies_anzsco2_occupations_gccsa_and_sa4_regions_-_june_2026.xlsx).

### 2.3 Jobs and Skills Australia Recruitment Experiences and Outlook Survey

The official 2020 employer report is useful for separating an opportunity shock from a worker-preference shock. More than 10,000 employers were surveyed in 2020. The report shows:

- the share of employers decreasing staffing fell from 27 per cent in April to 18 per cent in May, 9 per cent in June and 7 per cent in December;
- reported stand-downs fell from 19 per cent in May to 13 per cent in June, 11 per cent in July, 10 per cent in August and September, 6 per cent in October and November, and 4 per cent in December;
- reduced hours remained common, including 26 per cent in April, 33 per cent in June, and 15 per cent in December;
- the national recruitment rate increased from 24 per cent in June to 46 per cent in November and 44 per cent in December;
- among employers with a recent online vacancy in August-December, the mean was 14 applicants, with 28 per cent reporting 20 or more applicants;
- applicant congestion varied sharply across occupations: a mean of 37 applicants for clerical and administrative vacancies but 7 for labourers; and
- 22 per cent of recruiting employers had not advertised, confirming that IVI is not a census of opportunities.

These are not worker transition probabilities. In particular, “27 per cent of employers decreased staffing” must not be entered as a 27 per cent separation hazard, and the changing stand-down question wording limits time-series comparability. The series should instead be used in three ways:

1. to validate the sign and timing of a common employer-side destruction/retention path;
2. to set admissible ranges for how quickly that path recovers; and
3. to validate occupation/region differences in congestion after combining IVI with secure-sample composition.

Applicants per vacancy cannot be turned into applications per unemployed person without strong assumptions. It is a market-tightness/congestion benchmark, not a direct search-effort moment.

Source: [Employers' Insights on the Australian Labour Market: 2020 Data Report](https://www.jobsandskills.gov.au/sites/default/files/2022-03/employers-insights-on-the-australian-labour-market-2020-data-report.pdf); [current REOS page](https://www.jobsandskills.gov.au/data/recruitment-experiences-and-outlook-survey).

### 2.4 ABS Participation, Job Search and Mobility

The February 2021 PJSM release contains richer public information than the current paper uses. The relevant tables distinguish current unemployed people from people who started their current job in the previous 12 months, report multiple search methods, and report whether current unemployed people received zero, one, or two or more job offers while looking.

For February 2021, among 804,931 unemployed people:

- 688,056 reported no job offers;
- 96,290 reported one offer; and
- 20,585 reported two or more offers.

Thus 14.52 per cent reported at least one offer. In February 2020, 111,910 of 703,792 unemployed people reported one or more offers, or 15.90 per cent. The 1.38 percentage-point decline is directionally consistent with weaker opportunity conditions, but is much smaller than the April trough in vacancy advertisements. The timing and population differences explain why these series should not be mechanically equated.

Table 6 also reports search methods for 804,931 current unemployed people and 2,276,961 people who had started their current job within the previous year. Among current unemployed people, 727,366 had written, telephoned or applied directly to an employer; 599,515 had answered an internet or newspaper advertisement; and 333,089 had attended an interview. The categories are multiple-response. Differences between current unemployed people and successful job starters reflect duration and selection as well as search productivity, so they are not causal “success rates” for each method.

The value of these tables is as an external admissibility check:

- Does the model imply an offer probability grossly inconsistent with observed annual offer incidence?
- Does the implied decline in contact/offer opportunities have the right sign and broad magnitude?
- Are simulated duration patterns compatible with the concentration of reported search difficulty among longer-duration unemployed people?

They do not identify a weekly acceptance rate. A current unemployed person with an offer can be waiting to start, can have rejected an offer, or can be observed before an accepted match starts. The data also have no public split by JobSeeker receipt, citizenship or Supplement eligibility. Accordingly, PJSM cannot justify estimating a separate offer-acceptance mechanism from the paper's current eight moments.

Sources: [ABS PJSM February 2021 release](https://www.abs.gov.au/statistics/labour/employment-and-unemployment/participation-job-search-and-mobility-australia/feb-2021); [Table 5 workbook](https://www.abs.gov.au/statistics/labour/employment-and-unemployment/participation-job-search-and-mobility-australia/feb-2021/62280_Table05.xlsx); [Table 6 workbook](https://www.abs.gov.au/statistics/labour/employment-and-unemployment/participation-job-search-and-mobility-australia/feb-2021/62280_Table06.xlsx); [Table 7 workbook](https://www.abs.gov.au/statistics/labour/employment-and-unemployment/participation-job-search-and-mobility-australia/feb-2021/62280_Table07.xlsx); [Table 8 workbook](https://www.abs.gov.au/statistics/labour/employment-and-unemployment/participation-job-search-and-mobility-australia/feb-2021/62280_Table08.xlsx); [ABS PJSM February 2020 release](https://www.abs.gov.au/statistics/labour/employment-and-unemployment/participation-job-search-and-mobility-australia/feb-2020).

### 2.5 ABS Household Impacts of COVID-19 Survey

The September 2020 questionnaire is potentially attractive because the same survey asked whether respondents had actively looked for work in the previous four weeks, intended to look in the next four weeks, received the $550 Coronavirus Supplement, and received the $1,500 JobKeeper payment.

The exact workbook audit shows, however, that the public tables do **not** cross active search with Supplement receipt. They report separate population totals: 15.8 per cent of all adults actively looked for work in the previous four weeks; 16.9 per cent intended to look in the next four; 10.3 per cent received the Supplement; and 14.1 per cent received JobKeeper. The search denominators include employed, retired and other adults, so these totals are not model targets.

This source becomes useful only if one of the following is feasible:

- a custom ABS tabulation crossing unemployment status, Supplement receipt and active search;
- access to the survey microdata with adequate sample size; or
- a defensible linkage or comparable secure survey source.

Until then it should be described as an unrealised data opportunity, not an identifying moment already available online.

Sources: [ABS Household Impacts September 2020 release](https://www.abs.gov.au/statistics/people/people-and-communities/household-impacts-covid-19-survey/sep-2020); [public tables workbook](https://www.abs.gov.au/statistics/people/people-and-communities/household-impacts-covid-19-survey/sep-2020/49400DO002_2020%20-%20Tables%201-13%20%2011%E2%80%9321%20September%202020.xlsx); [data item list](https://www.abs.gov.au/statistics/people/people-and-communities/household-impacts-covid-19-survey/sep-2020/49400DO001_2020%20-%20Data%20Item%20List%20%20September%202020.xlsx).

### 2.6 ABS Job Mobility: reasons for leaving or losing a job

For the year ending February 2021, 1.810 million people reported leaving or losing a job in the previous 12 months. Using the detailed table categories:

- 863,673 (47.7 per cent) were in broadly voluntary categories;
- 718,276 (39.7 per cent) were in involuntary categories; and
- 228,315 (12.6 per cent) were in other categories.

The public release reports that retrenchment rose from 13.6 per cent of job leavers in 2019-20 to 21.7 per cent in 2020-21, while leaving for a better job or a change fell from 26.2 to 21.7 per cent. These shifts strongly support a material firm-side job-destruction component during COVID-19.

The data cannot be applied as direct shares of the paper's separation rate. They cover all workers over a 12-month period, whereas the paper conditions on people who were non-employed during 2020 and therefore selects a much higher-transition-risk population. Nor do the public tables isolate Supplement receipt. Their defensible role is to bound the voluntary/involuntary mixture and test whether simulated model exits are implausibly dominated by worker-side surplus decisions.

Sources: [ABS Job Mobility February 2021 release](https://www.abs.gov.au/statistics/labour/jobs/job-mobility/feb-2021); [reasons for leaving or losing a job workbook](https://www.abs.gov.au/statistics/labour/employment-and-unemployment/participation-job-search-and-mobility-australia/feb-2021/62230_Table02.xlsx).

### 2.7 ABS Labour Force gross flows and duration information

Public Labour Force releases provide monthly employment inflows and outflows and detailed tables by age, sex and state. In 2020, original employment outflows increased from 464,000 in March to 952,000 in April and 783,000 in May, before falling substantially. Inflows fell to 356,000 in April and then recovered. These series provide a valuable aggregate timing check for any model path.

They are not directly comparable with the paper's weekly job-finding and separation hazards. Labour Force flows include movements through non-participation, use a different population, and do not impose the paper's “at least four consecutive weeks non-employed” and “ever non-employed in 2020” conditions.

Public unemployment-duration tables are stocks, not duration-specific transition hazards. They cannot identify duration dependence without a longitudinal design. DataLab Labour Force microdata could generate comparable hazards, but that is additional secure analysis rather than a public online moment.

Sources: [ABS Labour Force Australia, December 2020](https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/dec-2020); [ABS Labour Force Australia Detailed, December 2020](https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/dec-2020).

### 2.8 JobKeeper retention evidence

JobKeeper is central because both Australian citizens and qualifying New Zealand citizens could receive it. It should therefore enter primarily as a common or exposure-weighted firm-side retention policy, not as part of the differential JobSeeker/Supplement treatment.

The Treasury administrative evaluation uses Single Touch Payroll and JobKeeper data with a fuzzy regression-discontinuity/instrumental-variables design. It finds an approximately 40 percentage-point peak increase in employment for recipient casuals near the 12-month tenure threshold, fading to zero by the ninth fortnight beginning 20 July. For newly hired permanent employees, the estimated effect is around 17-20 percentage points and more persistent. The RBA's earlier study estimated that roughly one in five JobKeeper recipients would otherwise not have remained employed, corresponding to about 700,000 employment relationships.

These are causal retention effects over particular samples and horizons; they are not weekly separation hazards. Model 16 should convert them through its transition accounting and use a range rather than subtracting 0.20 or 0.40 directly from a weekly separation rate.

The best application is:

`d_g,t = d0_t - rho_JK(t) * JKExposure_g,t`,

where `d0_t` is an underlying employer-destruction path, `JKExposure_g,t` is actual or predicted JobKeeper exposure in the paper sample, and `rho_JK(t)` is calibrated within externally defensible dynamic ranges. If actual exposure is available in the linked secure data, it should be estimated for each group and week. That would be much more persuasive than assigning the same arbitrary JobKeeper wedge to both groups.

Sources: [Treasury, The employment effects of JobKeeper receipt](https://treasury.gov.au/sites/default/files/2023-12/p2023-450106.pdf); [RBA, How Many Jobs Did JobKeeper Keep?](https://rba.gov.au/publications/rdp/2020/pdf/rdp2020-07.pdf); [ABS Business Conditions and Sentiments, April 2020](https://www.abs.gov.au/statistics/economy/business-indicators/business-conditions-and-sentiments/apr-2020).

## 3. What the public evidence identifies—and what it does not

### 3.1 Credibly externally disciplined objects

The public evidence can discipline the following objects without using the paper's treatment effect twice:

1. **Relative employment-opportunity path.** IVI can determine the timing and relative scale of the common vacancy/contact collapse and recovery.
2. **Employer-disruption timing and bounds.** REOS staffing, stand-down and recruitment series can restrict the common destruction/recruitment path.
3. **JobKeeper retention dynamics.** Treasury and RBA estimates can define sensitivity ranges for the retention component, ideally combined with observed sample exposure.
4. **Broad offer-incidence plausibility.** PJSM can reject model calibrations with plainly implausible offer/contact rates.
5. **Voluntary/involuntary separation mixture.** Job Mobility can rule out mechanisms in which nearly all pandemic separations are worker initiated.
6. **Aggregate transition timing.** Labour Force gross flows can validate the simulated path outside the matched sample.

These are important improvements. They reduce the degrees of freedom available to latent COVID wedges and transform several parameters into externally calibrated paths.

### 3.2 Objects still not identified by public data

No public source located provides the following for Australian JobSeeker recipients and comparable ineligible New Zealand citizens:

- weekly applications or active-search intensity;
- weekly job offers received;
- accepted versus rejected offers or reservation wages;
- employer-initiated versus worker-initiated job endings;
- JobKeeper receipt and employer eligibility within the exact matched sample;
- weekly hazards by non-employment duration under the paper's sample definition;
- treatment intensity measured by individual replacement rate or expected net gain from remaining non-employed; or
- region-by-occupation opportunities matched to each person's pre-period labour market.

Therefore, the current moments do not separately identify:

- the elasticity of search effort to benefit generosity;
- an offer-acceptance/reservation-wage elasticity;
- a worker-side separation elasticity;
- the differential health/work-disutility wedge; and
- the market contact/matching shifter,

when all are allowed to move simultaneously.

### 3.3 Consequence for claims in the paper

With the current evidence, the paper can make a strong reduced-form statement about how the Supplement changed employment transitions for eligible recipients relative to a closely matched ineligible group. It can also use a structural model to organise the relative roles of job-finding and job retention and to conduct disciplined counterfactuals.

It cannot yet claim to have empirically separated “less search” from “greater selectivity in accepting offers,” or “worker-chosen separation” from employer-driven destruction, unless those components are clearly labelled as externally calibrated scenarios. The structural section should be written around this boundary.

## 4. Recommended architecture for model 16

### 4.1 Collapse search and acceptance into effective job-finding effort

Until a direct offer or acceptance moment is available for the treatment groups, use a composite effective-search term:

`f_g,t = c_g,t * e_g,t`,

where:

- `c_g,t` is the exogenous or tightly calibrated opportunity/contact path from IVI and market composition; and
- `e_g,t` is a composite worker-side response incorporating search effort, application quality, reservation behaviour and acceptance.

The economic decision can still be derived from dynamic values and benefit generosity, but it should have one estimable curvature/elasticity rather than separate latent search and acceptance functions. In the paper, call it “effective job-finding effort” or a “worker-side job-finding margin,” not observed search effort.

If an independent offer-incidence moment later becomes available, the composite can be split:

`f_g,t = contact_g,t * search_g,t * acceptance_g,t`.

The version-16 code can be modular enough to allow that extension without calibrating the extra margin prematurely.

### 4.2 Use a mixed separation process

Represent separation as the combination of employer-driven destruction and surplus-sensitive worker/job-level separation:

`s_g,t = 1 - (1 - d_g,t) * (1 - q(S_g,t))`,

where `d_g,t` is employer-driven destruction net of JobKeeper retention, and `q(S_g,t)` is a parsimonious surplus-sensitive exit probability. This competing-risks form avoids additive probabilities above one and makes the interaction economically interpretable.

The common destruction path should be restricted by REOS, Labour Force flows and JobKeeper evidence. The treatment contrast should identify only the residual change in `q(S_g,t)` associated with the difference in benefit eligibility, subject to sensitivity over the voluntary/involuntary mixture. Avoid separately fitting a recipient-specific health wedge to the same recipient separation target unless there is an independent health/exposure moment.

Three nested versions should be reported:

1. **Employer-destruction-only separation:** benefits affect job finding but not retention.
2. **Surplus-sensitive separation:** benefits affect a worker/job-surplus exit margin in addition to common destruction.
3. **Health/exposure heterogeneity sensitivity:** a differential health wedge is imposed from an external range, not fitted freely.

This turns an underidentified point decomposition into a transparent mechanism comparison.

### 4.3 Use the actual benefit schedule and a corrected weekly discount factor

Model the Supplement announcement and implementation, the $550 phase, and the September reduction explicitly. If the current empirical window stops before the reduction, state that and use the later reduction only as an external validation event. Incorporate the base-payment distribution or replacement-rate heterogeneity if secure data permit.

Convert an annual discount factor to weekly frequency. Keep preference parameters fixed across groups unless a pre-period moment requires otherwise. Differential post-period preferences should be sensitivity parameters with external justification, not residuals that force exact fit.

### 4.4 Separate estimation, external calibration and validation

Every numerical object should be labelled in the output as one of:

- **estimated from the paper's secure matched sample;**
- **externally calibrated from a named public source;**
- **normalised;**
- **set by literature range;** or
- **held out for validation.**

This taxonomy should appear in a model-identification table in the paper and in a machine-readable CSV produced by the code.

### 4.5 Report sets and profiles, not only the optimum

Given the small number of aggregate transition targets, report:

- profile fit over the worker-response elasticity;
- ranges over vacancy exposure, JobKeeper retention and voluntary-separation shares;
- the set of parameter values that fit the empirical moments within their sampling confidence region; and
- counterfactual ranges across that admissible set.

The empirical uncertainty should enter the structural criterion. Calibrating to point estimates as if they were known exactly understates uncertainty, especially with 48 group-week regression cells and strong serial correlation in aggregate weekly outcomes.

## 5. Additional moments to construct in the secure environment

### 5.1 Essential before final model-16 calibration

#### A. Exact weekly paths and event timing

Export weekly, group-specific job-finding and separation rates with numerator, risk-set denominator, standard error and calendar date. Retain the full path rather than only pre/post means. Mark:

- 22 March announcement;
- the actual Supplement payment start;
- JobKeeper announcement and operation;
- state lockdown dates;
- mutual-obligation changes;
- the September Supplement reduction; and
- any sample or STP reporting discontinuities.

This is needed to align public monthly inputs and test anticipation/dynamics.

#### B. Duration-specific job-finding hazards

Estimate weekly job-finding hazards by non-employment duration, for example 4-7, 8-12, 13-25 and 26+ weeks, separately by group and period. These moments distinguish a direct policy response from dynamic selection and are obtainable from the same underlying spell data.

#### C. JobKeeper exposure and employer-side separation

For every person-week/job spell, construct actual JobKeeper receipt or employer eligibility if available. Estimate transitions by:

- group × JobKeeper exposure × week;
- employer closure or payroll-collapse status;
- mass-layoff events at the firm; and
- idiosyncratic exits from otherwise continuing firms.

Even without an explicit reason code, mass layoffs and employer cessation provide powerful proxies for employer-driven destruction. They are more directly comparable with the separation mechanism than aggregate public employer shares.

#### D. Fixed local/occupation vacancy exposure

Merge IVI using each person's pre-period SA4/state and occupation/industry. Construct exposure with pre-period fixed weights and estimate whether the transition response varies with the vacancy collapse. This creates a continuous opportunity moment and tests the assumption that the matched groups face a common labour market.

#### E. Policy-dose heterogeneity

Calculate the actual or predicted replacement rate and the dollar change in out-of-work income. Estimate treatment effects by pre-period earnings, household circumstances where available, and predicted replacement-rate bins. A monotone dose-response is a much stronger structural moment than a single eligible/ineligible indicator.

### 5.2 High-value extensions

- Split employment entries by new employer versus return to a previous employer.
- Split separations into transitions with zero STP earnings but continuing job attachment, permanent job endings, and switches to another employer.
- Estimate re-employment earnings, hours and tenure conditional on finding a job to test match-selectivity implications.
- Estimate treatment effects by age, sex, industry, state and local vacancy exposure using pre-specified interactions.
- Re-estimate with alternative non-employment definitions (one, two and four consecutive weeks) to reveal sensitivity to STP payment timing.
- Construct person-level or employer-clustered uncertainty for all exported structural moments, including covariance across targets.

### 5.3 Moments unlikely to be available in current administrative data

Applications, offers, offer rejections and reservation wages are unlikely to be observed in STP or payment records. Unless a survey or job-platform linkage is available, do not delay the paper indefinitely seeking them. Instead, retain the composite worker-side margin and present acceptance as an interpretation/sensitivity exercise.

## 6. Proposed identification map for version 16

| Model object | Preferred information | Status | Recommended treatment |
|---|---|---|---|
| Baseline job-finding scale | Pre-period group hazards | Available | Calibrate one scale parameter |
| Relative opportunity path | IVI with fixed pre-period weights | Publicly available | Import as data; monthly step function |
| Common employer-destruction path | REOS + Labour Force + secure mass layoffs | Partly available | External restriction plus secure calibration |
| JobKeeper retention | Treasury/RBA range + secure exposure | Public range; exposure to confirm | Scenario range, dynamic and exposure weighted |
| Worker-side job-finding elasticity | Policy-dose gradient and group post hazards | Partly missing | Estimate/profile as composite elasticity |
| Search versus acceptance split | Applications/offers/rejections | Missing | Do not point identify; sensitivity only |
| Worker-side separation response | Group gap after employer-driven exits | Missing but constructible | Estimate after secure exit decomposition |
| Differential health/work cost | Direct health/exposure moment | Missing | External sensitivity; do not fit residually |
| Duration dependence | Duration-specific hazards | Constructible | Estimate from spell data |
| Discount factor | Annual macro/literature value | Available by convention | Convert to weekly; sensitivity |
| Counterfactual uncertainty | Sampling covariance + calibration ranges | Partly missing | Identified/admissible set |

## 7. Recommended work plan

### Stage 1: public-data module and empirical audit trail

Create a standalone script that downloads or reads frozen copies of:

- the IVI state × occupation workbook;
- the IVI regional workbook if SA4 is used;
- the REOS 2020 report inputs transcribed with source-page notes;
- PJSM offer-incidence and search-method tables; and
- public JobKeeper effect ranges.

The script should save a compact CSV containing source URL, download date, vintage, unit, seasonal adjustment, frequency and any transformations. Preserve raw values and transformation code. Do not hard-code only the final index.

### Stage 2: secure empirical “shopping list”

Prioritise five exports: exact weekly paths; duration hazards; JobKeeper exposure and mass-layoff separations; fixed local/occupation vacancy exposure; and replacement-rate gradients. Export estimates and full covariance matrices, never unit records.

Before secure work begins, write the target schema so the structural code can ingest the outputs without manual transcription.

### Stage 3: model-16 specification memorandum

Fix the following decisions before coding:

- composite effective-search versus separate acceptance margin;
- competing-risks separation equation;
- which opportunity and destruction paths are fixed, ranged or estimated;
- exact policy/event timeline;
- annual-to-weekly discounting;
- objective function and use of empirical covariance; and
- which moments are held out.

The recommended default is composite effective search, competing-risks separation, IVI-fixed relative opportunities, dynamic JobKeeper retention ranges, and no freely fitted differential health wedge.

### Stage 4: implement and compare models 15 and 16

The requested comparison runner should use a common interface and produce:

1. data-versus-model moment table;
2. target/parameter identification table;
3. weekly job-finding and separation paths by group;
4. benefit, opportunity, destruction and JobKeeper counterfactuals;
5. factorial or Shapley decomposition with the interaction reported explicitly;
6. profile-fit and admissible-set plots;
7. sensitivity to national versus matched vacancy exposure; and
8. a concise machine-readable summary of differences from model 15.

Model 15 must remain unchanged and runnable. Version 16 should be a separate file with separate outputs and a detailed change log.

### Stage 5: paper integration

Reframe the structural contribution as disciplined interpretation of a strong natural experiment, not as a point-identified decomposition of every behavioural margin. The paper should lead with the reduced-form transition results, explain why job finding and retention both matter, then use the model to answer narrower counterfactual questions under transparent external restrictions.

The structural section should contain:

- a one-page economic mechanism overview;
- timing and state variables;
- a table mapping every parameter to its evidence source;
- a short identification subsection stating what the group contrast identifies;
- fit including held-out moments;
- counterfactual ranges; and
- explicit limitations concerning search versus acceptance and separation reasons.

## 8. Decision recommendation

### What should happen now

Proceed with preparatory work for version 16, but **do not yet lock in a final point calibration**. The online audit found important external restrictions, so copying the model-15 calibration logic into a new file would waste the information and preserve its underidentification.

The best next concrete deliverables are:

1. a public-moments ingestion script and frozen, documented transformed series;
2. a secure-environment moment specification/template;
3. a short model-16 design memorandum implementing the composite job-finding and mixed-separation architecture; and
4. after the secure outputs are available, the model-16 code and model-15-versus-16 comparison runner.

### Go/no-go condition for a richer three-margin model

Only restore separately estimated search effort and offer acceptance if an independent moment on applications, offers or rejected/accepted offers becomes available for sufficiently comparable groups and timing. PJSM is valuable for validation but does not satisfy that condition.

Only estimate a differential health/work-disutility shock if an independent health, exposure or inability-to-work moment exists. Otherwise show it as an external sensitivity parameter.

Only interpret the recipient separation gap as worker-side behaviour after demonstrating that group differences in JobKeeper exposure, employer closure, mass layoffs and industry/local demand do not account for it.

### Bottom line

The public moments are useful enough to warrant revisiting model-16 design before coding. They allow the model to replace fitted “COVID shocks” with observed opportunity and employer-side paths. They are not sufficient to identify separate search, acceptance and worker-separation mechanisms. A parsimonious composite-margin model, combined with secure duration, JobKeeper, vacancy-exposure and exit-type moments, is the strongest route to a journal-ready structural section.

## Appendix A. Exact public values used in this audit

### A.1 PJSM offer incidence

| February reference year | Total unemployed ('000) | No offers ('000) | One offer ('000) | Two or more ('000) | At least one offer (%) |
|---|---:|---:|---:|---:|---:|
| 2020 | 703.792 | 591.881 | 91.504 | 20.406 | 15.90 |
| 2021 | 804.931 | 688.056 | 96.290 | 20.585 | 14.52 |

Notes: The categories refer to current unemployed people and offers received while looking for work. They do not measure a weekly offer hazard or final acceptance. Totals differ slightly under rounded arithmetic.

### A.2 Job Mobility categories, year ending February 2021

| Broad classification | People ('000) | Share of all leaving/losing a job (%) |
|---|---:|---:|
| Voluntary | 863.673 | 47.71 |
| Involuntary | 718.276 | 39.68 |
| Other | 228.315 | 12.61 |
| Total | 1,810.264 | 100.00 |

Broad classifications are constructed from detailed ABS categories for diagnostic use and are not official ABS aggregates. Temporary/seasonal jobs ending, retrenchment and dismissal are classified as involuntary; better job/change, poor arrangements/pay/hours, retirement, family, study/holiday and own-business reasons are classified as voluntary; ill health and residual reasons are placed in “other” for a conservative split.

## Appendix B. Questions to resolve before model-16 coding

1. What are the exact start/end dates and averaging weights underlying each of the eight current moments?
2. Does the linked secure environment contain JobKeeper receipt at the person-job-week level, employer eligibility, or only a month-level control?
3. Can pre-period occupation and SA4/state be exported as fixed exposure weights?
4. Can separations be classified using employer payroll contraction, employer closure, multiple simultaneous exits or return to the same employer?
5. Can the treatment be expressed as a predicted dollar/replacement-rate dose rather than only citizenship × post?
6. Does the secure environment contain hours or only STP payments, and how are stood-down but job-attached workers classified?
7. Will the final empirical design include the September Supplement reduction as an event/validation period?
8. Are confidence intervals and the covariance matrix of the structural moments available at the same aggregation level used for calibration?

Until these questions are resolved, version 16 can be scaffolded, but its preferred calibration should remain provisional.
