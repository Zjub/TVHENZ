# Capital gains, normal returns and consumption taxation

Research snapshot: **11 November 2026**. This pack supports an evidence-led article and companion blog posts. It does not draft the argument or select a preferred tax rate.

**Worked extension:** [Ten years of saving: present-value tax and METRs](ten_year_example/EXAMPLE.pdf) compares the four tax designs and four saving vehicles with a common initial labour resource and pre-tax return. Its [12 plots](ten_year_example/index.html) also cover company integration, unpaid labour in a zero-basis business, annual-earnings cohorts and the distinction between a specified-return wedge and a required-return METR.

Start with the [62-chart gallery](figures/index.html). Every figure supplies PNG, SVG, its exact plotted CSV and an interpretation note. The [numeric notes](processed/key_findings.csv) retain units, period, observation/scenario status and limitations. Read [policy status](POLICY_STATUS.md) before describing the reforms as proposals or treating their detailed implementation as settled.

## A short first pass

**D03 → D06 → B03 → M01 → M05 → M06 → M10 → M15 → R11 → E01** gives a compact route from the composition of reported gains to the tax base, fiscal amounts, normal-return framing, inflation, progressive bunching, losses and behavioural uncertainty.

The full catalogue is organised as follows:

| Area | Charts | What they investigate |
|---|---|---|
| Reported gains and distribution | D01–D19 | Assets, entities, histories, losses, income/age/sex, concentration and concession benefits |
| Budget | B01–B06 | Total receipts, combined reform score, medium-term receipts and benchmark tax expenditures |
| Normal-return allowance and CGT design | M01–M17 | Consumption timing, cash-flow equivalence, inflation, allowance rates, payout form, progressive bunching and transition |
| Risk, financing and implementation | R01–R11 | Deferral, lock-in, delayed relief, risk sharing, leverage, imputation and real losses |
| Behavioural evidence | E01–E07 | Native definitions, conversions, finite responses, functional form, retiming and reclassification |
| Economic context | Q01–Q02 | Government bond yields and historical inflation |

## 1. Define the income concept before choosing the rate

Your slides distinguish inflation compensation, a normal return for postponing consumption, and returns above that benchmark. M01 makes the distinction explicit; M06 shows how much more a compounded normal-return basis protects than a CPI-indexed purchase cost.

There are **three separate policy questions**:

1. Should inflation compensation enter the tax base?
2. Should the real normal return to saving enter the tax base?
3. At what rates, and with what loss treatment, should above-normal returns be taxed?

Inflation indexation addresses the first. A normal-return allowance addresses both the first and second if the allowance is expressed in nominal terms consistently. Neither settles the third.

Calling the normal return “not income” is a choice of tax-design benchmark, rather than a universally accepted accounting definition. Comprehensive real income can include the normal real return. A consumption-oriented benchmark instead seeks to avoid taxing the choice to defer consumption. The evidence pack makes both comparisons visible: M02 includes nominal and real comprehensive income taxation alongside the RRA and consumption-tax treatments.

Above-normal returns can reflect rents, labour effort, entrepreneurial skill, luck and compensation for risk. A risk-free allowance does not identify every excess return as a monopoly rent. M17 uses a stylised fixer-upper to show why an owner's unpaid work can be embodied in a gain: the recorded acquisition/improvement cost does not include a tax deduction for the owner's forgone leisure or wages. The model does not determine whether a real transaction is main-residence-exempt, on revenue account or eligible for a concession.

The [IFS capital-gains reform study](https://ifs.org.uk/sites/default/files/2024-10/Captial-gains-tax-reform.pdf) is a useful primary research discussion of allowance design, loss deductions and cash-flow alternatives. The Mirrlees Review's [household-saving chapter](https://ifs.org.uk/books/13-taxation-household-savings) supplies the broader system framing. The latter was read online; no local download of that chapter is claimed.

## 2. In what sense is an RRA similar to GST?

The most defensible starting claim concerns **the tax wedge between consuming now and consuming later**, and the present value of taxes on a new investment. It is stronger and clearer than suggesting that the actual Australian GST and an income tax with an allowance would have identical effects on everyone.

M02–M03 start with the same $100,000 of new pre-tax labour resources. With a 30% income-inclusive tax, $70,000 can be consumed now. A matching tax-exclusive consumption rate is **42.857%**, because:

`consumption-tax rate s = income-inclusive rate t / (1 - t)`

A 10% GST corresponds to a 9.091% tax-inclusive rate on the same expenditure. A 30% income tax and a 30% GST are not matched comparisons. M04 shows the mapping across rates.

If saving earns only the specified normal return, a stable comprehensive consumption tax and labour-income tax plus a fully usable normal-return allowance leave the same relative opportunity to consume later. Comprehensive real-income taxation still reduces the reward to postponement; nominal-income taxation adds an inflation effect. These are simplified lifetime-budget comparisons with stable rates and a common new resource.

M05 provides an independent two-date present-value identity. Let `K` be new investment, `V` its terminal proceeds, `a` the nominal normal allowance and `T` the holding period. With a constant rate `t` and fully symmetric relief:

`PV(RRA tax) = t [V - K(1+a)^T] / (1+a)^T`

`PV(cash-flow tax) = -tK + tV / (1+a)^T`

The expressions are equal. The upfront cash-flow deduction and the compounded normal allowance have the same present value under these assumptions. CPI adjustment alone generally does not reproduce this result when the normal real rate is positive.

The cash-flow comparison is for a **new investment with a refundable/fully valuable initial deduction**. It is not a proposal to deduct every household asset purchase against wages without any other rules. For risky cash flows, a common risk-free discount applied to expected payoffs does not establish welfare or state-by-state equivalence; risk sharing, state pricing, financing and usable deductions matter.

The comparison also has material boundaries:

- **Existing wealth:** a newly introduced consumption tax can tax spending financed from wealth that has already borne income tax. A prospective labour tax plus RRA need not do so. R10 makes this transition difference visible.
- **Progressivity and timing:** stable proportional rates are the clean benchmark. Lifetime tax rates, annual progressive schedules, transfers and the timing of deductions can break simple equivalence.
- **Coverage:** the actual GST has exemptions and special treatment of financial services, housing and other transactions. A hypothetical comprehensive consumption tax is the model comparator.
- **Bequests and old gains:** terminal wealth, inheritances, tax at death and transition compensation need explicit treatment.
- **Capital-income coverage:** an allowance confined to CGT can leave incentives to change the form of income. M07 holds the total return fixed and changes its split between current payouts and appreciation.

These qualifications help make the proposed frame precise; they do not by themselves favour taxing or exempting the normal return.

## 3. What should the allowance be?

The central illustration assumes 2.5% inflation and a 3% real normal return, giving an exact nominal allowance of **5.575%**. These are assumptions, not a calibrated recommendation or forecasts.

M08 varies the real allowance from 1% to 5%. M09 asks whether unused allowance is lost, merely accumulated, or compounded. A terminal-appreciation asset earning the normal return can still be taxed if carry-forward does not preserve the allowance's time value.

Q01 provides observed nominal and indexed government-bond yields as context. A short-term risk-free rate, medium-term bond yield, after-tax yield and an investor's borrowing rate are not interchangeable. Term and liquidity premia, choice of averaging window, tax treatment and adjustment frequency all matter. Q02 shows why the inflation assumption should also be varied.

The [OECD working paper](https://www.oecd.org/content/dam/oecd/en/publications/reports/2025/02/taxing-capital-gains_76a32327/9e33bd2b-en.pdf) discusses Norway's shareholder allowance and practical deferral/administration limits. It is an international design example, not an estimate of the Australian fiscal effect of adopting an RRA.

## 4. What do Australian realised gains actually consist of?

The ATO's 2023–24 CGT table reports about **$90.6 billion** of individual gross current-year capital gains. Direct Australian real estate accounts for about **41.5%** of that published total; trust/managed-fund-origin gains account for about **32.3%**. These are distinct reporting categories.

Two limits materially affect the story:

- Australian real estate is **not exclusively residential investment housing**.
- The underlying assets behind trust distributions cannot be reconstructed from this table. Do not assign that entire category to either property or equities.

D03 displays a further $1.47 billion difference between the published total and the reported components for individuals in 2023–24. Across years and entities some differences have the opposite sign. `asset_reconciliation.csv` preserves them. The source notes warn of incomplete reporting and disclosure adjustments; this research has not identified a definitive cause for every difference. Components are not rescaled to pretend there is an exact decomposition.

D06 distinguishes gross current-year gains from net taxable gains after losses, discounts and concessions. Its balancing difference is labelled as a residual rather than assigned to a specific policy. Individual net-gain totals also differ slightly between the CGT associated-form table and the all-return individuals table; no false exact reconciliation is imposed.

D01–D07 cover 2012–13 to 2023–24. Individual reporting changed in 2019–20 from a CGT schedule to an associated form. This especially affects comparisons of counts; the charts are not evidence of a sudden behavioural response at that date. D16 extends the net-taxable-gain history using the all-return time series and approximate fiscal-year CPI deflation, with historical tax rules left visible as a comparability limitation.

These are **realised and reported** gains, not national accrued capital income or the increase in household wealth. Exempt gains and unrealised gains are largely outside this base.

## 5. Who receives the gains, and who receives the concession?

D08–D14 use disjoint ATO cells to compare annual taxable-income groups, age, recorded sex and the size of net gains. D12 compares concentration of gains and taxable income among taxable filers. D18–D19 separately plot Treasury's concession-benefit estimates for 2022–23.

Annual taxable-income ranking includes the gain itself. An unusually large sale can put someone in a high bracket for one year. Age comparisons also mix ownership, accumulated assets, holding periods and realisation choices. These tables cannot establish that a particular person's lifetime income is high or low.

The Budget Statement 4 has additional linked-record analysis of lifetime-income rank and tax rates before sale. Its editable DOCX was downloaded and inspected, but contains no embedded chart workbooks or numeric chart caches. The public figures therefore remain a **reading source**, not invented precise plot data. The pack does not claim to reproduce Treasury's linked longitudinal microdata. That is a meaningful evidence gap for the claim in the slides about decade-long income.

D13 maps several reported capital-income forms. It deliberately distinguishes gross interest/rent, dividend and franking components, and net taxable gains. Do not sum it as a complete measure of net economic capital income or calculate an RRA's fiscal cost from it: the relevant asset bases, economic depreciation and financing are missing.

## 6. What is in the Budget?

B01–B02 use the Budget's memorandum line for CGT receipts. This line is already included in individual/trust, company and superannuation taxes; adding it again double counts revenue. The pack keeps estimated tax allocated to gains in ATO returns (D15) separate from cash receipts in the Budget.

B03 extracts the published **combined CGT and negative-gearing measure**. Extra receipts total **$3.630 billion** over 2025–26 to 2029–30; related payments total about **$99 million**, giving roughly $3.531 billion of net improvement at the published precision. It is not a separately identified CGT score. The table's `..` entries are small rounded amounts, plotted as zero and preserved as source tokens.

B04–B05 show PBO's rounded preliminary July medium-term projections. The combined measure totals about $41 billion across 2026–27 to 2036–37, with its annual effect building over time. This is a different window and precision from the Budget table; it is not a November recosting of every legislative detail.

B06 uses December 2025 TEIS revenue-forgone estimates, including the discount and main-residence benchmark components. Benchmark choice, behaviour, interactions, transition and timing distinguish those amounts from repeal revenue. They should not be added to a behavioural reform score or used as money immediately available to spend.

No new behavioural haircut is applied to Treasury's published costing. The E-series exercises instead begin with a separate normalised artificial base, avoiding double adjustment.

## 7. Inflation, deferral, bunching and a minimum rate can pull in different directions

M10–M13 vary capital appreciation, holding period and other income under frozen 2027–28 basic resident rates. They assume a fresh $500,000 eligible asset with no pre-July-2027 gains. CPI is 2.5%; actual quarterly statutory calculations, selling costs, capital works adjustments, Medicare, offsets and transfers are omitted. A comparison using only a headline marginal rate can miss both inflation composition and progressive bunching.

M15 isolates the extra **undiscounted** tax from recognising several years' gains in one year under a progressive schedule. It does not value the benefit of paying that tax later. R01–R02 isolate deferral under a flat-rate real-dollar model instead. Assessing the net effect of both mechanisms requires specifying the entire income and asset path; neither chart alone establishes systematic overtaxation or undertaxation of all capital gains.

M14 illustrates the minimum-tax top-up and a specified-payment exemption. It is not simply `max(marginal rate, 30%)` applied to every dollar, and it is not a welfare-eligibility calculator. M16 supplies one transition illustration; the detailed post-2027 implementation remains outside the simple tax engine.

R03 compares holding an embedded gain with selling and switching to a better-returning asset. It shows the hurdle created by paying tax earlier; it does not estimate the quantity of capital locked into poor investments. An RRA is not asserted to eliminate every form of lock-in, particularly where financing, company retention or rate changes matter.

## 8. Losses and risk deserve equal attention

R04 values the delay before a capital loss can be used. It assumes certain eventual use; no future gains would make the relief less valuable. R11 adds the distinction between a **real loss** and a **nominal capital loss**. Protecting an inflation-adjusted basis on gains does not automatically give relief for all losses of purchasing power.

R05 reproduces the risk-sharing logic in your slides: a proportional tax with full symmetric loss refunds scales down both success and failure. If the investor can scale up, both original outcomes can be restored. Denied or delayed loss relief changes that result. Neither financing nor refunds can be assumed available to every household.

R06 uses illustrative log utility and an unscaled project to show that a welfare assessment additionally depends on wealth, preferences and probabilities. Those parameters are not estimated and the chart is not a welfare ranking of actual reforms. Neutrality under a symmetric stylised tax is not evidence that the enacted asymmetric system shares risk symmetrically.

R07–R08 address leverage and the inflation component of nominal interest deductions. A consistent real-income system considers both lender receipts and borrower expenses. The RRA model instead applies the allowance to equity and taxes the net excess return under its stated symmetric assumptions. These are mechanisms to examine, not a claim that every leveraged property is subsidised.

R09 checks the one-period imputation identity with full distributions and usable credits. It does **not** assume that a buyer pays full value for credits retained in a company or that company-tax-paid retained earnings always reproduce annual personal taxation. Foreign ownership, retention and capitalisation require separate evidence. That is a useful qualification to the corresponding illustration in the slides.

## 9. Behavioural responses: compare the right objects

The [evidence table](processed/literature.csv) records country, original source, version, definition, horizon, reference rate, identification limitations and whether a number is an empirical estimate or a scoring assumption. E01 shows native definitions; intervals are **specification ranges**, not confidence intervals. Canadian ranges are shown without a reported point estimate; their CSV midpoint is explicitly a display convenience.

Three definitions recur:

`tax-rate elasticity = d log(gains) / d log(t)`

`net-of-tax elasticity = d log(gains) / d log(1-t)`

`semi-elasticity = d log(gains) / dt`

At a specified reference rate, `e_tax = -e_net * t/(1-t)` and `e_tax = semi * t`. A coefficient of 3.6 on the retention-rate scale is not a tax-rate elasticity of -3.6. E02 demonstrates conversion while retaining differences in horizon and identification.

The Australian authors' main specification uses only 27 annual observations and a semi-elasticity of about -1.75, evaluated by them around the historical tax-rate midpoint. Their [own explanation](https://www.austaxpolicy.com/tax-rate-changes-impact-capital-gains-realisations-evidence-australia/) makes the macro-data limitations visible. This is not a modern quasi-experimental estimate of the 2026 reforms.

[Agersnap and Zidar](https://www.nber.org/papers/w27705) exploit US state reforms and separate migration from the nationally relevant realisation response. The pack uses their labelled working-paper tables and specification ranges rather than confusing the abstract's wording about revenues with a revenue elasticity. The local conversion is checked explicitly.

[Dowd and McClelland's 2024 study](https://taxpolicycenter.org/publications/sensitivity-tax-elasticity-capital-gains-lagged-tax-rates-and-migration) shows sensitivity to lag specification. Their native evaluation rate is not supplied in the extracted summary, so those values are not silently rebased to 22%.

The [Canadian exemption-cancellation working paper](https://www.nber.org/papers/w28514) finds markedly different short- and longer-run responses. Its setting included the opportunity to crystallise gains under an expiring exemption and wealth effects from losing exemption space. The downloaded 2021 version is not presented as identical to every estimate in the later published article.

The [OBR January 2025 release](https://obr.uk/docs/dlm_uploads/CGT-supplementary-release-Jan-2025.pdf) provides explicit official scoring assumptions for different UK gain populations. These aggregate several behaviours and are not causal estimates of an Australian policy. Direct download was denied; the parameter and locator were verified online.

E03–E05 ask what happens if one extrapolates a local elasticity using two different functional forms. They show why even matching a local estimate does not determine the response to a large reform. A revenue-curve peak is neither an Australian empirical estimate nor a welfare optimum.

E06 conserves gains over the full window while moving realisations around a tax change. A pre-change surge and post-change slump therefore need not establish a permanent effect. E07 shows that some disappearing gains might reappear as other taxable income. Realisation timing, relabelling, migration, investment and underlying accrued returns are separate margins.

US step-up at death, Canadian deemed dispositions and Australia's different estate/exit rules affect transferability. No single foreign elasticity is selected as “the Australian answer”.

## 10. Useful article questions and what would still be needed

One possible article sequence is: **define a gain → separate inflation and normal return → explain the current reform → assess symmetry and timing → explain fiscal and distributional trade-offs**. That is a research structure, not a conclusion.

Questions that could strengthen or overturn a proposed argument include:

- Does an allowance apply to deposits, dividends, rent and gains consistently, and to equity or gross assets?
- What happens to unused allowance, borrowed funds, negative returns and investors unable to use deductions?
- How much of the concession benefits normal returns, and how much benefits high returns or converted labour income?
- Would lower tax on marginal saving change national saving or investment, or mainly change asset ownership and tax planning?
- How much revenue is transition-related, and which tax rates, spending or other bases would change to finance an allowance?
- Which households gain after accounting for lifetime income, existing wealth, housing and transfers?

The public tables do not provide asset cost bases and holding periods jointly with total returns, financing and taxpayer histories. They therefore cannot cost a comprehensive RRA, distinguish normal returns from rents across the population, or deliver a reliable distributional microsimulation. Nor do they identify causal effects on Australian house prices, rents, construction or productivity. Those are research gaps, not zero effects.

`processed/slide_followup.md` maps the slides to the new evidence and identifies which slide claims require qualification. For blog reuse, retain observation/model labels, the relevant source vintage and the assumption captions. For a chart-free article, use the numeric notes and verify any policy detail against the current legislative position immediately before publication.
