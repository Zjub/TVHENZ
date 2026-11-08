# Measuring the five topics

This guide records measurement choices and possible extensions. It does not infer which policies should be adopted. See the charts' own notes and `processed/op_ed_topics/measurement_notes.csv` for current coverage.

## Digital services taxes

The needed base is **gross revenue from eligible services attributed to New Zealand users, after applying the proposal's service definitions and entity thresholds**. ICT exports, the information industry's GDP, household digital purchases and remote-services GST are all different populations. A revenue-base scenario is useful for scale but cannot replace this base.

Possible further measures:

| Measure | Calculation | What is needed |
|---|---|---|
| Eligible domestic base | Sum eligible user-linked revenue across in-scope entities | Service-level NZ revenues and attribution rules; confidential or voluntarily disclosed company data |
| Profit-equivalent burden | Retained DST / profit before DST | Profit margins on the same revenues, pass-through and corporate-tax interaction |
| Revenue retained after costs | Gross DST minus administration, other tax offsets and induced revenue losses | Current official costing and behavioural/incidence/trade assumptions |
| Customer incidence | Price change attributable to the tax / tax per purchase | Matched pricing observations around policy changes, demand and competing markets |

D04–D06 show the arithmetic for alternative assumptions. D07 uses the dated 2023 official estimate for scale. None measures uncollected tax or a current tax gap.

## Tax streamlining, R&D and expensing

There are at least three separate questions: how much administration a rule creates; what its fiscal cost is; and what behaviour or wider benefit it produces. Counting concessions answers only a narrow part of the first question. Tax-expenditure disclosure coverage is measurable, but an unestimated cost does not mean the rule costs nothing or should be repealed.

Useful additional measures include compliance hours **per filer and by firm size**, administrative cost **per return or dollar of tax**, and the number of distinct returns, eligibility tests and deadlines **actually faced by a representative business**. A count of sections in a tax Act is an especially weak proxy unless legislative scope and consolidation are controlled for.

For R&D, the relevant effectiveness measure is **additional eligible R&D caused by support / net public cost**, with spillovers and displacement assessed separately. Survey expenditure and credits in a fiscal year are not matched cohorts. Dividing one by the other can be misleading. The official evaluation or linked firm data would be needed for a counterfactual rather than an aggregate before/after chart.

For expensing, the deduction's timing is directly measurable using:

`present value of tax shield = tax rate × sum(deduction in year t / (1 + nominal discount rate)^t)`.

T13–T14 hold the asset, lifetime deductions and tax capacity constant. A fuller measure should vary economic depreciation, financing, inflation and firms' ability to use tax losses. The proper fiscal comparison includes later revenue recovery from deductions brought forward.

For screen and games support, separate **actual paid expense**, **announced allocation**, **appropriation limit**, **qualifying production spending**, **incremental domestic activity**, and **net fiscal return**. An industry's total expenditure divided by its rebate is a leverage ratio, not a benefit-cost ratio. Causal assessment needs the share of activity that would have happened without the rebate, displacement from other NZ industries, imported inputs, and the relevant economic and cultural outcomes.

## Taxation of inflation

The pack measures two distinct channels:

1. **Bracket erosion:** real threshold = nominal threshold / price-index growth. At a fixed real income, compare income tax under observed schedules with explicitly indexed schedules. The choice of baseline and whether to preserve later rate changes matters.
2. **Taxation of nominal interest:** compare real wealth after tax on nominal returns with wealth under an inflation-adjusted capital basis. Define the treatment of negative real returns and use the exact real-return formula.

An aggregate fiscal-drag estimate needs a taxpayer income distribution or microdata, all relevant tax credits and demographic/income changes. Four illustrative earners cannot be multiplied into an aggregate revenue estimate without those inputs.

Removing all taxation of inflation would also involve borrowing deductions, inventory and capital-asset accounting, financial arrangements, and whichever gains are taxable. The saving illustrations do not estimate that comprehensive reform's cost. A better next dataset would combine actual taxable interest, relevant balances, rates and tax bands; it would distinguish bank deposits from PIE products and account for differences in legal treatment.

## Public servants and value for money

Staff per resident, share of employment and expenditure on contractors describe scale. They do not establish the amount of staffing required or the value of services delivered. They also cannot be compared internationally until coverage is harmonised: departmental civil servants, general-government employees and the public sector including SOEs are different groups.

A useful agency/service measure is:

`quality-adjusted service outputs / (staff cost + contractor cost + allocated operating and capital cost)`.

Outcomes should be considered alongside this ratio because a fast or cheap process need not achieve its purpose. Potential measures include processing volumes and waiting times, successful resolution, error/rework rates, accessibility and user satisfaction. Account for case complexity, legal requirements, population need and transfers of functions between agencies.

P10–P11 provide OIA volumes and speed as one measured service dimension. There is no corresponding OIA staffing/cost allocation or output-quality measure. They must not be used to infer which agency should lose staff. An aggregate value-for-money measure would require a defensible way to combine unlike services; the pack does not invent that statistic.

## Benefit spending and possible cuts

The longer recipient and expenditure series, including their treatment of the 2013 reform, older definitions and accounting breaks, are documented in [BENEFIT_HISTORY.md](BENEFIT_HISTORY.md). Their additional plots extend quarterly receipt to 2008, annual actual expenses to 2001, and selected archival observations to 1940.

The first distinction is **programme**, **recipient employment status**, **work obligations/health status**, and **gross versus net expense**. NZ Super, family credits, housing support and main working-age benefits have different populations. Employment status does not follow directly from programme eligibility, and supplementary benefits overlap with main benefits.

A work-ready spending measure would ideally use **actual payments to the work-ready subgroup over the fiscal year**, including each recipient's entitlement and earnings abatement. A June recipient share cannot recover this exactly; B09–B10 use it only as an explicit scenario with a payment-rate sensitivity band.

For a specified proposal:

`net Crown saving = gross payment reduction − lost tax on transfers − replacement assistance − implementation costs + other induced fiscal effects`.

The last term can have either sign and requires a model or evidence. Recipient removal, a rate cut, sanctions and improved employment outcomes should be costed separately. The generic cut curves do not cost a particular announced proposal. If that proposal is later specified, its target population, rate/duration, exemptions and start date can be mapped into the pack rather than assuming a percentage cut to the entire benefit budget.
