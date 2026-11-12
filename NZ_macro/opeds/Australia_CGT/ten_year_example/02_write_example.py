"""Create the narrative and tables directly from the R model's CSV outputs."""
from pathlib import Path
import pandas as pd
ROOT=Path(__file__).resolve().parent
d=pd.read_csv(ROOT/'data/baseline.csv')
money=lambda v:f'${(0 if abs(v)<0.5 else v):,.0f}'
percent=lambda v:f'{100*v:.2f}%'
def table(frame,columns,labels,formats=None):
 rows=['| '+' | '.join(labels)+' |','| '+' | '.join(['---']*len(columns))+' |']
 for _,r in frame.iterrows():
  rows.append('| '+' | '.join((formats or {}).get(c,str)(r[c]) for c in columns)+' |')
 return '\n'.join(rows)

# Collapse numerically identical outcomes while listing all four vehicles in text.
compact=d[d.asset.isin(['Bank deposit','House: capital appreciation'])].copy()
compact['vehicle']=compact.asset.map({'Bank deposit':'Bank / fully credited company','House: capital appreciation':'House / new business, all appreciation'})
compact['regime']=compact.system.map({'Full-rate income tax':'Full rate; gains taxed on sale','Comprehensive normal-return allowance':'Normal-return allowance','CGT inflation allowance only':'Inflation allowance for CGT only','Broad consumption tax':'Broad GST-style consumption tax'})
pv=table(compact,['vehicle','regime','pv_total_tax','final_consumption_real'],['Saving vehicle','Tax system','PV of all tax','Real final consumption'],{'pv_total_tax':money,'final_consumption_real':money})
metrs=table(compact,['vehicle','regime','required_pre_real_return','metr'],['Saving vehicle','Tax system','Required pre-tax real return','METR'],{'required_pre_real_return':percent,'metr':percent})
payments=table(compact,['vehicle','regime','initial_tax','pv_year10_tax','pv_total_tax'],['Saving vehicle','Tax system','Tax at year 0','PV of tax paid in year 10','PV of all tax'],{c:money for c in ['initial_tax','pv_year10_tax','pv_total_tax']})
b=d[(d.asset=='Bank deposit')&(d.system=='Full-rate income tax')].iloc[0]
h=d[(d.asset=='House: capital appreciation')&(d.system=='Full-rate income tax')].iloc[0]
i=d[(d.asset=='House: capital appreciation')&(d.system=='CGT inflation allowance only')].iloc[0]
g=d[(d.asset=='Bank deposit')&(d.system=='Broad consumption tax')].iloc[0]
scope=pd.read_csv(ROOT/'data/company_allowance_scope.csv')
above=pd.read_csv(ROOT/'data/eight_percent_nominal_return.csv')
above=above[above.asset.isin(['Bank deposit','House: capital appreciation'])].copy()
above['vehicle']=above.asset.map({'Bank deposit':'Bank / company','House: capital appreciation':'House / new business'})
above_table=table(above,['vehicle','system','pv_total_tax','final_consumption_real'],['Saving vehicle','Tax system','PV of all tax at normal discount','Real final consumption'],{'pv_total_tax':money,'final_consumption_real':money})
matched=pd.read_csv(ROOT/'data/matched_project_equivalence.csv')
matched_table=table(matched,['nominal_return','investment','rra_project_pv','cashflow_project_pv'],['Annual nominal return','Same project investment','PV project tax: RRA','PV project tax: cash-flow'],{'nominal_return':percent,'investment':money,'rra_project_pv':money,'cashflow_project_pv':money})

text=f'''# Ten years of saving: present-value tax and METRs

This is a worked thought experiment, not an Australian tax-return calculation. It compares four tax designs on a common labour resource and a common pre-tax total return. The [plots](index.html), [complete numeric results](data/baseline.csv), [payment timeline](data/tax_payment_timeline.csv) and [reproducible R script](01_compare.R) accompany it.

**The main result:** when saving earns exactly the normal return, a comprehensively implemented rate-of-return allowance and a matched broad consumption tax both collect **$30,000 in present value** in this example. CPI indexation alone leaves a tax on the real normal return and treats bank interest differently from capital appreciation. These statements depend on the stated tax bases, integration and timing assumptions.

## 1. Start with the same labour resource

At date zero, Alex earns **$100,000 before tax**. All saving is consumed exactly ten years later. There is no subsequent labour input in the main example.

| Assumption | Value |
|---|---:|
| Initial gross labour income | $100,000 |
| Income tax rate | 30% |
| Company tax rate, where charged | 30% |
| Annual inflation | 2.5% |
| Normal annual real return | 3% |
| Annual nominal total return on every vehicle | 5.575% |
| Nominal normal-return allowance | 5.575% |
| PV discount rate | 5.575% nominal |
| Holding period | 10 years |
| Matching tax-exclusive broad consumption-tax rate | 42.857% |

The nominal return is `(1.025 × 1.03) - 1 = 5.575%`. The return and discount are specified assumptions. A common normal-rate discount makes the present values comparable; it is not a prediction of future market rates.

Under the three income-tax systems, Alex pays $30,000 immediately and invests **$70,000**. Under the broad GST-style system there is no labour-income tax, so Alex invests **$100,000** and pays consumption tax only on spending at year ten.

To match a 30% tax-inclusive income rate, the tax-exclusive consumption rate must be `0.30 / 0.70 = 42.857%`. Both leave $70,000 of consumption from $100,000 if Alex consumes immediately. Using a 30% GST would not match the tax levels. This broad tax is a hypothetical comprehensive consumption tax, not Australia's actual 10% GST with its coverage and housing/financial-service rules.

The distinction between an income base and a consumption base follows the saving-tax framework in the [Mirrlees Review](https://ifs.org.uk/books/13-taxation-household-savings). The following numerical calculations are our own illustrations.

## 2. Specify what each vehicle actually earns

All four have the same **total** pre-tax return. The tax-relevant composition is stated rather than hidden.

- **Bank deposit:** all returns are annual interest, reinvested after paying any tax.
- **Domestic company:** the investment produces annual taxable business profit, retained and reinvested after company tax. At year ten the capital is returned and accumulated profits are distributed with fully usable franking credits. This is an integrated final distribution, not an unfranked third-party share sale. With company and personal rates both 30%, already-taxed profits produce no additional personal tax on distribution.
- **House:** all returns are assumed to be capital appreciation, realised on sale. There is no rent, occupation or housing-service consumption, borrowing, depreciation or transaction cost. Think of the house as a pure capital-gain vehicle for this experiment, not a representative Australian household's home.
- **New unincorporated business:** the initial cash is invested, and its economic return takes the form of additional saleable capital value, such as goodwill. It is realised in a capital sale at year ten. This is **not** an assumption that ordinary sole-trader operating profit is untaxed until withdrawn or until the business is sold. The later section separately examines unpaid labour embodied in a zero-basis business.

The house and business therefore have the same numerical treatment in the main table. That equality is deliberate: tax consequences follow the specified cash flows, basis and timing, rather than the asset's label. T09 varies the split between current income and appreciation while holding the total return fixed.

Australian background sources: [PBO on dividend imputation](https://www.pbo.gov.au/about-budgets/budget-insights/budget-explainers/dividend-imputation-and-franking-credits), [government guidance on sole traders](https://business.gov.au/planning/business-structures-and-types/business-structures/sole-trader), and [business capital gains](https://business.gov.au/finance/tax/capital-gains-tax-for-business). Actual exclusions and concessions are not applied in this model.

## 3. Define the four tax systems

1. **Full-rate income tax, with realisation-based CGT:** labour, interest, current business profits and realised nominal capital gains all face 30%. There is no discount, inflation adjustment or normal allowance. Capital appreciation is taxed on sale, not annually. A strict comprehensive-accrual alternative is given below.
2. **Comprehensive normal-return allowance:** labour is taxed immediately at 30%; capital income above a 5.575% nominal normal allowance is taxed at 30%. The allowance covers all capital income. Unused allowance compounds, and its value is preserved through reinvestment and terminal settlement. Corporate normal returns receive business-level equity relief with an integrated shareholder settlement. This scope matters.
3. **Inflation allowance for capital gains only:** labour, interest and current company profits face the ordinary full rate. The basis of a qualifying capital-gain asset increases with CPI; only the gain over that indexed basis is taxed on sale. Interest and annual corporate profit do not receive this CPI allowance. This is a deliberately partial reform.
4. **Broad GST-style consumption tax:** no income tax on labour or saving returns. Financial saving and business investment are not final consumption; final consumption is taxed at 42.857% of the tax-exclusive price, equivalent to 30% of the total available spending resource.

The tax rules are stable, proportional and known in advance. The model omits progressive brackets, Medicare, transfers, current Australian CGT discounts/minimums, borrowing and asset-price responses. Symmetric-value relief for unused normal allowances is an explicit assumption, not a description of current loss rules.

## 4. Compare present values and final consumption

PV includes **every tax payment**, including year-zero labour tax, annual personal/company tax and the final settlement or consumption tax. Company tax is counted once, net of the shareholder settlement: franking credits are not counted as an additional payment.

`PV(all tax) = sum over years j of tax paid in year j / (1.05575)^j`

Real consumption means final nominal consumption divided by `(1.025)^10`.

{pv}

Under the normal-return allowance and broad consumption tax, final nominal consumption is **{money(g.final_consumption)}**, or **{money(g.final_consumption_real)}** in date-zero purchasing power. No-tax final real consumption would be $100,000 × 1.03^10 = $134,392; each neutral system takes the same 30% share in present value.

At the specified normal return, each row obeys a useful budget check:

`$100,000 = PV(all tax) + final nominal consumption / (1.05575)^10`

This is an accounting check under the common return/discount assumption, not a general identity allowing arbitrary risky payoffs to be discounted at a risk-free rate.

### Bank deposit, worked through

After paying $30,000 on initial wages, Alex deposits $70,000. Under full nominal income taxation, each year's net accumulation factor is `1 + 0.05575 × 0.70 = 1.039025`.

- Final bank balance: `$70,000 × 1.039025^10 = {money(b.final_consumption)}`.
- First year's interest tax: `$70,000 × 5.575% × 30% = $1,170.75`.
- PV of the ten annual interest-tax payments: **{money(b.pv_saving_tax)}**.
- Add initial labour tax: total PV **{money(b.pv_total_tax)}**.
- Annual after-tax real saving return: `1.039025 / 1.025 - 1 = {percent(b.real_net_return)}`.

A CGT-only inflation allowance changes none of those bank-deposit calculations. A comprehensive normal allowance exempts this deposit's entire normal return, leaving only the initial $30,000 tax.

### Pure capital-gain asset, worked through

The house/new-business investment starts at $70,000 and reaches `$70,000 × 1.05575^10 = $120,423` before sale tax.

- **Full-rate realised gain:** tax is `30% × ($120,423 - $70,000) = {money(h.exit_tax)}` at sale. Its PV is **{money(h.pv_saving_tax)}**, giving **{money(h.pv_total_tax)}** including initial labour tax.
- **CPI allowance only:** indexed cost is `$70,000 × 1.025^10`. Tax on the remaining gain is **{money(i.exit_tax)}** at sale, with PV **{money(i.pv_saving_tax)}**. Add initial labour tax for total PV **{money(i.pv_total_tax)}**.
- **Normal allowance:** compounded basis is `$70,000 × 1.05575^10`, equal to sale value. Saving-return tax is zero. Total PV remains **$30,000**.

The full-rate capital-gain asset pays less PV tax than the bank because appreciation is taxed later. Indexation removes the inflation component; it does not remove tax on the normal real return.

### Broad consumption tax, worked through

Alex invests the full untaxed $100,000, which becomes $172,033. A matched broad consumption tax takes **{money(g.exit_tax)}** at year ten, leaving **{money(g.final_consumption)}** of tax-exclusive consumption. Discount the tax by `1.05575^10` and its PV is **$30,000**.

There is no extra initial $30,000 wage tax in this case. Adding one would compare a combined income-plus-consumption tax with the alternative single-base systems.

## 5. Final-year tax is not the same as total PV tax

If “final tax paid” means only the bill in year ten, a bank or company can appear lightly taxed simply because earlier payments are omitted. Here are both quantities:

{payments}

For the bank, the tax on year-ten interest is about $1,652 nominal, or **{money(b.pv_year10_tax)}** in PV. The additional personal exit settlement is zero, but the investor has already borne ten years of interest tax. The timeline CSV keeps these cash flows separate.

## 6. Turn the example into METRs

Start by comparing the cost of consuming now and later. Immediate after-tax consumption is `$100,000 × 70% = $70,000` in **every** regime, including the matched broad GST.

Define Alex's annual real saving return as:

`s = (final consumption / $70,000)^(1/10) / 1.025 - 1`

This removes the initial labour/consumption tax's level effect. Otherwise a labour tax could misleadingly appear to be a tax on the saving return.

There are two useful, different measures:

**A. Tax wedge on the specified return.** Keep every asset's pre-tax real return fixed at 3% and calculate `(3% - s) / 3%`. For the bank under full nominal taxation, the net return is **{percent(b.real_net_return)}**, giving a **{percent(b.specified_return_wedge)}** wedge. This is a descriptive wedge for that specified project; it is not the required-return METR below.

**B. Required-return METR.** Give every investor the same required after-tax real return of 3%. For each tax/asset case, solve for the pre-tax real project return `p` that delivers that return. Then compute:

`METR = (p - 3%) / p`

{metrs}

For the bank, the required nominal return is `5.575% / 70% = 7.9643%`, equivalent to a **{percent(b.required_pre_real_return)}** real return. The resulting METR is **{percent(b.metr)}**. This differs from the **{percent(b.specified_return_wedge)}** wedge on a project whose pre-tax real return was fixed at 3%.

The RRA and broad consumption tax have **zero required-return METR on normal saving** in this model. This does not mean that nobody pays tax: both collect $30,000 PV on the original labour/consumption resource. Nor does it mean excess returns escape tax under the RRA.

The PV/consumption comparison fixes the same pre-tax return for all assets, as requested. A conventional required-return METR comparison necessarily varies the pre-tax return to hold the after-tax required return fixed instead. Presenting the two side by side prevents those experiments being conflated.

## 7. What if “full income tax” means annual economic accrual?

A genuinely comprehensive nominal income tax would include unrealised appreciation every year. With full accrual taxation, liquidity to pay the bill and the same total nominal return, **all four vehicles** would deliver the bank result: **{money(b.pv_total_tax)}** PV tax and **{money(b.final_consumption_real)}** real final consumption. Their METR would be **{percent(b.metr)}**.

The main full-rate table preserves realisation-based CGT so that its deferral advantage is visible. Applying the full rate at sale does not by itself make that system equivalent to annual comprehensive income taxation. [The separate accrual benchmark CSV](data/full_comprehensive_accrual_benchmark.csv) records this alternative explicitly.

## 8. Company integration is a substantive assumption

For ordinary full-rate profit taxation, annual company tax reduces the amount retained. When all profits are distributed, personal tax on the grossed-up distribution is offset by usable credits. With equal company and personal rates there is no extra top-up, but annual company tax was still paid and still reduced compounding.

The main RRA case gives normal equity returns relief **within the business** and uses a consistent shareholder settlement. Other implementations could work, but a shareholder-only allowance cannot be assumed to neutralise an unchanged corporate tax automatically.

T10 tests one explicit incomplete implementation: leave company tax unchanged, bank a compounded allowance on the shareholder's initial investment until exit, and settle all nominal franking credits then. Even allowing the resulting personal refund in full, PV tax is **{money(scope.iloc[1].pv_total_tax)}**, rather than $30,000; real final consumption is **{money(scope.iloc[1].final_consumption_real)}** rather than $94,074. This is a specified implementation example, not a theorem that every shareholder-level RRA fails.

A third-party share sale raises an additional question about the sale price, cost basis and value to the buyer of retained credits. We do not assume that those credits are automatically refunded to the seller or capitalised dollar for dollar in the price.

## 9. The “implicit labour income” variant: a zero-basis business

Now change the starting tax treatment explicitly. Instead of earning a cash wage and investing $70,000, Alex performs unpaid work that creates **$100,000 of immediately saleable business capital value** at date zero. No initial cash wage or recognised taxable profit is assumed. There is no further work or ordinary operating profit; the value then grows at the same 5.575% and is sold at year ten. Its recorded tax basis is zero.

There is no $70,000 after-tax cash investment and no invented $100,000 tax basis. Under full-rate realised-gain taxation, the entire final $172,033 is taxed at 30%. CPI indexation of a zero basis remains zero. A purchase-cost-based RRA also has zero basis to uplift. All three collect about **$51,610 at sale**, or **$30,000 PV**, leaving the same $94,074 of real final consumption as the matched broad GST in this particular variant.

Here, deferring recognition of the **original labour value** supplies a consumption-tax-like treatment of the whole deferred amount. It is a different mechanism from exempting normal returns on already-taxed saving. It must not be silently mixed with the main cash-wage case.

If the initial created value were immediately recognised and taxed under comprehensive economic income, Alex would need cash finance or a fractional disposal to pay the $30,000. The retained claim would then correspond to the already-taxed starting position. We cannot simultaneously assume immediate labour taxation, zero financing cost, and retention of the full untaxed asset value.

This hypothetical does not convert ordinary sole-trader profits into CGT, assume an owner can deduct their unpaid time, or establish that an actual business sale qualifies for CGT treatment. Since the recorded cash investment is zero, it is **not** reported as a capital-investment METR. [The variant's full results](data/labour_embodied_in_zero_basis_business.csv) are separate.

## 10. Why the above-normal extension has different PVs

The normal-return baseline matches. However, the following **8% nominal-return extension is not a clean test of full RRA/cash-flow-tax equivalence**. It invests $70,000 under labour tax plus RRA and $100,000 under the pure broad-consumption-tax scenario. Giving both investment amounts the same above-normal percentage return changes the amount of excess profit generated. This comparison holds asset prices and a scalable excess-return opportunity fixed; it does not hold the size of an economic-rent project fixed.

{above_table}

For the pure-gain house/business, PV tax is $35,354 under RRA and $37,649 under the broad-consumption-tax scenario. Yet final consumption is *higher* in the latter. Higher receipts here therefore cannot simply be read as a higher burden: the two paths generated different amounts of pre-tax income because investment scale differed. The earlier explanation of this extension was too loose about that distinction.

### A matched-project comparison does give exact PV equality

Hold initial investment `K`, final proceeds `V`, allowance `a`, tax rate `t` and horizon `T` identical. A full-value RRA on the project's terminal excess return has:

`PV(project tax under RRA) = t[V - K(1+a)^T] / (1+a)^T`

A cash-flow tax with an immediately valuable investment deduction has:

`PV(project tax under cash-flow tax) = -tK + tV / (1+a)^T`

These are algebraically equal **at normal and above-normal returns**. The upfront investment relief must be included. This project-tax comparison is separate from the main household endowment/financing experiment; it does not silently grant extra household wealth or claim that Australian GST refunds an individual's financial-asset purchase.

{matched_table}

At 8% nominal, the cash-flow tax charges about $64,768 at exit but gives $30,000 initial investment relief. Its net project-tax PV is $7,649, exactly the RRA project-tax PV. At the normal return both project-tax PVs are zero. This equality assumes matched bases, constant rates and fully valuable relief, with discounting at the allowance rate. An actual retail GST comparison must also match producer-level tax treatment, prices, financing, rents and initial asset ownership before claiming complete equivalence.

Thus the main zero-METR result on normal saving remains valid, but the unmatched-scale extension should not be used as evidence against the matched-project equivalence or as a distributional ranking of tax systems.

There are further sensitivities in the data and plots:

- **T07:** holding periods of 1, 2, 5, 10, 20 and 30 years.
- **T09:** change the fraction of the total return received currently and reinvested. Reinvestment adds to nominal, indexed and allowance bases; those contributions are not taxed again as gains.
- **T12:** an alternative interpretation of the earning period: $10,000 earned at the start of each of ten years, each cohort saved until the same final date. These are equal nominal earnings across vehicles, with horizons 10 through 1 years. They are not treated as $100,000 already available at date zero.
- **Discount sensitivity CSV:** PV taxes at 0%, 2.5%, 5.575% and 8% nominal discount rates. Equality of PV RRA and GST at the normal return depends on the chosen timing/opportunity-cost comparison; final consumption comparisons do not require choosing a PV discount.

Owner-occupied housing services, rents, borrowing, risk, imperfect loss relief, changing/progressive tax rates and actual Australian exemptions would each require further assumptions. The purpose here is to make the saving-versus-consumption and income-form mechanisms visible before introducing those complications.

## Reproduce

From `opeds` in PowerShell:

```powershell
& "C:\\Program Files\\R\\R-4.5.2\\bin\\Rscript.exe" "Australia_CGT\\ten_year_example\\01_compare.R"
& "C:\\Users\\OEM\\anaconda3\\python.exe" "Australia_CGT\\ten_year_example\\02_write_example.py"
& "C:\\Users\\OEM\\anaconda3\\python.exe" "Australia_CGT\\helpers\\export_documents.py" --only ten_year_example/EXAMPLE.md
```

All assumptions are in `01_compare.R` and exported to `data/assumptions.csv`. The script checks independent closed forms, the lifetime resource budget, corporate integration, allowance neutrality, basis/reinvestment accounting and the METR roots. No new data downloads or package installations are needed.
'''
(ROOT/'EXAMPLE.md').write_text(text,encoding='utf-8')
print('Wrote EXAMPLE.md from R outputs.')
