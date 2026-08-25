source(file.path("scripts", "00_config.R"))
check_packages(c("officer", "flextable", "data.table"))
suppressPackageStartupMessages({
  library(officer)
  library(flextable)
  library(data.table)
})

add_table <- function(doc, x, font_size = 8) {
  ft <- flextable(as.data.frame(x)) |>
    theme_booktabs() |>
    bg(part = "header", bg = "#D9EAF7") |>
    bold(part = "header") |>
    fontsize(size = font_size, part = "all") |>
    valign(valign = "top", part = "all") |>
    autofit() |>
    fit_to_width(max_width = 6.5)
  body_add_flextable(doc, ft)
}

add_reference <- function(doc, number, citation, url, relevance) {
  doc <- body_add_par(doc, paste0(number, ". ", citation), style = "heading 3")
  doc <- body_add_par(doc, relevance)
  body_add_par(doc, url)
}

methods <- data.table(
  Method = c(
    "Naive / ARIMA benchmark",
    "Structural levels regression",
    "Dynamic regression / ARIMAX",
    "ARDL / error-correction model",
    "VAR / Bayesian VAR",
    "Panel error-correction model",
    "Official projection-rule framework"
  ),
  `Main idea` = c(
    "Project persistence, trend and past forecast errors without imposing an economic equilibrium.",
    "Relate the spending level or GDP share directly to macroeconomic and demographic drivers.",
    "Model spending changes using current and lagged drivers while allowing serially correlated errors.",
    "Separate a tested long-run levels relationship from short-run adjustment and shocks.",
    "Model spending and macroeconomic variables jointly, allowing feedback among them.",
    "Use cross-country information to estimate common or average long-run relationships with country-specific dynamics.",
    "Join a near-term budget anchor to demographic, economic and policy projection rules over the long run."
  ),
  Strength = c(
    "Transparent benchmark; parsimonious; often hard to beat at short horizons.",
    "Easy to explain and useful for attribution.",
    "Good forecasting compromise when external drivers have credible projections.",
    "Economically interpretable if cointegration and stable adjustment are established.",
    "Captures feedback and supplies internally consistent scenarios.",
    "More statistical power than a short single-country annual sample.",
    "Closest to how fiscal institutions actually construct policy-consistent projections."
  ),
  `Main limitation` = c(
    "Can extrapolate a trend mechanically and ignores known policy or demographic information.",
    "Risk of spurious regression, unstable coefficients and implausible long-run extrapolation.",
    "Future paths depend on assumed external drivers; differencing can discard useful level information.",
    "Invalid as an equilibrium model when the variables are not cointegrated; highly sensitive to breaks in short samples.",
    "Parameter-heavy and difficult to estimate with annual data; unrestricted forecasts can drift.",
    "Cross-country pooling may conceal institutional differences and is not automatically transferable to Australia.",
    "Often relies on stylised assumptions and judgement rather than estimated aggregate behavioural coefficients."
  )
)

drivers <- data.table(
  Driver = c(
    "Output or income", "Relative public-service prices", "Business cycle",
    "Age structure", "Population", "Policy and institutions", "Interest costs and debt"
  ),
  `Typical treatment` = c(
    "Real GDP, potential output or real GDP per capita; levels in cointegrating models and changes in dynamic models.",
    "A government-consumption or public-service deflator relative to the GDP deflator; motivated by labour-intensive service production and Baumol-type cost pressure.",
    "Unemployment, output gap or cyclical adjustment; normally a short-run rather than permanent level driver.",
    "Youth and older-age shares or dependency ratios in aggregate equations; age-cost profiles in direct spending projections.",
    "Often used in spending-per-person identities; potentially redundant when the dependent variable and other regressors already embody scale.",
    "Budget rules, elections, programme eligibility, wars, crises and discrete reforms; represented by anchors, dummies or scenarios.",
    "Modelled separately when total expenditure includes interest because it is mechanically linked to debt stocks and interest rates."
  ),
  `Interpretive caution` = c(
    "A positive historical association does not by itself establish Wagner's law or causality.",
    "Choice of deflator can dominate long projections and requires a separately defensible future path.",
    "Automatic stabilisers and discretionary responses can produce different signs.",
    "Linear age effects are restrictive; age shares are highly correlated and profile indices impose their weights.",
    "Total population and age shares can create multicollinearity in small annual samples.",
    "Policy regimes are not stable statistical laws.",
    "Interest should not be treated as an ordinary age-related primary-spending category."
  )
)

doc <- read_docx()
doc <- body_add_par(doc, "Top-down government spending forecasts", style = "heading 1")
doc <- body_add_par(doc, "A short literature review of methods and their interpretation", style = "centered")
doc <- body_add_par(doc, "Internal reading note | 25 August 2026")

doc <- body_add_par(doc, "Executive summary", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "There is no single standard top-down model for forecasting aggregate government spending. ",
  "The literature divides into three overlapping strands: statistical forecasting models, econometric studies of the long-run spending-output relationship, and official fiscal projection frameworks. ",
  "These strands have different purposes. A model that explains historical co-movement is not necessarily the best forecast, while an official projection may deliberately impose current-policy assumptions rather than estimate a behavioural law."
))
doc <- body_add_par(doc, paste0(
  "For a single country with roughly 40-50 annual observations, the most defensible empirical strategy is usually a small model set: a univariate benchmark; a transparent structural levels equation; a parsimonious dynamic regression in changes; and an error-correction model only when cointegration is demonstrated. ",
  "Official or bottom-up projections should be retained as an anchor and cross-check. Forecast selection should be based primarily on rolling out-of-sample performance, stability, sensible long-run behaviour and transparent assumptions - not in-sample fit alone."
))

doc <- body_add_par(doc, "1. What is being forecast?", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "Government spending is unusually difficult to forecast because it is both an economic outcome and a policy choice. Economic conditions affect unemployment benefits, health demand, wages, prices and interest costs, but governments can also change eligibility, service standards, defence commitments and fiscal rules. ",
  "Forecasts therefore need a clearly stated conditional interpretation: they describe spending if the assumed macroeconomic path and policy regime hold."
))
doc <- body_add_par(doc, paste0(
  "The target also matters. Total expenditure, primary expenditure, government consumption, cash payments and National Accounts spending are not interchangeable. Interest costs behave differently from primary spending; investment is volatile; transfers react strongly to policy and the cycle; and nominal spending can rise simply because prices rise. ",
  "Researchers variously model real spending, real spending per person, log spending, or spending as a share of GDP. Each choice embeds a different economic question."
))
doc <- body_add_par(doc, paste0(
  "A forecast predicts an uncertain future outcome. A projection is usually conditional on stated assumptions, such as unchanged policy or a return to potential output. Long-horizon fiscal exercises are mostly projections in this latter sense."
))

doc <- body_add_par(doc, "2. Main top-down methods", style = "heading 1")
doc <- add_table(doc, methods, 7.2)

doc <- body_add_par(doc, "2.1 Univariate benchmarks", style = "heading 2")
doc <- body_add_par(doc, paste0(
  "Random walks, autoregressions, exponential smoothing and ARIMA models use only the spending series and its past errors. A generic model is y_t = c + sum_i phi_i y_(t-i) + epsilon_t, possibly after differencing. ",
  "They are valuable because any more elaborate model should demonstrate that its additional variables improve genuine forecast performance. Their weakness is substantive: they cannot use known changes in demographics, relative prices or policy. Over long horizons they often converge to a constant growth rate, a constant level or a deterministic trend."
))

doc <- body_add_par(doc, "2.2 Structural levels equations", style = "heading 2")
doc <- body_add_par(doc, paste0(
  "A structural reduced-form equation writes the spending measure as y_t = alpha + beta' x_t + u_t, where x_t may include income, relative prices, unemployment, terms of trade and demographic shares. ",
  "This is the simplest way to ask whether spending historically moved with plausible drivers and is useful for decomposition. However, trending variables can produce a high R-squared and plausible-looking coefficients even when no stable equilibrium exists. Unit-root, residual and stability diagnostics are therefore essential, and extrapolation can be very sensitive to small coefficient changes."
))

doc <- body_add_par(doc, "2.3 Dynamic regression and ARIMAX", style = "heading 2")
doc <- body_add_par(doc, paste0(
  "Dynamic regressions add lags and serially correlated errors. In a differenced form, a typical equation is Delta y_t = c + theta' Delta x_t + sum_i phi_i Delta y_(t-i) + intervention terms + epsilon_t. ",
  "ARIMAX extends this framework by modelling the remaining autoregressive and moving-average error structure. This is often a useful forecasting compromise: it avoids asserting an unverified equilibrium while allowing projected economic drivers to influence annual changes."
))
doc <- body_add_par(doc, paste0(
  "The main cost of differencing is that slow-moving level information may be discarded. A differenced model estimates whether changes in a driver coincide with changes in spending; it does not directly impose that an older population must support a permanently higher spending level. Forecasts are also conditional on the future paths of every external regressor."
))

doc <- body_add_par(doc, "2.4 ARDL and error correction", style = "heading 2")
doc <- body_add_par(doc, paste0(
  "An error-correction model separates short-run movements from adjustment toward a long-run relationship: Delta y_t = lambda[y_(t-1) - beta' x_(t-1)] + short-run changes + epsilon_t. ",
  "The adjustment coefficient lambda should normally be negative and statistically meaningful. ARDL bounds methods are attractive when regressors may be a mixture of stationary and first-difference stationary variables, but they do not permit variables integrated of order two."
))
doc <- body_add_par(doc, paste0(
  "Cointegration is a prerequisite, not a cosmetic diagnostic. Without evidence that the lagged levels form a stable equilibrium, the model is only an over-parameterised regression in changes with lagged levels attached. Structural breaks, deterministic terms, lag choice and the small-sample distribution of the bounds statistic can materially affect the conclusion."
))

doc <- body_add_par(doc, "2.5 VARs and panel models", style = "heading 2")
doc <- body_add_par(doc, paste0(
  "VAR and Bayesian VAR models treat spending and macroeconomic variables as jointly determined. They are useful when feedback from fiscal policy to output matters, but annual single-country samples support only very small systems. Bayesian shrinkage can reduce overfitting. ",
  "Panel error-correction models instead borrow information across countries. Arpaia and Turrini estimate a pooled mean-group ECM for EU countries, allowing short-run dynamics to differ while testing a common long-run expenditure-output elasticity. This gains precision but answers an average cross-country question rather than identifying Australia's relationship directly."
))

doc <- body_add_par(doc, "3. The government spending-output literature", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "Much of the academic top-down literature is organised around Wagner's law: as an economy develops, demand for public services and state activity may rise more than proportionately. Different formulations relate real expenditure, expenditure per person or the expenditure share to GDP or GDP per person. This variation is substantive. A regression of the spending share on income per person is a stronger claim than a regression of the spending level on total output."
))
doc <- body_add_par(doc, paste0(
  "The empirical evidence is mixed. Akitoby and co-authors combine long-run co-movement with short-run cyclicality and find cointegration for at least one spending aggregate in many developing countries, but results vary across categories and countries. Arpaia and Turrini find a pooled long-run elasticity close to one for cyclically adjusted primary spending and potential output in the EU-15, alongside heterogeneous adjustment speeds. ",
  "A multi-country study by Chang, Liu and Caudill reports no causal relationship for Australia in its sample. These findings support testing a long-run relationship; they do not justify assuming one."
))
doc <- body_add_par(doc, paste0(
  "This literature is primarily explanatory. It is useful for choosing transformations, distinguishing short and long runs, and identifying necessary tests. It rarely establishes that an ECM will outperform a simple dynamic model in a long-horizon real-time forecasting exercise."
))

doc <- body_add_par(doc, "4. Variables commonly used", style = "heading 1")
doc <- add_table(doc, drivers, 7.3)
doc <- body_add_par(doc, paste0(
  "Parsimonious specification is especially important for annual Australian data. Several age shares sum to one and move smoothly, so including many of them together creates mechanical collinearity. Population, GDP and GDP per person also overlap algebraically. ",
  "A variable should be included because it represents a distinct mechanism and has a credible projection, not merely because it improves the historical fit."
))

doc <- body_add_par(doc, "5. How official long-run projections differ", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "Official institutions generally use hybrid systems. Near-term forecasts incorporate budget decisions, administrative information and macroeconomic forecasts. Longer-term projections transition to rules or structural modules. Australian Treasury explicitly separates a near-term forecast period from a medium- and long-term projection period. The Intergenerational Report builds potential GDP from population, participation and productivity, then projects major spending programmes using programme-specific methods."
))
doc <- body_add_par(doc, paste0(
  "The OECD framework similarly projects pensions, health and long-term care separately, incorporating ageing and cost pressures, while other primary spending follows a simpler real-per-capita rule. In its more recent long-term model, health spending per person depends on real GDP per person, the share aged 65+, general inflation and excess health-sector inflation. ",
  "These are structured conditional scenarios rather than unrestricted forecasts from a single aggregate time-series equation."
))
doc <- body_add_par(doc, paste0(
  "This practice explains why top-down econometric models are most useful as an additional lens. They can reveal whether an official or bottom-up projection is far outside historical aggregate relationships, but they cannot incorporate all announced policies, entitlement rules and service-specific pressures without ceasing to be genuinely top-down."
))

doc <- body_add_par(doc, "6. Forecast evaluation", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "In-sample R-squared and residual standard error are not sufficient model-selection criteria. Forecast evaluation should mimic the actual exercise: estimate using only information available at an earlier date, project several years forward, repeat across forecast origins, and calculate errors separately by horizon. RMSE penalises large misses; MAE is more robust; bias identifies systematic over- or under-projection."
))
doc <- body_add_par(doc, paste0(
  "Stability tests and alternative estimation windows matter because fiscal relationships change with policy regimes and exceptional events. Forecasts should also be compared with naive benchmarks and official forecasts, and should include uncertainty intervals or scenario ranges. Australian Treasury uses historical RMSE by forecast horizon when constructing uncertainty intervals around budget forecasts."
))
doc <- body_add_par(doc, paste0(
  "Real-time data can matter. Government spending data are revised, while professional and official forecasters possess policy information unavailable to backward-looking models. Goemans' comparison of US spending forecasts finds that professional forecasts contain information beyond AR, ARX, VAR and central-bank alternatives in many periods. This cautions against interpreting a purely statistical projection as an unconditional forecast of future policy."
))

doc <- body_add_par(doc, "7. Implications for the Australian model set", style = "heading 1")
doc <- body_add_par(doc, paste0(
  "The literature supports retaining a small and interpretable set rather than searching across dozens of equations. A structural levels OLS is useful as a transparent historical comparator and attribution model. A levels ARIMAX tests whether persistence-aware errors improve that relationship, but its long-run path requires particular caution. A differenced ARIMAX is a legitimate forecasting model when cointegration is absent and the external drivers have defensible projections. A hybrid can combine slow structural pressure with short-run macroeconomic changes, but only if each block is parsimonious and stable."
))
doc <- body_add_par(doc, paste0(
  "An ECM is economically appealing because it explicitly connects levels and changes, but it should only be used as the central long-run model after the relevant integration, bounds, adjustment and stability tests support cointegration. Failure of those tests is evidence against the maintained equilibrium specification, not a reason to relax the tests."
))
doc <- body_add_par(doc, paste0(
  "For demographics, separate youth and older-age shares are closer to common aggregate econometric practice. Age-expenditure profiles are well established in official direct projections, but placing a constructed profile index in an aggregate regression imposes lifecycle weights before estimation. It is best treated as a sensitivity until those weights are based on Australian administrative evidence."
))
doc <- body_add_par(doc, paste0(
  "The preferred workflow is therefore triangulation: use official forecasts as the near-term policy anchor; compare a few top-down statistical paths using rolling forecast evidence and stability; and use bottom-up functional projections to explain the long-run pressures. The models should not be mechanically averaged because they condition on different concepts and assumptions."
))

doc <- body_add_par(doc, "8. Suggested reading", style = "heading 1")
doc <- add_reference(doc, 1,
  "Akitoby, Clements, Gupta and Inchauste (2004), The Cyclical and Long-Term Behavior of Government Expenditures in Developing Countries, IMF Working Paper 04/202.",
  "https://www.imf.org/external/pubs/ft/wp/2004/wp04202.pdf",
  "A clear application combining short-run fiscal cyclicality with long-run expenditure-output relationships.")
doc <- add_reference(doc, 2,
  "Arpaia and Turrini (2008), Government Expenditure and Economic Growth in the EU: Long-Run Tendencies and Short-Term Adjustment.",
  "https://www.bancaditalia.it/pubblicazioni/altri-atti-convegni/2007-fiscal-policy/Arpaia_Turrini.pdf?language_id=1",
  "Panel ARDL/error-correction application to cyclically adjusted primary spending and potential output.")
doc <- add_reference(doc, 3,
  "Pesaran, Shin and Smith (2001), Bounds Testing Approaches to the Analysis of Level Relationships.",
  "https://www.repository.cam.ac.uk/items/ba8503f6-0b0c-4952-a38b-6311057b76d5/full",
  "Foundation for ARDL bounds testing when regressors may be I(0) or I(1).")
doc <- add_reference(doc, 4,
  "Chang, Liu and Caudill (2004), A re-examination of Wagner's law for ten countries based on cointegration and error-correction modelling techniques.",
  "https://www.tandfonline.com/doi/abs/10.1080/0960310042000233872",
  "Cross-country time-series evidence including Australia; the abstract reports no causal relationship for Australia in the study sample.")
doc <- add_reference(doc, 5,
  "Guillemette and Turner (2017), The Fiscal Projection Framework in Long-Term Scenarios, OECD Working Paper 1440.",
  "https://www.oecd.org/en/publications/the-fiscal-projection-framework-in-long-term-scenarios_8eddfa18-en.html",
  "Explains the OECD hybrid of separate ageing-related modules and a simple rule for other primary spending.")
doc <- add_reference(doc, 6,
  "OECD (2021), The Long Game: Fiscal Outlooks to 2060 Underline Need for Structural Reform.",
  "https://www.oecd.org/content/dam/oecd/en/publications/reports/2021/10/the-long-game-fiscal-outlooks-to-2060-underline-need-for-structural-reform_93bcf0c3/a112307e-en.pdf",
  "A practical long-run model using income, ageing, inflation and excess health-cost pressures.")
doc <- add_reference(doc, 7,
  "Australian Treasury (2014), Treasury's Medium-Term Economic Projection Methodology.",
  "https://treasury.gov.au/publication/treasurys-medium-term-economic-projection-methodology",
  "Explains the transition from short-run forecasts to medium- and long-run projection rules.")
doc <- add_reference(doc, 8,
  "Australian Treasury (2023), Intergenerational Report 2023.",
  "https://treasury.gov.au/sites/default/files/2023-08/p2023-435150.pdf",
  "Australian application combining potential-output projections with programme-specific fiscal modules.")
doc <- add_reference(doc, 9,
  "Australian Treasury, Estimates of uncertainty around budget forecasts.",
  "https://treasury.gov.au/publication/estimates-of-uncertainty-around-budget-forecasts/estimates-of-uncertainty-around-budget-forecasts/2-theory",
  "Shows the use of horizon-specific historical RMSE to quantify forecast uncertainty.")
doc <- add_reference(doc, 10,
  "Goemans (2025), Time-Varying US Government Spending Anticipation in Real Time, Journal of Forecasting.",
  "https://onlinelibrary.wiley.com/doi/full/10.1002/for.3234",
  "Compares professional, central-bank, AR, ARX and VAR government-spending forecasts using rolling real-time data.")
doc <- add_reference(doc, 11,
  "IMF (2018), Fiscal Transparency Handbook: Fiscal Forecasting and Budgeting.",
  "https://www.elibrary.imf.org/display/book/9781484331859/ch03.xml",
  "Institutional guidance on transparent macro-fiscal assumptions, comparison with external forecasts and independent evaluation.")

output_path <- file.path(documentation_dir, "top_down_government_spending_literature_review.docx")
print(doc, target = output_path)
message("Top-down literature review written to ", output_path)
