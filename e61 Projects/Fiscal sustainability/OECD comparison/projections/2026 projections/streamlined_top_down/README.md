# Streamlined structural top-down model

This folder is the colleague-facing top-down workflow. It deliberately does
not reproduce the former grid of level, differenced, hybrid and ECM models.

The common specification is an anchored structural-change bridge:

- slow-moving factors: the population shares aged 0--14, 65--74 and 75+, the relative
  price of government services, and real GDP per capita;
- transitory conditioning factors: changes in unemployment and the terms of
  trade, including one annual lag. These help identify the historical slow
  coefficients and are used in short-horizon validation, but are reset at the
  FY2029-30 PBO anchor rather than accumulated for 40 years;
- separate FY2020, FY2021 and FY2022 level interventions, correctly differenced
  in the annual-change equation;
- conventional public-debt interest excluded from estimation and generated
  later from gross debt and the effective interest rate;
- three spending anchors are produced. The reporting baseline uses the PBO
  forecast through FY2029-30 and extrapolates model-implied changes after that
  date. Alternatives begin at the latest National Accounts actual in FY2024-25
  or the pre-COVID FY2018-19 actual and apply only slow structural changes after
  the relevant anchor;
- debt in the PBO case retains the official debt path through FY2029-30. Debt
  in the latest-actual case is instead simulated recursively from the official
  FY2024-25 debt stock, so it is internally consistent with that spending path.
  The FY2018-19 spending-anchor sensitivity does not generate a debt path,
  because the streamlined debt module begins from the official FY2024-25 stock.

Four main projection cases are produced under all three spending anchors.
Three real-GDP-per-capita paths are applied to the same estimated equation and
are labelled automatically as `GDP PC model: X% growth`, where `X` is that
path's annual real-GDP-per-capita growth rate in 2060. The middle path is
halfway between the higher- and lower-growth paths in log levels. The fourth
case is a separately re-estimated `Model excluding GDP PC`. All non-GDP
projection and debt assumptions use common baseline settings so that the
comparison isolates the real-income channel.

The combined population share aged 65+ is retained as a single robustness
test in the existing estimation-sensitivity table. It is not an additional
reported projection case.

Residual autocorrelation is checked by comparing the BIC-selected error model
with fixed ARMA(0,0), AR(1), MA(1), ARMA(1,1), AR(2) and MA(2) alternatives.
The diagnostic table reports residual tests, structural coefficients and the
long-run endpoint so that short-run error correction is not mistaken for a
new projection scenario.

The structural-only equation and a univariate ARIMA remain in the validation
outputs as diagnostic benchmarks. They are not included among the four
long-horizon GDP scenarios.

The workflow also reruns the complete ten-figure suite with every model
coefficient estimated only through FY2018-19. Those results use the same
projection cases, spending anchors, uncertainty calculations, decompositions
and debt mechanics as the full-sample suite. Their rolling forecast assessment
uses origins from 2005 to 2014, so all five-year evaluation windows also end by
FY2018-19. The separate outputs are written to
`outputs/pre_2020_estimation`; this is an estimation-sample sensitivity, not
merely the FY2018-19 starting-anchor sensitivity in Figure 10.

Run from this directory with:

```powershell
& 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe' --vanilla run_all.R
```

Inputs are read from the parent project's `data/processed` directory. All new
tables, figures, fitted objects and documentation remain inside this folder.
