# New Zealand election: presentation update pack

Prepared 12 November 2026 for an Australian audience. Separate from the op-ed and Australian CGT packs. The source presentation is unchanged.

Start with [slide-by-slide advice](SLIDE_ADVICE.md), [advice PDF](SLIDE_ADVICE.pdf), and the [23-chart gallery](figures/index.html). [Chart_pack.pptx](Chart_pack.pptx) contains one graph per slide for easy copying, with methodology/source notes in speaker notes. [Chart_pack.pdf](Chart_pack.pdf) is the chart contact deck. The PowerPoint embeds chart images; underlying figures are also supplied as SVG, PDF, PNG and exact CSV data. The added [special-vote guide](SPECIAL_VOTES.md) and [PDF](SPECIAL_VOTES.pdf) explain P20–P23: electorate leads relative to explicitly assumed outstanding special-vote volumes, using official 2023 local patterns and a uniform-ratio comparison.

## Recommended charts

- P05: total GDP and GDP per person since September 2023.
- P06: unemployment in matching quarterly periods.
- P11/P12: votes versus seats and coalition arithmetic.
- P13: electorate/list seats and the National list-MP issue.
- P18: NZ living-cost levels rather than inflation rates alone.
- P03/P19: useful backup on GDP definitions and electorate-flip arithmetic.

## Reproduce

From this folder, run with installed Python/R executables:

```powershell
python 01_pull_data.py
python 02_prepare_data.py
python 05_special_vote_hurdles.py
Rscript 03_make_graphs.R
python 04_package.py
```

The download script caches date-specific official releases by default; `--refresh` redownloads them. It does not install packages. Python uses pandas, requests, BeautifulSoup, python-pptx, PyMuPDF and Markdown. R uses ggplot2, dplyr, tidyr, readr, scales and svglite. Set `NZ_PRESENTATION_BROWSER` if Chrome/Edge is elsewhere. The existing Windows environment has these dependencies.

## Source and measurement notes

- Quarterly GDP: Stats NZ June 2026 CSV and ABS June 2026 Table 1. Bilateral charts use real expenditure GDP and its per-person equivalent; P03 separately shows NZ's headline production measure. Indexed growth does not compare absolute living standards.
- Labour: Stats NZ September 2026 HLFS CSV inside its release ZIP and ABS September 2026 Table 1. Age 15+, seasonally adjusted. Australian quarters are complete three-month averages of monthly rates. No interpolation or extrapolation.
- Politics: Electoral Commission's completed preliminary release, 7 November 2026. Seven represented-party rows are transcribed exactly and visibly documented in `data/election_provenance.csv`; the direct HTTP page returns 403, while it is readable through web search. No claim of an automated election feed. Total valid party votes includes minor parties omitted from the seven-party chart.
- Policy: National's current KiwiSaver policy page is saved. Other policy assertions in the original deck are assessed or flagged for verification in the advice, rather than treated as enacted outcomes.
- Fiscal and living-cost context: copied September PREFU and HLPI inputs from the earlier local election pack, retaining the original public source URLs, units and actual/forecast status. Original raw inputs are included. These are pinned snapshots, not newly published data.
- `data/source_manifest.csv` records raw file hashes and source URLs. `data/series_selection.csv` records exact series identifiers. `data/validation.csv` and `data/output_validation.csv` report the completed checks.
- `source_deck/` contains extracted text and embedded pictures from the user's six-slide deck. No embedded programs or instructions were executed. The original deck's file hash is recorded by the packaging script.

The election transcription requires manual replacement after official results; update House size and the threshold rather than retaining 66 automatically. P19 holds party-vote entitlements fixed, so it is a mechanism illustration and not a special-vote prediction. Forecast and observed fiscal values remain distinct.
