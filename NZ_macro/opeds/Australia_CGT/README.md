# Australian capital gains tax research pack

Research snapshot: **11 November 2026**. Independent of the New Zealand packs. This is research and reproducible illustration, not an op-ed, tax-return calculator or predetermined argument.

- [Open the 62-chart gallery](figures/index.html)
- [Ten-year worked comparison: PV taxes and METRs](ten_year_example/EXAMPLE.pdf), with [12 additional plots](ten_year_example/index.html) and a separate reproducible script
- [Research guide and suggested reading paths](RESEARCH_GUIDE.md)
- [Enacted rules, announcements and implementation work](POLICY_STATUS.md)
- [Behavioural evidence, definitions and original links](processed/literature.csv)
- [Numbers for writing without charts](processed/key_findings.csv)
- [Source notes and reporting breaks](processed/source_notes.csv)

Every chart has PNG, SVG and its exact plotted CSV. The searchable local gallery includes notes, source names and links to downloaded originals. Nothing is published externally.

## Run

From `opeds`, using the installed R:

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "Australia_CGT\00_run_all.R" --cached
```

Options:

- No flag: download a fresh copy of each **pinned release URL**, then rebuild. This does not discover new annual releases or update the policy-status research automatically.
- `--cached`: use saved public-source files without network requests; rebuild source tables, models and plots.
- `--missing`: download only sources missing locally, then rebuild.
- `--plots-only`: reuse extracted source tables; rebuild models and plots.

`01_pull_data.R` handles public-source acquisition and extraction; `02_models.R` holds scenario assumptions and calculations; `03_make_plots.R` exports charts and the gallery. The runner stops on required-source or validation failures.

R dependencies: `dplyr`, `tidyr`, `readr`, `purrr`, `ggplot2`, `scales`, `svglite`. Python dependencies: `requests`, `beautifulsoup4`, `pandas`, `openpyxl`, `PyMuPDF`, and, for slide context extraction, `python-pptx` and `Pillow`. These were already available here; the scripts install nothing. Set `AU_CGT_PYTHON` to another interpreter if needed. The local Windows interpreter is detected as a convenience.

The Python helpers perform TLS-verified downloads, extract workbook/PDF tables and read slide text/pictures; **all thought-experiment calculations and plots are in R**. If requests cannot see a Windows enterprise root certificate, urllib's default verified Windows SSL context is used. Certificate verification is never disabled.

Two optional original papers (OBR and Dowd/McClelland) deny direct programmatic downloads. Their parameters were checked through the web research tool and transcribed with URLs, locations, versions and definitions in `literature.csv`. They are not represented as downloaded PDFs. `source_availability.csv` records this limitation, including during cached runs.

`parameters.csv` and `personal_tax_schedule.csv` are **audit outputs**, not configuration files. Change assumptions in `02_models.R` and the corresponding captions in `03_make_plots.R`, then rerun. Keep source/model labels and units when reusing a CSV.

## Provenance and checks

- `processed/source_manifest.csv`: 32 downloaded public sources, URLs, sizes, timestamps and SHA-256 hashes.
- Source extracts retain worksheet/page and row/column locations where relevant.
- `processed/asset_reconciliation.csv`: differences between published ATO totals and their reported asset components; no forced rescaling.
- `processed/validation.csv`: source anchors, disjoint-group checks and accounting/model identities.
- `processed/output_validation.csv`: image, data, link and source-hash checks, written by `helpers/validate_outputs.py`.
- `figures/chart_catalogue.csv`: title, category, status, interpretation caveat and source for each chart.

The original slides remain unchanged. Their extracted text and contact sheets are in `processed/`; they are contextual research material and are not executed. Raw PDFs and slide material are provided for local research; link to the original publisher when sharing a blog post.

The source snapshot is dated; legal implementation details and the parliamentary position need rechecking before an article goes to press. Windows R may emit existing locale/package-build warnings; source scripts use ASCII with Unicode escapes to avoid corrupting labels.
