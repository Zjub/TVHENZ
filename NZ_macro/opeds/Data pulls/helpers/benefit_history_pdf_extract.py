"""Extract historical MSD tables and Treasury actual expense columns.

PyMuPDF is required. Published notes, original cells, PDF pages and source
vintages are retained. Archival MSD expenses are NOT spliced into Treasury
expenses: they include supplementary assistance and have different coverage.
"""
import csv
import re
import sys
from pathlib import Path

import fitz

if not hasattr(fitz.Page, "find_tables"):
    raise SystemExit("Historical PDF extraction requires PyMuPDF 1.23 or newer (Page.find_tables).")


def write_csv(path, rows):
    with path.open("w", encoding="utf-8", newline="") as file:
        writer = csv.DictWriter(file, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def value(text):
    if not text or not text.strip():
        return None
    # A footnote is attached to the 1950 Family Benefit count in the PDF.
    text = re.sub(r"(\d{1,3}(?:,\d{3})+)[5-9]$", r"\1", text.strip())
    return float(text.replace(",", ""))


def msd_tables(raw_dir, out_dir):
    doc = fitz.open(raw_dir / "msd_statistical_2012.pdf")
    table = doc[21].find_tables().tables[0].extract()
    names = ["Unemployment-related benefits", "Independent Youth Benefit", "Sickness Benefit",
             "Invalids Benefit", "Miners Benefit", "Domestic Purposes Benefit", "Widows Benefit",
             "Orphans and Unsupported Childs Benefit", "Family Benefit", "Transitional Retirement Benefit",
             "NZ Superannuation / predecessor pensions", "Veterans Pension"]
    counts = []
    for row in table:
        if not re.fullmatch(r"\d{4}", row[0] or ""):
            continue
        if len(row) != 13:
            raise ValueError("MSD 2012 table width changed")
        for programme, cell in zip(names, row[1:]):
            counts.append(dict(year=int(row[0]), programme=programme, recipients=value(cell),
                               original_cell=cell or "", source_id="msd_statistical_2012", pdf_page=22,
                               period_basis="March stock" if int(row[0]) < 1990 else "June stock"))
    write_csv(out_dir / "archival_benefit_counts.csv", counts)
    (out_dir / "archival_count_notes.txt").write_text(doc[22].get_text(), encoding="utf-8")

    doc = fitz.open(raw_dir / "msd_statistical_2002.pdf")
    table = doc[59].find_tables().tables[0].extract()
    names = ["Unemployment Benefit", "Independent Youth Benefit (included in unemployment)",
             "Unemployment Benefit - Training", "Sickness Benefit", "Invalids Benefit", "Miners Benefit",
             "EMPTY COLUMN", "Domestic Purposes Benefit", "Widows Benefit",
             "Orphans and Unsupported Childs Benefit", "Family Benefit", "Transitional Retirement Benefit",
             "NZ Superannuation / predecessor pensions", "Veterans Pensions"]
    expenses = []
    for row in table:
        if not re.fullmatch(r"\d{4}", row[0] or ""):
            continue
        if len(row) != 15 or (row[7] or "").strip():
            raise ValueError("MSD 2002 table width / empty column changed")
        for programme, cell in zip(names, row[1:]):
            if programme == "EMPTY COLUMN":
                continue
            amount = value(cell)
            expenses.append(dict(year=int(row[0]), programme=programme,
                                 value_millions=amount / 1000 if amount is not None else None,
                                 original_cell=cell or "", source_id="msd_statistical_2002", pdf_page=60,
                                 period_basis="March year" if int(row[0]) < 1990 else "June year",
                                 accounting_basis="Cash" if int(row[0]) <= 1994 else "Accrual"))
    write_csv(out_dir / "archival_benefit_expenses.csv", expenses)
    text = doc[59].get_text()
    (out_dir / "archival_expense_notes.txt").write_text(text[text.index("Notes:"):], encoding="utf-8")


def treasury_table(path, vintage):
    doc = fitz.open(path)
    page = next(p for p in doc if "welfare benefit expenses" in p.get_text().lower()
                and "New Zealand Superannuation" in p.get_text())
    heading = page.search_for("welfare benefit expenses")[0]
    words = page.get_text("words")
    year_words = [w for w in words if re.fullmatch(r"20\d{2}", w[4]) and w[1] > heading.y1]
    year_y = min(w[1] for w in year_words)
    year_words = sorted([w for w in year_words if abs(w[1] - year_y) < 2], key=lambda w: w[0])
    if len(year_words) != 10:
        raise ValueError(f"Expected ten years in Treasury table: {path}")
    years = [int(w[4]) for w in year_words]
    centers = [(w[0] + w[2]) / 2 for w in year_words]
    left = centers[0] - (centers[1] - centers[0]) / 2
    status_words = [w for w in words if w[4] in ("Actual", "Forecast")
                    and year_y < w[1] < year_y + 20 and w[0] >= left]
    status_words.sort(key=lambda w: w[0])
    if [w[4] for w in status_words] != ["Actual"] * 5 + ["Forecast"] * 5:
        raise ValueError(f"Treasury actual/forecast header changed: {path}")
    numeric = [w for w in words if w[0] >= left and w[1] > year_y + 14
               and re.fullmatch(r"(?:[\d,]+|\.\.)", w[4])]
    groups = []
    for w in sorted(numeric, key=lambda w: (w[1], w[0])):
        yc = (w[1] + w[3]) / 2
        if groups and abs(yc - groups[-1][0]) < 2:
            groups[-1][1].append(w)
        else:
            groups.append((yc, [w]))
    rows = []
    found_total = False
    for yc, amounts in groups:
        amounts.sort(key=lambda w: w[0])
        if not 1 <= len(amounts) <= 10:
            continue
        label_words = [w for w in words if w[0] < left and abs((w[1] + w[3]) / 2 - yc) < 3]
        label = " ".join(w[4] for w in sorted(label_words, key=lambda w: w[0]))
        label = re.sub(r"[1-4]+$", "", label).strip()
        if not label:
            continue
        # Verify the cells occupy the intended columns, rather than relying
        # only on the number of tokens on a row.
        columns = {}
        for w in amounts:
            nearest = min(range(10), key=lambda k: abs((w[0] + w[2]) / 2 - centers[k]))
            if nearest in columns:
                raise ValueError(f"Treasury column assignment failed for {label}")
            columns[nearest] = w[4]
        # In 2020, the Wage Subsidy row has a single '..' spanning several
        # pre-scheme years. Use positions, retaining blank cells as missing.
        for j, year in enumerate(years[:5]):
            cell = columns.get(j, "")
            rows.append(dict(year=year, measure=label,
                             value_millions=None if cell in ("", "..") else value(cell),
                             original_cell=cell, status="Actual", source_id=f"treasury_hyefu_{vintage}",
                             source_vintage=vintage, pdf_page=page.number + 1))
        if re.fullmatch(r"(?:Welfare )?Benefit [Ee]xpenses", label, re.I):
            found_total = True
            break
    if not found_total or not any(r["measure"] == "New Zealand Superannuation" for r in rows):
        raise ValueError(f"Treasury benefit table incomplete: {path}")
    return rows


if __name__ == "__main__":
    raw_dir, out_dir = map(Path, sys.argv[1:])
    msd_tables(raw_dir, out_dir)
    rows = []
    for vintage, name in [(2005, "hyefu_2005_expenses.pdf"), (2010, "hyefu_2010.pdf"),
                          (2015, "hyefu_2015.pdf"), (2020, "hyefu_2020.pdf")]:
        rows.extend(treasury_table(raw_dir / name, vintage))
    write_csv(out_dir / "treasury_benefit_history_extracted.csv", rows)
