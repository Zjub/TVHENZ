"""Extract the published 2026 Treasury PDF tables; called by the R data pull.

Requires PyMuPDF (import fitz). Coordinates identify columns, not estimates.
The full source wording is retained so every extracted estimate can be audited.
"""
import json
import re
import sys
from pathlib import Path

import fitz


def clean(text):
    return re.sub(r"\s+", " ", text).strip()


def extract_tax(path):
    doc = fitz.open(path)
    registry = []
    for page_number in range(8, len(doc)):
        page = doc[page_number]
        # Each provision is a separate text block. Joining nearby lines would
        # incorrectly merge provisions in the densely packed rows.
        groups = []
        for block in page.get_text("dict")["blocks"]:
            # Clip each block to the first column. One PDF block combines
            # 'Te Pou Tupua' with its legislation column; clipping keeps it.
            lines = []
            for line in block.get("lines", []):
                x0, y0, x1, y1 = line["bbox"]
                text = clean("".join(s["text"] for s in line["spans"] if s["bbox"][0] < 152))
                if 70 <= x0 < 152 and 110 <= y0 < 550 and text and text != "Tax Expenditure":
                    lines.append((y0, y1, text))
            if lines:
                groups.append([lines[0][0], lines[-1][1], " ".join(l[2] for l in lines)])
        groups.sort()
        for j, (y0, _, name) in enumerate(groups):
            bottom = groups[j + 1][0] - 1 if j + 1 < len(groups) else 550
            # GST table uses a slightly wider first column.
            if page_number == 20:
                continue
            def col(x0, x1):
                return clean(page.get_text("text", clip=fitz.Rect(x0, y0 - 1, x1, bottom)))
            estimate = col(638, 787)
            amount = re.search(r"\$([\d,.]+)\s*million\s*per annum", estimate, re.I)
            review = re.search(r"Last reviewed\s*(\d{4})", estimate)
            registry.append({
                "provision": name, "sections_and_type": col(152, 217),
                "explanation": col(217, 535), "rationale": col(535, 638),
                "estimate_text": estimate,
                "annual_estimate_millions": float(amount[1].replace(",", "")) if amount else None,
                "last_reviewed": int(review[1]) if review else None,
                "pdf_page": page_number + 1,
            })
    # One GST provision on the final page: use column boundaries from its header.
    page = doc[20]
    registry.append({
        "provision": clean(page.get_text("text", clip=fitz.Rect(73, 128, 166, 300))),
        "sections_and_type": clean(page.get_text("text", clip=fitz.Rect(166, 128, 227, 300))),
        "explanation": clean(page.get_text("text", clip=fitz.Rect(227, 128, 530, 300))),
        "rationale": clean(page.get_text("text", clip=fitz.Rect(530, 128, 665, 300))),
        "estimate_text": clean(page.get_text("text", clip=fitz.Rect(665, 128, 787, 300))),
        "annual_estimate_millions": None, "last_reviewed": None, "pdf_page": 21,
    })
    series = []
    # The rows with seven numbers include two vintage estimates of 2024/25.
    # Keep the Estimated Actual column, and preserve the older as-at value too.
    rows = [
        (6, "Charitable or other public benefit gifts by a company", "Company donations deduction", "income"),
        (6, "Charitable or other public benefits:", "Donations tax credit", "income"),
        (6, "Independent earner tax credit", "Independent earner tax credit", "income"),
        (6, "Māori Authority donations", "Maori Authority donations", "income"),
        (6, "Family tax credit", "Family tax credit", "fiscal"),
        (6, "In-work tax credit", "In-work tax credit", "fiscal"),
        (6, "Minimum Family", "Minimum family tax credit", "fiscal"),
        (6, "Best Start payment", "Best Start payment", "fiscal"),
        (7, "FamilyBoost", "FamilyBoost", "fiscal"),
        (7, "KiwiSaver tax", "KiwiSaver tax credit", "fiscal"),
        (7, "Research and", "R&D tax incentive", "fiscal"),
    ]
    for page_no, prefix, label, basis in rows:
        lines = doc[page_no].get_text(sort=True).splitlines()
        line = next(line for line in lines if line.strip().startswith(prefix))
        values = re.findall(r"(?:\d[\d,]*\.\d+|n/a)", line)
        expected = 7 if basis == "income" else 6
        if len(values) != expected:
            raise ValueError(f"Unexpected table layout for {label}: {line}")
        older = values[4] if basis == "income" else None
        if basis == "income":
            values = values[:4] + values[5:]
        for year, value in zip(range(2021, 2027), values):
            series.append({
                "programme": label, "year_end": year, "year_basis": basis,
                "value_millions": None if value == "n/a" else float(value.replace(",", "")),
                "status": "Actual" if year <= 2024 else ("Estimated actual" if basis == "income" and year == 2025 else "Estimate" if year == 2025 else "Forecast"),
                "pdf_page": page_no + 1,
                "earlier_as_at_value_2025": float(older) if older else None,
            })
    return {"registry": registry, "series": series}


def extract_vote(path):
    doc = fitz.open(path)
    candidates = [p for p in doc if "2026-2031 MYA" in p.get_text() and "Estimate for 2026/27" in p.get_text()]
    page = next(p for p in candidates if "What is Intended to be Achieved" in p.get_text())
    text = page.get_text(sort=True)
    match = re.search(r"Estimate for 2026/27\s+([\d,]+)", text)
    if not match:
        raise ValueError("International screen rebate budget row not found")
    return {"international_screen_budget_2027_millions": float(match[1].replace(",", "")) / 1000,
            "pdf_page": page.number + 1, "source_text": text}


if __name__ == "__main__":
    tax_path, vote_path, output_path = sys.argv[1:]
    result = extract_tax(tax_path)
    result.update(extract_vote(vote_path))
    Path(output_path).write_text(json.dumps(result, ensure_ascii=False, indent=2), encoding="utf-8")
