"""Build the Word documentation for the version-16 three-agent toy model."""

from pathlib import Path

from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.shared import Pt, RGBColor

from make_public_identifying_moments_audit_docx import (
    TEAL,
    configure_document,
    improve_pagination,
    render_markdown,
)


HERE = Path(__file__).resolve().parent
SOURCE = HERE / "Model_16_three_agent_toy_model_note.md"
OUTPUT = HERE / "Model_16_Three_Agent_Toy_Model_Documentation.docx"


def main() -> None:
    doc = Document()
    configure_document(doc)
    section = doc.sections[0]
    header = section.header.paragraphs[0]
    header.text = "MODEL 16  |  THREE-AGENT TOY SEARCH-AND-MATCHING MODEL"
    header.alignment = WD_ALIGN_PARAGRAPH.RIGHT
    for run in header.runs:
        run.font.name = "Aptos"
        run.font.size = Pt(8)
        run.font.bold = True
        run.font.color.rgb = RGBColor.from_string(TEAL)

    doc.core_properties.title = "Model 16: three-agent toy search-and-matching model"
    doc.core_properties.subject = (
        "Model equations, empirical mapping, calibration, changes from model 15, "
        "and assessment of whether a full-economy model is required"
    )
    doc.core_properties.author = "Prepared for the research team"
    doc.core_properties.keywords = (
        "Coronavirus Supplement, JobSeeker, New Zealand comparison group, "
        "search and matching, difference in differences, toy model"
    )

    lines = SOURCE.read_text(encoding="utf-8").splitlines()
    render_markdown(doc, lines)
    improve_pagination(doc)
    doc.save(OUTPUT)
    print(f"Saved {OUTPUT}")


if __name__ == "__main__":
    main()
