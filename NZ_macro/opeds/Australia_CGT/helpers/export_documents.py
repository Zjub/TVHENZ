"""Export the pack's Markdown documents to PDFs using installed Chrome/Edge.

No packages are installed and no content is uploaded. PDFs sit beside their
Markdown originals; links to other Markdown documents point to their PDFs.
"""
from pathlib import Path
import argparse, html, os, re, subprocess, tempfile
import markdown
import fitz

ROOT = Path(__file__).resolve().parents[1]
BROWSERS = [
    Path(os.environ.get('AU_CGT_BROWSER', '')),
    Path('C:/Program Files/Google/Chrome/Application/chrome.exe'),
    Path('C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe'),
]
browser = next((p for p in BROWSERS if p.is_file()), None)
if browser is None:
    raise RuntimeError('Set AU_CGT_BROWSER to Chrome or Edge.')

CSS = '''
@page { size: A4; margin: 17mm 16mm 19mm; }
html { color: #233744; font: 10.5pt/1.48 Arial, sans-serif; }
body { margin: 0; }
h1 { font-size: 22pt; line-height: 1.16; color: #146b8c; margin: 0 0 16pt; }
h2 { font-size: 15pt; line-height: 1.2; margin: 20pt 0 9pt; }
h3 { font-size: 12pt; margin: 14pt 0 7pt; }
h1, h2, h3, h4 { break-after: avoid; }
p, li { orphans: 3; widows: 3; }
p { margin: 0 0 8pt; }
li { margin: 4pt 0; }
ul, ol { padding-left: 19pt; }
a { color: #146b8c; text-decoration: underline; overflow-wrap: anywhere; }
code { font: 9pt/1.4 Consolas, monospace; overflow-wrap: anywhere; }
pre { white-space: pre-wrap; overflow-wrap: anywhere; background: #eef3f5;
      border-left: 3pt solid #146b8c; padding: 9pt; break-inside: avoid; }
table { border-collapse: collapse; width: 100%; margin: 10pt 0 15pt;
        font-size: 9pt; line-height: 1.35; }
thead { display: table-header-group; }
tr { break-inside: avoid; }
th, td { text-align: left; vertical-align: top; padding: 6pt;
         border: 0.6pt solid #b8c9d1; overflow-wrap: anywhere; }
th { background: #e6f0f4; }
blockquote { margin-left: 10pt; padding-left: 10pt; border-left: 2pt solid #b8c9d1; }
img { max-width: 100%; }
'''

parser=argparse.ArgumentParser()
parser.add_argument('--only',nargs='+',help='Markdown paths relative to Australia_CGT; default exports all documents')
args=parser.parse_args()
sources=sorted((ROOT/p).resolve() for p in args.only) if args.only else sorted(ROOT.rglob('*.md'))
for source in sources:
    if ROOT not in source.parents or source.suffix.lower()!='.md':
        raise ValueError('Expected Markdown within Australia_CGT: '+str(source))
results = []
for source in sources:
    text = source.read_text(encoding='utf-8-sig')
    body = markdown.markdown(text, extensions=['tables', 'fenced_code', 'sane_lists'])
    body = re.sub(r'(href="[^"#?]*)\.md(?=["#?])', r'\1.pdf', body)
    first_heading = re.search(r'^#\s+(.+)$', text, re.M)
    title = first_heading.group(1) if first_heading else source.stem
    document = ('<!doctype html><html lang="en"><meta charset="utf-8">'
                f'<base href="{source.parent.as_uri()}/">'
                f'<title>{html.escape(title)}</title><style>{CSS}</style>'
                f'<body>{body}</body></html>')
    dest = source.with_suffix('.pdf')
    with tempfile.TemporaryDirectory(prefix='au_cgt_pdf_') as tmp:
        temp = Path(tmp)
        page = temp / 'document.html'
        page.write_text(document, encoding='utf-8')
        subprocess.run([
            str(browser), '--headless', '--disable-gpu', '--no-first-run',
            '--no-default-browser-check', '--disable-extensions',
            '--no-pdf-header-footer', '--allow-file-access-from-files',
            '--user-data-dir=' + str(temp / 'profile'),
            '--print-to-pdf=' + str(dest), page.as_uri(),
        ], check=True, capture_output=True, timeout=60)
    with fitz.open(dest) as pdf:
        extracted = '\n'.join(p.get_text() for p in pdf)
        if len(extracted.strip()) < 100:
            raise RuntimeError('Empty PDF: ' + str(dest))
        if '\ufffd' in extracted:
            raise RuntimeError('Replacement glyph found in ' + str(dest))
        links = sum(len(p.get_links()) for p in pdf)
        results.append((source.relative_to(ROOT), dest.relative_to(ROOT), len(pdf), links))
        if source.name == 'RESEARCH_GUIDE.md':
            pdf[0].get_pixmap(matrix=fitz.Matrix(1.3, 1.3)).save(ROOT / 'processed' / 'pdf_preview.png')
    print(f'{dest.relative_to(ROOT)}: {results[-1][2]} pages, {links} links')

import csv
manifest=ROOT/'processed'/'pdf_exports.csv'
if args.only and manifest.exists():
    with manifest.open(newline='',encoding='utf-8') as f:
        old=list(csv.DictReader(f))
    replaced={str(r[0]) for r in results}
    results=[(r['markdown'],r['pdf'],r['pages'],r['links']) for r in old if r['markdown'] not in replaced]+results
with (ROOT / 'processed' / 'pdf_exports.csv').open('w', newline='', encoding='utf-8') as f:
    writer = csv.writer(f)
    writer.writerow(['markdown', 'pdf', 'pages', 'links'])
    writer.writerows(results)
print(f'Exported and checked {len(sources)} PDFs; manifest contains {len(results)} documents.')
