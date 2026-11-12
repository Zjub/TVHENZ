"""Assemble presentation-ready charts, document PDFs and output checks."""
from pathlib import Path
import csv, os, re, html, tempfile, subprocess, hashlib, json
from urllib.parse import unquote, urlparse
import pandas as pd, fitz, markdown
from pptx import Presentation
from pptx.util import Inches
from PIL import Image
from bs4 import BeautifulSoup
ROOT=Path(__file__).resolve().parent
FIG=ROOT/'figures'; DATA=ROOT/'data'
cat=pd.read_csv(FIG/'chart_catalogue.csv')
checks=[]
def check(name,ok,detail=''):
 if not ok: raise AssertionError(name+': '+str(detail))
 checks.append(dict(check=name,passed=True,detail=str(detail)))
check('Complete unique chart IDs',len(cat)>=19 and cat.id.tolist()==[f'P{i:02d}' for i in range(1,len(cat)+1)])
deck=Presentation(); deck.slide_width=Inches(14); deck.slide_height=Inches(7.875)
combined=fitz.open()
for r in cat.itertuples():
 for ext in ['png','svg','pdf','csv']:
  p=FIG/f'{r.id}.{ext}'; check(f'{r.id} {ext} present',p.is_file() and p.stat().st_size>50)
 with Image.open(FIG/f'{r.id}.png') as im: check(r.id+' image 16:9',im.size==(2800,1575))
 svg=BeautifulSoup((FIG/f'{r.id}.svg').read_text(encoding='utf-8'),'xml')
 text=' '.join(t.get_text() for t in svg.find_all('text'))
 for axis in [r.x_axis,r.y_axis]: check(r.id+' axis '+axis,axis in text)
 with fitz.open(FIG/f'{r.id}.pdf') as p:
  check(r.id+' one-page PDF',len(p)==1)
  t=p[0].get_text(); check(r.id+' PDF source and snapshot visible','Snapshot: 12 November 2026' in t and 'Source' in t)
  bad=[]
  for block in p[0].get_text('dict')['blocks']:
   for line in block.get('lines',[]):
    for span in line['spans']:
     rect=fitz.Rect(span['bbox'])
     if rect.x0 < -1 or rect.y0 < -1 or rect.x1 > p[0].rect.width+1 or rect.y1 > p[0].rect.height+1: bad.append(span['text'])
  check(r.id+' PDF text inside page',not bad,bad)
  combined.insert_pdf(p)
 slide=deck.slides.add_slide(deck.slide_layouts[6])
 slide.shapes.add_picture(str(FIG/f'{r.id}.png'),0,0,width=deck.slide_width,height=deck.slide_height)
 slide.notes_slide.notes_text_frame.text=f'{r.id}: {r.title}\nPriority: {r.priority}\n{r.note}\nX axis: {r.x_axis}\nY axis: {r.y_axis}\nSource/method: see data/series_selection.csv, data/source_manifest.csv and matching figures/{r.id}.csv.\nEvidence cutoff 12 November 2026; preliminary election results.'
deck.save(ROOT/'Chart_pack.pptx')
combined.save(ROOT/'Chart_pack.pdf',garbage=4,deflate=True); combined.close()
check('Chart PowerPoint has one slide per chart',len(Presentation(ROOT/'Chart_pack.pptx').slides)==len(cat))
original=Path('C:/Users/OEM/Documents/GitHub/TVHENZ/e61 Projects/Presentations/New Zealand election.pptx')
if original.exists():
 info=dict(original_path=str(original),sha256=hashlib.sha256(original.read_bytes()).hexdigest(),slides=len(Presentation(original).slides),method='Read only; chart deck saved separately')
 (ROOT/'source_deck'/'original_provenance.json').write_text(json.dumps(info,indent=2),encoding='utf-8')
browser=next((p for p in [Path(os.environ.get('NZ_PRESENTATION_BROWSER','')),Path('C:/Program Files/Google/Chrome/Application/chrome.exe'),Path('C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe')] if p.is_file()),None)
if browser is None: raise RuntimeError('Set NZ_PRESENTATION_BROWSER to Chrome or Edge')
css='''@page{size:A4;margin:17mm 16mm 19mm}body{font:10.5pt/1.45 Arial;color:#233744}h1{font-size:22pt;color:#007c91;line-height:1.15}h2{font-size:15pt;margin-top:20pt}h3{font-size:12pt}h1,h2,h3{break-after:avoid}p,li{orphans:3;widows:3}table{border-collapse:collapse;width:100%;font-size:9pt;line-height:1.35;margin:10pt 0}th,td{border:.6pt solid #bccbd2;padding:6pt;text-align:left;vertical-align:top;overflow-wrap:anywhere}th{background:#e8f1f4}thead{display:table-header-group}tr{break-inside:avoid}a{color:#007c91;overflow-wrap:anywhere}pre{white-space:pre-wrap;background:#edf3f5;padding:10pt;font-size:9pt}code{overflow-wrap:anywhere}'''
for filename in ['README.md','SLIDE_ADVICE.md']+(['SPECIAL_VOTES.md'] if (ROOT/'SPECIAL_VOTES.md').exists() else []):
 source=ROOT/filename; body=markdown.markdown(source.read_text(encoding='utf-8'),extensions=['tables','fenced_code','sane_lists'])
 body=re.sub(r'(href="[^"#?]*)\.md(?=["#?])',r'\1.pdf',body)
 with tempfile.TemporaryDirectory(prefix='nz_election_pdf_') as tmp:
  tmp=Path(tmp); page=tmp/'document.html'
  page.write_text(f'<!doctype html><html><head><meta charset="utf-8"><base href="{ROOT.as_uri()}/"><style>{css}</style></head><body>{body}</body></html>',encoding='utf-8')
  subprocess.run([str(browser),'--headless','--disable-gpu','--no-first-run','--no-default-browser-check','--disable-extensions','--no-pdf-header-footer','--allow-file-access-from-files','--user-data-dir='+str(tmp/'profile'),'--print-to-pdf='+str(source.with_suffix('.pdf')),page.as_uri()],check=True,capture_output=True,timeout=60)
 with fitz.open(source.with_suffix('.pdf')) as p:
  text='\n'.join(page.get_text() for page in p)
  check(filename+' PDF nonempty and readable',len(text)>300 and '\ufffd' not in text,len(p))
  print(source.with_suffix('.pdf').name,len(p),'pages')
links=0
for rel in ['figures/index.html','README.md','SLIDE_ADVICE.md']+(['SPECIAL_VOTES.md'] if (ROOT/'SPECIAL_VOTES.md').exists() else []):
 p=ROOT/rel
 soup=BeautifulSoup(markdown.markdown(p.read_text(encoding='utf-8'),extensions=['tables']) if p.suffix=='.md' else p.read_text(encoding='utf-8'),'html.parser')
 for tag in soup.find_all(['a','img']):
  target=tag.get('href',tag.get('src','')); parsed=urlparse(target)
  if parsed.scheme or not parsed.path: continue
  dest=(p.parent/unquote(parsed.path)).resolve(); check('Local link '+rel+' '+target,dest.exists()); links+=1
for r in pd.read_csv(DATA/'source_manifest.csv').itertuples():
 check('Raw source hash '+r.source_id,hashlib.sha256((ROOT/'raw'/r.file).read_bytes()).hexdigest()==r.sha256)
pd.DataFrame(checks).to_csv(DATA/'output_validation.csv',index=False)
print(len(checks),'output checks passed;',links,'local links;',len(cat),'charts and chart slides')
