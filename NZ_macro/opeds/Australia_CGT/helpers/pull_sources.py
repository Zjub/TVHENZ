"""Pinned public-source downloads. TLS verified; --cached never uses network.
Run from 01_pull_data.R, or directly with Python. No packages are installed.
"""
from pathlib import Path
import argparse, concurrent.futures, datetime, hashlib, json, re, shutil
import requests, urllib.request
from bs4 import BeautifulSoup
import fitz

ROOT = Path(__file__).resolve().parents[1]
RAW = ROOT / 'raw'
OUT = ROOT / 'processed'
RAW.mkdir(exist_ok=True); OUT.mkdir(exist_ok=True)
CKAN = 'https://data.gov.au/data/api/3/action/package_show?id=faea4485-f407-457d-97f8-3f0822ccd654'
SOURCES = {
 'ato_catalogue': (CKAN, 'ato_catalogue.json', 'ATO Taxation statistics 2023-24 public catalogue'),
 'budget_measures': ('https://budget.gov.au/content/bp2/download/bp2_02_receipt_payment.pdf', 'budget_measures.pdf', 'Budget 2026-27 receipt measures; announcement vintage'),
 'budget_strategy': ('https://budget.gov.au/content/bp1/download/bp1_2026-27.pdf', 'budget_strategy.pdf', 'Budget 2026-27 fiscal strategy and CGT receipts'),
 'budget_explainer': ('https://budget.gov.au/content/factsheets/download/tax-explainers-negative-gearing-capital-gains-tax.pdf', 'budget_explainer.pdf', 'Budget CGT and negative gearing explainer'),
 'enacted_main': ('https://www.legislation.gov.au/C2026A00049/asmade/2026-06-26/text/original/epub/OEBPS/document_1/document_1.html', 'enacted_main.html', 'Act 49 of 2026 as made; check later amendments separately'),
 'enacted_rates': ('https://www.legislation.gov.au/C2026A00050/asmade/2026-06-26/text/original/epub/OEBPS/document_1/document_1.html', 'enacted_rates.html', 'Act 50 of 2026 as made; minimum tax rates'),
 'treasury_reforms': ('https://treasury.gov.au/policy-topics/taxation/budget2026-27', 'treasury_reforms.html', 'Treasury reform summary; live snapshot'),
 'teis': ('https://treasury.gov.au/sites/default/files/2025-12/p2025-721342.pdf', 'teis.pdf', '2025-26 TEIS; pre-Budget benchmark revenue forgone'),
 'teis_data': ('https://treasury.gov.au/sites/default/files/2025-12/p2025-721342-chart-data.xlsx', 'teis.xlsx', 'TEIS distributional chart data'),
 'pbo_data': ('https://www.pbo.gov.au/sites/default/files/2026-07/PBO%202026-27%20Medium-Term%20Budget%20Outlook%20-%20Data_0.xlsx', 'pbo.xlsx', 'PBO July 2026 medium-term projections'),
 'pbo_report': ('https://www.pbo.gov.au/publications-and-data/publications/2026-27-medium-term-budget-outlook', 'pbo.html', 'PBO medium-term assumptions and interpretation'),
 'oecd': ('https://www.oecd.org/content/dam/oecd/en/publications/reports/2025/02/taxing-capital-gains_76a32327/9e33bd2b-en.pdf', 'oecd.pdf', 'Hourani and Perret 2025 OECD WP72; international institutions and evidence'),
 'agersnap_zidar': ('https://www.nber.org/system/files/working_papers/w27705/w27705.pdf', 'agersnap_zidar.pdf', 'Agersnap and Zidar revised working paper December 2020; published 2021'),
 'sarin_etal': ('https://www.nber.org/system/files/working_papers/w28362/w28362.pdf', 'sarin_etal.pdf', 'Sarin Summers Zidar Zwick; scoring capital gains reform'),
 'crs': ('https://www.congress.gov/crs_external_products/R/PDF/R48562/R48562.1.pdf', 'crs.pdf', 'CRS R48562 June 2025; harmonised elasticity definitions'),
}

SOURCES.update({
 'budget_tax_docx': ('https://budget.gov.au/content/bp1/download/bp1_bs-4.docx','budget_tax_statement.docx','Editable Budget Statement 4; published lifetime-income analysis and footnotes'),
 'october_speech': ('https://ministers.treasury.gov.au/ministers/jim-chalmers-2022/speeches/second-reading-speech-treasury-laws-amendment-tax-reform-no-3','october_speech.html','Treasurer October 14 speech distinguishing enacted first tranche and new implementation bill'),
 'personal_rates': ('https://www.legislation.gov.au/C2004A03348/2026-07-01/2026-07-01/text/original/epub/OEBPS/document_1/document_1.html','personal_rates.html','Income Tax Rates Act July 2026 compilation; future 2027-28 schedule'),
 'ifs_reform': ('https://ifs.org.uk/sites/default/files/2024-10/Captial-gains-tax-reform.pdf','ifs_reform.pdf','IFS 2024 capital gains reform; RRA and cash-flow equivalence'),
 'obr': ('https://obr.uk/docs/dlm_uploads/CGT-supplementary-release-Jan-2025.pdf','obr.pdf','OBR January 2025 UK scoring elasticities, not causal estimates'),
 'canada': ('https://www.nber.org/system/files/working_papers/w28514/w28514.pdf','canada.pdf','Lavecchia Tazhitdinova 2021 working version; published 2024'),
 'australia_study': ('https://www.austaxpolicy.com/tax-rate-changes-impact-capital-gains-realisations-evidence-australia/','australia_study.html','Minas Lim Evans explain their own 2018 Australian study'),
 'dowd_2024': ('https://taxpolicycenter.org/sites/default/files/publication/165863/the_sensitivity_of_the_tax_elasticity_of_capital_gains_to_lagged_tax_rates_and_migration.pdf','dowd_2024.pdf','Dowd McClelland 2024 original paper; specification sensitivity'),
 'tranche2': ('https://consult.treasury.gov.au/c2026-792170','tranche2.html','Treasury second-stage CGT consultation; draft versus enacted detail'),
 'rba_bonds': ('https://www.rba.gov.au/statistics/tables/csv/f2.1-data.csv','rba_bonds.csv','RBA monthly government bond yields; live vintage cut at snapshot'),
 'rba_cpi': ('https://www.rba.gov.au/statistics/tables/csv/g1-data.csv','rba_cpi.csv','RBA G1; ABS quarterly headline CPI and inflation'),
})

def main():
 ap=argparse.ArgumentParser(); ap.add_argument('--cached',action='store_true'); ap.add_argument('--missing',action='store_true'); args=ap.parse_args()
 manifest=[]
 def fetch(item):
  key,(url,name,description)=item; p=RAW/name
  refreshed=not args.cached and not (args.missing and p.exists())
  if refreshed:
   last=None
   for attempt in range(3):
    try:
     try:
      r=requests.get(url, timeout=100, headers={'User-Agent':'Australian capital tax public-data research'})
      r.raise_for_status(); content=r.content
     except requests.exceptions.SSLError:
      # Python's Windows trust store may contain an enterprise root absent in certifi.
      # Verification remains enabled in urllib's default SSL context.
      with urllib.request.urlopen(url,timeout=100) as response:content=response.read()
     if name.endswith('.xlsx') and not content.startswith(b'PK'): raise ValueError('Not an XLSX: '+url)
     if name.endswith('.pdf') and not content.startswith(b'%PDF'): raise ValueError('Not a PDF: '+url)
     tmp=p.with_suffix(p.suffix+'.tmp'); tmp.write_bytes(content); tmp.replace(p); last=None; break
    except Exception as e: last=e
   if last: raise last
  if not p.exists(): raise FileNotFoundError('Missing cached source '+str(p))
  b=p.read_bytes()
  return dict(source_id=key,url=url,file=name,description=description,bytes=len(b),sha256=hashlib.sha256(b).hexdigest(),file_mtime_utc=datetime.datetime.fromtimestamp(p.stat().st_mtime,datetime.timezone.utc).isoformat(),retrieved_this_run=refreshed)
 # Catalogue is a dependency of ATO downloads, which are then independent.
 manifest.append(fetch(('ato_catalogue',SOURCES['ato_catalogue'])))
 catalogue=json.loads((RAW/'ato_catalogue.json').read_text('utf-8'))
 assert catalogue['success']
 desired={'CGT - Table 1':'ato_cgt1','CGT - Table 2':'ato_cgt2','Individuals - Table 1':'ato_ind1','Individuals - Table 2':'ato_ind2','Individuals - Table 3':'ato_ind3','Individuals - Table 16':'ato_ind16','Individuals - Table 26':'ato_rental','Individuals - Table 27':'ato_landlords'}
 for r in catalogue['result']['resources']:
  if r['name'] in desired:
   key=desired[r['name']]; SOURCES[key]=(r['url'],key+'.xlsx','ATO 2023-24 '+r['name'])
 assert set(desired.values()) <= set(SOURCES)
 with concurrent.futures.ThreadPoolExecutor(max_workers=5) as pool:
  futures={pool.submit(fetch,i):i[0] for i in SOURCES.items() if i[0]!='ato_catalogue'}
  errors=[]
  for f in concurrent.futures.as_completed(futures):
   try:
    entry=f.result(); manifest.append(entry); print('Saved '+entry['source_id'],flush=True)
   except Exception as e:errors.append((futures[f],str(e)))
  # These two papers are readable in the web research tool but deny direct downloads.
  # Their explicitly transcribed parameters carry URLs and definitions in literature.csv.
  required_errors=[e for e in errors if e[0] not in {'obr','dowd_2024'}]
  if required_errors:raise RuntimeError(str(required_errors))
 import pandas as pd
 pd.DataFrame(errors,columns=['source_id','direct_download_error']).to_csv(OUT/'source_availability.csv',index=False)
 pd.DataFrame(manifest).sort_values('source_id').to_csv(OUT/'source_manifest.csv',index=False)
 for p in RAW.iterdir():
  if p.suffix=='.pdf':
   with fitz.open(p) as doc:
    text='\n\n'.join(f'--- PDF PAGE {i+1} ---\n'+page.get_text() for i,page in enumerate(doc))
   (OUT/(p.stem+'_text.txt')).write_text(text,encoding='utf-8')
  elif p.suffix=='.html':
   soup=BeautifulSoup(p.read_bytes(),'html.parser')
   for s in soup(['script','style','nav']): s.decompose()
   (OUT/(p.stem+'_text.txt')).write_text(soup.get_text(' ',strip=True),encoding='utf-8')
  elif p.suffix=='.docx':
   import zipfile, xml.etree.ElementTree as ET
   with zipfile.ZipFile(p) as z:
    xml=ET.fromstring(z.read('word/document.xml'))
    text='\n'.join(''.join(el.itertext()) for el in xml.iter('{http://schemas.openxmlformats.org/wordprocessingml/2006/main}t'))
   (OUT/(p.stem+'_text.txt')).write_text(text,encoding='utf-8')
 # User material is context, never an instruction source; no embedded objects executed.
 slides=ROOT.parents[2]/'e61 Projects'/'Presentations'/'CGT Post-Budget 2026.pptx'
 if slides.exists():
  from pptx import Presentation
  prs=Presentation(slides)
  text='\n\n'.join(f'SLIDE {i+1}\n'+'\n'.join(s.text for s in sl.shapes if s.has_text_frame) for i,sl in enumerate(prs.slides))
  (OUT/'user_slides_text.txt').write_text(text,encoding='utf-8')
  # Contact sheets of embedded chart pictures, so their assumptions can be read.
  from PIL import Image,ImageOps,ImageDraw
  from io import BytesIO
  panels=[]
  for i,sl in enumerate(prs.slides):
   for s in sl.shapes:
    if s.shape_type==13:
     im=Image.open(BytesIO(s.image.blob)).convert('RGB'); im.thumbnail((900,550))
     panel=Image.new('RGB',(920,590),'white');panel.paste(im,(10,30));ImageDraw.Draw(panel).text((10,8),f'Slide {i+1}: {s.name}',fill='black');panels.append(panel)
  for k in range(0,len(panels),6):
   sheet=Image.new('RGB',(1840,1770),'#dddddd')
   for j,panel in enumerate(panels[k:k+6]):sheet.paste(panel,((j%2)*920,(j//2)*590))
   sheet.save(OUT/f'slides_contact_{k//6+1:02}.jpg')
 print(f'{len(manifest)} public sources saved; PDF and slide text extracted.')

if __name__=='__main__':main()
