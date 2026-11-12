"""Pinned official releases; cached by default, --refresh to download again."""
from pathlib import Path
from urllib.parse import urljoin
from datetime import datetime, timezone
import sys, json, hashlib, requests, pandas as pd
from bs4 import BeautifulSoup
from inspect_sources import urls
ROOT=Path(__file__).resolve().parent
RAW=ROOT/'raw'; DATA=ROOT/'data'
RAW.mkdir(exist_ok=True); DATA.mkdir(exist_ok=True)
manifest=[]
def fetch(key,url,name):
 p=RAW/name
 if '--refresh' in sys.argv or not p.exists():
  r=requests.get(url,timeout=90); r.raise_for_status()
  if name.endswith(('.csv','.xlsx','.zip')) and 'text/html' in r.headers.get('Content-Type',''): raise ValueError('HTML instead of data: '+key)
  p.write_bytes(r.content)
 manifest.append(dict(source_id=key,url=url,file=name,sha256=hashlib.sha256(p.read_bytes()).hexdigest(),file_saved_utc=datetime.fromtimestamp(p.stat().st_mtime,timezone.utc).isoformat(),cutoff='2026-11-12'))
 return p
for key in ['nz_gdp','nz_labour','au_gdp','au_labour','national_kiwi','mmp']:
 try: p=fetch(key,urls[key],key+'.html')
 except requests.HTTPError as e:
  print('Context page unavailable:',key,e); continue
 if key.startswith('nz_'):
  soup=BeautifulSoup(p.read_bytes(),'html.parser')
  objs=[json.loads(v) for el in soup.find_all(True) for v in el.attrs.values() if isinstance(v,str) and 'PageBlocks' in v]
  assert len(objs)==1
  obj=objs[0]
  (RAW/(key+'.json')).write_text(json.dumps(obj,indent=2),encoding='utf-8')
  (RAW/(key+'.txt')).write_text('\n'.join(BeautifulSoup(b.get('Content',''),'html.parser').get_text(' ',strip=True) for b in obj['PageBlocks']),encoding='utf-8')
  name='gross-domestic-product-june-2026-quarter.csv' if key=='nz_gdp' else 'household-labour-force-survey-september-2026-quarter.xlsx'
  doc=next(d for b in obj['PageBlocks'] for d in b.get('BlockDocuments',[]) if d['Name']==name)
  fetch(key+'_table',urljoin(urls[key],doc['DocumentLink']),name)
  if key=='nz_labour':
   doc=next(d for b in obj['PageBlocks'] for d in b.get('BlockDocuments',[]) if d['Name'].endswith('.zip'))
   fetch(key+'_history',urljoin(urls[key],doc['DocumentLink']),doc['Name'])
for key,filename in [('au_gdp','5206001_Key_Aggregates.xlsx'),('au_labour','62020001.xlsx')]:
 fetch(key+'_table',urls[key]+'/'+filename,filename)
# Electoral Commission page is sometimes inaccessible to direct HTTP. The seven
# represented parties were checked against the official page via web browsing.
# Preserve exact published votes/seats; calculate percentages from the stated total.
rows=[('National',706680,42,0),('Labour',710330,21,15),('Greens',321902,2,14),('ACT',264121,1,12),('NZ First',236630,0,12),('Opportunity',144298,0,7),('Te Pati Maori',13845,5,0)]
e=pd.DataFrame(rows,columns=['party','votes','electorate','list'])
e['seats']=e.electorate+e['list']; e['vote_pct']=100*e.votes/2418565; e['seat_pct']=100*e.seats/131
e['status']='Preliminary; special votes not included'; e['source_url']=urls['election']
e.to_csv(DATA/'election_preliminary.csv',index=False)
pd.DataFrame([dict(source_id='election_manual',url=urls['election'],method='Exact seven-party table transcribed from Electoral Commission web view on 12 November 2026; full table totals 2418565 votes and 131 seats',total_valid_votes=2418565,special_votes_estimated=399365,official_results_date='2026-11-27')]).to_csv(DATA/'election_provenance.csv',index=False)
pd.DataFrame(manifest).to_csv(DATA/'source_manifest.csv',index=False)
print('Saved',len(manifest),'official source snapshots and preliminary election table')
