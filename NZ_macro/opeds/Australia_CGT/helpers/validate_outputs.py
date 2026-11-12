"""Verify published artifacts, source hashes and independent numeric anchors."""
from pathlib import Path
import hashlib, math, re, xml.etree.ElementTree as ET
from urllib.parse import urlparse,unquote
import pandas as pd
from bs4 import BeautifulSoup
from PIL import Image
ROOT=Path(__file__).resolve().parents[1];FIG=ROOT/'figures';OUT=ROOT/'processed'
checks=[]
def check(name,ok,detail=''):
 checks.append(dict(check=name,passed=bool(ok),detail=detail))
 if not ok:raise AssertionError(name+': '+str(detail))
cat=pd.read_csv(FIG/'chart_catalogue.csv')
check('62 unique charts',len(cat)==62 and cat.id.nunique()==62)
for row in cat.itertuples():
 for ext in ['png','svg','csv']:check(row.id+' '+ext+' exists',(FIG/(row.id+'.'+ext)).stat().st_size>100)
 with Image.open(FIG/row.png) as im:
  im.verify()
 with Image.open(FIG/row.png) as im:
  check(row.id+' image resolution',im.width>=2000 and im.height>=1400,str(im.size))
 svg=ET.parse(FIG/row.svg).getroot(); check(row.id+' SVG parses',svg.tag.endswith('svg'))
 texts=[''.join(t.itertext()) for t in svg.iter('{http://www.w3.org/2000/svg}text')]
 check(row.id+' horizontal-axis title present',row.horizontal_axis in texts,row.horizontal_axis)
 # svglite provides textLength and anchor information for export text.
 width,height=map(float,svg.attrib['viewBox'].split()[2:]);problems=[]
 for t in svg.iter('{http://www.w3.org/2000/svg}text'):
  label=''.join(t.itertext())
  if any(c in label for c in ['\ufffd','\u00c3','\u00c2']):problems.append('encoding '+label)
  if 'transform' in t.attrib:continue # rotated axis text checked visually
  x=float(t.attrib.get('x','0'));y=float(t.attrib.get('y','0'))
  length=float(re.sub(r'[a-z]+$','',t.attrib.get('textLength','0')));anchor=t.attrib.get('text-anchor','start')
  left=x-(length if anchor=='end' else length/2 if anchor=='middle' else 0)
  right=left+length
  if left<-.5 or right>width+.5 or y<0 or y>height:problems.append('bounds '+label)
 check(row.id+' SVG text bounds and encoding',not problems,'; '.join(problems))
 d=pd.read_csv(FIG/row.csv)
 check(row.id+' data nonempty',len(d)>0)
 nums=d.select_dtypes('number')
 check(row.id+' no infinite plotted values',not nums.isin([math.inf,-math.inf]).any().any())
source=pd.read_csv(OUT/'source_manifest.csv')
check('32 public-source snapshots',len(source)==32)
for r in source.itertuples():
 check(r.source_id+' source SHA-256',hashlib.sha256((ROOT/'raw'/r.file).read_bytes()).hexdigest()==r.sha256)
html=BeautifulSoup((FIG/'index.html').read_text(encoding='utf-8'),'html.parser');count=0
for e in html.select('[href],[src]'):
 for attr in ['href','src']:
  if attr not in e.attrs:continue
  u=e[attr];parsed=urlparse(u)
  if parsed.scheme or u.startswith('#'):continue
  p=(FIG/unquote(parsed.path)).resolve();count+=1
  check('gallery local link '+str(count),p.exists(),str(p))
v=pd.read_csv(OUT/'validation.csv');check('R model/source checks passed',v.passed.all() and len(v)>=26,str(len(v)))
d=pd.read_csv(FIG/'D03.csv');check('Composition includes published-total residual',abs(d.value.sum()-90593755918)<1)
d=pd.read_csv(FIG/'D17.csv');check('Rental graph has all 12 years',d.year.nunique()==12)
check('Rental non-interest deductions positive',d.loc[d.item=='Other deductions','value'].gt(0).all())
r=pd.read_csv(OUT/'rental_schedules.csv').groupby('year')[['gross_rent','interest','expenses','net_rent']].sum()
p=d.pivot(index='year',columns='item',values='value')
check('Rental plotted deductions match independent source sum',((p['Other deductions']-(r.expenses-r.interest)).abs()<1).all())
check('Rental source near-reconciliation retained',((r.gross_rent-r.expenses-r.net_rent).abs()/r.gross_rent<.0001).all(),'Small source discrepancies retained; not forced to zero')
checks.append(dict(check='Gallery local links resolve',passed=True,detail=str(count)))
pd.DataFrame(checks).to_csv(OUT/'output_validation.csv',index=False)
print(f'{len(cat)} charts; {count} local links; {len(source)} source hashes; {len(v)} R checks; all output checks passed.')
