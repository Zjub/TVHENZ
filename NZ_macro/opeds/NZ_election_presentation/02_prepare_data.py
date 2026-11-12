from pathlib import Path
import pandas as pd, numpy as np, zipfile, hashlib, json, shutil
ROOT=Path(__file__).resolve().parent; RAW=ROOT/'raw'; OUT=ROOT/'data'
checks=[]; selections=[]
def check(name,ok):
 assert ok,name
 checks.append(dict(check=name,passed=True))
def date_period(s):
 return pd.to_datetime(s.astype(str).str.replace(r'\.([0-9])$',r'.\g<1>0',regex=True),format='%Y.%m')
def nz_select(d,code,measure,source):
 x=d[d.Series_reference==code].copy()
 check('Nonempty NZ series '+code,len(x)>10)
 x['date']=date_period(x.Period)
 selections.append(dict(country='New Zealand',measure=measure,series_id=code,source=source))
 return pd.DataFrame(dict(country='New Zealand',measure=measure,date=x.date,value=pd.to_numeric(x.Data_value,errors='coerce'),source_series=code)).dropna()
def au_select(filename,code,measure):
 x=pd.read_excel(RAW/filename,sheet_name='Data1',header=None)
 cols=[j for j in range(1,x.shape[1]) if x.iloc[9,j]==code]
 check('Unique ABS series '+code,len(cols)==1); j=cols[0]
 check('ABS series seasonally adjusted '+code,x.iloc[2,j]=='Seasonally Adjusted')
 selections.append(dict(country='Australia',measure=measure,series_id=code,source=filename,description=x.iloc[0,j],unit=x.iloc[1,j]))
 return pd.DataFrame(dict(country='Australia',measure=measure,date=pd.to_datetime(x.iloc[10:,0]),value=pd.to_numeric(x.iloc[10:,j],errors='coerce'),source_series=code)).dropna()
d=pd.read_csv(RAW/'gross-domestic-product-june-2026-quarter.csv',low_memory=False)
g=[]
for code,measure in [('SNEQ.SG02RSC00B15','Real GDP'),('SNEQ.SG09RSC00B15NZ','Real GDP per person'),('SNEQ.SG01RSC00B01','Real GDP production'),('SNEQ.SG09RSC00B01NZ','Real GDP production per person')]:
 g.append(nz_select(d,code,measure,'gross-domestic-product-june-2026-quarter.csv'))
g += [au_select('5206001_Key_Aggregates.xlsx',code,m) for code,m in [('A2304402X','Real GDP'),('A2304404C','Real GDP per person')]]
g=pd.concat(g,ignore_index=True).sort_values(['country','measure','date'])
check('GDP unique country measure quarter',not g.duplicated(['country','measure','date']).any())
g['qoq']=g.groupby(['country','measure']).value.pct_change(fill_method=None)*100
g['yoy']=g.groupby(['country','measure']).value.pct_change(4,fill_method=None)*100
for base in ['2019-12-01','2023-09-01','2000-03-01']:
 b=g[g.date==base].set_index(['country','measure']).value
 g['index_'+base[:7].replace('-','_')]=[100*r.value/b.loc[(r.country,r.measure)] if (r.country,r.measure) in b.index else np.nan for r in g.itertuples()]
g.to_csv(OUT/'gdp_quarterly.csv',index=False)
z=zipfile.ZipFile(next(RAW.glob('labour*.zip')))
with z.open(next(n for n in z.namelist() if 'hlfs-' in n)) as f:
 d=pd.read_csv(f,usecols=['Series_reference','Period','Data_value'],low_memory=False)
lab=[nz_select(d,code,m,'HLFS September 2026 ZIP') for code,m in [('HLFQ.S1F3S','Unemployment rate'),('HLFQ.S1H3S','Employment rate'),('HLFQ.S1E3S','Participation rate'),('HLFQ.SXJ3UZS','Underutilisation rate')]]
monthly=pd.concat([au_select('62020001.xlsx',c,m) for c,m in [('A84423050A','Unemployment rate'),('A84423054K','Employment rate'),('A84423051C','Participation rate')]],ignore_index=True)
monthly.to_csv(OUT/'australia_labour_monthly.csv',index=False)
monthly['quarter']=monthly.date.dt.to_period('Q')
q=monthly.groupby(['country','measure','quarter']).agg(value=('value','mean'),months=('date','size'),source_series=('source_series','first')).reset_index()
q=q[q.months==3].copy(); q['date']=q.quarter.dt.end_time.dt.to_period('M').dt.to_timestamp(); q=q.drop(columns='quarter')
l=pd.concat(lab+[q],ignore_index=True).sort_values(['country','measure','date'])
check('Labour unique country measure quarter',not l.duplicated(['country','measure','date']).any())
check('All rate observations in plausible range',l.value.between(0,100).all())
check('NZ September unemployment equals release',abs(l.loc[(l.country=='New Zealand')&(l.measure=='Unemployment rate')&(l.date=='2026-09-01'),'value'].iloc[0]-5.7)<.001)
check('ABS September unemployment rounds to release',round(monthly.loc[(monthly.measure=='Unemployment rate')&(monthly.date=='2026-09-01'),'value'].iloc[0],1)==4.7)
l.to_csv(OUT/'labour_quarterly.csv',index=False)
e=pd.read_csv(OUT/'election_preliminary.csv')
check('Election electorate seats sum to 71',e.electorate.sum()==71)
check('Election list seats sum to 60',e['list'].sum()==60)
check('Election seats sum to 131',e.seats.sum()==131)
coalitions=[('National + ACT + NZ First',['National','ACT','NZ First']),('Labour + Greens + TPM + Opportunity',['Labour','Greens','Te Pati Maori','Opportunity']),('Labour + Greens + TPM + NZ First',['Labour','Greens','Te Pati Maori','NZ First']),('Labour + Greens + TPM + Opportunity + NZ First',['Labour','Greens','Te Pati Maori','Opportunity','NZ First'])]
c=pd.DataFrame([dict(group=name,seats=e[e.party.isin(parties)].seats.sum(),majority=66,status='Arithmetic only; no claim of coalition agreement') for name,parties in coalitions]); c['margin_to_66']=c.seats-66
c.to_csv(OUT/'coalition_arithmetic.csv',index=False)
check('Incumbent bloc preliminary 67 seats',c.iloc[0].seats==67)
# Reuse and copy explicit September Treasury snapshots from the earlier pack.
# The new pack remains runnable on its own once these local inputs are copied.
old=ROOT.parent/'Data pulls'
for name in ['prefu_fiscal','prefu_benefits','prefu_economic','hlpi']:
 target=OUT/(name+'.csv')
 if not target.exists(): shutil.copy2(old/'processed'/'election_2026'/(name+'.csv'),target)
for name in ['prefu.xlsx','prefu_expenses.xlsx','hlpi_september.csv']:
 target=RAW/name
 if not target.exists(): shutil.copy2(old/'raw'/'election_2026'/name,target)
manifest=pd.read_csv(OUT/'source_manifest.csv')
metadata=old/'processed'/'election_2026'/'source_manifest.csv'
if metadata.exists():
 old_manifest=pd.read_csv(metadata)
 old_manifest[old_manifest.local_file.isin(['prefu.xlsx','prefu_expenses.xlsx','hlpi_september.csv'])].to_csv(OUT/'copied_source_manifest.csv',index=False)
else:
 old_manifest=pd.read_csv(OUT/'copied_source_manifest.csv')
for name in ['prefu.xlsx','prefu_expenses.xlsx','hlpi_september.csv']:
 r=old_manifest[old_manifest.local_file==name].iloc[0]
 manifest=pd.concat([manifest,pd.DataFrame([dict(source_id='copied_'+name,url=r.url,file=name,sha256=hashlib.sha256((RAW/name).read_bytes()).hexdigest(),file_saved_utc=r.downloaded_utc,cutoff='2026-11-12')])],ignore_index=True)
manifest.drop_duplicates('source_id',keep='last').to_csv(OUT/'source_manifest.csv',index=False)
pd.DataFrame(selections).to_csv(OUT/'series_selection.csv',index=False)
pd.DataFrame(checks).to_csv(OUT/'validation.csv',index=False)
latest=pd.concat([g[(g.date=='2026-06-01')&g.measure.isin(['Real GDP','Real GDP per person'])].assign(kind='GDP'),l[l.date=='2026-09-01'].assign(kind='Labour')])
latest.to_csv(OUT/'latest_comparisons.csv',index=False)
print('Prepared GDP/labour histories; ',len(checks),'checks passed')
print(latest[['country','measure','date','value','qoq','yoy']].to_string(index=False))
