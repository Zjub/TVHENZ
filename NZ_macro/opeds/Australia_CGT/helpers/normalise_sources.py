"""Extract public workbooks/PDF tables, preserving worksheet/row provenance."""
from pathlib import Path
import csv, datetime, json, re
import openpyxl, pandas as pd, fitz
ROOT=Path(__file__).resolve().parents[1]; RAW=ROOT/'raw'; OUT=ROOT/'processed'
def save(name,rows):
 df=pd.DataFrame(rows); df.to_csv(OUT/(name+'.csv'),index=False);return df
def num(x):
 try:return float(x)
 except (TypeError,ValueError):return None
def clean(x):return re.sub(r'\s+',' ',str(x or '')).strip()
def fy(x):
 m=re.match(r'^(19|20)\d{2}',str(x));return int(m.group()) if m else None
def wb(name):return openpyxl.load_workbook(RAW/(name+'.xlsx'),read_only=True,data_only=True)

# Table 1: three entity columns per year; do not add trust-origin distributions
# to an independently measured universe of trust realisations.
s=wb('ato_cgt1')['Table 1']; a=list(s.values); rows=[]; year=None; section='Schedules'
for j in range(2,38):
 if a[1][j]:year=fy(a[1][j])
 section='Schedules'
 for i,row in enumerate(a[3:],4):
  if row[0] and not row[1]:section=clean(row[0])
  if row[0] and row[1]:rows.append(dict(year=year,entity=a[2][j],section=section,item=clean(row[0]),unit=row[1],value=num(row[j]),sheet='Table 1',source_row=i,source_column=j+1))
save('ato_cgt_items',rows)
a=list(wb('ato_cgt2')['Table 2'].values)
save('ato_gain_ranges',[dict(year=fy(r[0]),entity=r[1],taxable_status=r[2],gain_range=clean(r[3]),count=num(r[4]),net_gain=num(r[5]),estimated_tax=num(r[6]),sheet='Table 2',source_row=i) for i,r in enumerate(a[2:],3) if fy(r[0])])

# Table 3A rows are disjoint sex/status/age/income cells (income ranges vary
# where ATO combines sparse cells). No subtotal rows are present.
a=list(wb('ato_ind3')['Table 3A'].values)
cols={5:'people',7:'taxable_income',21:'wages',39:'interest',41:'unfranked_dividends',43:'franked_dividends',45:'franking_credits',94:'net_gain_people',95:'net_gain',97:'estimated_cgt',98:'gross_gain_people',99:'gross_gain',101:'capital_losses_carried',109:'gross_rent',111:'rental_interest',117:'rental_profit',119:'rental_loss'}
save('individual_cells',[dict(sex=r[0],taxable_status=r[1],age=clean(r[2]),income_range=clean(r[3]),tax_bracket=clean(r[4]),**{name:num(r[j]) for j,name in cols.items()},sheet='Table 3A',source_row=i) for i,r in enumerate(a[2:],3) if r[0] in ['Male','Female']])
a=list(wb('ato_ind16')['Table 16A'].values)
save('individual_percentiles',[dict(percentile=num(r[0]),income_range=clean(r[1]),state=clean(r[2]),sex=clean(r[3]),people=num(r[4]),taxable_income=num(r[5]),net_gain_people=num(r[30]),net_gain=num(r[31]),estimated_cgt=num(r[33]),gross_gain=num(r[35]),sheet='Table 16A',source_row=i) for i,r in enumerate(a[2:],3) if num(r[0]) is not None])
a=list(wb('ato_ind1')['Table 1A'].values); rows=[]
for i,r in enumerate(a[2:],3):
 if r[0] and r[1] in ['$','no.']:
  for j in range(2,len(r)):
   year=fy(a[1][j])
   if year and num(r[j]) is not None:rows.append(dict(year=year,item=clean(r[0]),unit=r[1],value=num(r[j]),sheet='Table 1A',source_row=i,source_column=j+1))
save('individual_history',rows)
a=list(wb('ato_rental')['Table 26'].values)
save('rental_schedules',[dict(year=fy(r[0]),state=clean(r[1]),position=clean(r[2]),schedules=num(r[3]),gross_rent=num(r[9]),interest=num(r[27]),capital_works=num(r[47]),expenses=num(r[49]),net_rent=num(r[51]),sheet='Table 26',source_row=i) for i,r in enumerate(a[2:],3) if fy(r[0])])

w=wb('teis'); a=list(w['Revenue forgone estimates'].values)
save('tax_expenditures',[dict(code=r[0],item=clean(r[1]),year=fy(a[0][j]),value_m=num(r[j]),published_value=r[j],sheet='Revenue forgone estimates',source_row=i,source_column=j+1) for i,r in enumerate(a[1:],2) if str(r[0]).startswith('E') for j in range(2,10)])
for sheet,name in [('C2.7','teis_discount_income'),('C2.8','teis_discount_age')]:
 a=list(w[sheet].values)
 save(name,[dict(group=clean(r[0]),benefit_share_pct=r[1],recipients_thousand=r[2],year=2022,sheet=sheet,source_row=i) for i,r in enumerate(a[2:],3) if r[0] and num(r[1]) is not None])
a=list(wb('pbo')['4-9'].values)
save('pbo_measures',[dict(measure=clean(r[0]),year=fy(a[3][j]),value_bn=num(r[j]),sheet='4-9',source_row=i,source_column=j+1) for i,r in enumerate(a[4:8],5) for j in range(1,12)])

# PDFs: extract line tokens from an identified page and label; assert table
# anchors so a later layout change fails rather than silently changing units.
def after_tokens(text,label,n):
 tail=text.split(label,1)[1]; result=[]
 for l in tail.splitlines():
  l=l.strip()
  if re.fullmatch(r'[-]|\.\.|-?\d[\d,]*(?:\.\d+)?',l):
   result.append(l)
   if len(result)==n:return result
 raise ValueError('Missing numeric tokens: '+label)
def vals(tokens):return [0 if t in ['-','..'] else float(t.replace(',','')) for t in tokens]
with fitz.open(RAW/'budget_measures.pdf') as doc:
 page=doc[20].get_text()
 assert 'From 1 July 2027, the 50 per cent CGT discount' in page
 receipts=after_tokens(page,'Australian Taxation Office',5)
 costs=after_tokens(page,'Total – Payments',5)
save('budget_reform',[dict(year=2025+i,receipts_m=vals(receipts)[i],payments_m=vals(costs)[i],receipt_token=receipts[i],payment_token=costs[i],source_pdf_page=21,concept='Combined negative-gearing and CGT Budget measure; not isolated CGT') for i in range(5)])
with fitz.open(RAW/'budget_strategy.pdf') as doc:
 page=doc[213].get_text();assert 'Table 5.7:' in page
 cgt=vals(after_tokens(page,'Capital gains tax(c)',6));total=vals(after_tokens(page,'Taxation receipts',6))
save('budget_cgt_receipts',[dict(year=2024+i,cgt_m=cgt[i],total_tax_m=total[i],status='Actual' if i==0 else 'Budget estimate',source_pdf_page=214) for i in range(6)])

for name in ['bonds','cpi']:
 with (RAW/('rba_'+name+'.csv')).open(encoding='utf-8-sig') as f:a=list(csv.reader(f))
 start=next(i for i,r in enumerate(a) if r and r[0]=='Series ID'); codes=a[start][1:]; rows=[]
 for row in a[start+1:]:
  if not row:continue
  date=None
  for fmt in ['%d-%b-%Y','%d/%m/%Y']:
   try:date=datetime.datetime.strptime(row[0],fmt).date();break
   except ValueError:pass
  if date is None or date>datetime.date(2026,11,11):continue
  for j,code in enumerate(codes,1):
   if j<len(row) and num(row[j]) is not None:rows.append(dict(date=date.isoformat(),series=code,value=num(row[j]),source_row=a.index(row)+1))
 save('rba_'+name,rows)

# Preserve workbook notes, including the 2019-20 associated-form coverage break.
notes=[]
for name in ['ato_cgt1','ato_cgt2','ato_ind1','ato_ind3','ato_ind16','ato_rental']:
 for i,r in enumerate(wb(name)['Notes'].values,1):
  if any(v is not None for v in r):notes.append(dict(source_id=name,source_row=i,note=' | '.join(clean(v) for v in r if v is not None)))
save('source_notes',notes)
print('Workbook and PDF tables extracted with provenance.')
