"""Official preliminary candidate counts and transparent specials-volume scenarios.

Direct official HTML downloads return 403; inputs below were checked against
the Electoral Commission's web-readable electorate-detail pages on 12 Nov 2026.
No third-party forecast probabilities or local volume predictions are used.
"""
from pathlib import Path
import pandas as pd, numpy as np
ROOT=Path(__file__).resolve().parent; DATA=ROOT/'data'
BASE='https://electionresults.govt.nz/electionresults_2026_preliminary/'
# Candidate totals include informals. All candidate totals, informal counts and
# the two candidates' votes below come from the same electorate-detail page.
rows=[
 ('Wairarapa',55,17943,17804,41514,236),
 ('Hamilton East',12,14139,13683,31274,324),
 ('Whangarei',63,15757,14665,39369,317),
 ('Nelson',29,16846,15636,38456,324),
 ('Takanini',46,12120,10633,25755,565),
 ('Kapiti',20,20689,18612,43767,207),
 ('Hamilton West',13,13616,11435,32264,384),
 ('Maungakiekie',24,14626,11674,31857,380),
 ('Northcote',32,15969,12905,33522,278),
 ('Mt Roskill',27,13320,13582,30572,422),
 ('Hutt South',15,17068,17957,39891,294),
]
d=pd.DataFrame(rows,columns=['electorate','electorate_number','national_votes','labour_votes','candidate_total_including_informal','candidate_informal'])
d['ordinary_valid']=d.candidate_total_including_informal-d.candidate_informal
d['signed_national_lead']=d.national_votes-d.labour_votes
d['leader']=np.where(d.signed_national_lead>0,'National','Labour')
d['absolute_margin']=d.signed_national_lead.abs()
d['source_url']=[BASE+f'electorate-details-{n}.html' for n in d.electorate_number]
d['input_method']='Manual transcription from official web view; checked 12 November 2026'
d['status']='Completed preliminary count; specials excluded; ordinary recount outstanding'
d['top_two_fraction_ordinary']=(d.national_votes+d.labour_votes)/d.ordinary_valid
d.to_csv(DATA/'electorate_candidate_counts.csv',index=False)
# These national ballot volumes are the Commission's preliminary release.
# First a uniform-ratio scenario; then allow for observed local variation in
# the 2023 official valid candidate specials/ordinary ratio. Neither is an
# official estimate for a 2026 electorate or a forecast model.
ratio=399365/2432403
d['national_specials_to_ordinary_ratio']=ratio
d['specials_uniform_ratio']=d.ordinary_valid*ratio
history=[('Wairarapa','Wairarapa',38520,5530),('Hamilton East','Hamilton East',29209,7843),('Whangarei','Whangarei',36763,5967),('Nelson','Nelson',35926,6098),('Takanini','Takanini',27417,10453),('Kapiti','Otaki',37605,5840),('Hamilton West','Hamilton West',30357,7225),('Maungakiekie','Maungakiekie',25495,9526),('Northcote','Northcote',30103,7778),('Mt Roskill','Mt Roskill',26870,8442),('Hutt South','Hutt South',35475,7311)]
h=pd.DataFrame(history,columns=['electorate','historical_electorate','ordinary_valid_2023','special_valid_2023'])
h['source_url']='https://electionresults.govt.nz/electionresults_2023/statistics/candidate-votes-and-turnout-by-electorate.html'
h['method']='Manual transcription of official 2023 columns (a) ordinary valid and (b) special valid; checked 12 November 2026'
h['boundary_note']=np.where(h.electorate=='Kapiti','Otaki is a predecessor proxy, not a boundary-adjusted 2026 result',np.where(h.electorate=='Hutt South','2025 boundary changes: unadjusted predecessor result','Same-named predecessor; no polling-place boundary adjustment'))
h.to_csv(DATA/'special_vote_history_2023.csv',index=False)
d=d.merge(h,on='electorate',suffixes=('','_2023'),validate='one_to_one')
d['national_valid_candidate_ratio_2023']=524823/2217854
d['historical_volume_multiplier']=(d.special_valid_2023/d.ordinary_valid_2023)/d.national_valid_candidate_ratio_2023
d['specials_benchmark']=d.specials_uniform_ratio*d.historical_volume_multiplier
d['net_margin_needed_pp_uniform']=100*d.absolute_margin/d.specials_uniform_ratio
d['specials_low_scenario']=d.specials_benchmark*.5
d['specials_high_scenario']=d.specials_benchmark*1.5
for tag,scale in [('central',1),('fewer',.5),('more',1.5)]:
 s=d.specials_benchmark*scale
 d['net_margin_needed_pp_'+tag]=100*d.absolute_margin/s
 d['labour_share_top_two_tie_'+tag]=.5*(1+d.signed_national_lead/(s*d.top_two_fraction_ordinary))*100
d['label']=[f'{r.electorate}  |  {r.leader} +{r.absolute_margin:,}' for r in d.itertuples()]
d['volume_note']='Historical-pattern benchmark: ordinary valid 2026 x (399365/2432403) x local/national 2023 valid specials-to-ordinary ratio; 0.5x and 1.5x are sensitivity scenarios, not confidence intervals'
d.to_csv(DATA/'special_vote_hurdles.csv',index=False)
curves=[]
for r in d[d.leader=='National'].nsmallest(6,'absolute_margin').itertuples():
 for s in range(2000,10001,100):
  curves.append(dict(electorate=r.electorate,special_valid_candidate_votes=s,margin=r.absolute_margin,net_labour_lead_needed_pp=100*r.absolute_margin/s))
pd.DataFrame(curves).to_csv(DATA/'special_vote_volume_curves.csv',index=False)
checks=[]
def check(name,ok):
 assert ok,name
 checks.append(dict(check=name,passed=True))
check('Eleven selected official electorate inputs',len(d)==11 and not d.electorate.duplicated().any())
check('Top two do not exceed valid candidate votes',((d.national_votes+d.labour_votes)<=d.ordinary_valid).all())
check('Every margin is positive and top two fraction valid',d.absolute_margin.gt(0).all() and d.top_two_fraction_ordinary.between(0,1).all())
check('Two smallest National leads are Wairarapa and Hamilton East',d[d.leader=='National'].nsmallest(2,'absolute_margin').electorate.tolist()==['Wairarapa','Hamilton East'])
check('Volume-hurdle identity',np.allclose(d.net_margin_needed_pp_central*d.specials_benchmark/100,d.absolute_margin))
check('Fewer specials require greater margin',np.allclose(d.net_margin_needed_pp_fewer,2*d.net_margin_needed_pp_central))
check('Share threshold algebra',np.allclose((2*d.labour_share_top_two_tie_central/100-1)*d.specials_benchmark*d.top_two_fraction_ordinary,d.signed_national_lead))
check('Benchmarks are positive and denominator exact',ratio>0 and d.specials_benchmark.gt(0).all())
check('Historical mapping one per selected seat',len(h)==len(d) and h.electorate.nunique()==len(d))
check('Historical counts positive and national ratio exact',h.ordinary_valid_2023.gt(0).all() and h.special_valid_2023.gt(0).all() and np.allclose(d.national_valid_candidate_ratio_2023,524823/2217854))
pd.DataFrame(checks).to_csv(DATA/'special_vote_validation.csv',index=False)
pd.DataFrame([dict(name='National estimated special ballots',value=399365,kind='Electoral Commission estimate',url='https://elections.nz/media-and-news/2026/2026-general-election-preliminary-results'),dict(name='National ordinary ballots including informal',value=2432403,kind='Electoral Commission preliminary count',url='https://elections.nz/media-and-news/2026/2026-general-election-preliminary-results'),dict(name='National valid candidate specials 2023',value=524823,kind='Electoral Commission official turnout table combined totals',url=h.source_url.iloc[0]),dict(name='National valid candidate ordinary 2023',value=2217854,kind='Electoral Commission official turnout table combined totals',url=h.source_url.iloc[0]),dict(name='Local benchmark ratio before historical adjustment',value=ratio,kind='Assumption: national specials/ordinary ballot ratio proxies valid candidate votes',url=''),dict(name='Volume low/high multipliers',value='0.5 / 1.5',kind='Sensitivity scenarios; not estimated intervals',url='')]).to_csv(DATA/'special_vote_assumptions.csv',index=False)
print(d[['electorate','leader','absolute_margin','ordinary_valid','specials_benchmark','net_margin_needed_pp_central','labour_share_top_two_tie_central']].to_string(index=False))
print(len(checks),'special-vote checks passed')
