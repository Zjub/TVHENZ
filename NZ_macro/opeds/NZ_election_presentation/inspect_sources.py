from pathlib import Path
import requests
from bs4 import BeautifulSoup
ROOT = Path(__file__).resolve().parent
urls = {
 'nz_gdp':'https://www.stats.govt.nz/information-releases/gross-domestic-product-june-2026-quarter/',
 'nz_labour':'https://www.stats.govt.nz/information-releases/labour-market-statistics-september-2026-quarter/',
 'au_gdp':'https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-national-income-expenditure-and-product/jun-2026',
 'au_labour':'https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/sep-2026',
 'election':'https://elections.nz/media-and-news/2026/2026-general-election-preliminary-results',
 'national_kiwi':'https://www.national.org.nz/policies/kiwisaver',
 'mmp':'https://elections.nz/democracy-in-nz/what-is-new-zealands-system-of-government/how-are-mps-elected',
}
if __name__ == '__main__':
 (ROOT/'raw').mkdir(exist_ok=True)
 for key,url in urls.items():
  p=ROOT/'raw'/f'{key}.html'
  if not p.exists():
   r=requests.get(url, timeout=60); r.raise_for_status(); p.write_bytes(r.content)
  soup=BeautifulSoup(p.read_bytes(),'html.parser')
  print(key, '\n'+'\n'.join(a.get('href','') for a in soup.select('a[href]') if any(v in a.get('href','').lower() for v in ['xlsx','.csv','.zip'])))
  (ROOT/'raw'/f'{key}.txt').write_text(soup.get_text(' ',strip=True),encoding='utf-8')
