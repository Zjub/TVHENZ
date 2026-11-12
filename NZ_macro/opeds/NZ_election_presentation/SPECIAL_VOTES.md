# Which electorates could change on special votes?

Snapshot: **12 November 2026**. The new charts are **P20–P23**, appended to the existing [gallery](figures/index.html), [PowerPoint](Chart_pack.pptx) and [PDF chart pack](Chart_pack.pdf).

Use **[P20](figures/P20.pdf)** for the presentation. It compares the preliminary lead with a clearly labelled benchmark for the number of valid special candidate votes still to come. **[P21](figures/P21.pdf)** shows how the hurdle changes across possible volumes. **[P22](figures/P22.pdf)** includes National gains in Labour-held seats. **[P23](figures/P23.pdf)** shows how the ranking changes under another allocation of special votes.

## What the comparison suggests

Wairarapa and Hamilton East have the lowest arithmetic hurdles under both volume benchmarks. Beyond those, Takanini, Whangārei and Nelson deserve attention as a group. Their ranking depends on assumptions about local special-vote volumes. A lower hurdle does **not** establish a higher probability of a flip: candidate preferences among special voters matter too.

| Electorate | National preliminary lead | Historical-pattern volume benchmark, rounded | Required Labour minus National gap among all valid specials | Labour share of the two candidates' specials needed to tie* |
|---|---:|---:|---:|---:|
| Wairarapa | 139 | 4,100 | 3.4 percentage points | 52.0% |
| Hamilton East | 456 | 5,800 | 7.9 points | 54.4% |
| Takanini | 1,487 | 6,700 | 22.3 points | 62.4% |
| Whangārei | 1,092 | 4,400 | 24.8 points | 65.9% |
| Nelson | 1,210 | 4,500 | 26.9 points | 65.8% |
| Maungakiekie | 2,952 | 8,200 | 36.2 points | 71.6% |

*The final column additionally assumes the two candidates take the same combined share of specials as they took of ordinary votes. It is a scenario, not an estimate of their actual special-vote shares. Calculations use unrounded volumes. The threshold is a tie; the challenger must exceed it to win.

These six have the lowest historical-pattern benchmark hurdles among National's **nine smallest raw preliminary leads**, which also include Kapiti, Hamilton West and Northcote. They are not a modelled ranking of every electorate's probability of changing hands. Under the uniform-volume-ratio benchmark, Takanini falls behind Whangārei and Nelson. See P23 rather than presenting its third-place ranking as established fact.

An intuitive example: with 4,000 valid special candidate votes, Wairarapa's Labour candidate needs a net advantage of **140 votes** to overturn a 139-vote National lead. A special-vote split of Labour 42%, National 38%, and others 20% would produce a net Labour gain of 160. Labour does not need half of *all* specials. NZ electorate votes are first past the post; votes for other candidates do not transfer as Australian preferences do.

## What is observed, and what is assumed?

**Observed:** exact preliminary candidate votes, candidate informal votes and candidate totals for eleven selected electorates, checked against the [Electoral Commission's electorate pages](https://electionresults.govt.nz/electionresults_2026_preliminary/electorate-status.html). Each row's own source URL is in [the input table](data/electorate_candidate_counts.csv). The Commission's [completed preliminary release](https://elections.nz/media-and-news/2026/2026-general-election-preliminary-results) estimates **399,365 special ballots** nationally and reports **2,432,403 ordinary ballots**, including informal votes.

**Historical data:** ordinary and special **valid candidate votes** in the [official 2023 turnout table](https://electionresults.govt.nz/electionresults_2023/statistics/candidate-votes-and-turnout-by-electorate.html), transcribed into [the historical input table](data/special_vote_history_2023.csv). National combined totals are 2,217,854 valid ordinary candidate votes and 524,823 valid special candidate votes. Candidate votes are distinguished from party votes; the national headline of special ballots includes votes that may be invalid or allowed only as a party vote.

**Not found:** an official 2026 electorate-by-electorate count or estimate of the valid special candidate votes still to come. The national estimate is therefore not labelled as a set of official local counts. Preliminary-page “100% counted” means all preliminary voting-place results, with specials still excluded. Checking declarations, ordinary recounts and the allocation of valid candidate votes can change the figures.

Two transparent volume benchmarks fill this gap:

1. **Uniform ratio:** local ordinary valid candidate votes multiplied by the national special-ballots/ordinary-ballots ratio, 399,365 / 2,432,403, approximately 16.4%. This assumes local specials/ordinary ratios and candidate-vote validity resemble the national benchmark.
2. **Historical local pattern:** multiply the first benchmark by the electorate's 2023 special-valid/ordinary-valid ratio divided by the corresponding national 2023 ratio. This allows for a seat historically having relatively many or few specials. It preserves local ordinary-vote scale in 2026 while borrowing the *relative* 2023 pattern.

For example, Takanini had 10,453 valid special candidate votes and 27,417 valid ordinary candidate votes in 2023, a ratio of about 38.1%. The national valid-candidate ratio was about 23.7%. That raises its benchmark volume relative to a uniform allocation. Wairarapa's ratio was about 14.4%, which lowers its benchmark. Neither calculation predicts how the special voters will vote.

The historical benchmark is **unadjusted for boundaries**, enrolment rules or a changing overseas share. Kapiti uses Ōtaki as a predecessor proxy; Hutt South's boundaries also changed. Same-named electorates are not treated as having rigorously identical boundaries. Changes in eligibility, disallowed ballots and party-only ballots affect the conversion from national ballots to local valid candidate votes. P20's whiskers vary the benchmark volume from **half to 1.5 times** its value. These are sensitivity scenarios, not statistical confidence intervals. A third-party forecast was consulted during source discovery, but its probabilities and modelled volumes are not used in these calculations.

## The measure

Let **D** be the preliminary National lead and **S** the number of valid special candidate votes still to come. Let **pL** and **pN** be Labour's and National's shares of those votes. Ignoring ordinary recount changes, Labour overtakes when:

```text
S × (pL − pN) > D

Break-even Labour-minus-National gap, in percentage points = 100 × D / S
```

This is a required **net lead among specials**, not the swing from ordinary voting and not a probability. Other candidates' votes count in the denominator. With a negative D (a Labour preliminary lead), the analogous hurdle applies to National. If the two leading candidates' combined share of special votes is q, Labour's break-even share of their combined votes is:

```text
0.5 × [1 + D / (S × q)]
```

The CSV includes the ordinary two-candidate share used for the optional q assumption. Recounts can move the starting D, so a very small margin deserves attention even independently of specials. The graphs do not assume each contest is independent or calculate the chance of four flips jointly.

## How this relates to four flips and NZ First

**Four net National-to-Labour flips are sufficient in the fixed-party-vote-entitlement example.** National loses four overhang seats, while Labour substitutes electorate MPs for list MPs. The House shrinks from 131 to 127; Labour, Greens, Te Pāti Māori and Opportunity retain 64 seats, enough for an absolute majority of 64. Their willingness to cooperate remains a political question. This does not remove NZ First from Parliament or establish a governing agreement.

There are three qualifications to keep in the talk:

- **Net changes matter.** Mt Roskill is Labour-led by 262 and Hutt South by 889. National gains there would offset National losses elsewhere. P22 includes those two seats; their hurdles say nothing about which way their specials will lean.
- **Four is conditional, not a universal requirement.** Special *party* votes can also change the allocation of list seats and overhangs, allowing a different route to a majority with fewer electorate changes. National-to-Te Pāti Māori transfers would have different overhang arithmetic from National-to-Labour transfers. The closest National challengers in this exercise are Labour candidates.
- **A flip between two opposition parties need not increase the opposition's total seats.** The effect depends on their list entitlements and overhangs, rather than simply the winner's bloc.

My suggested presentation line is: “Wairarapa and Hamilton East have the smallest hurdles. Getting to four net National losses requires additional contests with substantially larger hurdles, and the number of specials matters. The party-vote count is another route by which the majority can change.” This is an interpretation of the plotted arithmetic, not a forecast.

## Reproduce and inspect

Run `python 05_special_vote_hurdles.py`, then `Rscript 03_make_graphs.R`, then `python 04_package.py` from this folder. The new input and output CSVs are in `data/`; the ten algebra/input checks are in `data/special_vote_validation.csv`. The official site returns 403 to direct HTTP requests in this environment, so both official candidate tables were transcribed from the accessible web view with exact URLs and methods preserved. They will need refreshing after the official count, scheduled for 27 November 2026.
