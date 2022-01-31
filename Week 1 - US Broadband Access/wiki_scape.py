import pandas as pd
df = pd.read_html(
    'https://en.wikipedia.org/wiki/2020_United_States_presidential_election_in_Minnesota')[31].droplevel(0, axis=1)

headers = [
    'county',
    'biden_pct',
    'biden_total',
    'trump_pct',
    'trump_total',
    'jorg_pct',
    'jorg_total',
    'other_pct',
    'other_total',
    'votes'
]

df.set_axis(headers, axis=1, inplace=False).to_csv(
    '2020_election_results.csv', index=False)
