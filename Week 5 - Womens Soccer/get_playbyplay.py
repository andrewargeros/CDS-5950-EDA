from statsbombpy import sb
import pandas as pd

comps = sb.competitions()
comps[comps.competition_name == "Women's World Cup"]

matches = sb.matches(competition_id=72, season_id=30)
matches_list = matches[(matches.home_team == "United States Women's") | (matches.away_team == "United States Women's")]['match_id'].to_list()

def get_data(match_id, what):
  return sb.events(match_id=match_id, split=True, flatten_attrs=False)[what]

all_shots = pd.DataFrame()
all_passes = pd.DataFrame()

for match in matches_list:
  print(f'Querying Data for --- {match} ---')

  game_shots = get_data(match, 'shots')
  game_shots['match_id'] = match

  game_passe = get_data(match, 'passes')
  game_passe['match_id'] = match

  all_shots = all_shots.append(game_shots)
  all_passes = all_passes.append(game_passe)

all_shots.to_csv("WWC_US_matchshots.csv", index=False)
all_passes.to_csv('WWC_US_matchpasss.csv', index=False)