import pandas as pd
import collections
from transformers import pipeline
from tqdm import tqdm

classifier = pipeline('zero-shot-classification')

data = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')

def prep_song(lyrics):
  l = lyrics.split('\n')
  return [i for i in l if i is not '']

def classify(lyrics):
  class_opts = ['Breakup or Heartbreak', 'Love', 'Independence', 'Fun', 'Empowerment']
  c = classifier(lyrics, class_opts, multi_label = False)
  
  return c['labels'][0]

def full_classify(raw_lyrics):
  lyric_list = prep_song(raw_lyrics)
  processed = [classify(lyric) for lyric in lyric_list]
  return dict(collections.Counter(processed))

songs = [full_classify(song) for song in tqdm(data.Lyrics)]

pd.DataFrame(songs).to_csv('song_codes.csv', index=False)