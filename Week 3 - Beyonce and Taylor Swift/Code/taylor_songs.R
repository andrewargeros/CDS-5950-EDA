library(tidyverse)
library(lubridate)

sysfonts::font_add_google('Work Sans')
sysfonts::font_add_google('Bebas Neue')
sysfonts::font_add_google('Parisienne')

showtext::showtext_auto()

sales = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv') %>% 
  filter(artist == 'Taylor Swift') %>% 
  select(title, released) %>% 
  distinct() %>% 
  mutate(released = as.Date(released, format = '%B %d, %Y'),
         title = str_to_upper(title),
         year = year(released))

background_color = 'gray15'
title_color = '#F0EAD6'
text_color = 'gray85'


read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv') %>%
  bind_cols(read_csv("C:/personalscripts/CDS-5950-EDA/Week 3 - Beyonce and Taylor Swift/song_codes.csv")) %>% 
  janitor::clean_names('snake') %>% 
  mutate_if(is.numeric, ~replace_na(.x, 0)) %>% 
  mutate(total = independence + empowerment + fun + breakup_or_heartbreak + love,
         across(where(is.numeric), ~.x/total),
         album2 = str_to_upper(album)) %>% 
  select(-total) %>% 
  pivot_longer(where(is.numeric)) %>%   
  left_join(sales, by = c('album2' = 'title')) %>% 
  mutate(album = fct_reorder(album, year, .fun = min),
         name = ifelse(name == 'breakup_or_heartbreak', "Breakup", str_to_title(name))) %>% 
  ggplot() +
  aes(x = value, y = title, group = name, fill = name) +
  geom_bar(stat = 'identity') +
  scale_x_continuous(labels= scales::percent) +
  scale_fill_manual(values = c('#462245', '#e0af6b', '#813c60', '#be896d', '#cc6b96')) +
  facet_wrap(~album, scales = 'free', ncol = 4) +
  theme_minimal() +
  theme(legend.position = 'bottom',
        plot.title = element_text(family = 'Bebas Neue', size = 130, hjust = 0.5, color = title_color),
        plot.subtitle = element_text(family = 'Parisienne', size = 70, hjust = 0.5, color = title_color),
        text = element_text(family = 'Work Sans', size = 40, color = text_color),
        strip.text = element_text(family = 'Bebas Neue', size = 65, color = title_color),
        plot.background = element_rect(fill = background_color),
        panel.grid = element_line(color = 'gray20'),
        legend.text = element_text(family = 'Bebas Neue', size = 50, color = title_color)) +
  labs(x = element_blank(), 
       y = element_blank(),
       fill = element_blank(),
       caption = "A Chart (Andrew's Version)",
       title = "Joe, John, Jake... and Taylor ('s Version)",
       subtitle = "How have Taylor Swift's Songs, Albums, and Heartbreaks Changed over the Years?")

ggsave('C:/personalscripts/CDS-5950-EDA/Week 3 - Beyonce and Taylor Swift/taylors_songs.png',
       height = 12, width = 22, units = 'in')
