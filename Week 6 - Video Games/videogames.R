library(tidyverse)
library(lubridate)
library(ggnewscale)
library(ggimage)

sysfonts::font_add_google('PT Mono')
sysfonts::font_add_google('Play')
showtext::showtext_auto()

games = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

games2 = games %>% 
  mutate(floor_date = paste(month, "01,", year) %>% as.Date(format = "%B %d, %Y")) %>% 
  group_by(floor_date) %>% 
  mutate(rank = row_number(-avg)) %>% 
  inner_join(games %>% 
               group_by(gamename) %>% 
               summarise(avg = mean(avg)) %>% 
               slice_max(avg, n = 10) %>% 
               select(gamename)) %>% 
  ungroup() %>% 
  group_by(gamename) %>% 
  mutate(month_num = row_number(floor_date),
         mindate = min(month_num),
         maxdate = max(month_num)) %>% 
  ungroup() %>% 
  mutate(col = runif(522))

games2 %>% 
  ggplot() + 
  aes(y = reorder(gamename, avg), x = month_num, fill = col) +
  geom_tile(height = 0.1, alpha = 0.75, show.legend = F, linejoin = 'round') +
  scale_fill_gradient(low = '#F11D28', high = "#FFA12C") +
  new_scale_fill() +
  geom_tile(data = games2 %>% filter(rank < 6), 
            aes(x = month_num, fill = as_factor(rank) %>% fct_rev()),
            height = 0.33, width = 0.2) +
  scale_fill_manual(values = wesanderson::wes_palette('Zissou1')) +
  geom_image(aes(image = "C:/PersonalScripts/CDS-5950-EDA/Week 6 - Video Games/rocket.png", x = maxdate+2.5),
             hjust = -5) +
  geom_text(aes(label = gamename, x = maxdate), color = "white", family = "PT Mono", size = 8, vjust = 3.5) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#263238", color = NA),
        legend.position = 'bottom',
        legend.key.size = unit(0.25, 'in'),
        text = element_text(color = "white", family = "Play", size = 50),
        plot.title = element_text(hjust = 0.5, size = 75, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "3-2-1 Blast Off",
       subtitle = "How have Steam's top games 'Blasted Off'?",
       fill = "Game Rank")

ggsave("C:/PersonalScripts/CDS-5950-EDA/Week 6 - Video Games/blast_off.png", height = 7, width = 15)

games %>% 
  filter(str_detect(gamename, "NBA")) %>% 
  select(gamename) %>% 
  distinct()
