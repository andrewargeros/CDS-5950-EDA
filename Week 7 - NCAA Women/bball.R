library(tidyverse)
library(wesanderson)
library(ggimage)
library(cowplot)
library(magick)

sysfonts::font_add_google('Libre Franklin')
sysfonts::font_add_google('Red Hat Mono')
sysfonts::font_add_google('Teko')
sysfonts::font_add_google('Graduate')
showtext::showtext_auto()

df = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

champs = df %>% 
  filter(tourney_finish == 'Champ') %>% 
  group_by(school) %>% 
  summarise(champs = n_distinct(year)) %>% 
  filter(champs > 1,
         school != "Southern California")

df %>% 
  inner_join(champs) %>% 
  group_by(school) %>% 
  mutate(year_num = row_number(year),
         win_pct = mean(full_percent),
         diff_avg = full_percent - win_pct) %>% 
  ungroup() %>% 
  mutate(school = fct_reorder(school, champs) %>% fct_rev()) %>% 
  ggplot() + 
  aes(xmin = year, xmax = year+1, ymin = 0, ymax = tourney_w, fill = diff_avg) +
  geom_rect(alpha = 0.95) + 
  geom_step(aes(x = year, y = tourney_w), size = 0.75, color = 'gray30') +
  scale_fill_gradientn(colours = wesanderson::wes_palette('Zissou1', type = 'continuous')) +
  facet_wrap(~school) +
  theme_minimal(base_family = 'Red Hat Mono', base_size = 18) +
  theme(strip.text = element_text(family = 'Libre Franklin', face = 'bold', color = 'gray35', size = 22),
        plot.title = element_text(family = 'Libre Franklin', face = 'bold', color = 'gray20', size = 36),
        plot.subtitle = element_text(lineheight = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        plot.background = element_rect(fill = 'white', color = NA)) +
  labs(x = element_blank(),
       y = element_blank(),
       title = "Women's College Basketball's Power 6",
       subtitle = "Dominance in Women's College Basketball is centralized around dominant coaches and teams.
Coaches like Geno Auriemma (UConn), Pat Summitt (Tennessee), and Muffitt McGraw (Notre Dame) 
have led their teams to more victories than anyone else in the NCAA's Big Dance",
       caption = "Andrew Argeros | Data: FiveThirtyEight")

ggsave("C:/PersonalScripts/CDS-5950-EDA/Week 7 - NCAA Women/power6.png", height = 4, width = 6)

links = tribble(~conference, ~link,
'Southeastern', '<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/b/b2/Southeastern_Conference_logo.svg/1200px-Southeastern_Conference_logo.svg.png">',
'Big 12', '<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/2/29/Big_12_Conference_%28cropped%29_logo.svg/1200px-Big_12_Conference_%28cropped%29_logo.svg.png">',
'Atlantic Coast', '<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/c/c7/Atlantic_Coast_Conference_logo.svg/2560px-Atlantic_Coast_Conference_logo.svg.png">',
'Pac-12', '<img src="https://upload.wikimedia.org/wikipedia/en/thumb/a/ac/Pac-12_logo.svg/1200px-Pac-12_logo.svg.png">',
'Big Ten', '<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/3/39/Big_Ten_Conference_logo.svg/1280px-Big_Ten_Conference_logo.svg.png">') %>% 
  mutate(src = str_remove_all(link, "^<img src=|>$") %>% str_remove_all('"'))

df %>% 
  group_by(year, conference) %>% 
  summarise(n = n_distinct(school),
            seed = mean(seed, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(conference) %>% 
  summarise(m = mean(n),
            s = sum(n),
            avg_seed = mean(seed)) %>% 
  ungroup() %>% 
  slice_max(m, n = 5) %>% 
  inner_join(links) %>% 
  ggplot() +
  aes(x = 0, xend = m,  y = reorder(conference, m), yend = reorder(conference, m), color = conference) +
  geom_segment(size = 4, lineend = 'round') +
  geom_image(aes(x = m+0.37, y = reorder(conference, m), image = src), size = 0.07, inherit.aes = F) +
  scale_color_manual(values = c("#0039a8", "#e3443b", "#0081c3", "#0c294b", "#f2c83c")) +
  theme_minimal(base_family = 'Teko', base_size = 30) +
  theme(legend.position = 'none',
        title = element_text(family = 'Graduate', face = 'bold', size = 30, hjust = 3),
        plot.subtitle = element_text(lineheight = 0.5),
        text = element_text(color = 'black'),
        axis.text.y = element_text(size = 36),
        plot.background = element_rect(fill = 'white')) +
  labs(title = "The Power 5 in the Big Dance",
       subtitle = "How many teams from the power 5 make it\nto the big dance in an average year?",
       x = "",
       y = element_blank(), caption = "Data: FiveThirtyEight | Andrew Argeros")

ggsave("C:/PersonalScripts/CDS-5950-EDA/Week 7 - NCAA Women/conference.png", height = 6, width = 6)
