library(tidyverse)
library(wesanderson)

sysfonts::font_add_google('Libre Franklin')
sysfonts::font_add_google('Red Hat Mono')
showtext::showtext_auto()

df = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

champs = df %>% 
  filter(tourney_finish == 'Champ') %>% 
  group_by(school) %>% 
  summarise(champs = n_distinct(year)) %>% 
  filter(champs > 1,
         school != "Southern California")

notes = tribble(
  ~school, ~x, ~y, ~note,
  
)

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

confs = df %>% 
  group_by(year, conference) %>% 
  summarise(n = n_distinct(school),
            seed = mean(seed, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(conference) %>% 
  summarise(m = mean(n),
            s = sum(n),
            avg_seed = mean(seed)) %>% 
  ungroup() %>% 
  mutate(r = row_number())

byes = confs %>% 
  slice_max(s, n = 2) %>% 
  bind_cols(x = c(2.5, 7.5))

MMBracketR::plotTourn(62) +
  geom_text(data = byes, aes(x = x, y = 31, label = conference))
