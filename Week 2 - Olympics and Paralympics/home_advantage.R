library(tidyverse)
library(countrycode)
library(ggflags)

sysfonts::font_add_google("Josefin Sans")
sysfonts::font_add_google("Kanit")
showtext::showtext_auto()

df = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

nations = read_csv("C:/PersonalScripts/CDS-5950-EDA/Week 2 - Olympics and Paralympics/olympic_host_nations.csv") %>% 
  mutate(Country = str_remove(Country, '\\[.*\\]$'),
         City = str_remove(City, '\\[.*\\]$')) %>% 
  filter(Year %in% df$year) %>% 
  distinct() %>% 
  left_join(df %>% select(year, city) %>% distinct(), 
            by = c("Year" = 'year')) %>% 
  group_by(Year) %>% 
  filter(str_extract(City, "^.{1}") == str_extract(city, "^.{1}")) %>% 
  filter(Year != 1932) %>% 
  filter(City != "MelbourneStockholm") %>% 
  bind_rows(tribble(
    ~City, ~Country, ~Year, ~city,
    'Lake Placid',
    'United States',
    1932,
    'Lake Placid',
    'Los Angeles',
    'United States',
    1932,
    'Los Angeles',
    'Melbourne',
    'Australia',
    1956,
    'Melbourne',
    'Stockholm',
    'Sweden',
    1956,
    'Stockholm'
  ))

df %>% 
  mutate(team = str_remove(team, "-\\d$"),
         team = ifelse(team == "West Germany", "Germany", team)) %>% 
  group_by(year, team) %>% 
  mutate(n_athletes = n_distinct(name)) %>% 
  ungroup() %>% 
  filter(!is.na(medal)) %>% 
  group_by(year, city, team, n_athletes) %>% 
  summarise(medals = n()) %>% 
  mutate(medals_per_ath = medals/n_athletes) %>% 
  filter(team %in% nations$Country) %>% 
  left_join(nations, by = c('year' = 'Year', 
                            'city' = 'city',
                            'team' = 'Country')) %>% 
  mutate(host = ifelse(!is.na(City), 'host', 'nonhost')) %>% 
  group_by(team, host) %>% 
  summarise(avg_mpa = mean(medals_per_ath)) %>% 
  pivot_wider(names_from = host, values_from = avg_mpa) %>% 
  mutate(ccode = countrycode(team, origin = 'country.name', destination = 'iso2c', warn = F),
         ccode = replace_na(ccode, "BA")) %>% 
  ggplot() +
  aes(x = nonhost, y = host, country = tolower(ccode)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dotted', size = 0.75, color = 'gray60') +
  geom_flag(size = 10) +
  annotate("curve", x = 0.275, y = 0.67, xend = 0.31, yend = 0.77, size = 1.2,
           arrow = arrow(length = unit(0.01, "npc")), color = "grey40", curvature = -0.2) +
  annotate('text', x = 0.27, y = 0.66, label = "This is really the USSR...", family = 'Josefin Sans',
           color = "gray55", size = 15) +
  annotate("curve", x = 0.305, y = 0.1, xend = 0.31, yend = 0.31, size = 1.2,
           arrow = arrow(length = unit(0.01, "npc")), color = "grey40", curvature = -0.2) +
  annotate('text', x = 0.3, y = 0.08, label = "The *Not-So-Communist* one", family = 'Josefin Sans',
           color = "gray55", size = 15) +
  annotate("curve", x = 0.2, y = 0.5, xend = 0.155, yend = 0.52, size = 1.2,
           arrow = arrow(length = unit(0.01, "npc")), color = "grey40", curvature = -0.2) +
  annotate('text', x = 0.242, y = 0.52, label = "Belgium is reaping the successes of 1920", family = 'Josefin Sans',
           color = "gray55", size = 15) +
  annotate('segment', x = 0.175, y = 0, xend = 0.205, yend = 0.02, 
           arrow = arrow(length = unit(0.01, "npc")), color = "grey40", size = 1.2) +
  annotate('text', x = 0.15, y = -0.02, label = "Sarajevo was in Yugoslavia when they hosted", family = 'Josefin Sans',
           color = "gray55", size = 15) +
  annotate('text', x = 0.13, y = 0.72, label = "BETTER AT HOME", family = 'Kanit', size = 30, color = 'gray22') +
  annotate('text', x = 0.35, y = 0.02, label = "BETTER VISITORS", family = 'Kanit', size = 30, color = 'gray22') +
  theme_minimal() +
  theme(text = element_text(family = 'Josefin Sans', color = 'gray50', size = 30),
        plot.background = element_rect(fill = 'gray15'),
        panel.grid = element_line(color = 'gray20'),
        plot.title = element_text(family = "Kanit", size = 100, color = "gray80"),
        plot.subtitle = element_text(family = "Kanit", size = 50, color = "gray40")) +
  labs(x = "Expected Medals per Athlete when Not Hosting",
       y = "Expected Medals per Athlete as Hosts",
       title = "Home Court Advantage at the Olympics",
       subtitle = "Which countries shine as the hosts of the Games?")
ggsave("C:/PersonalScripts/CDS-5950-EDA/Week 2 - Olympics and Paralympics/home_advantage.png",
       width = 12.5, height = 8, units = 'in')
