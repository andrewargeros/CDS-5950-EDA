library(tidyverse)
library(sf)

options(scipen = 999)

sysfonts::font_add_google("Secular One")
sysfonts::font_add_google("Arvo")
showtext::showtext_auto()

df = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv') %>% 
  filter(!country_name %in% c("Dominican Republic", "Peru", "Timor-Leste")) %>% 
  mutate(water_tech2 = str_remove_all(water_tech, " -.*$") %>% str_trim() %>% replace_na("Unknown"))

africa_link = "https://raw.githubusercontent.com/codeforgermany/click_that_hood/main/public/data/africa.geojson"
map = st_read(africa_link)

df %>% 
  group_by(country_name, water_tech2) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(country_name = case_when(
    country_name == "Congo - Brazzaville" ~ "Congo",
    country_name == "Congo - Kinshasa" ~ "DR Congo",
    TRUE ~ country_name)) %>% 
  group_by(water_tech2) %>% 
  slice_max(n, n = 15) %>% 
  ungroup() %>% 
  left_join(map %>% select(name), by = c("country_name" = "name")) %>% 
  mutate(center = st_centroid(geometry)) %>% 
  ggplot() +
  geom_sf(data = map, aes(geometry = geometry), color = 'gray80') +
  geom_sf(aes(geometry = geometry, fill = n), color = NA, size = 0.3,) +
  scale_fill_stepsn(colors = MetBrewer::met.brewer('VanGogh3', 5), 
                    breaks = c(1, 10, 100, 1000, 10000, 100000), trans = "log", labels=scales::comma) +
  coord_sf(clip = 'off') +
  facet_wrap(~water_tech2) +
  theme_void() +
  theme(legend.position = c(0.65, 0.15),
        legend.direction = 'horizontal',
        legend.key.width = unit(1.25, "cm"),
        legend.key.height = unit(0.25, "cm"),
        legend.title = element_text(family = "Arvo", color = "gray20", size = 30),
        legend.text = element_text(family = "Arvo", color = "gray20", size = 30),
        plot.title = element_text(family = "Secular One", size = 75, 
                                  hjust = 0.5, color = 'forestgreen', vjust = 1),
        strip.text = element_text(family = "Arvo", color = "gray20", size = 30),
        plot.caption = element_text(family = "Arvo", color = "gray20", size = 30, hjust = 0.95),
        plot.background = element_rect(fill = "white", color = NA)) +
  guides(fill = guide_legend(title.position = 'top', title.hjust = .5, 
                             label.position = 'bottom')) +
  labs(title = "Where are the Wells in Africa?",
       fill = "Number of Water Sources",
       caption = "Andrew Argeros | Data: WPDX ")

ggsave("C:/PersonalScripts/CDS-5950-EDA/Week 9 - Water Sources/where_wells.png", height = 6.5, width = 6)

df %>% 
  inner_join(df %>% 
              group_by(country_name) %>% 
              summarise(total_water = n_distinct(row_id)) %>% 
              slice_max(total_water, n = 10)) %>% 
  group_by(country_name, year = install_year, total_water) %>% 
  summarise(n = n()) %>% 
  group_by(country_name) %>% 
  mutate(npct = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(country_name = factor(country_name) %>% fct_reorder(-total_water)) %>% 
  ggplot() +
  aes(x = factor(year), y = 0.15, fill = n) +
  geom_tile(height = 0.5) +
  scale_fill_stepsn(colors = MetBrewer::met.brewer('Hokusai2', 5), 
                    breaks = c(1, 10, 100, 1000, 10000, 100000), trans = "log", labels=scales::comma) +
  facet_wrap(~country_name, scales = 'free', nrow = 2) +
  theme_void(base_family = "Arvo", base_size = 35) +
  theme(legend.position = "top",
        legend.direction = 'horizontal',
        legend.key.width = unit(1.25, "cm"),
        legend.key.height = unit(0.25, "cm"),
        text = element_text(color = "gray80"),
        plot.title = element_text(family = "Secular One", size = 75, 
                                  hjust = 0.5, color = '#0970a5', vjust = 1),
        plot.background = element_rect(fill = '#222222')) +
  guides(fill = guide_legend(title.position = 'top', title.hjust = .5, 
                             label.position = 'bottom')) +
  labs(title = "I bless the {WATER SOURCES} down in Africa",
       fill = "Number of Water Sources",
       caption = "Andrew Argeros | Data: WPDX ")

ggsave("C:/PersonalScripts/CDS-5950-EDA/Week 9 - Water Sources/year_wells.png", height = 6, width = 12)
