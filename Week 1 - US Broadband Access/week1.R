library(tidyverse)
library(sf)
library(tidycensus)
library(patchwork)
library(sysfonts)

font_add_google(name = "Lato", family = "Lato")
showtext::showtext_auto()

link = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv'

data = read_csv(link) %>% 
  janitor::clean_names('snake') %>% 
  filter(st == "MN") %>% 
  transmute(GEOID = as.character(county_id),
            name = as.character(county_name),
            availability = parse_number(broadband_availability_per_fcc),
            usage = parse_number(broadband_usage))

census = get_acs(geography = "county", 
                 variables = "B01001_001", 
                 state = "MN",
                 geometry = TRUE)

election = read_csv("C:/RScripts/CDS-5950-EDA/Week 1 - US Broadband Access/2020_election_results.csv") %>% 
  janitor::clean_names('snake') %>% 
  mutate_at(vars('trump_pct', 'biden_pct', 'jorg_pct', 'other_pct'), ~parse_number(.x)/100) %>% 
  mutate(county = paste(county, "County"))

geo = data %>% 
  left_join(election, by = c('name' = 'county')) %>% 
  left_join(census, by = "GEOID") %>% 
  mutate(scale_usage = scale(usage))

geo %$% cor(usage, biden_pct) ## Looks like biden counties make better use of internet

election_plot = geo %>% 
  st_as_sf() %>% 
  ggplot() +
  aes(fill = biden_pct, color = biden_pct) +
  geom_sf() +
  coord_sf() +
  colorspace::scale_color_continuous_diverging(mid = 0.5, rev = T) +
  colorspace::scale_fill_continuous_diverging( mid = 0.5, rev = T) +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.35),
        text = element_text(family = "Lato", size = 56)) +
  labs(fill = "% of Biden Voters", 
       color = "% of Biden Voters")

usage_plot = geo %>% 
  st_as_sf() %>% 
  ggplot() +
  aes(fill = scale_usage, color = scale_usage) +
  geom_sf() +
  coord_sf() +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.35),
        text = element_text(family = "Lato", size = 56)) +
  labs(fill = "Broadband Usage Scaled", 
       color = "Broadband Usage Scaled")

county_plot = geo %>% 
  slice_max(order_by = usage, n = 6, with_ties = F) %>% 
  select(name, usage, biden_pct) %>% 
  mutate(election = ifelse(biden_pct < 0.5, -(1-biden_pct), biden_pct)) %>% 
  ggplot() +
  aes(x = ifelse(election > 0, 0.22, -0.22), xend = election, y = reorder(name, usage), yend = reorder(name, usage),
      color = election > 0.5) +
  geom_segment(size = 4.75, lineend = "round") +
  geom_text(aes(label = name, x = 0), family = "Lato", size = 20) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_manual(values = c('maroon', 'steelblue')) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'none',
        text = element_text(family = "Lato", size = 56)) +
  labs(x = element_blank(),
       y = element_blank())

election_plot + 
  labs(subtitle = "2020 Election Results by County") +
  county_plot +
  usage_plot + 
  labs(subtitle = "Scaled Broadband Usage by County") +
  plot_annotation(title = "2020 Election Results and Broadband Usage by MN Counties, Cor: 0.55",
                  theme = theme(title = element_text(family = "Lato", size = 60, lineheight = 1.5))) +
  plot_layout(widths = c(1,0.85, 1))
  
ggsave('C:/RScripts/CDS-5950-EDA/Week 1 - US Broadband Access/patchwork_plot.png',
       height = 15, width = 30, units = 'in')
