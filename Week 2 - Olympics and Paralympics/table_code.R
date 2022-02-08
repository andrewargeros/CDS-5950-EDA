library(tidyverse)
library(countrycode)
library(glue)
library(ggflags)
library(gt)
library(ggtext)
library(cowplot)
library(magick)

sysfonts::font_add_google('IBM Plex Mono')
sysfonts::font_add_google('Poppins')
showtext::showtext_auto()

df = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

medal_table = df %>% 
  filter(season == "Winter", !is.na(medal)) %>% 
  mutate(team = str_remove(team, "-\\d$")) %>% 
  select(year, team, medal, event, city) %>% 
  distinct() %>% 
  group_by(year, team, medal, city) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(name = paste(year, "-", city)) %>% 
  pivot_wider(names_from = medal, values_from = n) %>% 
  mutate_if(is.numeric, ~replace_na(.x, 0)) %>% 
  mutate(medal_score = 3*Gold + 2*Silver + Bronze) %>% 
  arrange(year, desc(medal_score)) %>% 
  group_by(year) %>% 
  mutate(rank = rank(-medal_score, ties.method = "min"),
         rank_listing = rank(-medal_score, ties.method = "random"),
         rank_ord = scales::ordinal(rank, rules = scales::ordinal_english()),
         iso2 = countrycode(sourcevar = team, origin = "country.name", destination = "fips", warn = FALSE),
         flag_URL = glue('https://user.iiasa.ac.at/~marek/fbook/04/flags/{tolower(iso2)}-lgflag.gif'),
         flag_URL = ifelse(is.na(iso2), 'https://www.freepnglogos.com/uploads/olympic-rings-png/olympic-rings-denver-fact-colorado-hates-the-olympic-games-the-4.png', flag_URL),
         ccode = countrycode(sourcevar = team, origin = "country.name", destination = "iso2c", warn = FALSE),
         team_rt = glue("*{team}*") %>% as.character()) %>% 
  ungroup() %>% 
  relocate(flag_URL, .before = team) %>% 
  rename(yr = year)

back_color = '#FFFFFF'

year_data = medal_table %>% filter(yr == 2010)

city = year_data %>% select(city) %>% distinct() %>% as.character()
games_year = year_data %>% select(yr) %>% distinct() %>% as.character()

year_data %>%  
  gt(rowname_col = c("team")) %>% 
  cols_hide(c(medal_score, rank, rank_listing, iso2, year, name, city)) %>% 
  gt::text_transform(
    #Apply a function to a column
    locations = cells_body(c(flag_URL)),
    fn = function(x) {
      #Return an image of set dimensions
      web_image(
        url = x,
        height = 15
      )
    }
  ) %>%
  cols_width(c(flag_URL) ~ px(30)) %>% 
  cols_label(flag_URL = "",
             rank_ord = "") %>% 
  tab_spanner(label = emo::ji('first'), columns = c(Gold)) %>% 
  tab_style(locations = cells_column_labels(columns = "Gold"),
            style = list(cell_text(color = "#FFD700", style = 'oblique', align = "center"))) %>% 
  tab_spanner(label = emo::ji('silver'), columns = c(Silver)) %>% 
  tab_style(locations = cells_column_labels(columns = "Silver"),
            style = list(cell_text(color = "#BBC2CC", style = 'oblique', align = "center"))) %>% 
  tab_spanner(label = emo::ji('bronze'), columns = c(Bronze)) %>% 
  tab_style(locations = cells_column_labels(columns = "Bronze"),
            style = list(cell_text(color = "#CD7F32", style = 'oblique', align = "center"))) %>% 
  tab_style(locations = cells_body("team"),
            style = list(cell_text(style = "oblique"))) %>% 
  tab_header(title = md(glue("**The {games_year} {city} Winter Olympic Games**"))) %>% 
  opt_table_font(font = list(google_font("Barlow"))) %>% 
  cols_width(c('Gold', 'Silver', 'Bronze') ~ px(70),
             rank_ord ~ px(40)) %>% 
  tab_options(table.background.color = NULL) 

olympic_rings = image_read("C:/PersonalScripts/CDS-5950-EDA/Week 2 - Olympics and Paralympics/Images/olympic-rings.png")

for (year in unique(medal_table$yr)){
  year_data = medal_table %>% filter(yr == year)
  city = year_data %>% select(city) %>% distinct() %>% as.character()
  games_year = as.character(year)
  
  plot = year_data %>% 
    ggplot() +
    aes(y = -rank_listing) +
    geom_segment(aes(x = 0.1, xend = 3.9,yend = -rank_listing), linetype = "dotted", size = 0.5) +
    geom_richtext(aes(label = team_rt, x = 0.5), fill = NA, label.color = NA, hjust = -0.25,
                  label.padding = unit(0, "inches"), family = "Poppins", size = 25) +
    geom_richtext(aes(label = Gold, x = 1.25), fill = '#FFD70070', label.color = '#FFD700', 
                   family = "IBM Plex Mono", size = 25,
                  color = 'gray20') +
    geom_richtext(aes(label = Silver, x = 2.25), fill = '#BBC2CC', label.color = '#BBC2CC', 
                  label.padding = unit(0, "inches"), family = "IBM Plex Mono", size = 25,
                  color = 'gray20') +
    geom_richtext(aes(label = Bronze, x = 3.25), fill = '#CD7F3270', label.color = '#CD7F32', 
                  label.padding = unit(0, "inches"), family = "IBM Plex Mono", size = 25,
                  color = 'gray20') +
    geom_richtext(aes(label = rank_ord, x = 4), fill = NA, label.color = NA, 
                  label.padding = unit(0, "inches"), family = "IBM Plex Mono", size = 25,
                  color = 'gray20') +
    geom_flag(aes(x = 0, country = tolower(ccode)), size = 20) +
    annotate("richtext", x = c(0.125, 1.25, 2.25, 3.25, 4), y = 0, 
             label = c("", "**Gold**", "**Silver**", "**Bronze**", "**Total**"), 
             hjust = 0.5, family = "Poppins", size = 30, label.color = NA, fill = NA) +
    geom_vline(xintercept = 0.9, size = 0.5) +
    geom_hline(yintercept = -0.55, size = 0.5) +
    theme_void() +
    theme(plot.title = element_text(family = "Poppins", face = "bold", size = 50)) +
    ggtitle(glue(' The {games_year} {city} Winter Olympic Games'))
  
  path_to_img = glue('C:/PersonalScripts/CDS-5950-EDA/Week 2 - Olympics and Paralympics/Images/{year}.png')
  
  img = image_read(path_to_img) %>% 
    image_colorize(50, "white")
  
  ggdraw() +
    draw_image(img, scale = 0.3) +
    draw_plot(plot) +
    draw_image(olympic_rings, x = -0.37, y = 0.46, scale = 0.15) +
    theme(plot.background = element_rect(fill = 'white'))
  
  ggsave(glue('C:/PersonalScripts/CDS-5950-EDA/Week 2 - Olympics and Paralympics/Plots/{year}_ggplot.png'),
         height = 40, width = 12, units = "in")
}
