library(tidyverse)
library(lubridate)
library(ggbump)
library(ggtext)
library(patchwork)

sysfonts::font_add_google('Montserrat')

billboard = read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv') 

billboard %>% 
  filter(performer %in% c('Taylor Swift', 'Beyonce')) %>% 
  mutate(week_id = as.Date(week_id, '%m/%d/%Y')) %>% 
  filter(year(week_id) == 2009) %>% 
  group_by(performer, month = floor_date(week_id, 'month')) %>% 
  summarise(rank = mean(week_position) %>% as.integer()) %>% 
  ggplot() +
  aes(x = month, y = -rank, group = performer, color = performer) + 
  geom_bump(smooth = 20, size = 5, lineend = "round") +
  # geom_line(size = 1.5) +
  geom_vline(xintercept = as.Date('2009-09-12'), linetype = 'dotted', size = 2, color = 'gray70') +
  scale_color_manual(values = c('#9ebbe7', '#be294c')) +
  scale_y_continuous(labels = abs) +
  theme_minimal(base_family = 'Montserrat', base_size = 20) +
  theme(plot.background = element_rect(fill = 'gray15'),
        panel.grid = element_line(color = 'gray20'),
        plot.title = element_markdown(face = 'bold', color = 'gray85', size = 50, lineheight = 1.25),
        axis.title = element_text(color = 'gray70'),
        legend.position = 'none') +
  labs(title = "<span style='color:#9ebbe7;'>Queen B</span> and <span style='color:#be294c;'>Tay-Tay</span> on the 2009 Billboard Top 100",
       x = "Date",
       y = "Average Billboard Position") +
  annotate('curve', x = as.Date('2009-07-15'), xend = as.Date('2009-09-11'), size = 1.75,
           y = -8, yend = -2, arrow = arrow(length = unit(0.01, "npc")), color = "grey60", curvature = -0.25)

sales = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv') %>% 
  filter(title %in% c('Fearless', 'I Am... Sasha Fierce')) %>% 
  filter(str_starts(country, 'W')) %>% 
  select(artist, sales) %>% 
  mutate(artist = ifelse(artist != 'Taylor Swift', 'Beyonce', artist))

artist_data =billboard %>% 
  filter(performer %in% c('Taylor Swift', 'Beyonce')) %>% 
  mutate(week_id = as.Date(week_id, '%m/%d/%Y')) %>% 
  filter(year(week_id) == 2009, week_position <= 25) %>% 
  group_by(performer) %>% 
  summarise(song = n_distinct(song)) %>% 
  bind_cols(tibble(youtube_plays = c(855837413, 1282405642),
                   kanye_references = c(3,1),
                   kanye_albums = c('Blood on the Leaves, Everything I Am, Thats my B****h', 
                                    'Famous (Granted this is a whole song)'))) %>% 
  inner_join(sales, by = c('performer' = 'artist')) %>% 
  mutate(performer = factor(performer))

make_subplot = function(data, var, hjust = 1){
  data %>% 
    ggplot() +
    aes(x = 0, xend = {{ var }}, y = performer, yend = performer, color = performer) +
    geom_segment(size = 5, lineend = 'round') +
    geom_text(aes(label = {{ var }}, x = {{ var }}), hjust = -hjust, size = 20, family = 'Montserrat') +
    scale_color_manual(values = c('#9ebbe7', '#be294c')) +
    theme_minimal(base_family = 'Montserrat', base_size = 20) +
    theme(plot.background = element_rect(fill = 'gray15'),
          panel.grid = element_line(color = 'gray20'),
          plot.title = element_markdown(face = 'bold', color = 'gray85', size = 50, lineheight = 1.25),
          axis.title = element_text(color = 'gray70'),
          axis.text.y = element_text(color = 'gray70', size = 30),
          legend.position = 'none') +
    labs(x = element_blank(),
         y = element_blank())
}

artist_data %>% make_subplot(song)

artist_data %>% 
  make_subplot(kanye_references) +
  geom_text(aes(label = kanye_albums, x = kanye_references), 
            size = 8, family = 'Montserrat', hjust = -0.13, color = 'gray30') +
  xlim(0, 5)

artistdata %>% 
  ggplot() +
  aes(x = 0, xend = {{ var }}, y = performer, yend = performer, color = performer) +
  geom_segment(size = 5, lineend = 'round') +
  geom_text(aes(label = {{ var }}, x = {{ var }}), hjust = -hjust, size = 20, family = 'Montserrat') +
  scale_color_manual(values = c('#9ebbe7', '#be294c')) +
  theme_minimal(base_family = 'Montserrat', base_size = 20) +
  theme(plot.background = element_rect(fill = 'gray15'),
        panel.grid = element_line(color = 'gray20'),
        plot.title = element_markdown(face = 'bold', color = 'gray85', size = 50, lineheight = 1.25),
        axis.title = element_text(color = 'gray70'),
        axis.text.y = element_text(color = 'gray70', size = 30),
        legend.position = 'none') +
  labs(x = element_blank(),
       y = element_blank())
