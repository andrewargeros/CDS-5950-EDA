library(tidyverse)
library(lubridate)
library(ggbump)
library(ggtext)
library(patchwork)

sysfonts::font_add_google('Montserrat')
showtext::showtext_auto()

billboard = read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv') 

top_plot = billboard %>% 
  filter(performer %in% c('Taylor Swift', 'Beyonce')) %>% 
  mutate(week_id = as.Date(week_id, '%m/%d/%Y')) %>% 
  filter(year(week_id) == 2009) %>% 
  group_by(performer, month = floor_date(week_id, 'month')) %>% 
  summarise(rank = mean(week_position) %>% as.integer()) %>% 
  ggplot() +
  aes(x = month, y = -rank, group = performer, color = performer) + 
  geom_bump(smooth = 20, size = 4, lineend = "round") +
  # geom_line(size = 1.5) +
  geom_vline(xintercept = as.Date('2009-09-12'), linetype = 'dotted', size = 2, color = 'gray70') +
  scale_color_manual(values = c('#9ebbe7', '#be294c')) +
  scale_y_continuous(labels = abs) +
  theme_minimal(base_family = 'Montserrat', base_size = 30) +
  theme(plot.background = element_rect(fill = 'gray15', color = NA),
        panel.grid = element_line(color = 'gray20'),
        plot.title = element_markdown(face = 'bold', color = 'gray85', size = 40, lineheight = 1.25),
        axis.title = element_text(color = 'gray70'),
        legend.position = 'none') +
  labs(title = "<span style='color:#9ebbe7;'>Queen B</span> and <span style='color:#be294c;'>Tay-Tay</span> on the 2009 Billboard Top 100",
       x = "Date",
       y = "Average Billboard Position") +
  annotate('curve', x = as.Date('2009-07-15'), xend = as.Date('2009-09-11'), size = 1.75,
           y = -8, yend = -2, arrow = arrow(length = unit(0.03, "npc")), color = "grey60", curvature = -0.25) +
  coord_cartesian(clip = 'off')

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

make_subplot = function(data, var, label_var, hjust = 1){
  data %>% 
    ggplot() +
    aes(x = 0, xend = {{ var }}, y = performer, yend = performer, color = performer) +
    geom_segment(size = 5, lineend = 'round') +
    geom_text(aes(label = {{ label_var }}, x = {{ var }}), hjust = -hjust, size = 15, family = 'Montserrat') +
    scale_color_manual(values = c('#9ebbe7', '#be294c')) +
    theme_minimal(base_family = 'Montserrat', base_size = 30) +
    theme(plot.background = element_rect(fill = 'gray15', color = NA),
          panel.grid = element_line(color = 'gray20'),
          plot.title = element_markdown(face = 'bold', color = 'gray85', size = 40, lineheight = 1.25),
          axis.title = element_text(color = 'gray70'),
          axis.text.y = element_text(color = 'gray70', size = 30),
          legend.position = 'none') +
    labs(x = element_blank(),
         y = element_blank())
}

song_plot = artist_data %>% 
  make_subplot(song, song) +
  xlim(0, 7.5) +
  ggtitle('Number of Top 25 Songs')

kanye_plot = artist_data %>% 
  make_subplot(kanye_references, kanye_references, 2) +
  geom_label(aes(label = kanye_albums, x = kanye_references), fill = 'gray15', label.size = NA,
            size = 15, family = 'Montserrat', hjust = -0.07, color = 'gray30') +
  xlim(0, 10) +
  ggtitle('References in Kanye Songs')

sales_plot = artist_data %>% 
  ggplot() +
  aes(x = 0, xend = sales, y = performer, yend = performer, color = performer) +
  geom_segment(size = 5, lineend = 'round') +
  geom_text(aes(label = scales::dollar(sales, scale = 1e-6, suffix = "M"), 
                x = sales), hjust = -0.25, size = 15, family = 'Montserrat') +
  scale_color_manual(values = c('#9ebbe7', '#be294c')) +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M"), limits = c(0, 15000000)) +
  theme_minimal(base_family = 'Montserrat', base_size = 30) +
  theme(plot.background = element_rect(fill = 'gray15', color = NA),
        panel.grid = element_line(color = 'gray20'),
        plot.title = element_markdown(face = 'bold', color = 'gray85', size = 40, lineheight = 1.25),
        axis.title = element_text(color = 'gray70'),
        axis.text.y = element_text(color = 'gray70', size = 30),
        legend.position = 'none') +
  labs(x = element_blank(),
       y = element_blank(),
       title = "First Year Sales")

yt_plot = artist_data %>% 
  mutate(plays_label = c('856 Million', '1.28 Billion')) %>% 
  make_subplot(youtube_plays, plays_label, hjust = 0.1) +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"),
                     limits = c(0, 2000000000)) +
  ggtitle('Youtube Streams of VMA Videos')

bottom_plot = (sales_plot + yt_plot )/ (song_plot + kanye_plot)

total_plot = top_plot / bottom_plot

total_plot + 
  plot_annotation(title = "YO TAYLOR... Imma let you finish, but Beyonce...",
                  subtitle = 'Was Yeezus Right? Who *really* had the better album of 2008-09?',
                  theme = theme(plot.background = element_rect(fill = 'gray15'),
                                plot.title = element_markdown(face = 'bold',
                                                              family = 'Montserrat',
                                                              color = 'gray85', 
                                                              size = 70, 
                                                              lineheight = 2.75),
                                plot.subtitle = element_markdown(face = 'bold', 
                                                                 family = 'Montserrat',
                                                                 color = 'gray85', 
                                                                 size = 60, 
                                                                 lineheight = 1.5))) 

ggsave("C:/PersonalScripts/CDS-5950-EDA/Week 3 - Beyonce and Taylor Swift/taylor_vs_beyonce.png",
       height = 10, width = 20, units = 'in')  
