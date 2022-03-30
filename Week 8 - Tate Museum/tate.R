library(tidyverse)
library(MetBrewer)
library(ggstream)

sysfonts::font_add_google('EB Garamond')
sysfonts::font_add_google('Zilla Slab')
showtext::showtext_auto()

art = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv') %>% 
  mutate(x1 = -width/2,
         x2 = width/2,
         y1 = -height/2,
         y2 = height/2) %>% 
  arrange(year)

art %>% 
  ggplot() +
  aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2) +
  geom_rect(fill = NA, color = '#c5dfff', size = 0.1, alpha = 0.5) +
  theme_void() +
  theme(plot.background = element_rect(fill = '#570140'))

ggsave("C:/PersonalScripts/CDS-5950-EDA/Week 8 - Tate Museum/obligatory.png", height = 6, width = 6)

art %>% 
  mutate(dim = round(height/width,2)) %>% 
  group_by(dim) %>% 
  summarise(n = n()) %>%
  filter(!is.na(dim)) %>% 
  arrange(desc(n)) %>% 
  mutate(dim2 = factor(dim) %>% fct_reorder(dim)) %>% 
  ggplot() +
  aes(x = dim2, y = 0.05, fill = n) +
  geom_tile(height = 0.25) +
  scale_fill_gradientn(colors = met.brewer('Hokusai3', type = 'continuous', direction = -1))+
  ylim(-0.5, 0.5) +
  theme_void() +
  theme(plot.background = element_rect(fill = '#041324'),
        legend.position = 'none',
        plot.title = element_text(family = 'EB Garamond', size = 88, color = 'white', hjust = 0.5),
        plot.subtitle = element_text(family = 'EB Garamond', size = 30, color = 'white', hjust = 0.5, lineheight = 0.6)) +
  labs(title = "Dimensions of the Tate",
       subtitle = "Andrew Argeros (2022). Pixels on Computer Screen.
The vast majority of pieces at the Tate have concentrated aspect ratios with the most common being the Golden Ratio of 1:phi. 
Some other avant garde artists choose extreme proportions for their art, thus concetrating the distribution. Artists also tend 
to choose square canvases, but avoid anything close-- since no one wants to look like they can't draw a square.")

ggsave("C:/PersonalScripts/CDS-5950-EDA/Week 8 - Tate Museum/aspect.png", height = 6, width = 12)

art %>% 
  group_by(artist) %>% 
  summarise(n = n_distinct(id)) %>% 
  slice_max(n, n = 6) %>% 
  inner_join(art) %>% 
  filter(artistRole == 'artist') %>% 
  group_by(artist, year = acquisitionYear) %>% 
  summarise(n = n_distinct(id)) %>% 
  mutate(artist = str_remove_all(artist, ',.*$'),
         artist = ifelse(str_detect(artist, "Briti"), "The British School", artist)) %>% 
  ggplot() +
  aes(x= year, y = n, group = artist, fill = artist) +
  geom_stream() +
  scale_fill_manual(values = met.brewer('Moreau', type = 'discrete', direction = -1)) +
  theme_minimal(base_family = 'Zilla Slab') +
  theme(plot.background = element_rect(fill = 'gray20'),
        legend.position = c(0.5, 0.25),
        legend.direction = 'horizontal',
        legend.key.size = unit(0.25, 'in'),
        legend.text = element_text(size = 14, color = 'gray40'),
        legend.title = element_text(size = 16, color = 'gray60'),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14, color = 'gray40'),
        plot.title = element_text(size = 38, hjust = 0.5, color = 'gray80'),
        plot.subtitle = element_text(size = 16, hjust = 0.5, color = 'gray60', lineheight = 0.6),
        plot.caption = element_text(size = 14, color = 'gray40')) +
  guides(fill = guide_legend(title.position = 'top', title.hjust = .5, 
                             label.position = 'bottom', override.aes = list(size = 4))) +
  labs(title = "Acquisitions of the Tate Gallery",
       fill = "Artist",
       y = element_blank(),
       x = element_blank(),
       subtitle = "To the Tate Gallery, the Sun must rise and set on the work of Joseph M. W. Turner. Since 1850, the gallery 
has acquired nearly 40,000 pieces by Turner, or about 1.4 pieces per day he was alive. These account for 11 total rooms in 
the gallery. This is 37x more than the collection of George Jones' 1,046 works.",
       caption = "Andrew Argeros | Data: Tate Gallery")

ggsave("C:/PersonalScripts/CDS-5950-EDA/Week 8 - Tate Museum/acquisitions.png", height = 4, width = 6)
