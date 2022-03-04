library(tidyverse)
library(ggsoccer)
library(rvest)

sysfonts::font_add_google('Barlow')
sysfonts::font_add_google('Josefin Sans')
showtext::showtext_auto()

shots = read_csv("C:/PersonalScripts/CDS-5950-EDA/Week 5 - Womens Soccer/Data/WWC_US_matchshots.csv")
passes = read_csv("C:/PersonalScripts/CDS-5950-EDA/Week 5 - Womens Soccer/Data/WWC_US_matchpass.csv")

shots %>% 
  mutate(loc_x = str_extract(location, '\\[\\d+\\.\\d,') %>% parse_number() - 20,
         loc_y = str_extract(location, ', \\d+\\.\\d]$') %>% parse_number() + 20,
         team = ifelse(str_starts(possession_team, 'United'), "Countries that Have Been to the Moon", "Countries that Have Not") %>% factor()) %>%   
  ggplot() +
  aes(x = loc_x, y = loc_y) +
  annotate_pitch(fill = '#567d46', colour = 'darkgreen', limits = F, goals = goals_line) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon", alpha = 0.75) +
  scale_fill_gradient(low = "navyblue", high = 'red') +
  coord_flip(xlim = c(49, 101)) +
  facet_wrap(~team, strip.position = 'bottom') + 
  theme_void() +
  theme(legend.position = 'none',
        plot.background = element_rect(fill = 'gray15'),
        strip.text = element_text(lineheight = 1.75, color = colorspace::darken('#567d46', 0.2), 
                                  size = 52, family = 'Barlow'),
        plot.title = element_text(color = 'gray75', family = 'Josefin Sans', hjust = 0.5, size = 85),
        plot.subtitle = element_text(color = 'gray75', family = 'Josefin Sans', hjust = 0.5, size = 55)) +
  labs(title = "Shooters Shoot- Just How Much and Where",
       subtitle = "The United States simply take more quality shots")

ggsave('C:/PersonalScripts/CDS-5950-EDA/Week 5 - Womens Soccer/shot_plot.png', height = 8, width = 14, units = 'in')


emoji_to_link = function(x) {
  paste0("https://emojipedia.org/emoji/",x) %>%
    read_html() %>%
    html_nodes("tr td a") %>%
    .[1] %>%
    html_attr("href") %>%
    paste0("https://emojipedia.org/", .) %>%
    read_html() %>%
    html_node('div[class="vendor-image"] img') %>%
    html_attr("src")
}

link_to_img = function(x, size = 35) {
  paste0("<img src='", x, "' width='", size, "'/>")
}

passes2 = passes %>% 
  mutate(body_part = str_extract(pass, "body_part.*?\\}") %>% 
           str_extract("name': '(.*?)$") %>% 
           str_remove_all("^name': '|'\\}$") %>% 
           str_trim(),
         body_part = ifelse(is.na(body_part) | body_part == 'No Touch', "Bad Pass/Catch", body_part),
         body_emoji = case_when(str_detect(body_part, "Foot") ~ emo::ji('foot') %>% emoji_to_link(),
                                str_detect(body_part, "Hand") ~ emo::ji('hand') %>% emoji_to_link(),
                                str_detect(body_part, "Head") ~ emo::ji('woman') %>% emoji_to_link(),
                                str_detect(body_part, 'Arm') ~ emo::ji('bicep') %>% emoji_to_link(),
                                str_detect(body_part, 'Kick') ~ emo::ji('leg') %>% emoji_to_link(),
                                str_detect(body_part, 'Other') ~ emo::ji('shrug') %>% emoji_to_link(),
                                T ~ emo::ji('stop') %>% emoji_to_link()),
         label = link_to_img(body_emoji),
         body_part = body_part %>% factor(levels = c('Right Foot', 'Left Foot', 'Head', 'Drop Kick', 'Keeper Arm', 'Other', 'Bad Pass/Catch'))) %>%  
  group_by(body_part, body_emoji, label) %>% 
  summarise(n = n())

passes2 %>%  
 ggplot() +
  aes(y = fct_rev(body_part), x = n, fill = str_detect(body_part, "Bad")) +
  geom_bar(stat = 'identity', width = 0.5) +
  ggtext::geom_richtext(aes(label = label), fill = NA, label.color = NA, nudge_x = 0.2) +
  geom_text(aes(x = n + 200, label = scales::comma(n, accuracy = 1)), 
            size = 20, color = colorspace::darken('#2C3E50'), family = 'Josefin Sans') +
  scale_x_continuous(labels = scales::comma) +
  scale_fill_manual(values = c('#2C3E50', '#E74C3C')) +
  theme_minimal(base_size = 55, base_family = 'Barlow') +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_line(linetype = 'dotted', size = 1.5), 
        legend.position = 'none',
        plot.title = element_text(family = 'Josefin Sans', color = colorspace::darken('#2C3E50'), size = 85),
        plot.subtitle = element_text(color = colorspace::darken('#E74C3C'), size = 55),
        plot.background = element_rect(fill = 'white', color = 'white')) + 
  labs(title = "Head, Shoulders, Knees, and *Feet*",
       subtitle = "How did teams catch passes during the 2019 World Cup?",
       x = element_blank(),
       y = element_blank())

ggsave('C:/PersonalScripts/CDS-5950-EDA/Week 5 - Womens Soccer/pass_plot.png', height = 8, width = 14, units = 'in') 
