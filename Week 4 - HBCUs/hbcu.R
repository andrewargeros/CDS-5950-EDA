library(tidyverse)
library(ggtext)
library(cowplot)

sysfonts::font_add_google('Lora')
sysfonts::font_add_google('Playfair Display')
sysfonts::font_add_google('Anton')
sysfonts::font_add_google('Cousine')
showtext::showtext_auto(enable = T)

hbcu = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv') %>% 
  janitor::clean_names('snake')

bubble_size = 2.75

mainplot = hbcu %>% 
  select(year, total_enrollment, total_public, total_private) %>% 
  mutate(enroll_pos = (total_public + total_private)/2, 
         enroll_change = total_enrollment - lag(total_enrollment), 
         enroll_change_dir = case_when(enroll_change > 0 ~ "- >",
                                       enroll_change < 0 ~ "- <", 
                                       T ~ ""),
         total_enrollment_fmt = scales::number(total_enrollment, scale = 1e-3, suffix = " k", accuracy = 1),
         tot_enroll_str = paste(total_enrollment_fmt, enroll_change_dir)) %>% 
  ggplot() +
  aes(x = year) +
  geom_segment(aes(xend = year, y = total_public, yend = total_private),
               color = 'gray20', lty = 3, size = 0.75) +
  geom_point(aes(y = total_private), color = '#219ebc', size = bubble_size) +
  geom_point(aes(y = total_public), color = '#fb8500', size = bubble_size) +
  geom_richtext(aes(label = tot_enroll_str, y = enroll_pos, angle = 90),
                fill = 'white', label.size= NA, family = "Playfair Display", size = 10, hjust = 0.1) +
  coord_cartesian(clip = 'off') +
  theme_minimal(base_family = 'Lora') +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        plot.title = ggtext::element_markdown(family = 'Lora', size = 38, lineheight = 1.25),
        plot.subtitle = element_text(family = 'Playfair Display', size = 28, lineheight = 0.9),
        axis.text = element_text(size = 25)) +
  labs(x = element_blank(),
       y = element_blank(),
       title = "<span style='color:#fb8500;'>Public </span>and <span style='color:#219ebc;'>Private</span> Annual Enrollment among HBCUs",
       subtitle = 'Do prospective HBCU students make smarter collegiate decisions than Hamline students?
Over the past 32 years, enrollment at HBCUs has risen, but most of that growth has come from
less expensive, public universities.',
       caption = "Data: Data.World | Viz: Andrew Argeos")

subplot = hbcu %>% 
  filter(year %in% c(1976, 2015)) %>% 
  select(year, total_enrollment, total_public, total_private) %>% 
  mutate(pct_pub = total_public/total_enrollment,
         pct_pri = total_private/total_enrollment) %>% 
  select(year, pct_pri, pct_pub) %>% 
  pivot_longer(starts_with('pct')) %>% 
  group_by(year) %>% 
  # filter(year == 1976) %>%
  ggplot() +
  aes(y = value, x = 1, fill = name) +
  geom_bar(stat = 'identity') +
  geom_label(aes(label = scales::percent(value), y = c(0.85, 0.4, 0.85, 0.4), color = rev(name)), 
             fill = 'white', label.size = NA, size = 10, family = 'Playfair Display') +
  facet_wrap(~year) +
  scale_fill_manual(values = c('#219ebc', '#fb8500')) +
  xlim(-0.4, 1.75) + 
  coord_polar(theta = 'y') +
  theme_void() +
  theme(legend.position = 'none',
        strip.text = element_text(size = 30, family = 'Lora', color = 'gray15')) 

mainplot %>% 
  ggdraw() +
  draw_plot(subplot, 0.07, 0.52, 0.23, 0.33) +
  theme(plot.background = element_rect(fill = 'white'))

ggsave2("C:/PersonalScripts/CDS-5950-EDA/Week 4 - HBCUs/plot.pdf", 
        width = 23, height = 12, units = "in", device = cairo_pdf)

students = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hs_students.csv') %>% 
  janitor::clean_names('snake') %>% 
  filter(!is.na(total)) %>% 
  filter(total < 2022) %>% 
  select(-starts_with('standard_err'), -starts_with('total_')) %>% 
  mutate_if(is.character, parse_number)

students %>% 
  pivot_longer(2:last_col()) %>% 
  mutate(value = replace_na(value, 0),
         race = ifelse(str_detect(name, 'white'), "white", "poc")) %>% 
  group_by(total) %>% 
  mutate(pct = value/sum(value)) %>% 
  group_by(total, race) %>% 
  summarise(pct = sum(pct)) %>%
  mutate(race = factor(race, levels = c('poc', 'white')),
         label = ifelse(race == 'poc' & total %% 10 == 0, scales::percent(pct), NA),
         ypos = ifelse(race == 'poc' & total %% 10 == 0, 1.02-pct, NA),
         vlines = ifelse(race == 'poc' & total %% 10 == 0, 1.001-pct, NA)) %>% 
  ggplot() + 
  aes(x = total, y = pct, group = race, color = race, fill = race) +
  geom_area(alpha = 0.9) +
  geom_segment(aes(xend = total, yend = vlines, y = 1), color = colorspace::darken('#409264', 0.05), size = 2) +
  geom_text(aes(label = label, y = ypos, x = total), 
            color = 'black', hjust = -0.1, family = 'Anton', size = 20) +
  scale_fill_manual(values = c('#4E996E', '#161515')) +
  scale_color_manual(values = c(colorspace::darken('#409264', 0.05), 'gray10')) +
  scale_x_continuous(position = 'top', breaks = scales::pretty_breaks(n = 10)) +
  annotate('text', x = 1980, y = 0.85, label = "PERSONS OF COLOR",
           size = 30, family = 'Anton') +
  annotate('text', x = 1980, y = 0.25, label = "WHITE PEOPLE",
           size = 30, family = 'Anton', color = '#e1d5c8') +
  theme_void() +
  theme(axis.text.x = element_text(color = 'black', vjust = -12, hjust = -0.25, 
                                   family = 'Cousine', face = 'bold', size = 30),
        legend.position = 'none',
        plot.background = element_rect(fill = '#e1d5c8'),
        plot.title = element_text(family = 'Anton', hjust = 0.5, lineheight = 1, size = 60),
        plot.subtitle = element_text(family = 'Anton', hjust = 0.5, lineheight = 1.25, size = 45, vjust = -3),
        plot.caption = element_text(family = 'Anton', size = 20, vjust = 3)) +
  labs(title = 'PROPORTION OF MINORITIES AND WHITE PEOPLE AT HBCUs\nPROPORTION DE MINORITÉS ET DE PERSONNES BLANCHES DANS LES HBCU',
       subtitle = 'DONE BY HAMLINE UNIVERSITY .',
       caption = "Data: Data.World | Viz: Andrew Argeros in the Style of W.E.B DuBois")
ggsave('C:/PersonalScripts/CDS-5950-EDA/Week 4 - HBCUs/web_plot.png', height = 11, width = 8.5, units = 'in')  
