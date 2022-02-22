library(tidyverse)
library(ggtext)

sysfonts::font_add_google('Roboto Slab')
sysfonts::font_add_google('Playfair Display')
showtext::showtext_auto()

hbcu = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv') %>% 
  janitor::clean_names('snake')

bubble_size = 3

hbcu %>% 
  select(year, total_enrollment, total_public, total_private) %>% 
  mutate(enroll_pos = (total_public + total_private)/2, 
         enroll_change = total_enrollment - lag(total_enrollment), 
         enroll_change_dir = case_when(enroll_change > 0 ~ "- <b>&gt;</b>",
                                       enroll_change < 0 ~ "- <b>&lt;</b>", 
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
                fill = 'white', label.size= NA, family = "Playfair Display") +
  coord_cartesian(clip = 'off') +
  theme_minimal(base_family = 'Roboto Slab') +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_markdown(family = 'Roboto Slab', size = 25),
        plot.subtitle = element_text(family = 'Playfair Display', size = 18)) +
  labs(x = element_blank(),
       y = element_blank(),
       title = "<span style='color:#fb8500;'>Public</span> and <span style='color:#219ebc;'>Private</span> Annual Enrollment among HBCUs",
       subtitle = 'Do prospective HBCU students make smarter collegiate decisions than Hamline students?
Over the past 32 years, enrollment at HBCUs has risen, but most of that growth has come from
less expensive, public universities.')

hbcu %>% 
  filter(year %in% c(1976, 2015)) %>% 
  select(year, total_enrollment, total_public, total_private) %>% 
  pivot_longer(starts_with('total_p')) %>% 
  mutate(pct = value/total_enrollment) %>% 
  group_by(year) %>% 
  # filter(year == 1976) %>%
  ggplot() +
  aes(ymin = 0, ymax = pct, xmax = 4, xmin = 3, fill = name, group = year) +
  geom_rect() +
  # coord_polar(theta = 'y') +
  facet_wrap(~year)
