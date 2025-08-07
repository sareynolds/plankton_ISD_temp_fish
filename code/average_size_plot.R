library(tidyverse)
library(dplyr)
library(brms)
library(tidybayes)



#posteriors (and unstandardizing)
raw_means = sizes_sep_sub %>% 
  group_by(heat, fish) %>% 
  reframe(mean_size = mean(pg_dm, na.rm = T))

post_average_size <- brm_average_size2_update$data %>% 
  select(-pg_dm_s) %>% 
  distinct() %>% 
  add_epred_draws(brm_average_size2_update, re_formula = NULL) %>% 
  filter(.draw < 1000) %>% 
  ungroup() %>% 
  left_join(raw_means) %>% 
  mutate(.epred_uns = .epred * mean_size)

post_as_qi <- post_average_size %>% 
  group_by(heat, fish) %>% 
  median_qi(.epred_uns)

max(post_as_qi$.epred_uns)
min(post_as_qi$.epred_uns)

#plot
brm_plot_average_size <- post_average_size %>% 
  ggplot(aes(x = interaction(heat, fish), y = .epred_uns)) +
  geom_pointrange(data = post_as_qi,
                  aes(y = .epred_uns,
                      ymin = .lower,
                      ymax = .upper),
                  color = "black",
                  fatten = 5,
                  size = 0.5) +
  labs(x= "Treatment", y = "Body Size (pg)") + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", family = "serif", size = 10),
        text = element_text(family = "serif", size = 12, color = "black"),
        axis.line = element_line(linewidth = 0.5, color = "black"),
        axis.ticks = element_line(linewidth = 0.5, color = "black")) +
  scale_x_discrete(labels = c('Control','Heated','Fish', 'Heated and Fish'))

brm_plot_average_size

ggsave(brm_plot_average_size, file = "plots/brm_plot_average_size.jpg", width = 5.5, height = 4.5, dpi = 500)
