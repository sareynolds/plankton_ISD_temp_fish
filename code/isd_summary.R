library(tidyverse)
library(tidybayes)
library(dplyr)

###general isd plot (to get lambda and standard error values)
cond_tank_1 = plot(conditional_effects(brm_full_tank_xmin_update,
                                       effects = "heat:fish"))

cond_tank_plot_1 = cond_tank_1$`heat:fish` +
  theme_classic() +
  labs(y = "Lambda (λ)",
       x = "Temperature")+
  scale_x_discrete(labels = c('Ambient', 'Heated'))+
  scale_color_discrete(name = "Fish", labels = c("Absence", "Present")) +
  guides(fill = "none")

cond_tank_plot_1 #viewing plot

ggsave(cond_tank_plot_1, file = "plots/cond_tank_plot_1.jpg", width = 5.5, height = 3.5, dpi = 500)


# lambda, standard error table
cond_tank_plot_1$data %>% 
  select("heat", "fish", "estimate__", "se__", "lower__", "upper__")

###probability
#getting posteriors
posts = brm_full_tank_xmin_update$data %>% 
  mutate(xmin = min(xmin),
         xmax = max(xmax),
         counts = 1) %>% 
  distinct(heat, fish, xmin, xmax, counts) %>% 
  mutate(chl = 0) %>% 
  add_epred_draws(brm_full_tank_xmin_update, re_formula = NA)
  
qi(posts$.epred)

#heat treatments
posts %>% 
  group_by(heat, fish) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = heat, values_from = .epred) %>% 
  mutate(diff_heat = yes - no) %>% 
  group_by(fish) %>% 
  mean_qi(diff_heat)

posts %>% 
  group_by(heat, fish) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = heat, values_from = .epred) %>% 
  mutate(diff_heat = yes - no) %>% 
  group_by(fish) %>% 
  reframe(prob_diff = sum(diff_heat>0)/max(.draw)) %>% 
  mean_qi(prob_diff)

#fish treatments
posts %>% 
  group_by(fish, heat) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = fish, values_from = .epred) %>% 
  mutate(diff_fish = yes - no) %>% 
  group_by(heat) %>% 
  mean_qi(diff_fish)

posts %>% 
  group_by(heat, fish) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = fish, values_from = .epred) %>% 
  mutate(diff_fish = yes - no) %>% 
  group_by(heat) %>% 
  reframe(prob_diff = sum(diff_fish>0)/max(.draw))
  mean_qi(prob_diff)