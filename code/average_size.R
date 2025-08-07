library(brms)
library(tidybayes)
library(tidyverse)
library(rstan)


#getting data and standardizing body size
sizes_sep_sub <- readRDS(file = "data/sizes_sep_sub.rds") %>% 
  ungroup() %>% 
  mutate(mean_pgdm = mean(pg_dm, na.rm = T)) %>% 
  mutate(pg_dm_s = pg_dm/mean_pgdm)
  
sizes_sep_sub %>% 
  summarise(mean = mean(pg_dm_s))

#average body size model
brm_average_size2 <- brm(pg_dm_s ~ 1 + heat + fish + heat:fish +
                           (1 + heat + fish + heat:fish|date),
                         data = sizes_sep_sub,
                        family = Gamma(link = "log"),
                        prior = c(prior(normal(0, 1), class = "Intercept"),
                                  prior(normal(0, 1), class = "b"),
                                  prior(exponential(1), class = "sd")),
                        chains = 1, iter = 10)

#model update
brm_average_size2_update <- update(brm_average_size2, chains = 4, iter = 2000,
                                   newdata = sizes_sep_sub,
                                   data2 = list(mean_pgdm = unique(sizes_sep_sub$mean_pgdm)))
saveRDS(brm_average_size2_update, "models/brm_average_size2_update.rds")

#visualizing model
plot(conditional_effects(brm_average_size2_update, effect = "heat:fish"), points = T)

#posterior checks
pp_check(brm_average_size2_update) + scale_x_log10()
pp_check(brm_average_size2_update, type = "boxplot") + scale_y_log10()



