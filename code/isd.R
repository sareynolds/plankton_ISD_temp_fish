library(rstan)
library(brms)
library(tidyverse)
library(tidybayes)
library(dplyr)
library(isdbayes)

# recalling data
dat_clauset_xmins = readRDS(file = "data/dat_clauset_xmins.rds")

#isd model
brm_full_tank_xmin = brm(pg_dm | vreal(counts, xmin, xmax) ~ 1 + heat + fish + 
                      heat:fish + chl + (1|date) + (1|tank),
                    data = dat_clauset_xmins,
                    stanvars = stanvars,  # keep don't change
                    family = paretocounts(),  # keep don't change
                    prior = c(prior(normal(-1.1, 0.5), class = "Intercept"),
                              prior(normal(0, 0.5), class = "b"),
                              prior(exponential(6), class = "sd")), # regularizing priors
                    chains = 1, iter = 10,  # keep don't change
                    cores = 4, # keep don't change
                    file = 'models/brm_full_tank_xmin.rds',  # keep don't change
                    file_refit = "on_change")       # keep don't change

#model update
brm_full_tank_xmin_update = update(brm_full_tank_xmin,  chains = 4, iter = 2000)
saveRDS(brm_full_tank_xmin_update, file = "models/brm_full_tank_xmin_update.rds")

#visualizing model
conditional_effects((brm_full_tank_xmin_update))

#posterior checks
pp_check(brm_full_tank_xmin_update) + scale_x_log10()
pp_check(brm_full_tank_xmin_update, type = "boxplot") + scale_y_log10()
