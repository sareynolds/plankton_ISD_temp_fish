library(poweRlaw)
library(tidyverse)

# this method follows Clauset et al. 2009 by estimating the minimum
# size for which the data follow a power law using by minimizing the K-S statistic.
# i.e., xmin is model-based

# load data
dat = readRDS("data/sizes_sep_sub.rds") %>% 
  group_by(tank, date) %>% 
  sample_n(2000, weight = counts, replace = T)

dat_list = dat %>% group_by(tank, date) %>% group_split()

xmin_list = list()

for(i in 1:length(dat_list)){
  powerlaw = conpl$new(dat_list[[i]]$pg_dm)
  xmin_list[[i]] = tibble(xmin_clauset = estimate_xmin(powerlaw)$xmin,
                          tank = unique(dat_list[[i]]$tank),
                          date = unique(dat_list[[i]]$date))
}

xmins_clauset = bind_rows(xmin_list)

dat_clauset_xmins = readRDS("data/sizes_sep_sub.rds") %>% 
  left_join(xmins_clauset) %>% 
  group_by(tank, date) %>% 
  filter(pg_dm >= xmin_clauset) %>%
  mutate(xmin = xmin_clauset)

saveRDS(dat_clauset_xmins, file = "data/dat_clauset_xmins.rds")
