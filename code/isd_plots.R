library(tidyverse)
library(dplyr)
library(brms)
library(tidybayes)


###histogram of data
sizes_sep_sub = readRDS(file = "data/sizes_sep_sub.rds") %>% 
  mutate(fishheat = paste0(fish, heat),
         trt = case_when(fishheat == "nono" ~ "a) Control",
                         fishheat == "noyes" ~ "b) - Fish, + Heat",
                         fishheat == "yesno" ~ "c) + Fish, - Heat",
                         TRUE ~ "d) + Fish, + Heat"))

hist = ggplot(sizes_sep_sub, aes(x = pg_dm))+
  geom_histogram(fill = "azure4") +
  theme_classic() +
  scale_x_log10() + 
  facet_wrap(~trt) +
  labs(x= "Dry Mass (pg)", y = "Frequency") +
  ylim(0, 1500) +
  theme(strip.text = element_text(color = "black", family = "serif", size = 12, hjust = 0),
        strip.background = element_blank(),
        axis.text = element_text(color = "black", family = "serif", size = 10),
        text = element_text(family = "serif", size = 12, color = "black"),
        axis.line = element_line(linewidth = 0.5, color = "black"),
        axis.ticks = element_line(linewidth = 0.5, color = "black"))

hist #viewing histograms

ggsave(hist, file = "plots/treat_hist.jpg", width = 5.5, height = 4.5, dpi = 500)

### brm model
brm_posts_samples = brm_full_tank_xmin_update$data %>% 
  select(-pg_dm) %>% 
  mutate(chl = mean(chl)) %>% 
  distinct() %>% 
  add_epred_draws(brm_full_tank_xmin_update, re_formula = NULL) %>% 
  glimpse()

brm_plot = brm_posts_samples %>% 
  ggplot(aes(x = interaction(heat, fish), y = .epred)) +
  stat_pointinterval()+
  labs(x= "Treatment", y = "Lambda (λ)") + 
  theme_classic() +
  theme(axis.text = element_text(color = "black", family = "serif", size = 10),
        text = element_text(family = "serif", size = 12, color = "black"),
        axis.line = element_line(linewidth = 0.5, color = "black"),
        axis.ticks = element_line(linewidth = 0.5, color = "black")) +
  scale_x_discrete(labels = c('Control','Heated','Fish', 'Heated and Fish'))

brm_plot #viewing plot

ggsave(brm_plot, file = "plots/brm_plot.jpg", width = 5.5, height = 4.5, dpi = 500)



###brm model for treatment groups (with tank estimates behind)
brm_post_tanks <- brm_full_tank_xmin_update$data %>% 
  select(-pg_dm) %>% 
  mutate(chl = mean(chl)) %>% 
  distinct() %>% 
  group_by(tank, date) %>% 
  add_epred_draws(brm_full_tank_xmin_update, re_formula = NULL) %>% 
  filter(.draw < 100)

brm_treat_tank_plot <- brm_post_tanks %>% 
  ggplot(aes(x = interaction(heat, fish), y = .epred)) +
  tidybayes::stat_pointinterval(aes(group = interaction(tank, date)),
                                position = position_jitter(width = 0.1),
                                size = 0.01,
                                alpha = 0.1,
                                color = "blue")+
  stat_pointinterval(color = "black",
                     alpha = 1) +
  labs(x = "Treatment", y = "Lambda (λ)") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")) +
  scale_x_discrete(labels = c('Control','Heated','Fish', 'Heated and Fish'))


brm_treat_tank_plot

###chlorophyll figure
brm_post_chl = brm_full_tank_xmin_update$data %>% 
  select(-pg_dm) %>% 
  distinct() %>% 
  add_epred_draws(brm_full_tank_xmin_update, re_formula = NULL)

post_chl_qi <- brm_post_chl %>% 
  group_by(chl) %>% 
  mean_qi(.epred)

plot_chl = brm_post_chl %>% 
  ggplot(aes(x = chl, y = .epred)) +
  geom_lineribbon(data = cond_chl$chl$data,
                  aes(y = estimate__, 
                      ymin = lower__,
                      ymax = upper__),
                  color = "black") +
  theme_classic() +
  labs(y = "Lambda (λ)",
       x = "Chlorophyll (μg/L)") +
  theme(axis.text = element_text(color = "black", family = "serif", size = 10),
        text = element_text(family = "serif", size = 12, color = "black"),
        axis.line = element_line(linewidth = 0.5, color = "black"),
        axis.ticks = element_line(linewidth = 0.5, color = "black"))

plot_chl

ggsave(plot_chl, file = "plots/plot_chl.jpg", width = 5.5, height = 4.5, dpi = 500)
