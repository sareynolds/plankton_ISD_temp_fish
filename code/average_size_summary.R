###summary
#probability
post_average_size %>% 
  group_by(heat, fish) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration, -.epred, -mean_size) %>% 
  pivot_wider(names_from = heat, values_from = .epred_uns) %>% 
  mutate(diff_heat = no - yes) %>%
  group_by(fish) %>% 
  #median_qi(diff_heat)
  reframe(prob_diff = sum(diff_heat>0)/max(.draw))

post_average_size %>% 
  group_by(heat, fish) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration, -.epred, -mean_size) %>% 
  pivot_wider(names_from = fish, values_from = .epred_uns) %>% 
  mutate(diff_fish = no - yes) %>%
  group_by(heat) %>% 
  #median_qi(diff_fish)
  reframe(prob_diff = sum(diff_fish>0)/max(.draw))

#mean and std of average sizes
post_average_size %>% 
  group_by(heat, fish) %>% 
  summarise(median = median(.epred_uns),
            sd = sd(.epred_uns))


#quantifying differences
diff_average_size <- post_average_size %>% 
  unite(treatment, c(heat, fish), sep = " ")

diff_average_size$treatment <- recode(diff_average_size$treatment, "no no" = "control",
                                      "yes no" = "heated",
                                      "no yes" = "fish",
                                      "yes yes" = "heated_fish")

diff_average_size %>% 
  group_by(treatment) %>% 
  (mean)

summary_as <- diff_average_size %>% 
  group_by(treatment) %>% 
  summarize(mean = mean(.epred_uns),
            sd = sd(.epred_uns),
            n = n())

summary_as %>% 
  rename_with(~ paste0(.x, "_1"), everything()) %>% 
  crossing(summary_as %>% 
             rename_with(~ paste0(.x, "_2"), everything())) %>% 
  filter(treatment_1 < treatment_2) %>% 
  mutate(
    mean_diff = mean_2 - mean_1,
    se_diff = sqrt((sd_1^2 / n_1) + (sd_2^2 / n_2))
  ) %>% 
  mutate(percent_mean = ((mean_2 - mean_1) / mean_1 * 100),
         percent_sd = ((sd_2 - sd_1) / sd_1 * 100)) %>%
  select(treatment_1, treatment_2, mean_1, mean_2, sd_1, sd_2, mean_diff, se_diff)