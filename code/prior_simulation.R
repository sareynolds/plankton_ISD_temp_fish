library(tidyverse)
library(tidybayes)

#prior simulations for isd model

set.seed(42)


### simulate priors  
prior_sim = tibble(trt = c("heat", "fish", "heat+fish", "control")) %>% 
  expand_grid(draw_id = 1:100) %>% 
  mutate(mu = rnorm(nrow(.), -1.1, 0.5),  # change to whatever your priors are
         beta = rnorm(nrow(.), 0, 0.5)) %>%  # change to whatever your priors are
  mutate(sample = case_when(trt == "control" ~ mu + beta*0, # simulate lambdas
                            TRUE ~ mu + beta*1))

#plot

prior_sim_plot <- prior_sim %>% 
  ggplot(aes(x = trt, y = sample)) +
  stat_pointinterval() +
  labs(x= "Treatment", y = "Lambda (λ)") + 
  theme_classic() +
  theme(axis.text = element_text(color = "black", family = "serif", size = 10),
        text = element_text(family = "serif", size = 12, color = "black"),
        axis.line = element_line(linewidth = 0.5, color = "black"),
        axis.ticks = element_line(linewidth = 0.5, color = "black")) +
  scale_x_discrete(labels = c('Control','Heated','Fish', 'Heated and Fish'))

prior_sim_plot

ggsave(prior_sim_plot, file = "plots/prior_sim_plot.jpg", width = 5.5, height = 4.5, dpi = 500)


