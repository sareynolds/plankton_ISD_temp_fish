###experiment summaries and plots

library(readxl)
library(ggplot2)
library(dplyr)


###temperature
temperature <- read_excel("data/temp.xlsx")


temp_daily = temperature %>% 
  left_join(treatments)

#daily temperature plot
daily_temp = ggplot(temp_daily, aes(day, temp, color = heat)) +
  geom_vline(xintercept = c(4,9,16,23,30),
             linetype = "dashed",
             color = "azure4",
             lwd=1.05) +
  geom_point(size = 1.9) +
  scale_color_manual(name = "",
                     labels = c("- Heat", "+ Heat"),
                     values = c("no" = "darkblue", "yes" = "red")) +
  ylim(0,35) +
  labs(x = "Experimental Day", y = "Water Temperature (°C)") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", family = "serif", size = 10),
        text = element_text(family = "serif", size = 12, color = "black"),
        axis.line = element_line(linewidth = 0.5, color = "black"),
        axis.ticks = element_line(linewidth = 0.5, color = "black"),
        legend.position = "top", 
        legend.text=element_text(size=13))

daily_temp

ggsave(daily_temp, file = "plots/daily_temp.jpg", width = 5.5, height = 4.5, dpi = 500)

#average temperature 
temp_average = temp_daily %>% 
  select(date, tank, temp) %>% 
  group_by(tank) %>% 
  summarise(mean = mean(temp),
            sd = sd(temp))

temp_treat_average = temp_daily %>% 
  select(fish, heat, temp) %>% 
  group_by(fish, heat) %>% 
  summarise(mean = mean(temp),
            sd = sd(temp))


###average/range fish length and dry mass
fish_size <- read.csv("data/fish_size.csv")

#all fish total length (mm) at start and end of experiment
fish_length = fish_size %>% 
  select(date, tank, treatment, tl_mm)

#total length(mm) at start and end of experiment for each heat treatment
fish_summary = fish_size %>% 
  select(date, treatment, tl_mm) %>% 
  group_by(treatment, date) %>% 
  summarise(mean = mean(tl_mm),
            sd = sd(tl_mm))


###chl average and standard deviation
chl <-  read_excel("data/chl.xlsx", 
                   sheet = "data") %>% 
  select(tank, date, chl)

chl$date <- as.character(chl$date)

#table with mean and std for each tank
chl_average = chl %>% 
  left_join(treatments) %>% 
  group_by(tank) %>% 
  summarise(mean = mean(chl),
            sd = sd(chl))

#table with mean and std for each treatment group
chl_treat_average = chl %>% 
  left_join(treatments) %>% 
  group_by(fish, heat) %>% 
  summarise(mean = mean(chl),
            sd = sd(chl))