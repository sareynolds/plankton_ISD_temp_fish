library(flowCore)
library(tidyverse)

#automatically input of body size data in multiple fcs files to 1 data set

#creating list of all fcs files, excluding cy5.fcs
filenames_fcs <- list.files("data/", pattern = "*.fcs", recursive = T)
filenames_fcs = filenames_fcs[grep("CY5", filenames_fcs, invert = TRUE)]

#create empty object
data_fcs = NULL

#importing files to object
for(i in seq_along(filenames_fcs)){
  data_fcs[[i]] = read.FCS(paste0("data/", filenames_fcs[i]))
}

#pulling sizes and file names out of the fcs
get_sizes = function(x){
  tibble(micron = x@exprs[,1]) %>% 
    mutate(file = x@description$GUID)
}

#creating one file
library(dplyr)

sizes = bind_rows(lapply(data_fcs, FUN = get_sizes))




#inputting treatment, chla, and sample volume data
library(readxl)

# get treatments
treatments <- read_excel("data/chl.xlsx", 
                         sheet = "tank_condition") %>% 
  select(tank, heat, fish)

saveRDS(treatments, file = "data/treatments.rds")

# get chla
chl <-  read_excel("data/chl.xlsx", 
                     sheet = "data") %>% 
  select(tank, date, chl)

chl$date <- as.character(chl$date)

saveRDS(chl, file = "data/chl.rds")

#sample volume (how much sample was analyzed)
sample_volume <- read_excel("data/sample_volume.xlsx") %>% 
  select(tank, date, samvol_ul)

sample_volume$date <- as.character(sample_volume$date)

saveRDS(sample_volume, file = "data/sample_volume.rds")



#data wrangling to create one data set with all needed information for model

#separating file name plus calculating pg and adding treatment information
library(tidyr)

sizes_sep = sizes %>% 
  separate(file, c('tank', 'date', 'sample', 'method')) %>% 
  mutate(volume = (1/6*pi*micron^3),
         log10pg_dm = (1.64*log10(volume)^0.82),
         pg_dm = 10^log10pg_dm,
         tank = parse_number(tank)) %>% 
  left_join(treatments) %>%
  mutate(date=case_when(date=="202300607"~"20230607",
                        TRUE~date))

saveRDS(sizes_sep, file = "data/sizes_sep.rds")

#adding sample_volume, chl, xmax, and counts
sizes_sep_add = sizes_sep %>%
  group_by(tank, date) %>% 
  left_join(sample_volume) %>%
  left_join(chl) %>% 
  mutate(xmax = max(pg_dm),
         counts = 1/samvol_ul)

saveRDS(sizes_sep_add, file = "data/sizes_sep_add.rds")

#Using only data from bright-field
sizes_sep_sub = sizes_sep_add %>% 
  subset(method == "BF")

saveRDS(sizes_sep_sub, file = "data/sizes_sep_sub.rds")



#visualizing size data
ggplot(data=sizes_sep, aes(x = micron, y= volume))+
  geom_point()

library(ggplot2)

ggplot(data=sizes_sep, aes(x = pg_dm))+
  geom_histogram() +
  scale_x_log10()
