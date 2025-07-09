# Author: Delphine Mossman
# Date Created: 8 July 2025
# Date Last Modified: 8 July 2025

#####

## Setup

rm(list = ls())

library(tidyverse)
library(readxl)
library(R.utils)
library(tcltk)

sourceDirectory(
  "H:/dm1679/Code/R_Functions",
  modifiedOnly = F
)

spp_index = read_xlsx("C:/Users/Delphine/Box/NJDEP Trawl Data/SPPLIST.xlsx")

tow_bio_data = read_xlsx("C:/Users/Delphine/Box/NJDEP Trawl Data/TowsCatchTable.xlsx") %>%
  drop_na() %>%
  mutate(SLAT = ddm2dd(paste(substr(SLAT, 1, 2), " ", substr(SLAT, 3, 4))),
         ELAT = ddm2dd(paste(substr(ELAT, 1, 2), " ", substr(ELAT, 3, 4))),
         SLONG = ddm2dd(paste(substr(SLONG, 1, 2), " ", substr(SLONG, 3, 4))) * -1,
         ELONG = ddm2dd(paste(substr(ELONG, 1, 2), " ", substr(ELONG, 3, 4))) * -1)

tow_bio_data$SPECIES_NAME = "UNKNOWN"

for(i in 1:nrow(spp_index)) {
  tow_bio_data$SPECIES_NAME[tow_bio_data$SPP == spp_index$SPP[i]] = spp_index$COMMON[i]
}

glider_dep = choose_directory() %>% substring(., regexpr("ru[0-9]{2}-*", .))
year = substr(glider_dep,6,9)

data_dir = paste0("C:/Users/Delphine/Box/Glider Data/",
                  glider_dep,
                  "/Derived Biomass Data/")

load(paste0(data_dir, "Glider_Data.rda"))

#####

## Find NJDEP tows in the same area and/or time as the glider

dist_time_subset = tow_bio_data %>%
  filter(SLAT >= range(gdata$latitude)[1] &
           ELAT <= range(gdata$latitude)[2] &
           SLONG >= range(gdata$longitude)[1] &
           ELONG <= range(gdata$longitude)[2]) %>%
  mutate(YRMODA = as.Date(ymd(YRMODA))) %>%
  filter(year(YRMODA) > 2012) %>%
  filter(month(YRMODA) >= 8 &
           month(YRMODA) <= 9) %>%
  group_by(year(YRMODA)) %>%
  mutate(NUM_PERCENTAGE = NUMBER/sum(NUMBER))

ggplot() +
  geom_col(data = dist_time_subset, aes(x = SPECIES_NAME, fill = SPECIES_NAME, y = NUMBER)) +
  facet_wrap(~year(YRMODA))
