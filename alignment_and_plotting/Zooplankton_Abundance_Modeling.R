# Author: Delphine Mossman
# Date Created: 22 May 2025
# Date Last Modified: 25 June 2025

# !!!!!!!!!!!!!!! EXTREMELY WORK IN PROGRESS !!!!!!!!!!!!!!!

#####
## Initialization/Setup

rm(list = ls())

library(tidyverse)
library(readxl)
library(ncdf4)
library(R.utils)
library(tcltk)
library(rsample)
library(mgcv)
library(mgcViz)
library(sf)
library(GGally)
library(gridExtra)
library(MASS)
library(pscl)
library(fitdistrplus)

sourceDirectory(
  "H:/dm1679/Code/R Functions",
  modifiedOnly = F
)

figure_dir = "H:/dm1679/Data/Glider Data/Statistics Plots/"

# home_dir = "H:/dm1679/Data/"
home_dir = "C:/Users/Delphine/Box/"

#####
## Loading in zooplankton data

load(paste0(home_dir,"Glider Data/ru39-20230420T1636/Derived Biomass Data/Zooplankton_FTLE_Correlation_Data.rda"))
assign("zoop_data_spring_2023", zoop_data)
rm(zoop_data)
assign("zoop_data_spring_2023_2", zoop_data2)
rm(zoop_data2)

load(paste0(home_dir,"Glider Data/ru39-20231103T1413/Derived Biomass Data/Zooplankton_FTLE_Correlation_Data.rda"))
assign("zoop_data_fall_2023", zoop_data)
rm(zoop_data)
assign("zoop_data_fall_2023_2", zoop_data2)
rm(zoop_data2)

load(paste0(home_dir,"Glider Data/ru39-20240215T1646/Derived Biomass Data/Zooplankton_FTLE_Correlation_Data.rda"))
assign("zoop_data_winter_2024", zoop_data)
rm(zoop_data)
assign("zoop_data_winter_2024_2", zoop_data2)
rm(zoop_data2)

load(paste0(home_dir,"Glider Data/ru39-20240429T1522/Derived Biomass Data/Zooplankton_FTLE_Correlation_Data.rda"))
assign("zoop_data_spring_2024", zoop_data)
rm(zoop_data)
assign("zoop_data_spring_2024_2", zoop_data2)
rm(zoop_data2)

load(paste0(home_dir,"Glider Data/ru43-20240904T1539/Derived Biomass Data/Zooplankton_FTLE_Correlation_Data.rda"))
assign("zoop_data_summer_2024", zoop_data)
rm(zoop_data)
assign("zoop_data_summer_2024_2", zoop_data2)
rm(zoop_data2)

zoop_data_spring_2023$Season = "Spring"
zoop_data_fall_2023$Season = "Fall"
zoop_data_winter_2024$Season = "Winter"
zoop_data_spring_2024$Season = "Spring"
zoop_data_summer_2024$Season = "Summer"

zoop_data_spring_2023_2$Season = "Spring"
zoop_data_fall_2023_2$Season = "Fall"
zoop_data_winter_2024_2$Season = "Winter"
zoop_data_spring_2024_2$Season = "Spring"
zoop_data_summer_2024_2$Season = "Summer"

zoop_data_full = rbind(zoop_data_spring_2023,zoop_data_fall_2023,zoop_data_winter_2024,zoop_data_spring_2024,zoop_data_summer_2024) %>% 
  # filter(Abundance > 0) %>%
  arrange(Date)
zoop_data_full$Season = factor(zoop_data_full$Season, levels = c("Spring","Summer","Fall","Winter"), ordered = T)

fname = "H:/dm1679/Data/Glider Data/RMI_Zoop_FTLE_Correlation_Data_Full.rda"
save(zoop_data_full, file = fname)

zoop_data_full_2 = rbind(zoop_data_spring_2023_2,zoop_data_fall_2023_2,zoop_data_winter_2024_2,zoop_data_spring_2024_2,zoop_data_summer_2024_2) %>% 
  # filter(D_Int_Abundance > 0) %>%
  arrange(Date)
zoop_data_full_2$Season = factor(zoop_data_full_2$Season, levels = c("Spring","Summer","Fall","Winter"), ordered = T)

fname = "H:/dm1679/Data/Glider Data/RMI_Zoop_FTLE_Correlation_Data_Full_2.rda"
save(zoop_data_full_2, file = fname)

#####

load("H:/dm1679/Data/Glider Data/RMI_Zoop_FTLE_Correlation_Data_Full.rda")

load("H:/dm1679/Data/Glider Data/RMI_Zoop_FTLE_Correlation_Data_Full_2.rda")

## Zero-inflated model tests for each explanatory variable
