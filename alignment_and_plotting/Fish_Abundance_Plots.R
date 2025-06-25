# Author: Delphine Mossman
# Date Created: 18 June 2025
# Date Last Modified: 25 June 2025

# WIP; meant to accomplish the same thing as Zooplankton_Abundance_Plots.R but for fish data

rm(list = ls())

library(tidyverse)
library(readxl)
library(hms)
library(sf)
library(sfheaders)
library(lubridate)
library(marmap)
library(rstatix)
library(R.utils)
library(ggpubr)
library(ggnewscale)
library(rnaturalearth)
library(tcltk)
library(cowplot)

# home_dir = "H:/dm1679/Data/"
home_dir = "C:/Users/Delphine/Box/"

#####

## Delta concentration/biomass figures

# This file is made by Zooplankton_Abundance_Modeling.R
# load("H:/dm1679/Data/Glider Data/RMI_Zoop_FTLE_Correlation_Data_Full.rda")

load(paste0(home_dir,"Glider Data/ru39-20230817T1520/Derived Biomass Data/Processed_Abundance_Biomass_Data.rda"))
assign("fish_data_summer_2023", data3)
rm(data3)

load(paste0(home_dir,"Glider Data/ru39-20240723T1442/Derived Biomass Data/Processed_Abundance_Biomass_Data.rda"))
assign("fish_data_summer_2024", data3)
rm(data3)

load(paste0(home_dir,"Glider Data/ru39-20241021T1717/Derived Biomass Data/Processed_Abundance_Biomass_Data.rda"))
assign("fish_data_fall_2024", data3)
rm(data3)

load(paste0(home_dir,"Glider Data/ru39-20250226T1700/Derived Biomass Data/Processed_Abundance_Biomass_Data.rda"))
assign("fish_data_winter_2025", data3)
rm(data3)

# load(paste0(home_dir,"Glider Data/ru39-20250423T1535/Derived Biomass Data/Processed_Abundance_Biomass_Data.rda"))
# assign("fish_data_spring_2025", data3)
rm(data, data2, data3, data4, data_filenames, data_ldf)

fish_data_summer_2023$Season = "Summer"
fish_data_summer_2024$Season = "Summer"
fish_data_fall_2024$Season = "Fall"
fish_data_winter_2025$Season = "Winter"
# fish_data_spring_2025$Season = "Spring"

fish_data_full = rbind(fish_data_summer_2023,fish_data_summer_2024,fish_data_fall_2024,fish_data_winter_2025) %>% 
  filter(Abundance > 0) %>%
  arrange(Date) %>% st_drop_geometry()
fish_data_full$Season = factor(fish_data_full$Season, levels = c("Spring","Summer","Fall","Winter"), ordered = T)

fname = "H:/dm1679/Data/Glider Data/RMI_Fish_Correlation_Data_Full.rda"
save(fish_data_full, file = fname)

load("H:/dm1679/Data/Glider Data/RMI_Fish_Correlation_Data_Full.rda")

fish_data_full_delta = fish_data_full %>%
  mutate(Year = year(Date)) %>%
  group_by(Year, Season, Species) %>%
  reframe(Avg_Abundance = mean(Abundance)) %>%
  complete(Species, nesting(Year, Season), fill = list(Avg_Abundance = 0)) %>%
  group_by(Species) %>%
  mutate(Delta_A = ifelse(row_number() == 1,
                          0,
                          Avg_Abundance - lag(Avg_Abundance, 1))) %>%
  mutate(Delta_A = ifelse(Delta_A > 0,
                          log10(Delta_A),
                          -1 * log10(abs(Delta_A)))) %>%
  mutate(YearSeason = paste0(Year, " ", Season)) %>%
  mutate(YearSeason = factor(YearSeason, levels = YearSeason, ordered = T)) %>%
  mutate(Delta_A = replace(Delta_A, Delta_A == Inf, 0))

ggplot() + 
  geom_boxplot(data = fish_data_full, aes(y = Abundance, x = Species, fill = Species)) + 
  scale_fill_viridis_d(option = "G") +
  scale_y_continuous(trans="log10") +
  labs(y = "Log10 of Concentration\n(individuals/m^3)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(~Season)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Seasonal_Fish_Concentration_Boxplot.png", scale = 2)

# fish_data_full$Shelf_Type[is.na(fish_data_full$Shelf_Type)] = "Offshore"

ggplot() + 
  geom_boxplot(data = fish_data_full, aes(y = Abundance, x = Species, fill = Species)) + 
  scale_fill_viridis_d(option = "G") +
  scale_y_continuous(trans="log10") +
  labs(y = "Log10 of Concentration\n(individuals/m^3)") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  facet_grid(Season~Shelf_Type)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Shelf_Type_Fish_Concentration_Boxplot.png", scale = 2)

ggplot() + 
  geom_boxplot(data = fish_data_full, aes(y = Abundance, x = Species, fill = Species)) + 
  scale_fill_viridis_d(option = "G") +
  scale_y_continuous(trans="log10") +
  labs(y = "Log10 of Large Copepod\nConcentration (individuals/m^3)") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  facet_grid(Season~Depth_Type)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Depth_Type_Fish_Concentration_Boxplot.png", scale = 2)

#####

fish_data_full[,c("Abundance","Biomass")] = log10(fish_data_full[,c("Abundance","Biomass")])

fish_data_full = complete(fish_data_full, Season, Species, Shelf_Type, fill = list(Abundance = -999, Biomass = -999), explicit  = F)

ggplot(data = fish_data_full, 
       aes(x = Season, color = Shelf_Type, y = Abundance)) + 
  geom_boxplot(linewidth = 0.75,
               notch = T) +
  scale_color_viridis_d(option = "G") +
  stat_summary(fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  labs(y = "Log10 of Concentration\n(individuals/m^3)",
       color = "NOAA Strata\nAssignment") +
  theme_bw() +
  coord_cartesian(ylim = c(-8,NA), clip = 'off') +
  facet_wrap(~Species)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Shelf_Type_Concentration_Fish_Boxplot.png", scale = 2)

ggplot(data = fish_data_full, 
       aes(x = Season, color = Shelf_Type, y = Biomass)) + 
  geom_boxplot(linewidth = 0.75,
               notch = T) +
  scale_color_viridis_d(option = "G") +
  stat_summary(fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  labs(y = "Log10 of Biomass\n(individuals/m^3)",
       color = "NOAA Strata\nAssignment") +
  theme_bw() +
  coord_cartesian(ylim = c(-6,NA), clip = 'off') +
  facet_wrap(~Species)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Shelf_Type_Biomass_Fish_Boxplot.png", scale = 2)
