# Author: Delphine Mossman
# Date Created: 18 June 2025
# Date Last Modified: 15 July 2025

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

## Initial dataframe creation

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
# rm(data3)
rm(data, data2, data4, data_filenames, data_ldf)

fish_data_summer_2023$Season = "Summer 2023"
fish_data_summer_2024$Season = "Summer 2024"
fish_data_fall_2024$Season = "Fall"
fish_data_winter_2025$Season = "Winter"
# fish_data_spring_2025$Season = "Spring"

fish_data_full = rbind(fish_data_summer_2023,fish_data_summer_2024,fish_data_fall_2024,fish_data_winter_2025) %>% 
  arrange(Date) %>% 
  mutate(TOD = case_when(
    hour(Date) >= 7 & hour(Date) <= 19 ~ "Day",
    hour(Date) < 7 | hour(Date) > 19 ~ "Night"
  ))
fish_data_full$Season = factor(fish_data_full$Season, levels = c("Summer 2023", "Summer 2024", "Fall", "Winter"), ordered = T)

files = list.files(
  'C:/Users/Delphine/Box/ACOUSTIC DATA PROCESSING PROTOCOLS/AZFP Processing/Shapefiles/',
  pattern = '*.shp$',
  full.names = T
)

Study_Areas = lapply(files, function(x)
  read_sf(x) %>% st_transform(crs = st_crs(4326)))

Study_Areas_2 = read_sf(
  "C:/Users/Delphine/Box/COOL/Offshore Wind/wind energy shapefiles/BOEM_shp_kmls/shapefiles/wind_leases/BOEM_Wind_Lease_Outlines_06_06_2024.shp"
) %>%
  st_transform(crs = st_crs(4326))

Study_Area_Final = st_union(Study_Areas[[2]], st_union(Study_Areas_2[25:28,]))

fish_data_full$Wind_Farm = as.character(t(st_intersects(Study_Area_Final, fish_data_full, sparse = FALSE)))

fish_data_full = fish_data_full %>% st_drop_geometry()

fname = "H:/dm1679/Data/Glider Data/RMI_Fish_Correlation_Data_Full.rda"
save(fish_data_full, file = fname)

#####

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
  # mutate(Delta_A = ifelse(Delta_A > 0,
  #                         log10(Delta_A),
  #                         -1 * log10(abs(Delta_A)))) %>%
  mutate(YearSeason = paste0(Year, " ", Season)) %>%
  mutate(YearSeason = factor(YearSeason, levels = YearSeason, ordered = T)) %>%
  mutate(Delta_A = replace(Delta_A, Delta_A == Inf, 0))

sample_sizes_species = fish_data_full %>% 
  group_by(Season, Shelf_Type, Species) %>%
  summarise(Percentage = n()) %>%
  group_by(Species) %>%
  mutate(Percentage=Percentage/sum(Percentage)*100) %>%
  ungroup() %>%
  complete(., Season, Shelf_Type, Species, fill = list(Percentage = 0))

sample_sizes = fish_data_full %>%
  filter(Abundance > 0) %>%
  group_by(Season, Shelf_Type) %>%
  summarise(num =n())

fish_data_full = fish_data_full %>% filter(Abundance > 0)

ggplot() + 
  geom_boxplot(data = fish_data_full, aes(y = Abundance, x = Species, fill = Species)) + 
  scale_fill_viridis_d(begin = 0.1, option = "H") +
  scale_y_continuous(trans="log10") +
  labs(y = "Log10 of Concentration\n(individuals/m^3)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(~Season)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Seasonal_Fish_Concentration_Boxplot.png", scale = 2)

# fish_data_full$Shelf_Type[is.na(fish_data_full$Shelf_Type)] = "Offshore"

ggplot() + 
  geom_boxplot(data = fish_data_full, aes(y = Abundance, x = Species, fill = Species)) + 
  scale_fill_viridis_d(begin = 0.1, option = "H") +
  scale_y_continuous(trans="log10") +
  labs(y = "Log10 of Concentration\n(individuals/m^3)") +
  theme_bw() +
  theme(theme(axis.text.x = element_text(angle = 45, hjust = 1))) +
  facet_grid(Season~Shelf_Type)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Shelf_Type_Fish_Concentration_Boxplot.png", scale = 2)

ggplot() + 
  geom_boxplot(data = fish_data_full, aes(y = Abundance, x = Species, fill = Species)) + 
  scale_fill_viridis_d(begin = 0.1, option = "H") +
  scale_y_continuous(trans="log10") +
  labs(y = "Log10 of\nConcentration (individuals/m^3)") +
  theme_bw() +
  theme(theme(axis.text.x = element_text(angle = 45, hjust = 1))) +
  facet_grid(Season~Depth_Type)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Depth_Type_Fish_Concentration_Boxplot.png", scale = 2)

#####

fish_data_full[,c("Abundance","Biomass")] = log10(fish_data_full[,c("Abundance","Biomass")])

fish_data_full = complete(fish_data_full, Season, Species, Shelf_Type, fill = list(Abundance = -999, Biomass = -999), explicit  = F)

ggplot(data = fish_data_full, 
       aes(x = Season, color = Shelf_Type, y = Abundance)) + 
  geom_boxplot(linewidth = 0.75,
               notch = T) +
  scale_color_viridis_d(end = 0.8) +
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
  scale_color_viridis_d(end = 0.8) +
  stat_summary(fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  labs(y = "Log10 of Biomass\n(individuals/m^3)",
       color = "NOAA Strata\nAssignment") +
  theme_bw() +
  coord_cartesian(ylim = c(-6,NA), clip = 'off') +
  facet_wrap(~Species)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Shelf_Type_Biomass_Fish_Boxplot.png", scale = 2)

#####

## Load up clean version of fish abundance data

load("H:/dm1679/Data/Glider Data/RMI_Fish_Correlation_Data_Full.rda")

fish_data_TOD = fish_data_full %>% 
  filter(Abundance > 0) %>%
  group_by(Species) %>%
  mutate(Abundance = log10(Abundance), Biomass = log10(Biomass)) %>%
  ungroup() %>%
  complete(., TOD, Species, Season, Shelf_Type, fill = list(Abundance = -999, Biomass = -999))

# Boxplots of concentration/biomass separated by day/night and inside/outside study area

ggplot(data = fish_data_TOD, 
       aes(x = Season, color = TOD, y = Abundance)) + 
  geom_boxplot(linewidth = 0.75,
               notch = T) +
  scale_color_viridis_d(option="G", end = 0.8, direction = -1) +
  stat_summary(fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  labs(y = "Log10 of\nConcentration (individuals/m^3)",
       color = "Time of Day") +
  coord_cartesian(ylim = c(-6, 4), clip = 'off') +
  theme_bw() +
  facet_wrap(~Species)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_TOD_Concentration_Fish_Boxplot.png", scale = 2)

ggplot(data = fish_data_TOD, 
       aes(x = Season, color = Shelf_Type, y = Abundance)) + 
  geom_boxplot(linewidth = 0.75,
               notch = T) +
  scale_color_viridis_d(begin = 0, end=0.8) +
  stat_summary(fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  labs(y = "Log10 of\nConcentration (individuals/m^3)",
       color = "NOAA Strata\nAssignment") +
  coord_cartesian(ylim = c(-7, 3), clip = 'off') +
  theme_bw() +
  facet_grid(TOD~Species)
ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Shelf_Type_TOD_Concentration_Fish_Boxplot.png", scale = 2)


fish_data_wind_farm = fish_data_full %>% 
  filter(Abundance > 0) %>%
  group_by(Species) %>%
  mutate(Abundance = log10(Abundance), Biomass = log10(Biomass)) %>%
  ungroup() %>%
  complete(., Species, Season, Wind_Farm, Shelf_Type, fill = list(Abundance = -999, Biomass = -999))

ggplot(data = fish_data_wind_farm, 
       aes(x = Season, color = Wind_Farm, y = Abundance)) + 
  geom_boxplot(linewidth = 0.75,
               notch = T) +
  scale_color_viridis_d(option = "A", end = 0.7, labels = c("Outside","Inside")) +
  stat_summary(fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  labs(y = "Log10 of\nConcentration (individuals/m^3)",
       color = "Location Relative to\nWind Farm Lease Areas") +
  coord_cartesian(ylim = c(-6, 3), clip = 'off') +
  theme_bw() +
  facet_wrap(~Species)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Wind_Farm_Concentration_Fish_Boxplot.png", scale = 2)

ggplot(data = fish_data_wind_farm, 
       aes(x = Season, color = Shelf_Type, y = Abundance)) + 
  geom_boxplot(linewidth = 0.75,
               notch = T) +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  stat_summary(fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  labs(y = "Log10 of\nConcentration (individuals/m^3)",
       color = "NOAA Strata\nAssignment") +
  coord_cartesian(ylim = c(-7, 3), clip = 'off') +
  theme_bw() +
  facet_grid(Wind_Farm~Species, labeller = labeller(Wind_Farm = c("FALSE" = "Outside",
                                                           "TRUE" = "Inside")))

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Shelf_Type_Wind_Farm_Concentration_Fish_Boxplot.png", scale = 2)

#####
# Load up a clean version of fish_data_full

load("H:/dm1679/Data/Glider Data/RMI_Fish_Correlation_Data_Full.rda")

# Integrate top 5 and bottom 5 m of water column
# Since the cells are 1 m thick, it's just a sum ("silent" multiplication by 1)

fish_data_surface_bottom = fish_data_full %>% 
  filter(Abundance > 0) %>%
  group_by(Seafloor_Depth) %>%
  mutate(Surface_Bottom = case_when (
    Depth <= 5 ~ "Surface",
    Depth >= (Seafloor_Depth - 5) ~ "Bottom"
  )) %>%
  ungroup() %>%
  drop_na(Surface_Bottom) %>%
  group_by(Species, Surface_Bottom, Shelf_Type, Season) %>%
  reframe(
    Int_Abundance = log10(sum(Abundance)),
    Int_Biomass = log10(sum(Biomass))) %>%
  complete(., Species, Surface_Bottom, Shelf_Type, Season, fill = list(Int_Abundance = -999, Int_Biomass = -999))

# Boxplots separated by surface/bottom

ggplot(data = fish_data_surface_bottom, aes(x = Surface_Bottom, y = Int_Abundance, color = Shelf_Type)) +
  geom_boxplot(linewidth = 0.75, notch = T) +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  stat_summary(fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  coord_cartesian(clip = 'off', ylim = c(-6,6)) +
  labs(y = "Log10 ofDepth-Integrated\nConcentration (individuals/m^2)",
   color = "NOAA Strata\nAssignment",
   x = "Surface or Bottom") +
  facet_grid(Season~Species)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Shelf_Type_Surface_Bottom_Concentration_Fish_Boxplot.png", scale = 2)

#####
# Load up a clean version of fish_data_full

load("H:/dm1679/Data/Glider Data/RMI_Fish_Correlation_Data_Full.rda")

# Vertical bar plot profiles

fish_data_vertical = fish_data_full %>%
  mutate(Depth = floor(Depth)) %>%
  group_by(Season, Depth) %>%
  reframe(Abundance = mean(Abundance), Species = "Fish")

ggplot() +
  geom_col(data = fish_data_vertical, aes(y = Abundance, x = Depth, fill = Species)) +
  scale_x_reverse() +
  scale_fill_viridis_d(option = "H") + guides(fill = "none") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(y = expression("Fish Concentration (ind/m"^3*")"),
       x = "Depth (m)") +
  facet_wrap(~Season, scales = "free_x")

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Fish_Vertical_Profile_Mean.png", scale = 2)

fish_data_full %>%
  group_by(Season, Shelf_Type) %>%
  summarize(mean(Abundance))
  
