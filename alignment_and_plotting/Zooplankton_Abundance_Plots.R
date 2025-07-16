# Author: Delphine Mossman
# Date Created: 22 May 2025
# Date Last Modified: 15 July 2025

# 1. Load in the zooplankton data from every deployment so far and combine them into one big dataframe
# 2. Create boxplots of concentration/biomass by season, shelf type, and depth type
# 3. Create boxplots of concentration/biomass by shelf type and season
# 4. Create a bar graph of change in concentration/biomass over the seasons
# 5. Repeat for depth-integrated concentration/biomass values


#####
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

load(paste0(home_dir,"Glider Data/ru39-20230420T1636/Derived Biomass Data/Processed_Abundance_Biomass_Data.rda"))
assign("zoop_data_spring_2023", data3)
rm(data3)

load(paste0(home_dir,"Glider Data/ru39-20231103T1413/Derived Biomass Data/Processed_Abundance_Biomass_Data.rda"))
assign("zoop_data_fall_2023", data3)
rm(data3)

load(paste0(home_dir,"Glider Data/ru39-20240215T1646/Derived Biomass Data/Processed_Abundance_Biomass_Data.rda"))
assign("zoop_data_winter_2024", data3)
rm(data3)

load(paste0(home_dir,"Glider Data/ru39-20240429T1522/Derived Biomass Data/Processed_Abundance_Biomass_Data.rda"))
assign("zoop_data_spring_2024", data3)
rm(data3)

load(paste0(home_dir,"Glider Data/ru43-20240904T1539/Derived Biomass Data/Processed_Abundance_Biomass_Data.rda"))
assign("zoop_data_summer_2024", data3)
rm(data, data2, data3, data4, data_filenames, data_ldf)

zoop_data_spring_2023$Season = "Spring (2023)"
zoop_data_fall_2023$Season = "Fall"
zoop_data_winter_2024$Season = "Winter"
zoop_data_spring_2024$Season = "Spring (2024)"
zoop_data_summer_2024$Season = "Early Fall"

zoop_data_full = rbind(zoop_data_spring_2023,zoop_data_fall_2023,zoop_data_winter_2024,zoop_data_spring_2024,zoop_data_summer_2024) %>% 
  arrange(Date) %>% 
  mutate(TOD = case_when(
    hour(Date) >= 7 & hour(Date) <= 19 ~ "Day",
    hour(Date) < 7 | hour(Date) > 19 ~ "Night"
  ))
zoop_data_full$Season = factor(zoop_data_full$Season, levels = c("Spring (2023)","Fall","Winter", "Spring (2024)", "Early Fall"), ordered = T)

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

zoop_data_full$Wind_Farm = as.character(t(st_intersects(Study_Area_Final, zoop_data_full, sparse = FALSE)))

zoop_data_full = zoop_data_full %>% st_drop_geometry()

# Just in caseies
zoop_data_full$Shelf_Type[is.na(zoop_data_full$Shelf_Type)] = "Offshore"
zoop_data_full$Depth_Type[is.na(zoop_data_full$Depth_Type)] = "Deep"

fname = "H:/dm1679/Data/Glider Data/RMI_Zoop_Correlation_Data_Full.rda"
save(zoop_data_full, file = fname)

#####
load("H:/dm1679/Data/Glider Data/RMI_Zoop_Correlation_Data_Full.rda")

# Making a dataframe of the change in average abundance

zoop_data_full_delta = zoop_data_full %>%
  filter(Abundance > 0) %>%
  mutate(Year = year(Date)) %>%
  group_by(Year, Season, Species) %>%
  reframe(Avg_Abundance = mean(Abundance)) %>%
  group_by(Species) %>%
  mutate(Delta_A = ifelse(row_number() == 1,
                          0,
                          Avg_Abundance - lag(Avg_Abundance, 1))) %>%
  mutate(YearSeason = paste0(Year, " ", Season)) %>%
  mutate(YearSeason = factor(YearSeason, levels = YearSeason, ordered = T)) %>%
  mutate(Delta_A = replace(Delta_A, Delta_A == Inf, 0))

# Getting the total sample sizes

sample_sizes_species = zoop_data_full %>% 
  group_by(Season, Shelf_Type, Species) %>%
  summarise(Percentage = n()) %>%
  group_by(Species) %>%
  mutate(Percentage=Percentage/sum(Percentage)*100) %>%
  ungroup() %>%
  complete(., Season, Shelf_Type, Species, fill = list(Percentage = 0))

sample_sizes = zoop_data_full %>%
  group_by(Season, Shelf_Type) %>%
  summarise(num =n())

#####

# Plots of abundance by season, shelf type, and depth type

zoop_data_full[,c("Abundance","Biomass")] = log10(zoop_data_full[,c("Abundance","Biomass")])

zoop_data_full = (complete(zoop_data_full, Season, Species, Shelf_Type, fill = list(Abundance = -999, Biomass = -999, Depth_Type = "Deep")))
zoop_data_full[zoop_data_full$Abundance == -Inf, c("Abundance", "Biomass")] = -999

ggplot() + 
  geom_boxplot(data = zoop_data_full[zoop_data_full$Species == "Large Copepod",], aes(y = Abundance)) + 
  scale_fill_viridis_d(guide = NULL, begin = 0.2) +
  labs(y = "Log10 of Large Copepod\nConcentration (individuals/m^3)") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  coord_cartesian(ylim = c(0,5), clip = 'off') +
  facet_grid(~Season)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Seasonal_Concentration_Boxplot.png", scale = 2)

ggplot() + 
  geom_boxplot(data = zoop_data_full[zoop_data_full$Species == "Large Copepod",], aes(y = Abundance)) + 
  scale_fill_viridis_d(guide = NULL, begin = 0.2) +
  labs(y = "Log10 of Large Copepod\nConcentration (individuals/m^3)") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  coord_cartesian(ylim = c(0,5), clip = 'off') +
  facet_grid(Season~Shelf_Type)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Shelf_Type_Concentration_Boxplot.png", scale = 2)

ggplot() + 
  geom_boxplot(data = zoop_data_full[zoop_data_full$Species == "Large Copepod",], aes(y = Abundance)) + 
  scale_fill_viridis_d(guide = NULL, begin = 0.2) +
  labs(y = "Log10 of Large Copepod\nConcentration (individuals/m^3)") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  coord_cartesian(ylim = c(0,5), clip = 'off') +
  facet_grid(Season~Depth_Type)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Depth_Type_Concentration_Boxplot.png", scale = 2)

#####

# Boxplots of concentration/biomass separated by shelf strata and season

ggplot(data = zoop_data_full[zoop_data_full$Species == "Large Copepod", ], 
       aes(x = Season, color = Shelf_Type, y = Abundance)) + 
  geom_boxplot(linewidth = 0.75,
               notch = T) +
  scale_color_viridis_d(begin = 0, end=0.8) +
  stat_summary(fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  labs(y = "Log10 of Large Copepod\nConcentration (individuals/m^3)",
       color = "NOAA Strata\nAssignment") +
  coord_cartesian(ylim = c(-4, 5), clip = 'off') +
  theme_bw()

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Shelf_Type_Concentration_Large_Copepods_Boxplot.png", scale = 2)

ggplot(data = zoop_data_full[zoop_data_full$Species == "Large Copepod", ], 
       aes(x = Season, color = Shelf_Type, y = Biomass)) + 
  geom_boxplot(linewidth = 0.75,
               notch = T) +
  stat_summary(fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  labs(y = "Log10 of Large Copepod\nBiomass (g/m^3)",
       color = "NOAA Strata\nAssignment") +
  coord_cartesian(ylim = c(-4, 5), clip = 'off') +
  theme_bw()

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Shelf_Type_Biomass_Large_Copepods_Boxplot.png", scale = 2)

#####

# Load up a clean version of zoop_data_full

load("H:/dm1679/Data/Glider Data/RMI_Zoop_Correlation_Data_Full.rda")

zoop_data_TOD = zoop_data_full %>% 
  filter(Species == "Large Copepod") %>%
  mutate(Abundance = log10(Abundance), Biomass = log10(Biomass)) %>%
  complete(., TOD, Shelf_Type, Season, fill = list(Abundance = -999, Biomass = -999))

# Boxplots of concentration/biomass separated by day/night and inside/outside study area

ggplot(data = zoop_data_TOD, 
       aes(x = Season, color = TOD, y = Abundance)) + 
  geom_boxplot(linewidth = 0.75,
               notch = T) +
  scale_color_viridis_d(option="G", end = 0.8, direction = -1) +
  stat_summary(fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  labs(y = "Log10 of Large Copepod\nConcentration (individuals/m^3)",
       color = "Time of Day") +
  coord_cartesian(ylim = c(-4, 6), clip = 'off') +
  theme_bw()

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_TOD_Concentration_Large_Copepods_Boxplot.png", scale = 2)

ggplot(data = zoop_data_TOD, 
       aes(x = Season, color = Shelf_Type, y = Abundance)) + 
  geom_boxplot(linewidth = 0.75,
               notch = T) +
  scale_color_viridis_d(begin = 0, end=0.8) +
  stat_summary(fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  labs(y = "Log10 of Large Copepod\nConcentration (individuals/m^3)",
       color = "NOAA Strata\nAssignment") +
  coord_cartesian(ylim = c(-4, 5), clip = 'off') +
  theme_bw() +
  facet_wrap(~TOD)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Shelf_Type_TOD_Concentration_Large_Copepods_Boxplot.png", scale = 2)

zoop_data_wind_farm = zoop_data_full %>% 
  filter(Species == "Large Copepod") %>%
  mutate(Abundance = log10(Abundance), Biomass = log10(Biomass)) %>%
  complete(., Wind_Farm, Shelf_Type, Season, fill = list(Abundance = -999, Biomass = -999))

ggplot(data = zoop_data_wind_farm, 
       aes(x = Season, color = Wind_Farm, y = Abundance)) + 
  geom_boxplot(linewidth = 0.75,
               notch = T) +
  scale_color_viridis_d(option = "A", end = 0.7, labels = c("Outside","Inside")) +
  stat_summary(fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  labs(y = "Log10 of Large Copepod\nConcentration (individuals/m^3)",
       color = "Location Relative to\nWind Farm Lease Areas") +
  coord_cartesian(ylim = c(-4, 5), clip = 'off') +
  theme_bw()

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Wind_Farm_Concentration_Large_Copepods_Boxplot.png", scale = 2)

ggplot(data = zoop_data_wind_farm, 
       aes(x = Season, color = Shelf_Type, y = Abundance)) + 
  geom_boxplot(linewidth = 0.75,
               notch = T) +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  stat_summary(fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  labs(y = "Log10 of Large Copepod\nConcentration (individuals/m^3)",
       color = "NOAA Strata\nAssignment") +
  coord_cartesian(ylim = c(-4, 5), clip = 'off') +
  theme_bw() +
  facet_wrap(~Wind_Farm, labeller = labeller(Wind_Farm = c("FALSE" = "Outside",
                                                           "TRUE" = "Inside")))

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Shelf_Type_Wind_Farm_Concentration_Large_Copepods_Boxplot.png", scale = 2)

#####
# Load up a clean version of zoop_data_full

load("H:/dm1679/Data/Glider Data/RMI_Zoop_Correlation_Data_Full.rda")

# Integrate top 5 and bottom 5 m of water column
# Since the cells are 1 m thick, it's just a sum ("silent" multiplication by 1)

zoop_data_surface_bottom = zoop_data_full %>% 
  group_by(Seafloor_Depth) %>%
  mutate(Surface_Bottom = case_when (
    Depth <= 5 ~ "Surface",
    Depth >= (Seafloor_Depth - 5) ~ "Bottom"
  )) %>%
  ungroup() %>%
  drop_na(Surface_Bottom) %>%
  filter(Species == "Large Copepod") %>%
  group_by(Surface_Bottom, Wind_Farm, Shelf_Type, Season) %>%
  reframe(
    Int_Abundance = log10(sum(Abundance)),
    Int_Biomass = log10(sum(Biomass))) %>%
  complete(., Surface_Bottom, Shelf_Type, Season, fill = list(Int_Abundance = -999, Int_Biomass = -999))

# Boxplots separated by surface/bottom

ggplot(data = zoop_data_surface_bottom, aes(x = Surface_Bottom, y = Int_Abundance, color = Shelf_Type)) +
  geom_boxplot(linewidth = 0.75, notch = T) +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  stat_summary(fun = mean, geom = "point", show.legend = F, position = position_dodge(0.75), shape=4, size=4, stroke = 1) +
  coord_cartesian(clip = 'off', ylim = c(1,6)) +
  labs(y = "Log10 of Depth-Integrated Large\nCopepod Concentration (individuals/m^2)",
       color = "NOAA Strata\nAssignment",
       x = "Surface or Bottom") +
  theme_bw() +
  facet_wrap(~Season)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Shelf_Type_Surface_Bottom_Concentration_Large_Copepods_Boxplot.png", scale = 2)

#####
# Load up a clean version of zoop_data_full

load("H:/dm1679/Data/Glider Data/RMI_Zoop_Correlation_Data_Full.rda")

# Vertical bar plot profiles

zoop_data_vertical = zoop_data_full %>%
  group_by(Season, Depth) %>%
  reframe(Abundance = sum(Abundance), Species = "Large Copepod")

ggplot() +
  geom_col(data = zoop_data_vertical, aes(y = Abundance, x = Depth, fill = Species)) +
  scale_fill_viridis_d(begin = 0.2) + guides(fill = "none") +
  scale_x_reverse() +
  coord_flip() + 
  labs(y = expression("Large Copepod Concentration (ind/m"^3*")"),
       x = "Depth (m)") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~Season)

ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Large_Copepods_Vertical_Profile_Sum.png", scale = 2)

zoop_data_full %>%
  group_by(Season) %>%
  summarize(mean(Abundance))

#####
# Change in average concentration plot

ggplot() + 
  geom_col(dat = zoop_data_full_delta, position = position_dodge(), aes(x = Species, y = Delta_A, group = YearSeason, fill = YearSeason)) +
  scale_fill_viridis_d(
    breaks = zoop_data_full_delta$YearSeason,
    labels = c("Spring 2023", "Fall 2023", "Winter 2024", "Spring 2024", "Early Fall 2024")
  ) +
  labs(x = "Species",
       y = "Change in Average Concentration",
       fill = "Year and Season")
ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Avg_Concentration_Change_Plot.png", scale = 2)

# ggplot() + 
#   geom_col(dat = zoop_data_full_delta, position = position_dodge(), aes(x = Species, y = log10(Abundance), group = YearSeason, fill = YearSeason)) +
#   scale_fill_viridis_d(
#     breaks = c("2023 Spring", "2023 Fall", "2024 Winter", "2024 Spring", "2024 Summer")
#   ) +
#   labs(x = "Species",
#        y = "log10 of Abundance",
#        fill = "Year and Season")
# ggsave("H:/dm1679/Data/Glider Data/RMI_Abundance_Plot.png", scale = 2)

#####

load(paste0(home_dir,"Glider Data/ru39-20230420T1636/Derived Biomass Data/Processed_Abundance_Biomass_Data.rda"))
assign("zoop_data_spring_2023_d_int", data4)
rm(data4)

load(paste0(home_dir,"Glider Data/ru39-20231103T1413/Derived Biomass Data/Processed_Abundance_Biomass_Data.rda"))
assign("zoop_data_fall_2023_d_int", data4)
rm(data4)

load(paste0(home_dir,"Glider Data/ru39-20240215T1646/Derived Biomass Data/Processed_Abundance_Biomass_Data.rda"))
assign("zoop_data_winter_2024_d_int", data4)
rm(data4)

load(paste0(home_dir,"Glider Data/ru39-20240429T1522/Derived Biomass Data/Processed_Abundance_Biomass_Data.rda"))
assign("zoop_data_spring_2024_d_int", data4)
rm(data4)

load(paste0(home_dir,"Glider Data/ru43-20240904T1539/Derived Biomass Data/Processed_Abundance_Biomass_Data.rda"))
assign("zoop_data_summer_2024_d_int", data4)
rm(data, data2, data3, data4, data_filenames, data_ldf)

zoop_data_spring_2023_d_int$Season = "Spring (2023)"
zoop_data_fall_2023_d_int$Season = "Fall"
zoop_data_winter_2024_d_int$Season = "Winter"
zoop_data_spring_2024_d_int$Season = "Spring (2024)"
zoop_data_summer_2024_d_int$Season = "Early Fall"

zoop_data_full_2 = rbind(zoop_data_spring_2023_d_int,zoop_data_fall_2023_d_int,
                       zoop_data_winter_2024_d_int,zoop_data_spring_2024_d_int,
                       zoop_data_summer_2024_d_int) %>% 
  arrange(Date) %>% 
  mutate(TOD = case_when(
    hour(Date) >= 7 & hour(Date) <= 19 ~ "Day",
    hour(Date) < 7 | hour(Date) > 19 ~ "Night"
  ))
zoop_data_full_2$Season = factor(zoop_data_full_2$Season, levels = c("Spring (2023)","Fall","Winter", "Spring (2024)", "Early Fall"), ordered = T)

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

zoop_data_full_2$Wind_Farm = as.character(t(st_intersects(Study_Area_Final, zoop_data_full_2, sparse = FALSE)))

zoop_data_full_2 = zoop_data_full_2 %>% st_drop_geometry()

# Just in caseies
zoop_data_full_2$Shelf_Type[is.na(zoop_data_full_2$Shelf_Type)] = "Offshore"
zoop_data_full_2$Depth_Type[is.na(zoop_data_full_2$Depth_Type)] = "Deep"

fname = "H:/dm1679/Data/Glider Data/RMI_Zoop_Correlation_D_Int_Data_Full.rda"
save(zoop_data_full_2, file = fname)

zoop_data_full_delta_2 = zoop_data_full_2 %>%
  mutate(Year = year(Date)) %>%
  group_by(Year, Season, Species, Depth_Type) %>%
  reframe(Avg_D_Int_Abundance = mean(D_Int_Abundance)) %>%
  slice(1:12, 25:30, 13:24) %>%
  mutate(YearSeason = paste0(Year, " ", Season)) %>%
  group_by(Species, Depth_Type) %>%
  mutate(Delta = ifelse(row_number() == 1,
                        0,
                        Avg_D_Int_Abundance - lag(Avg_D_Int_Abundance, 1))) %>%
  mutate(Delta = ifelse(Delta > 0,
                        log10(Delta),
                        -1 * log10(abs(Delta)))) %>%
  mutate(YearSeason = factor(YearSeason, levels = YearSeason, ordered = T))
zoop_data_full_delta_2$Delta[1:6] = 0

#####

ggplot() + 
  geom_boxplot(data = zoop_data_full_2, aes(x = Species, fill = Species, y = D_Int_Abundance)) + 
  scale_fill_viridis_d(guide = NULL, begin = 0.2) +
  scale_y_continuous(trans="log10") +
  labs(y = "Log10 of Concentration (individuals/m^3)") +
  facet_grid(rows=vars(Depth_Type), cols=vars(Season))
ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Seasonal_Concentration_Depth_Boxplot.png", scale = 2)

ggplot() + 
  geom_col(dat = zoop_data_full_delta_2, position = position_dodge(), aes(x = Species, y = Delta, group = YearSeason, fill = YearSeason)) +
  scale_fill_viridis_d(
    breaks = c("2023 Spring", "2023 Fall", "2024 Winter", "2024 Spring", "2024 Summer")
  ) +
  labs(x = "Species",
       y = "log10 of Change in Abundance",
       fill = "Year and Season") +
  facet_grid(~Depth_Type)
ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Concentration_Change_Depth_Type_Plot.png", scale = 2)

#####

zoop_data_full_delta_3 = zoop_data_full_2 %>%
  mutate(Year = year(Date)) %>%
  group_by(Year, Season, Species, Shelf_Type) %>%
  reframe(Avg_D_Int_Abundance = mean(D_Int_Abundance)) %>%
  slice(1:12, 24:27, 13:23) %>%
  mutate(YearSeason = paste0(Year, " ", Season))

## Need to do some fiddling to account for missing seasons

padding = data.frame(Year = 2024,
                     Season = c("Winter","Winter","Summer"),
                     Species = c("Large Copepod","Small Copepod","Large Copepod"),
                     Shelf_Type = "Inshore",
                     Avg_D_Int_Abundance = 0,
                     YearSeason = c("2024 Winter","2024 Winter","2024 Summer"))

zoop_data_full_delta_3 = rbind(zoop_data_full_delta_3, padding)

zoop_data_full_delta_3 = zoop_data_full_delta_3 %>%
  slice(1:12,28,13:14,29,15:22,30,23:27) %>%
  group_by(Species, Shelf_Type) %>%
  mutate(Delta = ifelse(row_number() == 1,
                        0,
                        Avg_D_Int_Abundance - lag(Avg_D_Int_Abundance, 1))) %>%
  mutate(Delta = ifelse(Delta > 0,
                        log10(Delta),
                        -1 * log10(abs(Delta))))

zoop_data_full_delta_3$Delta[1:6] = 0
zoop_data_full_delta_3$YearSeason = factor(zoop_data_full_delta_3$YearSeason, levels = c("2023 Spring", "2023 Fall", "2024 Winter", "2024 Spring", "2024 Summer"), ordered = T)

#####

ggplot() + 
  geom_boxplot(data = zoop_data_full_2, aes(x = Species, fill = Species, y = D_Int_Abundance)) + 
  scale_fill_viridis_d(guide = NULL, begin = 0.2) +
  scale_y_continuous(trans="log10") +
  labs(y = "Log10 of Concentration (individuals/m^3)") +
  facet_grid(rows=vars(Shelf_Type), cols=vars(Season))
ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Seasonal_Concentration_Shelf_Boxplot.png", scale = 2)

ggplot() + 
  geom_col(dat = zoop_data_full_delta_3, position = position_dodge(), aes(x = Species, y = Delta, group = YearSeason, fill = YearSeason)) +
  scale_fill_viridis_d(
    breaks = c("2023 Spring", "2023 Fall", "2024 Winter", "2024 Spring", "2024 Summer")
  ) +
  labs(x = "Species",
       y = "log10 of Change in Abundance",
       fill = "Year and Season") +
  facet_grid(~Shelf_Type)
ggsave("H:/dm1679/Data/Glider Data/Statistics Plots/RMI_Concentration_Change_Shelf_Type_Plot.png", scale = 2)

