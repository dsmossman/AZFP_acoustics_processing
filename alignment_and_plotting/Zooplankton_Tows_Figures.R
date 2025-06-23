# Author: Delphine Mossman
# Date Created: 24 Mar 2025
# Date Last Modified: 23 June 2025

# 1. Import and plot data from Rutgers zooplankton tows
# 2. Import and plot data from ECOMON tows
# 3. Perform some comparisons between the two

#####
## Setup and path assignment

rm(list = ls())

library(tidyverse)
library(readxl)
library(sf)
library(R.utils)
library(tcltk)

sourceDirectory(
  "H:/dm1679/Code/R_Functions",
  modifiedOnly = F
)

figure_dir = "C:/Users/Delphine/Box/Zooplankton Protocols and Data/Figures/"

#####
## Rutgers zooplankton tow data

net_data_2023 = read_csv("C:/Users/Delphine/Box/Zooplankton Protocols and Data/RMI Datasheets/2023_zooplankton_tows_RMI.csv", show_col_types = FALSE)
net_data_2024 = read_csv("C:/Users/Delphine/Box/Zooplankton Protocols and Data/RMI Datasheets/2024_zooplankton_tows_RMI.csv", show_col_types = FALSE)
net_data_2025 = read_csv("C:/Users/Delphine/Box/Zooplankton Protocols and Data/RMI Datasheets/2025_zooplankton_tows_RMI.csv", show_col_types = FALSE)

net_data_full = rbind(net_data_2023,net_data_2024,net_data_2025)

save(net_data_full, file = "C:/Users/Delphine/Box/Zooplankton Protocols and Data/RMI Datasheets/zooplankton_tows_RMI.rda")

load("C:/Users/Delphine/Box/Zooplankton Protocols and Data/RMI Datasheets/zooplankton_tows_RMI.rda")

net_data_copepods = net_data_full %>% 
  filter(taxa_group %in% c("Copepod (small)","Copepod (large)")) %>%
  complete(taxa, season, fill = list(concentration = 0)) %>%
  group_by(taxa, season) %>%
  summarize(mean_concentration = mean(concentration))

Concentrations_By_Season_Species = ggplot() +
  geom_col(data = net_data_copepods, aes(x = taxa, y = mean_concentration, group=season, fill=season), position = "dodge") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(axis.text.x = element_text(
    angle = 60,
    hjust = 1,
    vjust = 1
  ))

ggsave(Concentrations_By_Season_Species, file = paste0(figure_dir,"Concentrations_By_Season_Species_Net.png"), scale = 2)

Concentrations_By_Species_Season = ggplot() +
  geom_col(data = net_data_copepods, aes(x = season, y = mean_concentration, group=taxa, fill=taxa), position = "stack") +
  scale_fill_viridis_d() +
  theme_bw()

ggsave(Concentrations_By_Species_Season, file = paste0(figure_dir,"Concentrations_By_Species_Season_Net.png"), scale = 2)

#####
## ECOMON tow data

# Read the full (EXTREMELY LARGE) dataset into R

ECOMON_data = read_xlsx("C:/Users/Delphine/Box/Zooplankton Protocols and Data/NOAA ecomon Zooplankton data 1977_2021/EcoMon_Plankton_Data_v3_8.xlsx",
                        col_names=T,
                        skip=1) %>% 
  # Constrain to only off the coast of Joisey, remove unnecessary columns
  filter(lon <= -72.5 & lon >= -75 & lat <= 41.5 & lat >= 38.5) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
  select(-c(8:11)) %>%
  # Reformat dates to datetime object
  mutate(Date_Time = paste0(date,' ',time)) %>%
  mutate(Date_Time = as.POSIXlt(Date_Time, format = "%d-%b-%y %H:%M", tz = "UTC")) %>%
  # Arrange by date and time
  arrange(Date_Time) %>%
  # Add seasons column based on month tow was performed
  mutate(Season = case_when(
    month(Date_Time) %in% c(12,1,2) ~ "Winter",
    month(Date_Time) %in% c(3,4,5) ~ "Spring",
    month(Date_Time) %in% c(6,7,8) ~ "Summer",
    month(Date_Time) %in% c(9,10,11) ~ "Fall"
  ))

## Reformatting for plotting

species = colnames(ECOMON_data)[9:283]

ECOMON_concentrations = ECOMON_data %>%
  st_drop_geometry() %>%
  select(9:283) %>%
  t() %>% as.data.frame()

ECOMON_data_expanded = ECOMON_data %>%
  select(c(1:8,284:286)) %>%
  slice(rep(1:n(), each = length(species))) %>%
  mutate(Species = rep(species, times = nrow(ECOMON_data))) %>%
  mutate(data.frame(unlist(ECOMON_concentrations)))

colnames(ECOMON_data_expanded)[13] = "Concentrations"

# Only small/large copepod species
ECOMON_data_expanded = ECOMON_data_expanded %>%
  filter(Species %in%
           c("ctyp_100m3",
             "calfin_100m3",
             "pseudo_100m3",
             "tlong_100m3",
             "cham_100m3",
             "para_100m3",
             "acarspp_100m3",
             "mlucens_100m3",
             "mlucens_100m3",
             "oithspp_100m3",
             "copepoda_100m3",
             "acarlong_100m3",
             "calspp_100m3",
             "temspp_100m3",
             "paraspp_100m3")
         ) %>%
  mutate(Concentrations = Concentrations/100) %>%
  na.omit()

species_names = read.csv("C:/Users/Delphine/Box/Zooplankton Protocols and Data/NOAA ecomon Zooplankton data 1977_2021/3.3/data/0-data/EcoMon_Plankton_Data_v3_7-Taxa and Col names.csv",
                header=F)

for(i in 1:length(species)) {
  ECOMON_data_expanded$Species[ECOMON_data_expanded$Species == species[i]] = species_names$V2[species_names$V1 == species[i]]
}

ECOMON_data_mean = ECOMON_data_expanded %>%
  st_drop_geometry() %>%
  group_by(Species, Season) %>%
  summarize(Mean_Concentration = mean(Concentrations))

save(list = c("ECOMON_concentrations", "ECOMON_data", "ECOMON_data_expanded", "ECOMON_data_mean"),
     file = "C:/Users/Delphine/Box/Zooplankton Protocols and Data/NOAA ecomon Zooplankton data 1977_2021/ECOMON_R_Data.rda")

## Plotting
load("C:/Users/Delphine/Box/Zooplankton Protocols and Data/NOAA ecomon Zooplankton data 1977_2021/ECOMON_R_Data.rda")

Concentrations_By_Season_Species = ggplot() +
  geom_col(data = ECOMON_data_mean, aes(x = Species, y = Mean_Concentration, group=Season, fill=Season), position = "dodge") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(axis.text.x = element_text(
    angle = 60,
    hjust = 1,
    vjust = 1
  ))

ggsave(Concentrations_By_Season_Species, file = paste0(figure_dir,"Concentrations_By_Season_Species.png"), scale = 2)

Concentrations_By_Species_Season = ggplot() +
  geom_col(data = ECOMON_data_mean, aes(x = Season, y = Mean_Concentration, group=Species, fill=Species), position = "stack") +
  scale_fill_viridis_d() +
  theme_bw()

ggsave(Concentrations_By_Species_Season, file = paste0(figure_dir,"Concentrations_By_Species_Season.png"), scale = 2)

#####
## Comparisons between net tows/glider data

glider_dep = choose_directory() %>% substring(., regexpr("ru[0-9]{2}-*", .))
year = substr(glider_dep,6,9)

data_dir = paste0("C:/Users/Delphine/Box/Glider Data/",
                  glider_dep,
                  "/Derived Biomass Data/")

load(paste0(data_dir, "Processed_Abundance_Biomass_Data.rda"))

# Comparing average concentrations over entire deployment to net tow concentrations

data3 %>%
  group_by(Species) %>%
  reframe(Concentration = mean(Abundance))

net_data_full %>% 
  filter(glider_trajectory == glider_dep & taxa_group %in% c("Copepod (small)", "Copepod (large)")) %>%
  group_by(taxa_group) %>%
  reframe(Concentration = mean(concentration))

ECOMON_data_expanded %>%
  filter(Season == "Spring") %>% # currently needs to be changed on a per-deployment basis
  mutate(Species = case_when(
    Species == "Calanus finmarchicus" ~ "Large Copepod",
    Species != "Calanus finmarchicus" ~ "Small Copepod"
  )) %>%
  st_drop_geometry() %>%
  group_by(Species) %>%
reframe(Concentration = mean(Concentrations))

# With correction factor from my MSc work

data %>%
  mutate(Abundance = case_when(
    (Frequency == 455 & Species == "Large Copepod") ~
       10^(((-54 + Sv_mean * 0.44) - -108.3)/10), .default = Abundance
  )) %>%
  group_by(Echo_Num, Species) %>%
  reframe(
    Abundance = sum(Abundance),
    Biomass = sum(Biomass),
    NASC = mean(NASC),
    Ping = mean(c(Ping_S, Ping_E)),
    Depth = mean(Depth_mean, na.rm = T),
    Date = mean(c(Time_M)),
    Lat = mean(c(Lat_M)),
    Long = mean(c(Lon_M)),
    Seafloor_Depth = mean(c(Exclude_below_line_depth_mean + 1),
                          na.rm = T)
  ) %>%
  ungroup() %>%
  group_by(Species) %>%
  reframe(Concentration = mean(Abundance))

# Comparing to specific shelf strata

load("H:/dm1679/Data/Shapefiles/NOAA_NJ_LI_Strata.rda")

temp = st_intersects(data3$geometry, NOAA_NJ_LI_Strata$geometry)
temp[lengths(temp) == 0] = NA
data3$Shelf_Type = temp %>% unlist()
rm(temp)

for(k in 1:nrow(data3)) {
  data3$Shelf_Type[k] = NOAA_NJ_LI_Strata$Shelf_Type[as.numeric(data3$Shelf_Type[k])]
}

data3 %>%
  group_by(Species, Shelf_Type) %>%
  reframe(Concentration = mean(Abundance))

temp = st_intersects(ECOMON_data_expanded$geometry, NOAA_NJ_LI_Strata$geometry)
temp[lengths(temp) == 0] = NA
ECOMON_data_expanded$Shelf_Type = temp %>% unlist()
rm(temp)

for(k in 1:nrow(ECOMON_data_expanded)) {
  ECOMON_data_expanded$Shelf_Type[k] = NOAA_NJ_LI_Strata$Shelf_Type[as.numeric(ECOMON_data_expanded$Shelf_Type[k])]
}

ECOMON_data_expanded %>%
  filter(Season == "Spring") %>% # currently needs to be changed on a per-deployment basis
  mutate(Species = case_when(
    Species == "Calanus finmarchicus" ~ "Large Copepod",
    Species != "Calanus finmarchicus" ~ "Small Copepod"
  )) %>%
  st_drop_geometry() %>%
  group_by(Species) %>%
  reframe(Concentration = mean(Concentrations))
