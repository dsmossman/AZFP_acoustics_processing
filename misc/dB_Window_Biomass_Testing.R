rm(list = ls())

library(tidyverse)
library(readxl)
library(hms)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(sfheaders)
library(lubridate)
library(marmap)
library(rstatix)
library(R.utils)
library(cowplot)
library(ggpubr)
library(ggnewscale)

glider_dep = "ru39-20240215T1646"

data_dir = paste0("C:/Users/dmossman/Box/Glider Data/",
                  glider_dep,
                  "/Derived Biomass Data/")
figure_dir = paste0("C:/Users/dmossman/Box/Glider Data/", glider_dep, "/Figures/")

bathy = fortify(getNOAA.bathy(-80, -70, 30, 45))
bathy$z[bathy$z >= 0] = 0
bathy$z = abs(bathy$z)
bathy2 = st_as_sf(bathy, coords = c("x", "y"))
st_crs(bathy2) = st_crs(4326)

load(paste0(data_dir, 'RU39_2024_Processed_Abundance_Biomass_Data.rda'))
assign("larvaceans_only",data)

# load(paste0(data_dir, 'RU39_2023_Processed_Abundance_Biomass_Data_Full.rda'))
# assign("all_echoes",data)

load(paste0(data_dir, 'RU39_2024_Processed_Abundance_Biomass_Data_Simplified.rda'))
assign("small_copepods_only",data)

rm(data)

#####

# all_echoes2 = all_echoes %>%
#   group_by(Echo_Num, Species) %>%
#   reframe(
#     Abundance = sum(Abundance),
#     Biomass = sum(Biomass),
#     Depth = mean(Depth_mean, na.rm = T),
#     Date = mean(c(Time_M)),
#     Lat = mean(c(Lat_M)),
#     Long = mean(c(Lon_M)),
#     Seafloor_Depth = mean(c(Exclude_below_line_depth_mean + 1),
#                           na.rm = T)
#   ) %>%
#   filter(Species != "Salp") %>%
#   group_by(Lat, Long, Species) %>%
#   reframe(
#     D_Int_Abundance = sum(Abundance),
#     D_Int_Biomass = sum(Biomass),
#     Seafloor_Depth = mean(Seafloor_Depth),
#     Date = mean(Date))
# 
# all_echoes2$Depth_Type = "Medium"
# for (i in 1:nrow(all_echoes2)) {
#   if (all_echoes2$Seafloor_Depth[i] < 40) {
#     all_echoes2$Depth_Type[i] = "Shallow"
#   } else if (all_echoes2$Seafloor_Depth[i] > 70) {
#     all_echoes2$Depth_Type[i] = "Deep"
#   }
# }
# 
# all_echoes2 %>% 
#   filter(Species != "Larvacean") %>%
#   group_by(Depth_Type) %>% 
#   summarise(sum(D_Int_Biomass))

larvaceans_only2 = larvaceans_only %>%
  group_by(Echo_Num, Species) %>%
  reframe(
    Abundance = sum(Abundance),
    Biomass = sum(Biomass),
    Depth = mean(Depth_mean, na.rm = T),
    Date = mean(c(Time_M)),
    Lat = mean(c(Lat_M)),
    Long = mean(c(Lon_M)),
    # Seafloor_Depth = mean(c(Exclude_below_line_depth_mean + 1),
    #                       na.rm = T)
  ) %>%
  filter(Species != "Salp") %>%
  group_by(Lat, Long, Species) %>%
  reframe(
    D_Int_Abundance = sum(Abundance),
    D_Int_Biomass = sum(Biomass),
    # Seafloor_Depth = mean(Seafloor_Depth),
    Date = mean(Date)) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

larvaceans_only2$Depth_Type = "Medium"
for (i in 1:nrow(larvaceans_only2)) {
  temp = bathy[which.min(st_distance(bathy2, larvaceans_only2[i, 5])),]
  if (temp$z < 40) {
    larvaceans_only2$Depth_Type[i] = "Shallow"
  } else if (temp$z > 70) {
    larvaceans_only2$Depth_Type[i] = "Deep"
  }
}

larvaceans_only2 %>% 
  filter(Species != "Larvacean") %>%
  group_by(Depth_Type) %>% 
  summarise(sum(D_Int_Biomass))

small_copepods_only2 = small_copepods_only %>%
  group_by(Echo_Num, Species) %>%
  reframe(
    Abundance = sum(Abundance),
    Biomass = sum(Biomass),
    Depth = mean(Depth_mean, na.rm = T),
    Date = mean(c(Time_M)),
    Lat = mean(c(Lat_M)),
    Long = mean(c(Lon_M)),
    # Seafloor_Depth = mean(c(Exclude_below_line_depth_mean + 1),
    #                       na.rm = T)
  ) %>%
  filter(Species != "Salp") %>%
  group_by(Lat, Long, Species) %>%
  reframe(
    D_Int_Abundance = sum(Abundance),
    D_Int_Biomass = sum(Biomass),
    # Seafloor_Depth = mean(Seafloor_Depth),
    Date = mean(Date)) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

small_copepods_only2$Depth_Type = "Medium"
for (i in 1:nrow(small_copepods_only2)) {
  temp = bathy[which.min(st_distance(bathy2, small_copepods_only2[i, 5])),]
  if (temp$z < 40) {
    small_copepods_only2$Depth_Type[i] = "Shallow"
  } else if (temp$z > 70) {
    small_copepods_only2$Depth_Type[i] = "Deep"
  }
}

small_copepods_only2 %>% 
  filter(Species != "Larvacean") %>%
  group_by(Depth_Type) %>% 
  summarise(sum(D_Int_Biomass))

#####

load(paste0(data_dir, 'RU39_2023_Processed_Geometry_Data.rda'))
assign("larvaceans_only3", data4)

# load(paste0(data_dir, 'RU39_2023_Processed_Geometry_Data_Full.rda'))
# assign("all_echoes3", data4)

load(paste0(data_dir, 'RU39_2023_Processed_Geometry_Data_Simplified.rda'))
assign("small_copepods_only3", data4)

rm(data4)

# all_echoes3 %>% 
#   filter(Species != "Larvacean") %>%
#   group_by(Depth_Type) %>% 
#   summarise(sum(D_Int_Biomass))

larvaceans_only3 %>% 
  filter(Species != "Larvacean") %>%
  group_by(Depth_Type) %>% 
  summarise(sum(D_Int_Biomass))

small_copepods_only3 %>% 
  filter(Species != "Larvacean") %>%
  group_by(Depth_Type) %>% 
  summarise(sum(D_Int_Biomass))

#####

## Boxplots of original and simplified biomasses (in log10 space) faceted by 
# depth type (shallow/mid/deep)

larvaceans_only4 = larvaceans_only2 %>%
  filter(Species != "Larvacean") %>%
  group_by(Date, Depth_Type) %>%
  summarise(Total_Biomass = sum(D_Int_Biomass),Total_Abundance = sum(D_Int_Abundance)) %>%
  mutate(Group = "Small Copepod/\nLarvacean Overlap")

small_copepods_only4 = small_copepods_only2 %>%
  filter(Species != "Larvacean") %>%
  group_by(Date, Depth_Type) %>%
  summarise(Total_Biomass = sum(D_Int_Biomass),Total_Abundance = sum(D_Int_Abundance)) %>%
  mutate(Group = "Small Copepods Only")

comparison_data = rbind(larvaceans_only4, small_copepods_only4)

ggplot() + 
  geom_boxplot(data = comparison_data, aes(x = Group, y = log10(Total_Biomass), fill = Group)) +
  facet_grid(~factor(Depth_Type, levels = c("Shallow","Medium","Deep"),
                     labels = c("Shallow (<40 m)","Medium (40-70 m)","Deep (>70 m)"))) +
  labs(title = "Winter 2024 dB Window Procedure Comparison",
       x = NULL,
       y = "Log10 of Water Column Biomass\n(excluding pure Larvacean echoes)")

ggsave(filename = paste0(figure_dir, "dB_Window_Procedure_Comparison.png"), scale = 2)
