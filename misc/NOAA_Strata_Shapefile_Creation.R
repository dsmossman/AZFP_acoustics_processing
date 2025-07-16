# Author: Delphine Mossman
# Date Created: 22 May 2025
# Date Last Modified: 11 July 2025

#####
rm(list = ls())

library(tidyverse)
library(sf)
library(sfheaders)
library(rnaturalearth)

inshore_IDs = c(3150, 3160, 3170, 3180, 3190, 3200, 3230, 3120, 3130, 3140, 3100, 3090, 3110, 3060, 3070, 3080)
midshelf_IDs = c(1730, 1010, 1690)
offshore_IDs = c(1740, 1750, 1760, 1020, 1030, 1040, 1700, 1710, 1720)

NOAA_strata = read_sf("C:/Users/Delphine/Box/Shapefiles/NES_BOTTOM_TRAWL_STRATA.shp") %>%
  st_transform(crs = 4326) %>%
  filter(FINSTR_ID != 0)

# ggplot(data = NOAA_strata) + 
#   geom_sf(aes(fill = as.factor(FINSTR_ID))) + 
#   geom_sf_text(aes(label=FINSTR_ID)) +
#   coord_sf(xlim = xlim,
#            ylim = ylim,
#            crs = st_crs(world))

NOAA_inshore = NOAA_strata %>%
  filter(FINSTR_ID %in% inshore_IDs) %>%
  st_make_valid() %>%
#  st_buffer(0.5) %>%
  st_union() %>%
  st_sf() %>%
  st_write("C:/Users/Delphine/Box/Shapefiles/NES_BOTTOM_TRAWL_STRATA_Inshore.shp", append=F)

NOAA_midshelf = NOAA_strata %>%
  filter(FINSTR_ID %in% midshelf_IDs) %>%
  st_union() %>%
  st_sf() %>%
  st_write("C:/Users/Delphine/Box/Shapefiles/NES_BOTTOM_TRAWL_STRATA_Midshelf.shp", append=F)

offshore_extra_poly = st_polygon(
  list(
    cbind(
      c(-72.85,-72.85,-73.2,-73.2,-72.85), 
      c(38.86,38.75,38.75,38.86,38.86))
  )
) %>%
  st_sfc(crs = 4326)

NOAA_offshore = NOAA_strata %>%
  filter(FINSTR_ID %in% offshore_IDs) %>%
  st_union() %>%
  st_union(offshore_extra_poly) %>%
  st_sf() %>%
  st_write("C:/Users/Delphine/Box/Shapefiles/NES_BOTTOM_TRAWL_STRATA_Offshore.shp", append=F)

world = ne_countries(scale = "medium")
world = world[world$geounit == "United States of America",]
xlim = c(-75, -72.5)
ylim = c(38.5, 41.5)

NOAA_NJ_LI_Strata = data.frame(Shelf_Type = c("Inshore","Midshelf","Offshore"),
                               rbind(NOAA_inshore, NOAA_midshelf, NOAA_offshore))

st_write(NOAA_NJ_LI_Strata, 
         "C:/Users/Delphine/Box/Shapefiles/NOAA_NJ_LI_Strata.shp",
         append=F)

save(NOAA_NJ_LI_Strata, file = "C:/Users/Delphine/Box/Shapefiles/NOAA_NJ_LI_Strata.rda")

#####
## Plotting with zooplankton glider tracks

load("C:/Users/Delphine/Box/Shapefiles/NOAA_NJ_LI_Strata.rda")

# home_dir = "H:/dm1679/Data/"
home_dir = "C:/Users/Delphine/Box/"

load(paste0(home_dir,"Glider Data/ru39-20230420T1636/Derived Biomass Data/Glider_Data.rda"))
assign("spring_2023_gdata", gdata[,1:3])
rm(gdata)

load(paste0(home_dir,"Glider Data/ru39-20231103T1413/Derived Biomass Data/Glider_Data.rda"))
assign("fall_2023_gdata", gdata[,1:3])
rm(gdata)

load(paste0(home_dir,"Glider Data/ru39-20240215T1646/Derived Biomass Data/Glider_Data.rda"))
assign("winter_2024_gdata", gdata[,1:3])
rm(gdata)

load(paste0(home_dir,"Glider Data/ru39-20240429T1522/Derived Biomass Data/Glider_Data.rda"))
assign("spring_2024_gdata", gdata[,1:3])
rm(gdata)

load(paste0(home_dir,"Glider Data/ru43-20240904T1539/Derived Biomass Data/Glider_Data.rda"))
assign("summer_2024_gdata", gdata[,1:3])
rm(g_coords, g_SA_intersect, gdata, closest)

spring_2023_gdata$Deployment = "Spring 2023"
fall_2023_gdata$Deployment = "Fall 2023"
winter_2024_gdata$Deployment = "Winter 2024"
spring_2024_gdata$Deployment = "Spring 2024"
summer_2024_gdata$Deployment = "Early Fall 2024"

gdata_full = rbind(spring_2023_gdata, fall_2023_gdata, winter_2024_gdata, spring_2024_gdata, summer_2024_gdata)
gdata_full$Deployment = factor(gdata_full$Deployment, levels = c("Spring 2023", "Fall 2023", "Winter 2024", "Spring 2024", "Early Fall 2024"), ordered = T)

ggplot() + 
  geom_sf(data = world, fill = "gray15") +
  geom_sf(data = NOAA_inshore, aes(fill = "Inshore")) +
  geom_sf(data = NOAA_midshelf, aes(fill = "Midshelf")) +
  geom_sf(data = NOAA_offshore, aes(fill = "Offshore")) +
  scale_fill_viridis_d(begin = 0, end=0.8) +
  geom_path(data = gdata_full, aes(x = longitude, y = latitude, linetype = Deployment, color = Deployment), linewidth = 1) +
  scale_color_viridis_d(option = "B", end = 0.8) +
  coord_sf(xlim = xlim,
           ylim = ylim,
           crs = st_crs(world)) +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  labs(fill = "NOAA Strata\nAssignment")
ggsave(filename = "H:/dm1679/Data/Shapefiles/NOAA_NJ_LI_Strata_Map_Zooplankton.png", scale = 1.5)

#####
## Plotting with fish glider tracks

load("C:/Users/Delphine/Box/Shapefiles/NOAA_NJ_LI_Strata.rda")

# home_dir = "H:/dm1679/Data/"
home_dir = "C:/Users/Delphine/Box/"

load(paste0(home_dir,"Glider Data/ru39-20230817T1520/Derived Biomass Data/Glider_Data.rda"))
assign("summer_2023_gdata", gdata[,1:3])
rm(gdata)

load(paste0(home_dir,"Glider Data/ru39-20240723T1442/Derived Biomass Data/Glider_Data.rda"))
assign("summer_2024_gdata", gdata[,1:3])
rm(gdata)

load(paste0(home_dir,"Glider Data/ru39-20241021T1717/Derived Biomass Data/Glider_Data.rda"))
assign("fall_2024_gdata", gdata[,1:3])
rm(gdata)

load(paste0(home_dir,"Glider Data/ru39-20250226T1700/Derived Biomass Data/Glider_Data.rda"))
assign("winter_2025_gdata", gdata[,1:3])
rm(gdata)

# load(paste0(home_dir,"Glider Data/ru43-20250423T1535/Derived Biomass Data/Glider_Data.rda"))
# assign("spring_2025_gdata", gdata[,1:3])
rm(g_coords, g_SA_intersect, gdata, closest)

summer_2023_gdata$Deployment = "Summer 2023"
summer_2024_gdata$Deployment = "Summer 2024"
fall_2024_gdata$Deployment = "Fall 2024"
winter_2025_gdata$Deployment = "Winter 2025"
# spring_2025_gdata$Deployment = "Spring 2025"

gdata_full = rbind(summer_2023_gdata, summer_2024_gdata, fall_2024_gdata, winter_2025_gdata)
gdata_full$Deployment = factor(gdata_full$Deployment, levels = c("Summer 2023", "Summer 2024", "Fall 2024", "Winter 2025"), ordered = T)

ggplot() + 
  geom_sf(data = world, fill = "gray15") +
  geom_sf(data = NOAA_inshore, aes(fill = "Inshore")) +
  geom_sf(data = NOAA_midshelf, aes(fill = "Midshelf")) +
  geom_sf(data = NOAA_offshore, aes(fill = "Offshore")) +
  scale_fill_viridis_d(begin = 0, end=0.8) +
  geom_path(data = gdata_full, aes(x = longitude, y = latitude, linetype = Deployment, color = Deployment), linewidth = 1) +
  scale_color_viridis_d(option = "F", begin = 0.1) +
  coord_sf(xlim = xlim,
           ylim = ylim,
           crs = st_crs(world)) +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  labs(fill = "NOAA Strata\nAssignment")
ggsave(filename = "H:/dm1679/Data/Shapefiles/NOAA_NJ_LI_Strata_Map_Fish.png", scale = 1.5)

#####

## Percentage overlap of wind farms and strata
# https://gis.stackexchange.com/questions/362466/calculate-percentage-overlap-of-2-sets-of-polygons-in-r

load("C:/Users/Delphine/Box/Shapefiles/NOAA_NJ_LI_Strata.rda")

Atlantic_Shores_Study_Areas = read_sf("C:/Users/Delphine/Box/Shapefiles/Atlantic Shores Offshore Wind.shp") %>% 
  st_transform(crs = st_crs(4326))

BOEM_Study_Areas = read_sf(
  "C:/Users/Delphine/Box/COOL/Offshore Wind/wind energy shapefiles/BOEM_shp_kmls/shapefiles/wind_leases/BOEM_Wind_Lease_Outlines_06_06_2024.shp"
) %>%
  st_transform(crs = st_crs(4326)) %>%
  slice(25:28)

BOEM_Study_Areas = st_union(st_union(BOEM_Study_Areas[1,], BOEM_Study_Areas[2,]),
                            st_union(BOEM_Study_Areas[3,], BOEM_Study_Areas[4,]))

ggplot() + 
  geom_sf(data = world, fill = "gray15") +
  geom_sf(data = NOAA_inshore, aes(fill = "Inshore")) +
  geom_sf(data = NOAA_midshelf, aes(fill = "Midshelf")) +
  geom_sf(data = NOAA_offshore, aes(fill = "Offshore")) +
  scale_fill_viridis_d(begin = 0, end=0.8) +
  geom_sf(data = Atlantic_Shores_Study_Areas, alpha = 0.8, fill = "red") +
  geom_sf(data = BOEM_Study_Areas, alpha = 0.8, fill = "blue") +
  coord_sf(xlim = xlim,
           ylim = ylim,
           crs = st_crs(world)) +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  labs(fill = "NOAA Strata\nAssignment")
  
Study_Area_Final = st_union(Atlantic_Shores_Study_Areas, BOEM_Study_Areas)

intersection = rbind(st_intersection(BOEM_Study_Areas, NOAA_inshore),
                          st_intersection(BOEM_Study_Areas, NOAA_midshelf),
                          st_intersection(BOEM_Study_Areas, NOAA_offshore)) %>%
  mutate(intersect_area = st_area(.),
         percentage_overlap = intersect_area/st_area(BOEM_Study_Areas))
