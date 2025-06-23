rm(list = ls())

library(tidyverse)
library(sf)
library(sfheaders)
library(rnaturalearth)

inshore_IDs = c(3150, 3160, 3170, 3180, 3190, 3200, 3230, 3120, 3130, 3140, 3100, 3090, 3110, 3060, 3070, 3080)
midshelf_IDs = c(1730, 1010, 1690)
offshore_IDs = c(1740, 1750, 1760, 1020, 1030, 1040, 1700, 1710, 1720)

NOAA_strata = read_sf("H:/dm1679/Data/Shapefiles/NES_BOTTOM_TRAWL_STRATA.shp") %>%
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
  st_write("H:/dm1679/Data/Shapefiles/NES_BOTTOM_TRAWL_STRATA_Inshore.shp", append=F)

NOAA_midshelf = NOAA_strata %>%
  filter(FINSTR_ID %in% midshelf_IDs) %>%
  st_union() %>%
  st_sf() %>%
  st_write("H:/dm1679/Data/Shapefiles/NES_BOTTOM_TRAWL_STRATA_Midshelf.shp", append=F)

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
  st_write("H:/dm1679/Data/Shapefiles/NES_BOTTOM_TRAWL_STRATA_Offshore.shp", append=F)

world = ne_countries(scale = "medium")
world = world[world$geounit == "United States of America",]
xlim = c(-75, -72.5)
ylim = c(38.5, 41.5)

NOAA_NJ_LI_Strata = data.frame(Shelf_Type = c("Inshore","Midshelf","Offshore"),
                               rbind(NOAA_inshore, NOAA_midshelf, NOAA_offshore))

st_write(NOAA_NJ_LI_Strata, 
         "H:/dm1679/Data/Shapefiles/NOAA_NJ_LI_Strata.shp",
         append=F)

save(NOAA_NJ_LI_Strata, file = "H:/dm1679/Data/Shapefiles/NOAA_NJ_LI_Strata.rda")

#####
## Plotting with glider tracks

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

spring_2023_gdata$Deployment = "ru39-20230420T1636"
fall_2023_gdata$Deployment = "ru39-20231103T1413"
winter_2024_gdata$Deployment = "ru39-20240215T1646"
spring_2024_gdata$Deployment = "ru39-20240429T1522"
summer_2024_gdata$Deployment = "ru43-20240904T1539"

gdata_full = rbind(spring_2023_gdata, fall_2023_gdata, winter_2024_gdata, spring_2024_gdata, summer_2024_gdata)

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
ggsave(filename = "H:/dm1679/Data/Shapefiles/NOAA_NJ_LI_Strata_Map.png", scale = 1.5)
