rm(list = ls())

library(tidyverse)
library(sf)
library(sfheaders)
library(rnaturalearth)

inshore_IDs = c(3150, 3160, 3170, 3180, 3190, 3200, 3120, 3130, 3140, 3100, 3090, 3110, 3060, 3070, 3080)
midshelf_IDs = c(1730, 1010)
offshore_IDs = c(1740, 1750, 1760, 1020, 1030, 1040)

NOAA_strata = read_sf("H:/dm1679/Data/Shapefiles/NES_BOTTOM_TRAWL_STRATA.shp") %>%
  st_transform(crs = 4326)

NOAA_inshore = NOAA_strata %>%
  filter(FINSTR_ID %in% inshore_IDs) %>%
  st_make_valid() %>%
#  st_buffer(0.5) %>%
  st_union() %>%
  st_sf() %>%
  st_write("H:/dm1679/Data/Shapefiles/NES_BOTTOM_TRAWL_STRATA_Inshore.shp")

NOAA_midshelf = NOAA_strata %>%
  filter(FINSTR_ID %in% midshelf_IDs) %>%
  st_union() %>%
  st_sf()

NOAA_offshore = NOAA_strata %>%
  filter(FINSTR_ID %in% offshore_IDs) %>%
  st_union() %>%
  st_sf()

world = ne_countries(scale = "medium")
world = world[world$geounit == "United States of America",]
xlim = c(-75, -72.5)
ylim = c(38.5, 41.5)

ggplot() + 
  geom_sf(data = world, fill = "gray15") +
  geom_sf(data = NOAA_inshore, aes(fill = "Inshore")) +
  geom_sf(data = NOAA_midshelf, aes(fill = "Midshelf")) +
  geom_sf(data = NOAA_offshore, aes(fill = "Offshore")) +
  scale_fill_viridis_d(begin = 0, end=0.8) +
  coord_sf(xlim = xlim,
           ylim = ylim,
           crs = st_crs(world)) +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  labs(fill = "NOAA Strata\nAssignment")
ggsave(filename = "H:/dm1679/Data/Shapefiles/NOAA_NJ_LI_Strata_Map.png", scale = 1.5)

NOAA_NJ_LI_Strata = data.frame(Shelf_Type = c("Inshore","Midshelf","Offshore"),
                               rbind(NOAA_inshore, NOAA_midshelf, NOAA_offshore))

st_write(NOAA_NJ_LI_Strata, 
         "NOAA_NJ_LI_Strata.shp",
         append=F)

save(NOAA_NJ_LI_Strata, file = "H:/dm1679/Data/Shapefiles/NOAA_NJ_LI_Strata.rda")
