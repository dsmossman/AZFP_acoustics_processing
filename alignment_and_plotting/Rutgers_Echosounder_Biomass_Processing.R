# Author: Delphine Mossman
# Date Created: 17 July 2023
# Date Last Modified: 23 June 2025

# 1. Load libraries and assign some initial variables
# 2. Read in the acoustically-derived abundance data into a single dataframe and do some reformatting
# 3. Create dataframes for time of day, bathymetry, and marine mammal detections, and a shapefile for the wind farm lease areas
# 4. Read and reformat in glider data
# 5. Create some new dataframes for later plotting steps, including depth-integration
# 6. Save all these dataframes into three separate RDA files


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

sourceDirectory(
  "H:/dm1679/Code/R_Functions",
  modifiedOnly = F
)

glider_dep = choose_directory() %>% substring(., regexpr("ru[0-9]{2}-*", .))
year = substr(glider_dep,6,9)

data_dir = paste0("C:/Users/Delphine/Box/Glider Data/",
                  glider_dep,
                  "/Derived Biomass Data/")

world = ne_countries(scale = "medium")
world = world[world$geounit == "United States of America",]

## Load what you have already if you need it

load(paste0(data_dir, "Glider_Data.rda"))
load(paste0(data_dir, "Peripheral_Data.rda"))
# load(paste0(data_dir, "Processed_Abundance_Biomass_Data.rda"))

#####

## Acoustic estimates data

# Read in all the data and make a big dataframe
data_filenames = list.files(data_dir, pattern = "*_Biomass_Data.csv$",full.names = T)

data_ldf = lapply(data_filenames, function(x) read_csv(x, show_col_types = F))

data = data.frame()

for(j in 1:length(data_ldf)) {
  if(nrow(data_ldf[[j]]) == 0)
    next
  else
    data = rbind(data,data_ldf[[j]])
}

# Assigning echo number
if(glider_dep == "ru39-20230817T1520") {
  data = data %>% arrange(Frequency) %>% filter(Exclude_below_line_depth_mean >=0)
  data$Echo_Num = NA
  
  data$Echo_Num[data$Frequency == 38] = seq(1, nrow(data[data$Frequency == 38,]))
  data$Echo_Num[data$Frequency == 125] = seq(1, nrow(data[data$Frequency == 125,]))
  data$Echo_Num[data$Frequency == 200] = seq(1, nrow(data[data$Frequency == 200,]))
  
} else {
  
  data = data %>% arrange(Frequency) %>% filter(Exclude_below_line_depth_mean >=0)
  
  # data$Echo_Num = rep(1:(nrow(data)/4),times=4)
  data$Echo_Num = rep(1:(nrow(data)/3),times=3)
}

# Reformatting timestamps and converting to eastern time zone
data$Time_M = paste0(data$Date_M,' ',data$Time_M)
data$Time_M = as.POSIXct(data$Time_M, format = "%d-%b-%Y %H:%M:%S", tz = "UTC")
data$Time_M = as.POSIXct(format(data$Time_M, tz="America/Detroit", usetz=T))

data = data %>% arrange(Time_M, Echo_Num)

#####
## Day, bathymetry, study area shapefiles, marine mammal detections import/formatting

start_date = as.POSIXlt(paste0(data$Date_M[1], ' ', "00:00:00"),
                        format = "%d-%b-%Y %H:%M:%S",
                        tz = "EST")
end_date = as.POSIXlt(paste0(data$Date_M[nrow(data)], ' ', "00:00:00"),
                      format = "%d-%b-%Y %H:%M:%S",
                      tz = "EST")

day_data = data.frame(Date = seq.POSIXt(start_date, end_date, by = "min"))

day_data$TOD = NA

for (i in 1:nrow(day_data)) {
  if (as_hms(day_data$Date[i]) > hms(0, 0, 7) &
      as_hms(day_data$Date[i]) < hms(0, 0, 19)) {
    day_data$TOD[i] = "Day"
  } else {
    day_data$TOD[i] = "Night"
  }
}

bathy = fortify(getNOAA.bathy(-80, -70, 30, 45))
bathy$z[bathy$z >= 0] = 0
bathy$z = abs(bathy$z)
bathy2 = st_as_sf(bathy, coords = c("x", "y"))
st_crs(bathy2) = st_crs(4326)

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

# Sometimes it's year-month, sometimes it's month-year...
robots4whales_URL = paste0("http://dcs.whoi.edu/rutgers", substr(glider_dep,8,11), "/rutgers", substr(glider_dep,8,11), "_ru40_html/ptracks/manual_analysis.csv")

paired_detection = download.file(url = robots4whales_URL, destfile = paste0("C:/Users/Delphine/Box/Glider Data/DMON/ru40-", substr(glider_dep, 6, 13), "-dmon.csv"))

dmon_files = list.files(
  "C:/Users/Delphine/Box/Glider Data/DMON/",
  pattern = "^ru[0-9]{2}.*.csv",
  full.names = T
)

dmon_ldf = lapply(dmon_files, function(x)
  read_csv(x, col_types = "c"))

marine_mammal_detections = data.frame()

for (j in 1:length(dmon_ldf)) {
  if (nrow(dmon_ldf[[j]]) == 0)
    next
  else
    marine_mammal_detections = rbind(marine_mammal_detections, dmon_ldf[[j]])
}

marine_mammal_detections$datetime_utc = as_datetime(paste(
  paste(
    substr(marine_mammal_detections$datetime_utc, 1, 4),
    substr(marine_mammal_detections$datetime_utc, 5, 6),
    substr(marine_mammal_detections$datetime_utc, 7, 8),
    sep = "-"
  ),
  paste(
    substr(marine_mammal_detections$datetime_utc, 9, 10),
    substr(marine_mammal_detections$datetime_utc, 11, 12),
    substr(marine_mammal_detections$datetime_utc, 13, 14),
    sep = ":"
  )
))

marine_mammal_detections = marine_mammal_detections %>% filter(sei == "present" |
                                                                 fin == "present" |
                                                                 right == "present" |
                                                                 humpback == "present") %>%
  arrange(datetime_utc) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(
    Whale_Species = case_when(
      sei == "present" ~ "Sei",
      fin == "present" ~ "Fin",
      right == "present" ~ "North Atlantic Right",
      humpback == "present" ~ "Humpback"
    )
  )

## Save the "peripheral" data
fname = paste0(data_dir, "Peripheral_Data.rda")
save(list=c("bathy","bathy2","day_data","Study_Area_Final","marine_mammal_detections"),file=fname)

#####
## Glider data import/formatting
gdata = read_csv(
  paste0(
    "C:/Users/Delphine/Box/Glider Data/",
    glider_dep,
    "/",
    glider_dep,
    "-profile-sci-delayed.csv"
  ),
  show_col_types = F
)[-1,]

gdata$time = gsub('T', ' ', gdata$time)
gdata$time = gsub('Z', '', gdata$time)
gdata$time = as.POSIXct(gdata$time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
gdata$time = format(gdata$time, tz="America/Detroit")
gdata$time = as.POSIXct(gdata$time, format = "%Y-%m-%d %H:%M")

gdata = gdata %>% 
  group_by(time) %>% 
  mutate_if(is.character, as.numeric) %>%
  summarize(across(everything(), ~mean(., na.rm=T))) %>%
  arrange(time)

g_coords = gdata[,c("time","latitude","longitude")] %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  arrange(time)

## Nearshore/mid-shelf/offshore based on NOAA strata polygons

load("H:/dm1679/Data/Shapefiles/NOAA_NJ_LI_Strata.rda")

temp = st_intersects(g_coords$geometry, NOAA_NJ_LI_Strata$geometry)
temp[lengths(temp) == 0] = NA
g_coords$Shelf_Type = temp %>% unlist()
rm(temp)

for(k in 1:nrow(g_coords)) {
  g_coords$Shelf_Type[k] = NOAA_NJ_LI_Strata$Shelf_Type[as.numeric(g_coords$Shelf_Type[k])]
}

# Seafloor depth along glider track
closest = data.frame()

for (i in 1:nrow(g_coords)) {
  depth = bathy2$z[which.min(st_distance(bathy2, g_coords$geometry[i]))]
  
  time = as.character(g_coords$time[i])
  
  closest[i, 1:2] = cbind(depth, time)
}
names(closest) = c("Depth", "Time")
closest$Depth = as.numeric(closest$Depth)
closest$Time = as.POSIXct(closest$Time, format = "%Y-%m-%d %H:%M:%S")
closest = closest %>% arrange(Time)

# Depth type assignments
g_coords$Depth_Type = "Medium depth"

for (i in 1:nrow(g_coords)) {
  if (closest$Depth[i] < 40) {
    g_coords$Depth_Type[i] = "Shallow"
  } else if (closest$Depth[i] > 70) {
    g_coords$Depth_Type[i] = "Deep"
  }
}

g_SA_intersect = st_intersection(Study_Area_Final, g_coords)

gdata$Wind_Farm = as.character(t(st_intersects(g_coords, Study_Area_Final, sparse = FALSE)))

## Save the glider data
fname = paste0(data_dir,"Glider_Data.rda")
save(list=c("gdata", "g_coords", "g_SA_intersect", "closest"), file=fname)

#####
## Abundance and biomass by date

data2 = data %>%
  #  filter(!Species %in% c("Gelatinous Zooplankton","Empty Cell")) %>%
  group_by(as.factor(as.Date(Time_M)), Species) %>%
  # group_by(as.factor(as.Date(Date_S,format="%d-%b-%Y")), Species) %>%
  reframe(Abundance = log10(sum(Abundance)), Biomass = log10(sum(Biomass)))
names(data2)[1] = "Date"
data2 = data2[which(data2$Abundance != -Inf),]

## Presence/Absence, abundance, and biomass by depth and time

data3 = data %>%
  #  filter(!Species %in% c("Gelatinous Zooplankton","Empty Cell")) %>%
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
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

## Align glider variables and zooplankton data

data3$pH = NA
data3$salinity = NA
data3$chlorophyll_a = NA
data3$temperature = NA
for (i in 1:nrow(data3)) {
  idx = which.min(st_distance(data3$geometry[i], g_coords$geometry))
  
  data3$pH[i] = gdata$pH[idx]
  data3$salinity[i] = gdata$salinity[idx]
  data3$chlorophyll_a[i] = gdata$chlorophyll_a[idx]
  data3$temperature[i] = gdata$temperature[idx]
  
}

## Depth-integrated abundance and biomass over glider track

# Depth integration procedure
# Vertical bins are 1 m
# If I have 1 m depth bins, then each concentration is multiplied by 1 to go from
# m^-3 to m^-2
# Then summed
# The actual numbers don't change due to the multiplication by 1; it would be
# different if my depth bins were larger or smaller

data4 = data3 %>%
  arrange(Date) %>%
  #filter(Species != "Gelatinous Zooplankton") %>%
  mutate(Long = unlist(map(geometry, 1)),
         Lat = unlist(map(geometry, 2))) %>%
  group_by(Lat, Long, Species) %>%
  reframe(
    D_Int_Abundance = sum(Abundance),
    D_Int_Biomass = sum(Biomass),
    Seafloor_Depth = mean(Seafloor_Depth),
    Date = mean(Date)#,
    #pH = mean(pH),
    #salinity = mean(salinity),
    #chlorophyll_a = mean(chlorophyll_a),
    #temperature = mean(temperature)
  ) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  arrange(Date) %>%
  mutate(
    dist = c(0, st_distance(geometry[1:(nrow(.) - 1)], geometry[2:nrow(.)], by_element = T)),
    long = unlist(map(geometry, 1)),
    lat = unlist(map(geometry, 2))
  )

# Need to average them into larger horizontal bins

dist = 6000 # horizontal distance in meters
# 1 cell/0.1 nau mi * 1 nau mi/1852 m = num cells/m * 6000 m distance = num cells
num_cells = (1/0.1) * (1/1852) * dist # number of 0.1 nau mi cells in new horizontal dist

data4$cumulative = 0
data4$group = 1
j = 1

for (i in 2:nrow(data4)) {
  data4$cumulative[i] = data4$cumulative[i - 1] + data4$dist[i]
  data4$group[i] = j
  if (data4$cumulative[i] > dist) {
    data4$cumulative[i] = 0
    data4$group[i] = j + 1
    j = j + 1
  }
}

data4 = data4 %>%
  group_by(group, Species) %>%
  summarize(D_Int_Abundance = sum(D_Int_Abundance)/num_cells, # mean doesn't work because it doesn't account for empty cells
            D_Int_Biomass = sum(D_Int_Biomass)/num_cells,
            across(Seafloor_Depth:lat, ~mean(., na.rm=T))) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

data3$Wind_Farm = as.character(t(st_intersects(Study_Area_Final, data3, sparse = FALSE)))
data4$Wind_Farm = as.character(t(st_intersects(Study_Area_Final, data4, sparse = FALSE)))


## Bathymetry stuff

data3$Depth_Type = "Medium depth"

for (i in 1:nrow(data3)) {
  if (data3$Seafloor_Depth[i] < 40) {
    data3$Depth_Type[i] = "Shallow"
  } else if (data3$Seafloor_Depth[i] > 70) {
    data3$Depth_Type[i] = "Deep"
  }
}

data4$Depth_Type = "Medium depth"

for (i in 1:nrow(data4)) {
  if (data4$Seafloor_Depth[i] < 40) {
    data4$Depth_Type[i] = "Shallow"
  } else if (data4$Seafloor_Depth[i] > 70) {
    data4$Depth_Type[i] = "Deep"
  }
}

## Nearshore/mid-shelf/offshore based on NOAA strata polygons

load("H:/dm1679/Data/Shapefiles/NOAA_NJ_LI_Strata.rda")

temp = st_intersects(data3$geometry, NOAA_NJ_LI_Strata$geometry)
temp[lengths(temp) == 0] = NA
data3$Shelf_Type = temp %>% unlist()
rm(temp)

for(k in 1:nrow(data3)) {
  data3$Shelf_Type[k] = NOAA_NJ_LI_Strata$Shelf_Type[as.numeric(data3$Shelf_Type[k])]
}

temp = st_intersects(data4$geometry, NOAA_NJ_LI_Strata$geometry)
temp[lengths(temp) == 0] = NA
data4$Shelf_Type = temp %>% unlist()
rm(temp)

for(k in 1:nrow(data4)) {
  data4$Shelf_Type[k] = NOAA_NJ_LI_Strata$Shelf_Type[as.numeric(data4$Shelf_Type[k])]
}

## Save the abundance/biomass data
fname = paste0(data_dir, "Processed_Abundance_Biomass_Data.rda")
save(list=c("data_ldf", "data_filenames","data","data2","data3","data4"), file = fname)

#####
## Some misc tests
# data4 %>% reframe(Abundance = D_Int_Abundance,
#                   Shelf_Type = Shelf_Type,
#                   Species = Species) %>%
#   group_by(Species) %>%
#   levene_test(Abundance ~ Shelf_Type)
# data4 %>%
#   reframe(Abundance = D_Int_Abundance,
#           Shelf_Type = Shelf_Type,
#           Species = Species) %>%
#   group_by(Species) %>%
#   anova_test(Abundance ~ Shelf_Type)
# 
# data.frame(data4) %>% group_by(Species) %>% levene_test(D_Int_Abundance ~ Wind_Farm)
# data.frame(data4) %>% group_by(Species) %>% t_test(D_Int_Abundance ~ Wind_Farm, var.equal = F)
# 
# ## Seafloor depth correlation with biomass/abundance
# 
# data.frame(data4) %>%
#   mutate(D_Int_Abundance = log10(D_Int_Abundance)) %>%
#   group_by(Species) %>%
#   levene_test(D_Int_Abundance ~ Depth_Type)
# data.frame(data4) %>%
#   mutate(D_Int_Abundance = log10(D_Int_Abundance)) %>%
#   group_by(Species) %>%
#   anova_test(D_Int_Abundance ~ Depth_Type)
# 
# models = data3 %>%
#   mutate(Abundance = log10(Abundance)) %>%
#   filter(Abundance > 0) %>%
#   group_by(Species) %>%
#   do(model = summary(lm(Seafloor_Depth ~ Abundance, data = .)))
# print(models[[2]][1:nrow(models)])
# 
# ggplot(data = data3[data3$Species != "Gelatinous Zooplankton", ],
#        aes(
#          x = Seafloor_Depth,
#          y =
#            log10(Abundance),
#          group =
#            Species,
#          color = Species
#        )) +
#   geom_point() +
#   geom_smooth(method = "lm", se = F) +
#   stat_regline_equation(aes(label = after_stat(rr.label)),
#                         show.legend = F)
# 
# d_int_models = data4 %>%
#   filter(D_Int_Abundance > 0) %>%
#   mutate(D_Int_Abundance = log10(D_Int_Abundance)) %>%
#   group_by(Species) %>%
#   do(model = summary(lm(Seafloor_Depth ~ D_Int_Abundance, data = .)))
# print(d_int_models[[2]][1:nrow(d_int_models)])
# 
# ggplot(data = data4,
#        aes(
#          x = Seafloor_Depth,
#          y = log10(D_Int_Abundance),
#          group = Species,
#          color = Species
#        )) +
#   geom_point() +
#   geom_smooth(method = "lm", se = F) +
#   stat_regline_equation(aes(label = after_stat(rr.label)),
#                         show.legend = F)
# 
# # Large copepod/total copepod biomass for fall and winter deps
# 
# data %>%
#   group_by(Echo_Num, Species) %>%
#   reframe(
#     Abundance = sum(Abundance),
#     Biomass = sum(Biomass)
#   ) %>%
#   group_by(Species) %>%
#   reframe(sum(Abundance), sum(Biomass))
