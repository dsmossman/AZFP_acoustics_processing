# Author: Delphine Mossman
# Date Created: 17 July 2023
# Date Last Modified: 30 July 2025

# 1. Load libraries and assign some initial variables
# 2. Load glider and peripheral data from the other processing R file
# 3. Read in the acoustically-derived abundance data that are gridded in 1x1 meter cells into a single dataframe and do some reformatting
# 4. Create some new dataframes for later plotting steps, including depth-integration
# 5. Save the 1x1 gridded data into a new .RDA file


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
                  "/Derived Biomass Data 1x1 Grid/")

world = ne_countries(scale = "medium")
world = world[world$geounit == "United States of America",]

## Load what you have already

load(paste0("C:/Users/Delphine/Box/Glider Data/",
                  glider_dep,
                  "/Derived Biomass Data/Glider_Data.rda"))
load(paste0("C:/Users/Delphine/Box/Glider Data/",
            glider_dep,
            "/Derived Biomass Data/Peripheral_Data.rda"))
# load(paste0(data_dir, "Processed_Abundance_Biomass_Data_1x1_Grid.rda"))

#####

## Acoustic estimates data

# Read in all the data and make a big dataframe
# data_filenames = list.files(data_dir, pattern = "(^RMI) *", full.names = T)
data_filenames = list.files(data_dir, pattern = "*(Biomass_Data)*.csv", full.names = T)

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

# Locating each dive and assigning a number

start_dive = c(1, which(diff(data$Exclude_above_line_depth_mean) < 0)+1)
dive_index = 1
data$Dive = 0

# This assigns an index for all but the very last dive
for(i in 1:(length(start_dive) - 1)) {
  data$Dive[start_dive[i]:(start_dive[i+1]-1)] = dive_index
  dive_index = dive_index + 1
}

# This assigns the index for the last dive of the deployment
data$Dive[start_dive[length(start_dive)]:nrow(data)] = dive_index

#####
## For summer and fall fish, assuming all swimbladder echoes are menhaden and
# all swimbladderless echoes are longfin squid

if (month(data$Time_M[1]) %in% c(6, 7, 8, 9, 10, 11)) {
  squid_L = 6.2 #mean mantle length in cm, from Loranger et al. 2022
  squid_W = exp(-1.04605 + 2.05558 * log(squid_L)) #squid mean weight in g based on L, from Wigley 2003
  
  squid_TS = 20 * log10(squid_L) - 58.6 #intercept from the fisheries acoustics textbook, 120 kHz
  squid_obs = 10^(squid_TS / 10)
  
  # all values/equations derived from Lucca and Warren 2019
  menh_L = 25.5 #menhaden mean total length in cm
  menh_W = exp(-11.396 + 3.08 * log(menh_L)) #menhaden mean weight in g based on TL
  
  menh_TS = 20.4 * log10(menh_L) - 68.88 #120 kHz
  menh_obs = 10^(menh_TS / 10)
  
  data = data %>%
    mutate(
      Species = case_when(
        Species == "Swimbladder fish" ~ "Menhaden",
        Species == "Swimbladderless fish" ~ "Longfin squid",
        .default = Species
      )
    )
  
  for (i in 1:nrow(data)) {
    if (data$Species[i] == "Menhaden" && data$Frequency[i] == 120) {
      data$Abundance[i] = data$ABC[i] / menh_obs
      data$Biomass[i] = data$Abundance[i] * menh_W
    } else if (data$Species[i] == "Longfin squid" &&
               data$Frequency[i] == 120) {
      data$Abundance[i] = data$ABC[i] / squid_obs
      data$Biomass[i] = data$Abundance[i] * squid_W
    }
  }
  
} else if (month(data$Time_M[1]) %in% c(12, 1, 2, 3, 4, 5)) {
  ## For spring and winter fish, assuming all swimbladder echoes are herring and all
  #  swimbladderless echoes are mackerel
  
  # This code doesn't affect zooplankton data at all
  
  herr_L = 19.7 # herring mean length in cm
  herr_W = exp(-11.7972 + 3.0314 * log(herr_L)) * 1000 # herring mean weight in g based on L
  
  # Depth-dependent herring TS and obs function
  herr_obs = function(herr_z) {
    herr_TS = 20 * log10(herr_L) - 2.3 * log10(1 + herr_z / 10) - 65.4
    herr_obs = 10^(herr_TS / 10)
    return(herr_obs)
  }
  
  mack_L = 22.8 # mean length of mackerel in cm
  mack_W = exp(-12.6713 + 3.3119 * log(mack_L)) * 1000 # mean weight of mackerel in g based on L
  
  mack_TS = 20 * log10(mack_L) - 53.58
  mack_obs = 10^(mack_TS / 10)
  
  data = data %>%
    mutate(
      Species = case_when(
        Species == "Swimbladder fish" ~ "Atlantic herring",
        Species == "Swimbladderless fish" ~ "Atlantic mackerel",
        .default = Species
      )
    )
  
  for (i in 1:nrow(data)) {
    if (data$Species[i] == "Atlantic herring" && data$Frequency[i] == 38) {
      data$Abundance[i] = data$ABC[i] / herr_obs(data$Depth_mean[i])
      data$Biomass[i] = data$Abundance[i] * herr_W
    } else if (data$Species[i] == "Atlantic mackerel" &&
               data$Frequency[i] == 200) {
      data$Abundance[i] = data$ABC[i] / mack_obs
      data$Biomass[i] = data$Abundance[i] * mack_W
    }
  }
}
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
    NASC = mean(NASC, na.rm = T),
    Ping = mean(c(Ping_S, Ping_E), na.rm = T),
    Depth = mean(Depth_mean, na.rm = T),
    Date = mean(c(Time_M), na.rm = T),
    Lat = mean(c(Lat_M), na.rm = T),
    Long = mean(c(Lon_M), na.rm = T),
    Seafloor_Depth = mean(c(Exclude_below_line_depth_mean + 1),
                          na.rm = T),
    Dive = mean(Dive)
  ) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

## Align glider variables and zooplankton data

data3$pH = NA
data3$salinity = NA
data3$chlorophyll_a = NA
data3$temperature = NA

# for (i in 1:nrow(data3)) {
#   idx = which.min(st_distance(data3$geometry[i], g_coords$geometry))
# 
#   data3$pH[i] = gdata$pH[idx]
#   data3$salinity[i] = gdata$salinity[idx]
#   data3$chlorophyll_a[i] = gdata$chlorophyll_a[idx]
#   data3$temperature[i] = gdata$temperature[idx]
# 
#   if(i %% 10000 == 0) {
#     print(i) # this is just to check that it hasn't crashed
#   }
# }

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
    Seafloor_Depth = mean(Seafloor_Depth, na.rm = T),
    Date = mean(Date, na.rm = T),
    pH = mean(pH, na.rm = T),
    salinity = mean(salinity, na.rm = T),
    chlorophyll_a = mean(chlorophyll_a, na.rm = T),
    temperature = mean(temperature, na.rm = T),
    Dive = mean(Dive)
  ) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  arrange(Date) %>%
  mutate(
    dist = c(0, st_distance(geometry[1:(nrow(.) - 1)], geometry[2:nrow(.)], by_element = T)),
    long = unlist(map(geometry, 1)),
    lat = unlist(map(geometry, 2))
  )

# Need to average them into larger horizontal bins (also counting number of dives)

dist = 6000 # horizontal distance in meters
# 1 cell/1 m * 6000 m distance = num cells
num_cells = dist # number of 1 m cells in new horizontal dist

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

dive_counts = data4 %>%
  st_drop_geometry() %>%
  group_by(group) %>%
  summarize(count = n_distinct(Dive))

data4 = data4 %>%
  group_by(group, Species) %>%
  summarize(D_Int_Abundance = sum(D_Int_Abundance)/num_cells, # mean doesn't work because it doesn't account for empty cells
            D_Int_Biomass = sum(D_Int_Biomass)/num_cells,
            across(c(Seafloor_Depth:temperature, geometry:lat), ~mean(., na.rm=T))) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

for(i in 1:nrow(data4)) {
  data4$N_Dives[i] = dive_counts$count[data4$group[i] == dive_counts$group]
}

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
fname = paste0(data_dir, "Processed_Abundance_Biomass_Data_1x1_Grid.rda")
save(list=c("data_ldf", "data_filenames","data","data2","data3","data4"), file = fname)

