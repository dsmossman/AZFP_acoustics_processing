# Author: Delphine Mossman
# Date Created: 23 Aug 2024
# Date Last Modified: 25 June 2025

# WIP

## Initialization/Setup

rm(list = ls())

library(tidyverse)
library(readxl)
library(ncdf4)
library(CFtime)
# library(hms)
# library(rnaturalearth)
# library(rnaturalearthdata)
library(sf)
# library(sfheaders)
# library(lubridate)
# library(marmap)
library(rstatix)
library(R.utils)
# library(cowplot)
# library(ggpubr)
# library(ggnewscale)
library(tcltk)
library(MASS)
library(GGally)
library(lmtest)
library(units)
library(ggpubr)
library(rnaturalearth)
library(quest)
library(ggcorrplot)
library(mgcv)

sourceDirectory(
  "C:/Users/dmossman/Box/ACOUSTIC DATA PROCESSING PROTOCOLS/AZFP Processing/R Functions/",
  modifiedOnly = F
)

glider_dep = choose_directory(caption = "Select glider data folder") %>% substring(., regexpr("ru[0-9]{2}-*", .))
year = substr(glider_dep, 6, 9)

zoop_data_dir = paste0("C:/Users/dmossman/Box/Glider Data/",
                       glider_dep,
                       "/Derived Biomass Data/")
figure_dir = paste0("C:/Users/dmossman/Box/Glider Data/", glider_dep, "/Figures/")

ftle_data_dir = "C:/Users/dmossman/Box/FTLE Work/Processed Data/Glider Deployment Data/"
ftle_processed_dir = "C:/Users/dmossman/Box/FTLE Work/Processed Data/Hourly CSVs/"

ftle_file_name = choose.files(caption = "Select FTLE .nc file") %>% substring(., regexpr("MARACOOS_*", .))

## Data import (run the echosounder code first for the zoop data)

load(paste0(zoop_data_dir, 'Processed_Abundance_Biomass_Data.rda'))
assign("zoop_data", data3)
assign("zoop_data2", data4)
rm(data, data2, data3, data4)
load(paste0(zoop_data_dir, 'Glider_Data.rda'))

#####
## Oceanography correlation testing (no FTLE included)

test_data = zoop_data %>%
  filter(Species == "Large Copepod") %>%
  mutate(log10(Abundance)) %>%
  dplyr::select(Species,
                Abundance,
                `log10(Abundance)`,
                salinity,
                temperature,
                pH,
                chlorophyll_a) %>%
  na.omit()

model = lm(`log10(Abundance)` ~ salinity + temperature + pH + chlorophyll_a, data = test_data)
stepAIC(model, trace = 2)

#####
## FTLE data import/formatting/saving
ftle_data = nc_open(paste0(ftle_data_dir, ftle_file_name))
# print(ftle_data)

ftle_lon_data = ncvar_get(ftle_data, "lon")
ftle_lon_attr = ncatt_get(ftle_data, "lon", "units")
ftle_lat_data = ncvar_get(ftle_data, "lat")
ftle_lat_attr = ncatt_get(ftle_data, "lat", "units")
ftle_time_data = ncvar_get(ftle_data, "time")
ftle_time_att = ncatt_get(ftle_data, "time", "units")

# because of a fun little quirk in how Matlab calculates dates (days since January 1st of year 0,
# I'll have to do some fiddling to get it to line up)
if (as.numeric(year) %% 4 != 0) {
  ftle_time_att$value = "days since 0001-12-31 00:00:00" # This gets it to +2 yrs of the actual dates
  ftle_value_data = ncvar_get(ftle_data, "ftle_noEdges")
  fillvalue = ncatt_get(ftle_data, "ftle", "_FillValue")
  nc_close(ftle_data)
  
  cf = CFtime(ftle_time_att$value, calendar = "proleptic_gregorian", ftle_time_data) # convert time to CFtime class
  timestamps = as_timestamp(cf, format = "timestamp") # get character-string times
  substr(timestamps, 1, 4) = as.character(as.double(substr(timestamps, 1, 4)) - 2)
  time_cf = CFparse(cf, timestamps) # parse the string into date components
  
  # this might cause me some grief if/when it runs over a leap year... will cross that
  # bridge when I get to it......
} else {
  # here's the leap year stuff
  ftle_value_data = ncvar_get(ftle_data, "ftle_noEdges")
  fillvalue = ncatt_get(ftle_data, "ftle", "_FillValue")
  nc_close(ftle_data)
  
  cf = CFtime(ftle_time_att$value, calendar = "proleptic_gregorian", ftle_time_data) # convert time to CFtime class
  timestamps = as_timestamp(cf) # get character-string times
  substr(timestamps, 1, 4) = as.character(as.double(substr(timestamps, 1, 4)) - 2000)
  time_cf = CFparse(cf, timestamps) # parse the string into date components
}

# Change the fill value from 0 to NA
ftle_value_data[ftle_value_data == fillvalue$value] = NA

## Reshape each day into a 2D matrix that can be saved as a CSV

for (i in 1:nrow(time_cf)) {
  ftle_vec_long = as_vector(ftle_value_data[, , i])
  ftle_matrix = matrix(
    ftle_vec_long,
    nrow = nrow(ftle_lat_data) * nrow(ftle_lon_data),
    ncol = 1
  )
  
  lonlattime = as.matrix(expand.grid(ftle_lat_data, ftle_lon_data, timestamps[i]))
  ftle_value_data2 = data.frame(cbind(lonlattime, ftle_matrix))
  names(ftle_value_data2) = c("Latitude", "Longitude", "Time", "FTLE")
  ftle_value_data2$FTLE = as.double(ftle_value_data2$FTLE)
  ftle_value_data2 = ftle_value_data2 %>% filter((Longitude >= -72.5) &
                                                   (Longitude <= -75)) %>%
    filter((Latitude <= 41.5) &
             (Latitude >= 38.5))
  
  # ggplot() + geom_sf(data = na.omit(ftle_value_data2 %>% st_as_sf(coords=c("Longitude","Latitude"),crs=4326)), aes(color=FTLE))
  
  # fname = paste0(substr(ftle_file_name, 1, nchar(ftle_file_name) - 13),
  #                "_",
  #                timestamps[i],
  #                ".csv")
  fname = paste0(
    substr(ftle_file_name, 1, 13),
    "_",year,"_",
    substr(timestamps[i], 6, 10),
    "_",
    substr(timestamps[i], 12, 13),
    ".csv"
  )
  print(paste0("Writing file for ", timestamps[i]))
  write.csv(ftle_value_data2,
            paste0(ftle_processed_dir, fname),
            row.names = F)
}

#####
## For each day of zooplankton data, load in the corresponding FTLE file and look
#  for strong FTLE values within some distance of the zooplankton point and then
#  add that value to the zooplankton dataset
## The goal is to look for strong FTLEs close to where we saw zooplankton gathering

data_filenames = list.files(ftle_processed_dir,
                             pattern = paste0(substr(ftle_file_name, 1, 13),"_",year,"_*"),
                            full.names = T)

# data_filenames = list.files(
#  ftle_processed_dir,
#  pattern = paste0(substr(ftle_file_name, 1, 13), "_11-13_[0-9]{2}"),
#  full.names = T
# )

## Reshape zoop data to HFR grid of 6km squares

# Grab any old FTLE data, they all have the same grid
df = read_csv(data_filenames[1], show_col_types = F) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
#ggplot() + geom_sf(data = df, aes(color=FTLE))

# try bSquare method

bSquare <- function(x, a) {
  a <- sqrt(a) / 2
  return(sf::st_buffer(
    x,
    dist = a,
    nQuadSegs = 1,
    endCapStyle = "SQUARE"
  ))
}

df_squares = df %>% 
  st_transform(crs = 3857) %>%
  bSquare(., 36000000) %>%
  st_transform(crs = 4326) %>%
  mutate(grid_id = 1:nrow(.))
#ggplot() + geom_sf(data = df_squares) + geom_sf(data = df, aes(color=FTLE))
# Issue with overlapping squares...

# # Convert to a 6km square grid
# area_grid = st_make_grid(df %>% st_transform(crs = 3857),
#                          n = c(74,89),
#                          what = "polygons",
#                          offset = c((-8347003 - 3000), (4652602 - 3000))
#                          ) %>%
#   st_sf() %>%
#   # add grid ID
#   mutate(grid_id = 1:nrow(.)) %>%
#   st_transform(crs = 4326)
# ggplot() + geom_sf(data = area_grid) + geom_sf(data = df, aes(color=FTLE))# + coord_sf(xlim=c(-73,-72.5),ylim=c(39.5,41))

# zoop_data = zoop_data %>% filter(format(Date, format = "%Y-%m-%d") == "2023-11-13")

# For each zooplankton data entry, find the cell it is within
zoop_data$Cell_ID = NA
zoop_data2$Cell_ID = NA

for (j in 1:nrow(zoop_data)) {
  idx = st_intersects(zoop_data[j, ], df_squares)
  if(lengths(idx) == 0) {
    next
  } else if (lengths(idx) == 1) {
    zoop_data$Cell_ID[j] = idx
  } else {
    zoop_data$Cell_ID[j] = paste0(idx)
  }
}

for (k in 1:nrow(zoop_data2)) {
  idx = st_intersects(zoop_data2[k, ], df_squares)
  if(lengths(idx) == 0) {
    next
  } else if (lengths(idx) == 1) {
    zoop_data2$Cell_ID[k] = idx
  } else {
    zoop_data2$Cell_ID[k] = paste0(idx)
  }
}

# Average by date/hour, cell, and species
zoop_data = zoop_data %>%
  mutate(Hour = format(Date, format = "%Y-%m-%d %HH")) %>%
  group_by(Species, Cell_ID, Hour) %>%
  mutate(Long = unlist(map(geometry, 1)), Lat = unlist(map(geometry, 2))) %>%
  st_drop_geometry() %>%
  summarize(across(Abundance:Lat, ~ mean(., na.rm = T))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  arrange(Date)

zoop_data2 =  zoop_data2 %>%
  mutate(Hour = format(Date, format = "%Y-%m-%d %HH")) %>%
  group_by(Species, Cell_ID, Hour) %>%
  mutate(Long = unlist(map(geometry, 1)), Lat = unlist(map(geometry, 2))) %>%
  st_drop_geometry() %>%
  summarize(across(D_Int_Abundance:temperature, ~ mean(., na.rm = T)),
          across(Long:Lat, ~ mean(., na.rm = T)),
          Depth_Type = Depth_Type[1],
          Shelf_Type = Shelf_Type[1]) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  arrange(Date)

zoop_dates = unique(format(zoop_data$Date, format = "%Y-%m-%d %HH"))
zoop_data$FTLE_value = NA

zoop_dates2 = unique(format(zoop_data2$Date, format = "%Y-%m-%d %HH"))
zoop_data2$FTLE_value = NA

for (i in 1:length(data_filenames)) {
  df = read_csv(data_filenames[i], show_col_types = F) %>%
    mutate(Cell_ID = df_squares$grid_id) %>%
    na.omit() %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  # remove non-strong FTLEs and the fill values here so I don't have to keep saving the files over and over
  
  # cut off everything below 90th percentile of data (Jackie used 0.1)
  cutoff = 0.0201 #quantile(df$FTLE[df$FTLE > 0], 0.9, names = F)
  df$FTLE[df$FTLE <= cutoff] = NA
  df = na.omit(df)
  ftle_day = format(df$Time[1], format = "%Y-%m-%d %HH")
  
  print(paste0("Number of strong FTLE values for ", df$Time[1], " is ", nrow(df)))
  
  if (nrow(df) == 0) {
    # if the dataframe is empty
    rm(df) # ditch the dataframe
    next # proceed to the next file
  } else {
    # if the dataframe is not empty
    if (ftle_day %in% zoop_dates) {
      #     # if the dates match AND there are strong FTLE values

      ## Do the same cell averaging procedure to assign cell IDs to the FTLE values

      # for (k in 1:nrow(df)) {
      #   idx = as.numeric(st_intersects(df[k, ], area_grid)[1])
      #   df$Cell_ID[k] = area_grid$grid_id[idx]
      # }


      # grab the subset of the zoop data from that date
      zoop_data_subset = zoop_data[format(zoop_data$Date, format = "%Y-%m-%d %HH") == ftle_day, ]

      for (m in 1:nrow(zoop_data_subset)) {
        cell_IDs = eval(parse(text = unlist(zoop_data_subset$Cell_ID[m])))
        if(length(cell_IDs) >= 1) {
          zoop_data_subset$FTLE_value = mean(df_squares$FTLE[cell_IDs])
        } else
          next
      }

      # insert the subset back into the regular dataframe
      zoop_data$FTLE_value[format(zoop_data$Date, format = "%Y-%m-%d %HH") == ftle_day] = zoop_data_subset$FTLE_value
    }
    
    if (ftle_day %in% zoop_dates2) {
 
      # grab the subset of the zoop data from that date
      zoop_data_subset = zoop_data2[format(zoop_data2$Date, format = "%Y-%m-%d %HH") == ftle_day, ]
      
      for (m in 1:nrow(zoop_data_subset)) {
        cell_IDs = eval(parse(text = unlist(zoop_data_subset$Cell_ID[m])))
        if(length(cell_IDs) >= 1) {
          zoop_data_subset$FTLE_value = mean(df_squares$FTLE[cell_IDs])
        } else
          next
      }
      
      # insert the subset back into the regular dataframe
      zoop_data2$FTLE_value[format(zoop_data2$Date, format = "%Y-%m-%d %HH") == ftle_day] = zoop_data_subset$FTLE_value
    }
    
  }
}

fname = paste0(zoop_data_dir,
               "Zooplankton_FTLE_Correlation_Data.rda")
save(list = c("zoop_data", "zoop_data2"), file = fname)

#####
## Graphing

load(
  "C:/Users/dmossman/Box/Glider Data/ru39-20230420T1636/Derived Biomass Data/Zooplankton_FTLE_Correlation_Data.rda"
)
assign("zoop_data_spring_2023", zoop_data)
zoop_data_spring_2023$Season = "Spring"

load(
  "C:/Users/dmossman/Box/Glider Data/ru39-20231103T1413/Derived Biomass Data/Zooplankton_FTLE_Correlation_Data.rda"
)
assign("zoop_data_fall_2023", zoop_data)
zoop_data_fall_2023$Season = "Fall"

load(
  "C:/Users/dmossman/Box/Glider Data/ru39-20240215T1646/Derived Biomass Data/Zooplankton_FTLE_Correlation_Data.rda"
)
assign("zoop_data_winter_2024", zoop_data)
zoop_data_winter_2024$Season = "Winter"

load(
  "C:/Users/dmossman/Box/Glider Data/ru39-20240429T1522/Derived Biomass Data/Zooplankton_FTLE_Correlation_Data.rda"
)
assign("zoop_data_spring_2024", zoop_data)
zoop_data_spring_2024$Season = "Spring"

rm(zoop_data)

zoop_data_full = rbind(
  #zoop_data_spring_2023,
  zoop_data_fall_2023,
  zoop_data_winter_2024#,
  #zoop_data_spring_2024
) %>%
  filter(Species == "Large Copepod")

ggplot(data = zoop_data_full, aes(
  x = FTLE_value,
  y = log10(Abundance),
  group = Season,
  color = Season
)) +
  geom_point(na.rm = T) +
  geom_smooth(method = "lm", se = F, na.rm = T) +
  stat_regline_equation(aes(label = ..eq.label..),
                        # adds equation to linear regression
                        label.x = 0,
                        show.legend = F) +
  stat_cor(
    aes(label = paste(..rr.label..)),
    # adds R^2 value
    r.accuracy = 0.01,
    label.x = 0.01,
    show.legend = F
  ) +
  scale_color_viridis_d(end = 0.8) +
  labs(y = bquote(atop(
    log10 ~ of ~ Concentration ~ phantom(), (individuals / m ^ 3)
  )), x = "FTLE Value") +
  theme(text = element_text(size = 16))

ggsave(filename = "C:/Users/dmossman/Box/Glider Data/FTLE_Correlation_Full.png", scale = 2)

## FTLE heat map of strong values
# Percentage of deployment where there was a strong FTLE value in each grid cell of the FTLE data

data_filenames = list.files(
  "C:/Users/dmossman/Box/FTLE Work/Processed Data/Daily CSVs/",
  pattern = paste0("MARACOOS_fall_deployment", "_", year, "-*"),
  full.names = T
)

ftle_value_data_full = data.frame()

for (i in 1:length(data_filenames)) {
  df = read_csv(data_filenames[i], show_col_types = F)
  # remove non-strong FTLEs and the fill values here so I don't have to keep saving the files over and over
  
  # cut off everything below the mean 90th percentile of the four deployments
  df[df$FTLE < 0.001348607, 4] = NA
  # cut off everything below 0.05 (Jackie used 0.1)
  # df[df$FTLE < 0.05,4] = NA
  # df = na.omit(df)
  
  ftle_value_data_full = rbind(ftle_value_data_full, df)
  
  if (nrow(df) == 0) {
    # if the dataframe is empty
    rm(df) # ditch the dataframe
    next # proceed to the next file
  } else {
    
  }
}

percentage_data = ftle_value_data_full %>%
  count(Longitude, Latitude, name = "Num_Strong_FTLE", .drop = F) %>%
  mutate(Persistence = Num_Strong_FTLE / 33) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

percentage_data_2 = percentage_data %>%
  st_transform(crs = 3857) %>%
  bSquare(., 36000000) %>%
  st_transform(crs = 4326)

world = ne_countries(scale = "medium")
world = world[world$geounit == "United States of America", ]

xlim = c(-75, -72.5)
ylim = c(38.5, 41.5)

ggplot() +
  geom_sf(data = world, fill = "gray15") +
  geom_sf(data = percentage_data, aes(color = Persistence)) +
  coord_sf(crs = st_crs(4326),
           xlim = xlim,
           ylim = ylim)
