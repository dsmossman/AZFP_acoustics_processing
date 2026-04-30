rm(list = ls())

library(tidyverse)
library(R.utils)
library(tcltk)

sourceDirectory(
  "H:/dm1679/Code/R_Functions",
  modifiedOnly = F
)

# Read in the glider data (downloaded previously from ERDDAP)
glider_dep = choose_directory(caption = "Select the glider deployment folder.")

glider_data = read_csv(paste0(glider_dep, "/", 
                              substring(glider_dep, regexpr("ru[0-9]{2}-*", glider_dep)), 
                              "-profile-sci-delayed.csv"),
                       show_col_types = F)[-1,] %>%
  select(time, latitude, longitude, m_pitch, m_roll, depth)

file_folder = paste0(glider_dep, "/Echoview CSV Import Files/")

# Format and write GPS data

gps_datetime = glider_data[,"time"] %>% 
  mutate(time = gsub('T', ' ', time)) %>%
  mutate(time = gsub('Z', '', time)) %>%
  mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
gps_date = data.frame(GPS_date = format(gps_datetime$time, format = "%Y-%m-%d"))
gps_time = data.frame(GPS_time = format(gps_datetime$time, format = "%H:%M:%S"))

gps_data = cbind(gps_date, gps_time, data.frame(GPS_milliseconds = 0, 
                                                Latitude = glider_data$latitude,
                                                Longitude = glider_data$longitude))

write_csv(gps_data, file = paste0(file_folder, "data.gps.csv"))

# Format and write pitch data

pitch_indexes = which(glider_data$m_pitch != "NaN")

pitch_data = cbind(Pitch_date = gps_date[pitch_indexes,],
                   Pitch_time = gps_time[pitch_indexes,],
                   Pitch_milliseconds = 0,
                   Pitch_angle = as.numeric(glider_data$m_pitch[pitch_indexes]) * 180/pi) %>%
  as.data.frame()

write_csv(pitch_data, file = paste0(file_folder, "data.pitch.csv"))

# Format and write roll data

roll_indexes = which(glider_data$m_roll != "NaN")

roll_data = cbind(Roll_date = gps_date[roll_indexes,],
                   Roll_time = gps_time[roll_indexes,],
                   Roll_milliseconds = 0,
                   Roll_angle = as.numeric(glider_data$m_roll[roll_indexes]) * 180/pi) %>%
  as.data.frame()

write_csv(roll_data, file = paste0(file_folder, "data.roll.csv"))

# Format and write depth data
# Need to take special care with this one

depth_indexes = which(glider_data$depth != "NaN")

depth_date = gsub('-', '', gps_date$GPS_date[depth_indexes])
depth_time = paste0(gsub(':', '', gps_time$GPS_time[depth_indexes]), '0000')

EVL_file_header = "EVBD 3 16.1.69"

depth_file = file(paste0(file_folder, "data.depth.evl"), "w")
writeLines(c(EVL_file_header, length(depth_indexes)), con = depth_file)
close(depth_file)

depth_data = cbind(depth_date, depth_time, glider_data$depth[depth_indexes], 3)

write.table(depth_data, file = paste0(file_folder, "data.depth.evl"), append = T, col.names = F, row.names = F, quote = F)
