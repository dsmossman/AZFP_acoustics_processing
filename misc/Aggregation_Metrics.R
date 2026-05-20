# Author: Delphine Mossman
# Date Created: 3 Mar 2026

# Getting metrics/figures/summary statistics of schools as measured by Echoview's
# school detection algorithm

#####
rm(list = ls())

library(tidyverse)
library(readxl)
library(R.utils)
library(tcltk)
library(hms)

source("./AZFP_acoustics_processing/misc/choose_directory.R")
source("./AZFP_acoustics_processing/misc/create_dir.R")

glider_dep = choose_directory() %>% substring(., regexpr("ru[0-9]{2}-*", .))
year = substr(glider_dep,6,9)

data_dir = paste0("C:/Users/Delphine/Box/Glider Data/",
                  glider_dep,
                  "/Echoview CSV Export Files/School Detection Tests/")

#####
## Aggregation data formatting

data_filenames = list.files(pattern = "*aggregation.csv$", data_dir, recursive = T, full.names = T)
data = lapply(data_filenames, function(x) read_csv(x, show_col_types = F)) %>%
  bind_rows() %>%
  mutate(Date_M = as.Date(as.character(Date_M), format = "%Y%m%d"),
         Seafloor_Depth = Exclude_below_line_depth_mean + 1)

# Reformatting timestamps and converting to eastern time zone
data$Time_M = paste0(data$Date_M,' ',data$Time_M)
data$Time_M = as.POSIXct(data$Time_M, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data$Time_M = as.POSIXct(format(data$Time_M, tz="America/Detroit", usetz=T))

data = data %>% arrange(Time_M) %>%
  select(Region_ID:Region_class,
         Sv_mean,Depth_mean,
         Date_M:Lon_M,Seafloor_Depth,
         Corrected_length:Corrected_MVBS) %>%
  mutate(Seafloor_Depth_Bin = cut(Seafloor_Depth, seq(0,100,5), right = F),
         Depth_Bin = cut(Depth_mean, seq(0,100,5), right = F)) %>%
  filter(Sv_mean > -999 & 
           Seafloor_Depth > 0 & 
           Corrected_length > -9999 &
           Corrected_MVBS > -999 & Corrected_MVBS < -11 &
           Corrected_perimeter > 0)

#####
## Summary statistics

data %>%
  # group_by(Depth_Bin) %>%
  summarise(across(Corrected_length:Corrected_MVBS,
                                 list(min = ~min(.x, na.rm = T),
                                   mean = ~mean(.x, na.rm = T),
                                      SD = ~sd(.x, na.rm = T),
                                   median = ~median(.x, na.rm = T),
                                   q25 = ~quantile(.x, 0.25, na.rm = T),
                                   q75 = ~quantile(.x, 0.75, na.rm = T),
                                   max = ~max(.x, na.rm = T)),
                   .names = "{.col}-{.fn}")) %>%
  pivot_longer(everything(), names_sep = "-", names_to=c('variable', '.value'))

#####
## Figures of data alone

ggplot() +
  geom_histogram(data = data, aes(x = log10(Corrected_area)), na.rm = T, bins = 15)

#####
## Explanatory variables

data_TOD = data %>% mutate(TOD = case_when(
  as_hms(Time_M) > hms(0, 0, 7) &
    as_hms(Time_M) < hms(0, 0, 19) ~ "Day",
  .default = "Night"
))

ggplot() +
  geom_histogram(data = data_TOD, aes(x = log10(Corrected_area)), na.rm = T, bins = 15) +
  facet_wrap(~TOD)

data_DVM = data_TOD %>%
  group_by(Depth_Bin, TOD) %>%
  reframe(Count = n())

ggplot() +
  geom_col(data = data_DVM, aes(x = Count, y = Depth_Bin)) +
  scale_y_discrete(limits = rev) +
  facet_wrap(~TOD)

data_DVM_size = data_TOD %>%
  mutate(Patch_Size = case_when(
    Corrected_area < quantile(Corrected_area, 0.05) ~ "Small",
    Corrected_area > quantile(Corrected_area, 0.95) ~ "Large",
    .default = "Medium"
  )) %>%
  group_by(Depth_Bin, TOD, Patch_Size) %>%
  reframe(Count = n())

ggplot() +
  geom_col(data = data_DVM_size, aes(x = Count, y = Depth_Bin)) +
  scale_y_discrete(limits = rev) +
  facet_grid(Patch_Size~TOD)
