#####
rm(list = ls())

library(tidyverse)
library(R.utils)
library(readxl)
library(rstatix)
library(lubridate)
library(sf)

data_dir = paste0("C:/Users/Delphine/Box/Glider Data/ru39-20230817T1520/Derived Biomass Data/")
figure_dir = paste0("C:/Users/Delphine/Box/Glider Data/ru39-20230817T1520/Figures/")

sourceDirectory(
  "H:/dm1679/Code/R Functions",
  modifiedOnly = F
)

load(paste0(data_dir,'Processed_Abundance_Biomass_Data.rda'))
full_data = data %>% filter(Time_M < ISOdatetime(2023,8,23,0,0,0))
rm(data, data2, data3, data4)

data_filenames = list.files(data_dir, pattern = "*28_125_Only_*",full.names = T)
data_ldf = lapply(data_filenames, function(x) read_csv(x, show_col_types = F))

partial_data = data.frame()

for(j in 1:length(data_ldf)) {
  if(nrow(data_ldf[[j]]) == 0)
    next
  else
    partial_data = rbind(partial_data,data_ldf[[j]])
}

partial_data$Time_M = paste0(partial_data$Date_M,' ',partial_data$Time_M)
partial_data$Time_M = as.POSIXct(partial_data$Time_M, format = "%d-%b-%Y %H:%M:%S", tz = "UTC")
partial_data$Time_M = as.POSIXct(format(partial_data$Time_M, tz="America/Detroit", usetz=T))

partial_data$Echo_Num = NA

for(k in 1:nrow(full_data)) {
  partial_data$Echo_Num[partial_data$Interval == full_data$Interval[k] &
                          partial_data$Layer == full_data$Layer[k]] = full_data$Echo_Num[k]
}

partial_data$Species[partial_data$Species == 0] = "Unidentified"

partial_data = partial_data %>% arrange(Time_M, Echo_Num)

full_data = full_data %>%
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
  )

partial_data = partial_data %>%
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
  )

# Trying to determine where the partial and full dB differencing methods agree 
# and disagree

full_data$Species_2 = "Missing"

# Go thru and match the species assignments between the full and partial data
for(j in 1:nrow(partial_data)) {
  full_data$Species_2[full_data$Echo_Num == partial_data$Echo_Num[j]] = partial_data$Species[j]
}

ID_tally = merge(full_data %>% 
  count(Species, Species_2), full_data %>% count(Species), by = "Species") %>%
  mutate(percentage = n.x/n.y * 100) %>%
  pivot_wider(id_cols = Species, names_from = Species_2, values_from = percentage, values_fill = 0)
