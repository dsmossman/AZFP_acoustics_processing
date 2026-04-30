#####
# Echoview fish classificiation

echoview_to_fish_class = function(yr, mo, da, input_dir, output_dir) {
  ## Load echo integration .csv files of interest
  
  ### Get file for each day
  files = dir(
    input_dir,
    pattern = paste0(sprintf("%02d", da), "_[0-9]{2,3}(kHz.csv)$"),
    full.names = T
  )
  
  ### Read in data, add frequency column, convert to datetime, sort
  data = lapply(files, function(x)
    read_csv(x, show_col_types = F)) %>%
    bind_rows(.id = "Frequency") %>%
    mutate(Frequency = case_when(Frequency == 1 ~ 38, Frequency == 2 ~ 120, Frequency == 3 ~ 200)) %>%
    mutate(Date_M = as.Date(as.character(Date_M), format = "%Y%m%d")) %>%
    arrange(Frequency, Interval)
  
  ### Remove empty rows, cells with too-strong response, and cells without a response in all 4 frequencies
  
  data = data %>%
    filter(Sv_mean != -999)
  
  data_38 = data[(data$Frequency == 38), ]
  data_120 = data[(data$Frequency == 120), ]
  data_200 = data[(data$Frequency == 200), ]
  
  if (yr == "2023" &
      (mo == 8 &
       da >= 23) |
      mo == 9) {
    # removing bad 200 kHz data from the 2023 summer deployment from analysis
    data = inner_join(data, intersect(data_38[, c("Interval", "Layer")], data_120[, c("Interval", "Layer")]))
    num_cells = nrow(data) / 2
    bad_data = TRUE
  } else {
    data = inner_join(data, intersect(intersect(data_38[, c("Interval", "Layer")], data_120[, c("Interval", "Layer")]), data_200[, c("Interval", "Layer")]))
    num_cells = nrow(data) / 3
    bad_data = FALSE
  }
  
  ### dB differencing
  data$Difference = NA
  
  data$Difference[data$Frequency == 120] = data$Sv_mean[data$Frequency == 120] - data$Sv_mean[data$Frequency == 38]
  
  if (bad_data == FALSE) {
    data$Difference[data$Frequency == 200] = data$Sv_mean[data$Frequency == 200] - data$Sv_mean[data$Frequency == 120]
  }
  
  ### Species ID
  
  data$Species = "Empty Cell"
  
  Sv_38 = data$Sv_mean[i]
  Sv_120 = data$Sv_mean[i + num_cells]
  
  sv_38 = 10^(Sv_38 / 10)
  sv_120 = 10^(Sv_120 / 10)
  
  Diff_120_38 = data$Difference[i + num_cells]
  
  
  if (bad_data == FALSE) {
    Sv_200 = data$Sv_mean[i + 2 * num_cells]
    
    sv_200 = 10^(Sv_200 / 10)
    
    Diff_200_120 = data$Difference[i + 2 * num_cells]
    
  }
  
  for (i in 1:num_cells) {
    if (bad_data == FALSE) {
      if (Diff_120_38 > 0 & Diff_200_120 > 0) {
        if (sv_200 / sv_38 > 3 & sv_200 / sv_38 < 6 &
            sv_120 / sv_38 > 1.5 & sv_120 / sv_38 < 4) {
          # swimbladderless
          data$Species[c(i, i + num_cells, i + 2 * num_cells)] = "Swimbladderless fish"
        }
      } else if (sv_120 / sv_38 < 1 & sv_200 / sv_38 < 1 &
                 Sv_38 > -60 & Sv_120 > -70 & Sv_200 > -70) {
        # swimbladder, most likely herring or alewife, maybe menhaden ?
        # Gorska et al., 2004 and Lucca and Warren, 2019
        data$Species[c(i, i + num_cells, i + 2 * num_cells)] = "Swimbladder fish"
      } else {
        salp_depth = data$Depth_mean[i]
        
        if (salp_depth <= 10) {
          min_diff = -0.2
          max_diff = 1.5
        } else {
          min_diff = 4.2
          max_diff = 5.1
        }
        
        if (Diff_120_38 > min_diff & Diff_120_38 < max_diff) {
          data$Species[c(i, i + num_cells, i + 2 * num_cells)] = "Gelatinous Zooplankton"
        }
      }
    } else if(bad_data == TRUE) {
      if (Diff_120_38 > 0) {
        if (sv_120 / sv_38 > 1.5 & sv_120 / sv_38 < 4) {
          # swimbladderless
          data$Species[c(i, i + num_cells)] = "Swimbladderless fish"
        }
      } else if (sv_120 / sv_38 < 1 &
                 Sv_38 > -60 & Sv_120 > -70) {
        # swimbladder, most likely herring or alewife, maybe menhaden ?
        # Gorska et al., 2004 and Lucca and Warren, 2019
        data$Species[c(i, i + num_cells)] = "Swimbladder fish"
      } else {
        salp_depth = data$Depth_mean[i]
        
        if (salp_depth <= 10) {
          min_diff = -0.2
          max_diff = 1.5
        } else {
          min_diff = 4.2
          max_diff = 5.1
        }
        
        if (Diff_120_38 > min_diff & Diff_120_38 < max_diff) {
          data$Species[c(i, i + num_cells)] = "Gelatinous Zooplankton"
        }
      }
    }
  }
  
  
  ### Export
  base_name = paste0("RMI",
                     yr,
                     "_",
                     sprintf("%02d", mo),
                     "_",
                     sprintf("%02d", da),
                     "_Biomass_Data.csv")
  paste0("Writing data for ",
         as.character(mo),
         "/",
         as.character(da),
         " to file.")
  write.csv(data, paste0(output_dir, base_name))
}
#####
# Convert Echoview integrated output to target species abundance (zooplankton)

echoview_to_zoop_abundance = function(yr, mo, da, input_dir, output_dir) {
  ## Load echo integration .csv files of interest
  
  ### Get file for each day
  files = dir(
    input_dir,
    pattern = paste0(sprintf("%02d", da), "_[0-9]{3}(kHz.csv)$"),
    full.names = T
  )
  
  ### Read in data, add frequency column, convert to datetime, sort
  data = lapply(files, function(x)
    read_csv(x, show_col_types = F)) %>%
    bind_rows(.id = "Frequency") %>%
    mutate(
      Frequency = case_when(
        Frequency == 1 ~ 120,
        Frequency == 2 ~ 200,
        Frequency == 3 ~ 455,
        Frequency == 4 ~ 769
      )
    ) %>%
    mutate(Date_M = as.Date(as.character(Date_M), format = "%Y%m%d")) %>%
    arrange(Frequency, Interval)
  
  ### Remove empty rows, cells with too-strong response, and cells without a response in all 4 frequencies
  
  data = data %>%
    filter(Sv_mean != -999)
  
  data_120 = data[(data$Frequency == 120) & (data$Sv_mean <= -60), ]
  data_200 = data[(data$Frequency == 200) & (data$Sv_mean <= -60), ]
  data_455 = data[(data$Frequency == 455) & (data$Sv_mean <= -60), ]
  data_769 = data[(data$Frequency == 769) & (data$Sv_mean <= -60), ]
  
  data = inner_join(data, intersect(
    intersect(data_120[, c("Interval", "Layer")], data_200[, c("Interval", "Layer")]),
    intersect(data_455[, c("Interval", "Layer")], data_769[, c("Interval", "Layer")])
  ), by = join_by(Interval, Layer))
  
  ### dB differencing
  data$Difference = NA
  
  data$Difference[data$Frequency == 200] = data$Sv_mean[data$Frequency == 200] - data$Sv_mean[data$Frequency == 120]
  data$Difference[data$Frequency == 455] = data$Sv_mean[data$Frequency == 455] - data$Sv_mean[data$Frequency == 200]
  data$Difference[data$Frequency == 769] = data$Sv_mean[data$Frequency == 769] - data$Sv_mean[data$Frequency == 455]
  
  ### Species ID from dB difference windows
  
  data$Species = "Empty Cell"
  num_cells = nrow(data) / 4
  
  for (i in 1:num_cells) {
    if (data$Difference[i + num_cells * 2] >= -0.93 &
        data$Difference[i + num_cells * 2] <= 8.23) {
      data$Species[i] = "Gelatinous Zooplankton"
      data$Species[i + num_cells] = "Gelatinous Zooplankton"
      data$Species[i + 2 * num_cells] = "Gelatinous Zooplankton"
      data$Species[i + 3 * num_cells] = "Gelatinous Zooplankton"
    } else if (data$Difference[i + num_cells * 2] >= 14.5 &
               data$Difference[i + num_cells * 2] <= 17.5) {
      data$Species[i] = "Large Copepod"
      data$Species[i + num_cells] = "Large Copepod"
      data$Species[i + 2 * num_cells] = "Large Copepod"
      data$Species[i + 3 * num_cells] = "Large Copepod"
    }
  }
  
  ### Abundance calculations
  
  data$Abundance = 0
  data$Biomass = 0
  
  for (k in 1:nrow(data)) {
    if (data$Species[k] == "Large Copepod" & data$Frequency[k] == 455) {
      data$Abundance[k] = 10^((data$Sv_mean[k] --108.3) / 10)
      data$Biomass[k] = data$Abundance[k] * 269.66e-6
    }
  }
  
  ### Export
  base_name = paste0("RMI",
                     yr,
                     "_",
                     sprintf("%02d", mo),
                     "_",
                     sprintf("%02d", da),
                     "_Biomass_Data.csv")
  write.csv(data, paste0(output_dir, base_name), row.names = F)
  message(paste0("Writing data for ",
                as.character(mo),
                "/",
                as.character(da),
                " to file."))
}

#####
# Using Echoview's School Detection algorithm for zooplankton

echoview_to_zoop_abundance_aggregations = function(yr, mo, da, input_dir, output_dir) {
  ## Load echo integration .csv files of interest
  
  ### Get file for each day
  file = dir(
    input_dir,
    pattern = paste0("*", sprintf("%02d", da), "_455kHz_abundance.csv"),
    full.names = T
  )
  
  ### Read in data, convert to datetime, filter, sort
  data = read.csv(file) %>%
    mutate(Date_M = as.Date(as.character(Date_M), format = "%Y%m%d")) %>%
    filter(Region_class == "C. fin swarm" & Sv_mean != -999) %>%
    arrange(Region_ID, Interval)
  
  ### Calculate abundance with inverse problem
  for (k in 1:nrow(data)) {
    data$Species[k] = "C. finmarchicus"
    data$Abundance[k] = 10^((data$Sv_mean[k] --108.3) / 10)
    data$Biomass[k] = data$Abundance[k] * 269.66e-6
  }
  
  ### Export
  base_name = paste0("RMI",
                     yr,
                     "_",
                     sprintf("%02d", mo),
                     "_",
                     sprintf("%02d", da),
                     "_Biomass_Data.csv")
  print(paste0(
    "Writing data for ",
    as.character(mo),
    "/",
    as.character(da),
    " to file."
  ))
  write.csv(data, paste0(output_dir, base_name), row.names = F)
  
}
