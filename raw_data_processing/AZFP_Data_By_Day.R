rm(list = ls())

library(tidyverse)
library(R.utils)
library(tcltk)

sourceDirectory(
  "H:/dm1679/Code/R_Functions",
  modifiedOnly = F
)

glider_dep = choose_directory(caption = "Select the glider deployment directory folder")
azfp_dep = paste0(glider_dep, "/AZFP Data/") # Raw AZFP data directory, in folders by month

monthly_folders = list.dirs(azfp_dep, recursive = F)

for(f in 1:length(monthly_folders)) { # for each month
  data_dir = monthly_folders[f]
  
  files = list.files(data_dir, recursive = T, include.dirs = F, full.names = F) # get all the files
  
  for(g in 1:length(files)) { # for each file
    current_file = files[g]
    
    if(substr(current_file, nchar(current_file)-2, nchar(current_file)) == "LOG") {
    # Each file type has slightly different naming conventions/lengths, so need
    # to check what sort of file it is before proceeding
      
    folder_name = substr(current_file, 7, 8) # get the day of the month
    
    create_dir(paste0(data_dir, "/", folder_name)) # create the daily folder, if it does not already exist
    
    file.rename(from = paste0(data_dir, "/", current_file),
                to = paste0(data_dir, "/", folder_name, "/", current_file))
    } else if(substr(current_file, nchar(current_file) - 2, nchar(current_file)) != "cfg") {
      
      folder_name = substr(current_file, 5, 6)
      
      create_dir(paste0(data_dir, "/", folder_name))
      
      file.rename(from = paste0(data_dir, "/", current_file),
                  to = paste0(data_dir, "/", folder_name, "/", current_file))
    } else if(substr(current_file, nchar(current_file)-2, nchar(current_file)) == "cfg") {
      # The cfg (config file) has to be copied into each daily folder
      
     daily_folders = list.dirs(data_dir, recursive = F)
     
     for(h in 1:length(daily_folders)) {
       file.copy(from = paste0(data_dir, "/", current_file),
                 to = paste0(daily_folders[h], "/", current_file))
     }
     file.remove(paste0(data_dir, "/", current_file))
    }
  }
}
