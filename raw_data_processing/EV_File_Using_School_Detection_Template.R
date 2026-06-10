rm(list = ls())

library(tidyverse)
library(RDCOMClient) # Download package using pak::pak("jkylearmstrong-temple/RDCOMClient") for R version >= 4.6
library(tcltk)

source("./AZFP_acoustics_processing/misc/choose_directory.R")
source("./AZFP_acoustics_processing/misc/create_dir.R")

## Setup
glider_dep = choose_directory(caption = "Select the glider deployment directory folder")

# School detection template
template = "C:/Users/dm1679/Box/Glider Data/AZFP_Zooplankton_Template_School_Detection.EV"

EVAppObj = COMCreate('EchoviewCom.EvApplication')

azfp_dep = paste0(glider_dep, "/AZFP Data/") # Raw AZFP data directory, in folders by month and day
CSV_dir = paste0(glider_dep, "/Echoview CSV Import Files/") # Raw AZFP data directory, in folders by month

## File creation
# Create an Echoview file from template for each day of the deployment, import
# the AZFP and glider data, and save

monthly_folders = list.dirs(azfp_dep, recursive = F)

for(folder in monthly_folders) {
  
  daily_folders = list.dirs(folder, recursive = F)
  
  for(daily in daily_folders) {
    
    day = substr(daily, nchar(daily)-1, nchar(daily))
    
    # Create a new file from the template
    EVFile = EVAppObj$NewFile(template)
    
    # Import the calibration file
    EVFile[["Filesets"]][[0]]$SetCalibrationFile(paste0(azfp_dep, substring(glider_dep, regexpr("ru[0-9]{2}-*", glider_dep)), ".ecs"))
    
    # Get a list of all the AZFP files in the folder
    AZFP_files = list.files(daily, pattern = "*[.]01[A-Z]$", full.names = T)
    
    AZFP_fileset = EVFile[["Filesets"]]$FindByName('AZFP')
    
    # Then import the files one by one
    for(i in AZFP_files) {
      AZFP_fileset[["DataFiles"]]$Add(i)
    }
    
    # Get a list of the glider CSV files for GPS/pitch/roll
    CSV_files = list.files(CSV_dir, pattern = "*.csv$", full.names = T)
    
    # Then import the files in order: GPS, then pitch, then roll
    ### GPS
    GPS_fileset = EVFile[["Filesets"]]$FindByName('GPS')
    GPS_fileset[["DataFiles"]]$Add(CSV_files[1])
    
    ### Pitch
    Pitch_fileset = EVFile[["Filesets"]]$FindByName('Pitch')
    Pitch_fileset[["DataFiles"]]$Add(CSV_files[2])
    
    ### Roll
    Roll_fileset = EVFile[["Filesets"]]$FindByName('Roll')
    Roll_fileset[["DataFiles"]]$Add(CSV_files[3])
    
    ### Depth (replace M69_DEPTH line)
    
    # Get the M69_DEPTH line
    EVLine = EVFile[["Lines"]]$FindByName("M69_DEPTH")
    
    # Import the data.depth.evl file; default name for a line is 'data'
    EVFile$Import(paste0(CSV_dir, "data.depth.evl"))
    
    # Get the new depth line
    depth = EVFile[["Lines"]]$FindByName("data")
    
    # Replace the M69_DEPTH line with the depth line
    EVLine$OverwriteWith(depth)
    
    # Delete the orphan depth line
    EVFile[["Lines"]]$Delete(depth)
    
    # Create a folder for the month (if it does not already exist) and define the file name
    create_dir(gsub("AZFP Data", "Echoview Files/School Detection Tests", folder))
    
    EV_file_name = paste0(gsub("AZFP Data", "Echoview Files/School Detection Tests", folder),"/",
                          substring(glider_dep, regexpr("ru[0-9]{2}-*", glider_dep)),
                          "_Echogram_School_Detection_",day,".EV")
    
    # Save the file
    EVFile$SaveAs(EV_file_name)
    
    
    # Close the file
    EVFile$Close()
    
  }
}

EVAppObj$Quit()
