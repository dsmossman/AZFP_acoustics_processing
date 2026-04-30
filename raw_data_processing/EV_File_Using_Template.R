rm(list = ls())

library(tidyverse)
library(RDCOMClient)
library(R.utils)
library(tcltk)

sourceDirectory(
  "H:/dm1679/Code/R_Functions",
  modifiedOnly = F
)

## Setup
glider_dep = choose_directory(caption = "Select the glider deployment directory folder")

# Template needs to be set to match the AZFP (3 frequency fish or 4 frequency zoop)
template = choose.files(default = "C:/Users/Delphine/Box/Glider Data/AZFP*Template*.EV",
                        caption = "Select the EV template file to use") %>%
  gsub("\\\\","/",.) # Windows file selection weirdness

EVAppObj = COMCreate('EchoviewCom.EvApplication')

azfp_dep = paste0(glider_dep, "/AZFP Data/") # Raw AZFP data directory, in folders by month and day
CSV_dir = paste0(glider_dep, "/Echoview CSV Import Files/") # Raw AZFP data directory, in folders by month

## File creation
# Create an Echoview file from template for each day of the deployment, import
# the AZFP and glider data, and save

for(folder in list.dirs(azfp_dep, recursive = F)) {
  
  for(daily in list.dirs(folder, recursive = F)) {
    
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
    create_dir(gsub("AZFP Data", "Echoview Files", folder))
    
    EV_file_name = paste0(gsub("AZFP Data", "Echoview Files", folder),"/",
                          substring(glider_dep, regexpr("ru[0-9]{2}-*", glider_dep)),
                          "_Echogram_",day,".EV")
    
    # Save the file
    EVFile$SaveAs(EV_file_name)
    
    
    # Close the file
    EVFile$Close()
    
  }
}

EVAppObj$Quit()
