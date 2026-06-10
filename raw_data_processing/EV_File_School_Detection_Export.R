rm(list = ls())

library(tidyverse)
library(RDCOMClient)
library(tcltk)

source("./AZFP_acoustics_processing/misc/choose_directory.R")
source("./AZFP_acoustics_processing/misc/create_dir.R")

## Setup
glider_dep = choose_directory(caption = "Select the glider deployment directory folder")

EV_dir = paste0(glider_dep, "/Echoview Files/School Detection Tests")


EVAppObj = COMCreate('EchoviewCom.EvApplication')

for(folder in list.files(EV_dir, recursive = F, 
                         pattern = "[0-9]{6}", full.names = T, 
                         include.dirs = T)) {
  for(file in list.files(folder, full.names = T, pattern = "*.EV")) {
    # Open each file
    EVFile = EVAppObj$OpenFile(file)
    
    # Get the day of the month from the file name
    day = substr(file, nchar(file)-4, nchar(file)-3)
    
    # Create the export directory, if it does not already exist
    create_dir(gsub("Echoview Files", "Echoview CSV Export Files", folder))
    
    ### Copepod School Detection
    # (has to happen here after the seafloor is manually set)
    
    # playing with different school detection parameters
    EVAppObj$Exec("Properties SchoolsDistanceMode = | GPSDistance")
    EVAppObj$Exec("Properties SchoolsMaximumHorizontalLinkDistance = | 30.0")
    EVAppObj$Exec("Properties SchoolsMaximumVerticalLinkDistance = | 2.0")
    EVAppObj$Exec("Properties SchoolsMinimumCandidateHeight = | 5.0")
    EVAppObj$Exec("Properties SchoolsMinimumCandidateLength = | 5.0")
    EVAppObj$Exec("Properties SchoolsMinimumTotalHeight = | 2.0")
    EVAppObj$Exec("Properties SchoolsMinimumTotalLength = | 12.0")
    
    
    # copepod_schools = EVFile[["Variables"]]$FindByName("Minus 1")
    copepod_schools = EVFile[["Variables"]]$FindByName("Median filter 3x3 1")
    
    copepod_schools$DetectSchools("C. fin swarm", 0, -1, T)
    
    EVFile$Save()
    
    export_file_name_abundance = paste0(gsub("Echoview Files", "Echoview CSV Export Files", folder),
                              "/RMI_",substr(folder, nchar(folder)-5, nchar(folder)),
                              "_",day,"_455kHz_abundance_median.csv")
    ExpVar_abundance = EVFile[["Exporters"]]$FindByDynamicName('Sv integration [{AnalysisDomain}]')
    ExpVar_abundance$Export(export_file_name_abundance)
    
    export_file_name_aggregation = paste0(gsub("Echoview Files", "Echoview CSV Export Files", folder),
                                        "/RMI_",substr(folder, nchar(folder)-5, nchar(folder)),
                                        "_",day,"_455kHz_aggregation_median.csv")
    ExpVar_aggregation = EVFile[["Exporters"]]$FindByDynamicName('Aggregation analysis [{AnalysisDomain}]')
    ExpVar_aggregation$Export(export_file_name_aggregation)
    
    EVFile$Close()
  }
}

EVAppObj$Quit()
