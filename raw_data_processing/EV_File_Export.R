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

EV_dir = paste0(glider_dep, "/Echoview Files/")

# Needs to be manually changed depending on whether the AZFP is zoop or fish configured
# varlist = c('Surface and Bottom Exclusion T1',
#            'Surface and Bottom Exclusion T2',
#            'Surface and Bottom Exclusion T3')
# freqlist = c('38','120','200')

varlist = c('Surface and Bottom Exclusion T1',
           'Surface and Bottom Exclusion T2',
           'Surface and Bottom Exclusion T3',
           'Surface and Bottom Exclusion T4')
freqlist = c('120', '200', '455', '769')

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
    
    for(variable in varlist){
      # Index-match the frequency to the variable
      freq = freqlist[which(varlist == variable)]
      
      export_file_name = paste0(gsub("Echoview Files", "Echoview CSV Export Files", folder),
                                "/RMI_",substr(folder, nchar(folder)-5, nchar(folder)),
                                "_",day,"_",freq,"kHz.csv")
      
      EVVar = EVFile[["Variables"]]$FindByName(variable)
      
      EVVar$ExportIntegrationByCellsAll(export_file_name)
      
    }
    EVFile$Close()
  }
}

EVAppObj$Quit()
