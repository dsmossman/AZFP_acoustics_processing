# Setup

rm(list = ls())

library(tidyverse)
library(tcltk)

source("./AZFP_acoustics_processing/misc/choose_directory.R")
source("./AZFP_acoustics_processing/misc/create_dir.R")
source("./AZFP_acoustics_processing/acoustic_abundance_estimates/echoview_to_abundance_functions.R")

# Getting file paths set up

glider_dep = choose_directory() %>% substring(., regexpr("ru[0-9]{2}-*", .))
year = substr(glider_dep,6,9)

output_dir = paste0("C:/Users/dm1679/Box/Glider Data/",
                    glider_dep,
                    "/Derived Biomass Data/"
                    ,"School Detection Tests/"
)
create_dir(output_dir)

dep_start = as.numeric(readline("Enter the numerical month the deployment started: "))
dep_end = as.numeric(readline("Enter the numerical month the deployment ended: "))

# For each month
for(i in dep_start:dep_end) {
  
  # Get input folder and list of files
  input_dir = paste0("C:/Users/dm1679/Box/Glider Data/",
                     glider_dep,
                     "/Echoview CSV Export Files/",
                     "School Detection Tests/",
                     year,sprintf("%02d",i),"/")
  
  days = list.files(input_dir, pattern = "*_abundance_median.csv")
  # days = list.files(input_dir, pattern = "*200kHz.csv")
  
  if(length(days) == 0){
    next
  }
  
  # For each day
  for(j in 1:length(days)) {
    mo = i
    da = as.numeric(substr(days[j], 12, 13))
    
    # Calculate the abundance metrics
    echoview_to_zoop_abundance_aggregations(year, mo, da, input_dir, output_dir)
    # echoview_to_zoop_abundance(year, mo, da, input_dir, output_dir)
    # echoview_to_fish_class(year, mo, da, input_dir, output_dir)
  }
}
