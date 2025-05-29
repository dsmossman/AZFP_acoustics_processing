# Last updated: 15 June 2022

#####
rm(list = ls())


## Libraries

library(tidyverse)
library(R.utils)
library(readxl)
library(ZooScatR)
library(acousticTS)

## Project Structure
home_dir = "C:/Users/Delphine/Box/"


#####
## Target Strength Models

## ZooScatR version

DWBAapp() # launches the ZooScatR shiny app

# DWBA from ZooScatR *almost* matches DWBA from acousticTS; why the slight discrepancy?

## acousticTS version

# copepod parameters

# these are REALLY IMPORTANT and affect the TS in a major way
# Brandyn used g = 1.017 +/- 0.015 and h = 1.023 +/- 0.009

# Kitamura et al., 2017, copepod material properties
# collected in the Bering Sea; is that significant?
g = 1.005 # density contrast
h = 1.007 # sound speed contrast

cfin_length = c(2.2, 2.02, 2.38, 1.77, 2.63) # from stage 5 calanoid Cfin measurements from BoF 2020

cfin_L_a = 3.25 # from stage 5 calanoid Cfin measurements from BoF 2020

# orientation: either N(0,30) (Kitamura) or U(0,360) (Brandyn)

df_col_names = c(
  "Length (mm)",
  "g",
  "h",
  "model",
  "TS_120kHz",
  "TS_200kHz",
  "TS_455kHz",
  "TS_769kHz"
)

# large copepod (testing copepod material property values)

l_copepod_df = setNames(data.frame(matrix(ncol = length(df_col_names), nrow = 10)), df_col_names)

for(i in 1:length(cfin_length)) {
temp_DWBA = data.frame()
temp_SDWBA = data.frame()

for (theta in seq(0, 2 * pi, pi / 180)) {
  # uniform distribution of angle of incidence
  l_copepod = fls_generate(
    shape = "prolate_spheroid",
    length_body = cfin_length[i] * 10^-3,
    length_units = "m",
    length_radius_ratio = cfin_L_a,
    g_body = g,
    h_body = h,
    theta_body = theta
  )
  l_copepod_TS = target_strength(
    l_copepod,
    frequency = c(120e3, 200e3, 455e3, 769e3),
    model = c("DWBA", "SDWBA")
  )
  # plot(l_copepod_TS, type = "model")
  temp_DWBA = rbind(temp_SDWBA, l_copepod_TS@model$DWBA$TS)
  temp_SDWBA = rbind(temp_SDWBA, l_copepod_TS@model$SDWBA$TS)
}

l_copepod_df[i, ] = c(cfin_length[i], g, h, "DWBA", colMeans(temp_DWBA))
l_copepod_df[i+length(cfin_length), ] = c(cfin_length[i], g, h, "SDWBA", colMeans(temp_SDWBA))
}

# small copepods

s_copepod_L_a = 2.38 # from small copepods (Centropages and Oithona similis) in tows

s_copepod_length = c(0.76, 0.43, 1.09, 0.4, 1.41) # from small copepods (Centropages and Oithona similis) in tows

s_copepod_df = setNames(data.frame(matrix(ncol = length(df_col_names), nrow = 10)), df_col_names)

for(i in 1:length(s_copepod_length)) {
  temp_DWBA = data.frame()
  temp_SDWBA = data.frame()
  
  for (theta in seq(0, 2 * pi, pi / 180)) {
    # uniform distribution of angle of incidence
    s_copepod = fls_generate(
      shape = "prolate_spheroid",
      length_body = s_copepod_length[i] * 10^-3,
      length_units = "m",
      length_radius_ratio = s_copepod_L_a,
      g_body = g,
      h_body = h,
      theta_body = theta
    )
    s_copepod_TS = target_strength(
      s_copepod,
      frequency = c(120e3, 200e3, 455e3, 769e3),
      model = c("DWBA", "SDWBA")
    )
    # plot(s_copepod_TS, type = "model")
    temp_DWBA = rbind(temp_SDWBA, s_copepod_TS@model$DWBA$TS)
    temp_SDWBA = rbind(temp_SDWBA, s_copepod_TS@model$SDWBA$TS)
  }
  
  s_copepod_df[i, ] = c(s_copepod_length[i], g, h, "DWBA", colMeans(temp_DWBA))
  s_copepod_df[i+length(s_copepod_length), ] = c(s_copepod_length[i], g, h, "SDWBA", colMeans(temp_SDWBA))
}
