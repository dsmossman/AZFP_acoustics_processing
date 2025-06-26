# alignment_and_plotting

These files take estimated concentration/abundance data derived from the acoustics and format them in a way that makes plotting in R possible.

# Assumptions

1. You've already run the acoustic_abundance_estimates code.
2. The data are contained in a file structure that follows the format [name of deployment]/Derived Biomass Data (e.g. ru39-20230420T1636/Echoview CSV Export Files).
3. You also have data from a separate simultaneous glider deployment about whale detections.

# Steps

1. Run the Rutgers_Echosounder_Biomass_Processing.R file to create three different .rdata files: one for glider data, one for "peripheral" (bathymetry, wind farm lease areas, and marine mammal detections) data, and one for acoustic abundance estimate data.
2. Run the Rutgers_Echosounder_Biomass_Plotting.R file to create several different kinds of plots, including concentration/biomass by day, by depth and time, and by lat/long and time.

# Other Files

Zooplankton_FTLE_Correlation.R aligns the acoustic estimates of zooplankton abundance with corresponding values of FTLE (based on the work of Dr. Jackie Veatch). Zooplankton_Abundance_Modeling.R is for statistical investigations into correlating zooplankton abundance and other oceanographic variables. Zooplankton_Abundance_Plots.R does boxplots of abundance (both concentration and biomass) by season and shelf strata; Fish_Abundance_Plots.R will eventually do the same for fish abundance values. Zooplankton_Tows_Figures.R is for comparing the acoustic estimates to EcoMon and deployment/recovery zooplankton tows. choose_directory.R is a helper function for picking the folder with your data in it.
