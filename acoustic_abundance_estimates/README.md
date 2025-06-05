# acoustic_abundance_estimates

These files take the cleaned acoustic data exported by Echoview and calculate species assignments, concentration, and biomass estimates. The result is a single .csv file for each day of the deployment with all frequencies combined and the aforementioned assignments/estimates.

# Assumptions

1. You've already run the raw_data_processing code.
2. The data are contained in a file structure that follows the format [name of deployment]/Echoview CSV Export Files/[monthly folder] (e.g. ru39-20230420T1636/Echoview CSV Export Files/202304).
3. You are in the MAB or a similar ecosystem where the dB differencing windows hold.

# Steps

1. Open the Echoview_Processing.m file.
2. Change the function in the final for loop to either Echoview_to_fish_biomass or Echoview_to_zoop_biomass, depending on the configuration of your echosounder.
3. Run the Echoview_Processing file.
4. Proceed to alignment_and_plotting.
