# misc

One-off files that aren't necessarily part of the main acoustic data processing pipeline but which are included here for completeness.

The .EV files are the templates used in raw_data_processing.

Acoustic_Modeling.R was used to fiddle with the target strength models of zooplankton.

Masking_testing.m was used to test whether dB windows were actually picking out zooplankton patterns or were just generating random noise.

NEFSC_processing.py and NJDEP_processing.py were both used to plot historical tow data in the MAB.

NOAA_Strata_Shapefile_Creation.R was used to create a custom three-level shapefile from the inshore/midshelf/offshore strata as assigned by the NES bottom trawl surveys.

RU39_20230817T1520_Filter_Check.R and RU39_20241021T1717_Pitch_Roll_Check.R were used to check oddities that appeared in the two referenced deployments.

TS_to_Sv.m was an experimental little file to try and convert threshold target strength values to threshold Sv values.

dB_Window_Biomass_Testing.R was written when I thought larvaceans had similar scattering properties to copepods (they don't, apparently they're gelatinous) to try and test different methods of differentiating the two.

intersectm.m and ismemberm.m are both used in the acoustic_abundance_estimates Matlab files.
