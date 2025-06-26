# raw_data_processing

These files take the raw data offloaded from the glider-mounted AZFP and put them through Echoview in order to remove background and impulse noise, near- and far-field scattering, and below-seafloor scattering. The result is a set of csv files, one for every frequency for every day of the deployment.

# Assumptions

1. The data are initially organized into data for each month of the deployment (e.g. 202304 for data from April 2023).
2. The data are contained in a file structure that follows the format [name of deployment]/AZFP Data/[monthly folder] (e.g. ru39-20230420T1636/AZFP Data/202304)
3. Each folder of data also contains a copy of the .cfg file for the deployment.
4. You have downloaded a .csv file of the deployment that contains at minimum time/date/lat/long/depth/pitch/roll information about the platform.
5. You have an activated Echoview license.
6. You have an Echoview .ecs file in the AZFP Data folder.

# Steps

1. Run the AZFP_data_by_day.py file to organize the AZFP data into daily subfolders within the monthly folder.
2. Run the Echoview_File_Formatting_From_ERRDAP.m file to create specifically formatted time/date, pitch, roll, and depth files that Echoview can read and use.
3. Plug in your Echoview dongle, then run the EV_File_Using_Template.py file to generate an Echoview file from each day of data (my templates can be found in the misc folder).
4. Manually open each new EV file and set the seafloor using whatever algorithm best suits your data (unfortunately this step can't be automated in the code, at least as far as I can figure out).
5. Run the EV_File_Export.py file to export your processed data to .csv files - make sure that the frequencies being exported line up with the AZFP configuration!
6. Proceed to acoustic_abundance_estimates.
