
# Required libraries
import os
import shutil

# Directory where the AZFP monthly data are stored
dep_name = input("Enter the full name of the deployment: ")
basedir = os.path.join("H:/dm1679/Data/Glider Data/",dep_name,"AZFP Data","")

for folder in next(os.walk(basedir))[1]:
    datadir = os.path.join(basedir,folder,"")

    for file in os.listdir(datadir): # For each file in the month's directory
        if file[-3:] == 'LOG': # Because the log files and AZFP files have slightly different naming conventions, need to check what kind of file it is first
            folder_name = file[6:8] # Gets the day of the month from how the log file is named, which will be our folder name
            # print(f'dir name: {folder_name}')

            folder_path = datadir + folder_name # Gets the path of the day folder
            # print(f'folder path = {folder_path}')

            if not os.path.exists(folder_path): # If the folder doesn't already exist...
                os.makedirs(folder_path) # Make a new folder for the day

            if os.path.exists(folder_path): # If the folder DOES exist...
                file_path = datadir + file # Get the full path to our file
                # print(f'file path: {file_path}')
                shutil.move(file_path, folder_path) # Move the file into the folder

        elif not os.path.isdir(datadir + file) and file[-3:] != 'cfg': # Steps below are similar to above but for the proper acoustic files
            folder_name = file[4:6]
            # print(f'dir name: {folder_name}')
            folder_path = datadir + folder_name
            # print(f'folder path = {folder_path}')

            if not os.path.exists(folder_path):
                os.makedirs(folder_path)

            if os.path.exists(folder_path):
                file_path = datadir + file
                shutil.move(file_path, folder_path)

        elif file[-3:] == 'cfg': # cfg is our configuration file and needs to be copied to every folder
            for fldr in os.listdir(datadir)[:-1]: # once we reach this step, we should have nothing but the day folders and the cfg file remaining; this gets the day folder names
                file_path = datadir + file
                folder_path = datadir + fldr
                # print(folder_path)
                shutil.copy2(file_path, folder_path) # copy the cfg file into every day folder
            os.remove(file_path) # and remove it from the month folder