###############################################################################

# EV script 010 - New EV file using template, add data, create lines

# Example Echoview COM script originally submitted to the Echoview forum by Jeremy Holden

# Downloaded from www.echoview.com

# For assistance, contact Echoview support <support@echoview.com>

###############################################################################

# Adapted January 27th 2023 by Delphine Mossman

# The goal of this script is to automate the creation and import of new Echoview files for each day of AZFP data
# %%

# Required libraries
import win32com.client
import os
import re

# %%
# Define some variables

homedir = "H:/dm1679/Data/Glider Data/"
dep_name = input('Enter the full name of the deployment: ')

template = os.path.join(homedir,'AZFP_Fish_Template_2023.EV')
# template = os.path.join(homedir,'AZFP_Zooplankton_Template_2023.EV')

workdir = os.path.join(homedir,dep_name,'')

# workdir will have to be changed for each new glider deployment

# Open EV connection

EvApp = win32com.client.Dispatch("EchoviewCom.EvApplication")



# Define the year and month to work with, and where the day files are found
AZFPyear = dep_name[5:9]

basedir = os.path.join(workdir,'AZFP Data','')

for folder in next(os.walk(basedir))[1]:
# for folder in ['202410']:
    AZFPmonth = folder[-2:]
    daydir = os.path.join(workdir, 'AZFP Data',str(AZFPyear) + str(AZFPmonth),'')

    for day in os.listdir(daydir):
    # for day in ['21']:
        # Define where the AZFP and GPS/roll/pitch files are
        AZFPdir = os.path.join(daydir,day,'')

        CSVdir = os.path.join(workdir,'Echoview CSV Import Files','')
        # Open a new EV file using the template
        EvFile = EvApp.NewFile(template)

        # Add all AZFP files from within AZFPdir

        ## Specify file extension for AZFP data

        extension = '[.]01.$'

        ## Get list of files to add

        myfiles = [file for file in os.listdir(AZFPdir) if re.search(extension, file)]

        ## Create loop to add all AZFP data to EV file

        for file in myfiles:

            addfile ="".join([AZFPdir,file])

            EvFile.Filesets.Item(0).Datafiles.Add(addfile)

        # Import the calibration file

        EvFile.Filesets(0).SetCalibrationFile(os.path.join(workdir, 'AZFP Data',dep_name+'.ecs'))

        # Add the CSV files into their proper tabs

        ## Specify file extension for CSV data

        extension = 'csv'

        ## Get list of files to add

        myfiles = [file for file in os.listdir(CSVdir) if file.lower().endswith(extension)]

        ## Add into proper tabs; order is always GPS, then pitch, then roll

        ### GPS
        addfile = "".join([CSVdir,myfiles[0]])
        EvFile.Filesets.Item(1).Datafiles.Add(addfile)

        ### Pitch
        addfile = "".join([CSVdir,myfiles[1]])
        EvFile.Filesets.Item(2).Datafiles.Add(addfile)

        ### Roll
        addfile = "".join([CSVdir,myfiles[2]])
        EvFile.Filesets.Item(3).Datafiles.Add(addfile)

        ### Depth (replace M69_DEPTH line)

        # input('Go open and close the 38 kHz echogram, then hit Enter to continue.')

        #### Get the M69_DEPTH line
        EvLine = EvFile.Lines.FindByName("M69_DEPTH")

        #### Import the data.depth.evl file; default name for a line is 'data'
        addfile = "".join([CSVdir,'data.depth.evl'])
        EvFile.Import(addfile)

        #### Get the new depth line
        depth = EvFile.Lines.FindByName("data")

        #### Replace the M69_DEPTH line with the depth line
        EvLine.OverwriteWith(depth)

        #### Delete the orphan depth line
        EvFile.Lines.Delete(depth)

        ### Find bottom line
        # EvFile.Properties.LinePick.Algorithm = 2
        # EvFile.Properties.LinePick.StartDepth = 15
        # EvFile.Properties.LinePick.StopDepth = 45
        # EvFile.Properties.LinePick.MinSv = -70
        # EvFile.Properties.LinePick.UseBackstep = True
        # EvFile.Properties.LinePick.DiscriminationLevel = -50.0
        # EvFile.Properties.LinePick.BackstepRange = 0

        # myvar = EvFile.Variables.FindByName("AZFP: Sv pings (averaged) T1")
        # oldline = EvFile.Lines.FindByName("Editable bottom")

        # newline = EvFile.Lines.CreateLinePick(myvar,True)
        # newline = EvFile.Lines.FindByName("Line 1")

        # oldline.OverwriteWith(newline)
        # EvFile.Lines.Delete(newline)

        # Create a folder for the month (if it does not already exist) and define the file name
        folder_path = os.path.join(workdir,'Echoview Files',str(AZFPyear) + str(AZFPmonth.zfill(2)),'')
        if not os.path.exists(folder_path):
            os.makedirs(folder_path)

        myname = os.path.join(folder_path,'',dep_name+'_Echogram_' + day + '.EV')

        # Save the file
        EvFile.SaveAs(myname)

        # Close the file
        EvApp.CloseFile(EvFile)
    


# for day in os.listdir(daydir):
# # for day in ['20']:    
#     myname = workdir + \
#     'Echoview Files/' + \
#     str(AZFPmonth.zfill(2)) + '/' + \
#     day + '/' + \
#     'ru28-20220520T1425_Echogram_' + \
#     day + '.ev'
    
#     EvFile = EvApp.OpenFile(myname)
    
#     ### Create bottom line

#     #### Set various properties of the line

    
#     # Save the file
#     EvFile.SaveAs(myname)

#     # Close the file
#     EvApp.CloseFile(EvFile)
    

# Once you are all done, quit out of Echoview entirely
EvApp.Quit()

# %%
