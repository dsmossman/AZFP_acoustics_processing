# The goal of this script is to facilitate exporting integrated Sv data from Echoview from all frequencies

# Author: Delphine Mossman
# Date Created: 23 Mar 2023
# Date Last Modified: 23 June 2025

# 1. Open each EV file
# 2. Locate the Sv variables to be exported
# 3. Export each variable and save it with a filename that includes the frequency

# Required libraries
# %%
import win32com.client
import os

# %%

# Define some variables
homedir = "C:/Users/Delphine/Box/Glider Data/"
dep_name = input('Enter the full name of the deployment: ')

workdir = os.path.join(homedir,dep_name,'')

# Open connection to Echoview
EvApp = win32com.client.Dispatch("EchoviewCom.EvApplication")

# Define the year and month to work with, and where the day files are found
AZFPyear = dep_name[5:9]
basedir = os.path.join(workdir,'Echoview Files', '')

# for folder in ['202409']:
for folder in next(os.walk(basedir))[1]:
    AZFPmonth = folder[-2:]
    filedir = os.path.join(workdir,'Echoview Files', str(AZFPyear) + str(AZFPmonth))

    for file in os.listdir(filedir):
    # for file in ['ru39-20230817T1520_Echogram_12.EV']:
        if file[-2:] == 'EV':


            EvFile = EvApp.OpenFile(os.path.join(filedir,file))
            day = file[-5:-3]

            # Needs to be manually changed depending on whether the AZFP is zoop or fish configured
            varlist = ['Surface and Bottom Exclusion T1',
                       'Surface and Bottom Exclusion T2',
                       'Surface and Bottom Exclusion T3']

            freqlist = ['38','120','200']

            # varlist = ['Surface and Bottom Exclusion T1',
            #            'Surface and Bottom Exclusion T2',
            #            'Surface and Bottom Exclusion T3',
            #            'Surface and Bottom Exclusion T4']
            #
            # freqlist = ['120', '200', '455', '769']

            exportpath = os.path.join(workdir, 'Echoview CSV Export Files',
                                      str(AZFPyear) + str(AZFPmonth.zfill(2)))

            if not os.path.exists(exportpath):
                os.makedirs(exportpath)

            for var in varlist:
            # for var in ['Surface and Bottom Exclusion T4']:
                freq = freqlist[varlist.index(var)]

                exportname = 'RMI_'+str(AZFPyear) + \
                             str(AZFPmonth.zfill(2)) + \
                             '_' + day + '_' + \
                            freq + \
                            'kHz.csv'

                EvVar = EvFile.Variables.FindByName(var)

                EvVar.ExportIntegrationByCellsAll(os.path.join(exportpath,exportname))


            EvApp.CloseFile(EvFile)
        else:
            next


EvApp.Quit()

# %%
