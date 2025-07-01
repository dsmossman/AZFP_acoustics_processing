# ## Formatting and plotting tow data from NEFSC tows

# Used to provide another source of ground-truthing for glider data

# Check species that were collected and biomass
#%%
    
# Import packages

import pandas as pd
import os
# from geopy import distance as dist
# import xarray as xr
# import matplotlib.pyplot as plt
# import numpy as np

#%%
    
homedir = "C:/Users/dmossman/Box/ACOUSTIC DATA PROCESSING PROTOCOLS/AZFP Processing/NEFSC Trawl Data/"    

tow_bio_data = pd.read_excel(os.path.join(homedir,'NEFSC_Bottom_Trawl_Spring.xlsx'),sheet_name="NEFSC_Bottom_Trawl_Spring")
spp_index = pd.read_excel(os.path.join(homedir,'NEFSC_Bottom_Trawl_Spring.xlsx'),sheet_name="Species_ID")

for k in range(len(spp_index)):
    tow_bio_data.loc[tow_bio_data['speciesID'] == spp_index['SPP'][k], 'speciesID'] = spp_index['COMMON'][k]
