# ## Formatting and plotting NJDEP abundance data
# 
# Used for ground-truthing glider data in the same area
# 
# Goal is to get a sense of fish biomass over the water column in the same area as the glider
# Also check the species that were collected
# In[]:


import pandas as pd
import os
from geopy import distance as dist
import xarray as xr
import matplotlib.pyplot as plt
import numpy as np
# import seaborn as sns

# In[]:
    
homedir = "C:\\Users\\dmossman\\Box\\ACOUSTIC DATA PROCESSING PROTOCOLS\\AZFP Processing\\NJDEP Trawl Data\\"    

tow_bio_data = pd.read_excel(os.path.join(homedir,'TowsCatchTable.xlsx'))
tow_metadata = pd.read_excel(os.path.join(homedir,'NJOceanTrawlField&StrataDescriptions.xlsx'))
spp_index = pd.read_excel(os.path.join(homedir,'SPPLIST.xlsx'))

# In[]:

def ddm2dd(ddm):
    
    deg = float(str(ddm)[:2])
    dec = float(str(ddm)[3:])/60
    
    if(deg < 1):
      dd = deg-dec
    else:
      dd = deg+dec
    
    return dd

# In[]:


# tow_bio_data = tow_bio_data[tow_bio_data['CRUCODE'] > 20220]
tow_bio_data = tow_bio_data.dropna(axis = 0).copy()


# In[]:


tow_bio_data['SLAT_DD'] = tow_bio_data['SLAT'].apply(ddm2dd)
tow_bio_data['ELAT_DD'] = tow_bio_data['ELAT'].apply(ddm2dd)

tow_bio_data['SLONG_DD'] = -tow_bio_data['SLONG'].apply(ddm2dd)
tow_bio_data['ELONG_DD'] = -tow_bio_data['ELONG'].apply(ddm2dd)


# In[ ]:

# group by tow ID number
test = tow_bio_data.groupby('ID',as_index = False)

# only need the first member of each group for distance calculations
test2 = test.first()

# In[]:

    
# get distance of each tow

test2['TOW_DISTANCE_KM'] = 0

for i in range(len(test2)):
    start = (test2.iloc[i,-5], test2.iloc[i,-3])
    end = (test2.iloc[i,-4], test2.iloc[i,-2])
    
    test2.loc[test2.index[[i]],'TOW_DISTANCE_KM']  = dist.distance(start, end).km
    
    del start, end

# In[]:

# put distance back into the original dataframe

tow_bio_data['TOW_DISTANCE_KM'] = 0

for j in range(len(test2)):
    tow_bio_data.loc[tow_bio_data['ID'] == test2['ID'][j],'TOW_DISTANCE_KM'] = test2['TOW_DISTANCE_KM'][j]
    
    
del test, test2
# In[]:

# Need to find tows near the glider in time/space

# first, import the glider data
dep_name = input('Enter the full name of the deployment: ')
gds = xr.open_dataset(os.path.join('C:/Users/dmossman/Box/Glider Data',dep_name,dep_name+'-profile-sci-delayed.nc'))

# find entries in tow_bio_data that are within the distance bounds of the glider data
# tow lat/long both need to be greater than glider start lat/long and less than glider end lat/long
dist_subset = tow_bio_data[(gds.longitude.actual_range[0] <= tow_bio_data['SLONG_DD']) & (gds.longitude.actual_range[1] >= tow_bio_data['ELONG_DD']) &
             (gds.latitude.actual_range[0] <= tow_bio_data['SLAT_DD']) & (gds.latitude.actual_range[1] >= tow_bio_data['ELAT_DD'])]

# find entries in tow_bio_data that are within the time bounds of the glider data
time_subset = tow_bio_data[(tow_bio_data['YRMODA'] >= 20220800) & (tow_bio_data['YRMODA'] <= 20220930)]

# In[]:

# Going to assume that all biomass in net tows came from fish

# How do I want to go about visualizing this?

total_biomass = tow_bio_data[tow_bio_data['CRUCODE'] > 20220].groupby('ID',as_index=False)
total_biomass = total_biomass['WEIGHT'].sum()

total_biomass_subset = total_biomass[(total_biomass['ID'].isin(dist_subset['ID'])) | (total_biomass['ID'].isin(time_subset['ID']))]

filename = os.path.join('C:/Users/dmossman/Box/Glider Data',dep_name,'Figures','Concurrent_Biomass_Plot.png')

total_biomass_subset.plot.bar(x='ID',
                               xlabel = 'Tow ID',
                               ylabel = 'Total Weight (kg)',
                               legend = False,
                               figsize=(12,8)).get_figure().savefig(filename)

# In[]:
    
# Examine total tow biomass for net tows in glider months of every year I have

tow_bio_data['YRMODA'] = tow_bio_data['YRMODA'].astype('string')

seasonal_biomass = tow_bio_data[(tow_bio_data.YRMODA.str[4:6] == '07') | (tow_bio_data.YRMODA.str[4:6] == '08')].groupby('ID',as_index=False)
seasonal_biomass = seasonal_biomass['WEIGHT'].sum()

seasonal_biomass.plot.bar(x = 'ID')

seasonal_biomass_subset = tow_bio_data[(tow_bio_data['CRUCODE'] > 20180) & ((tow_bio_data.YRMODA.str[4:6] == '07') | (tow_bio_data.YRMODA.str[4:6] == '08'))].groupby('ID',as_index=False)
seasonal_biomass_subset = seasonal_biomass_subset['WEIGHT'].sum()

filename = os.path.join('C:/Users/dmossman/Box/Glider Data',dep_name,'Figures','Summer_Biomass_Plot.png')

seasonal_biomass_subset.plot.bar(x='ID',
                               xlabel = 'Tow ID',
                               ylabel = 'Total Weight (kg)',
                               legend = False,
                               figsize=(12,8)).get_figure().savefig(filename)

# In[]:
    
# Examine biomass by species in one tow that was in the same year (not necessarily same area)

spp_biomass = tow_bio_data[tow_bio_data['ID'] == 20223075]

for k in range(len(spp_index)):
    spp_biomass.loc[spp_biomass['SPP'] == spp_index['SPP'][k], 'SPP'] = spp_index['COMMON'][k]

filename = os.path.join(homedir,'20223075_Species_Biomass_Plot.png')

fig = spp_biomass.plot.bar(
                     x = 'SPP',
                     y='NUMBER',
                     xlabel = 'Species',
                     ylabel = 'Contributed Number',
                     legend = False,
                     rot=45,
                     figsize = (13,13))
plt.xticks(ha="right")

fig.get_figure().savefig(filename)

# In[]:
    
# Examine biomass by species in several tows within same area but not necessarily
# during the same year as glider

dist_subset['YRMODA'] = dist_subset['YRMODA'].astype('string')

fish_index = ['ATLANTIC HERRING', 'ATLANTIC MACKEREL', 'ALEWIFE',
        'BLUEBACK HERRING', 'SILVER HAKE', 'STRIPED BASS', 'ATLANTIC MENHADEN',
        'UNCLASSIFIED SQUID','LONGFIN SQUID',
        'SPINY DOGFISH',
        'BUTTERFISH', 'BLUEFISH'
        ]

spp_seasonal_biomass = dist_subset.loc[((dist_subset.YRMODA.str[4:6] == '08') |
                                  (dist_subset.YRMODA.str[4:6] == '09')) &
                                  (dist_subset.CRUCODE > 20100)]

for k in range(len(spp_index)):
    spp_seasonal_biomass.loc[spp_seasonal_biomass['SPP'] == spp_index['SPP'][k], 'SPP'] = spp_index['COMMON'][k]

spp_seasonal_biomass = spp_seasonal_biomass[spp_seasonal_biomass['SPP'].isin(fish_index)]

filename = os.path.join('C:/Users/dmossman/Box/Glider Data',dep_name,'Figures','Summer_Species_Number_Plot.png')

spp_seasonal_biomass_pivot = pd.pivot_table(
    spp_seasonal_biomass,
    values='NUMBER',
    index='SPP',
    columns='ID',
    aggfunc=np.sum
)

spp_seasonal_biomass_pivot = np.log10(spp_seasonal_biomass_pivot)

fig=spp_seasonal_biomass_pivot.plot.bar(xlabel='Species',
                                     ylabel='Log10 Number Caught',
                                     legend=False,
                                     rot=45,
                                     figsize=(20,10))
plt.xticks(ha="right")

filename = os.path.join('C:/Users/dmossman/Box/Glider Data',dep_name,'Figures','Summer_Species_log10_Number_Plot.png')

fig.get_figure().savefig(filename)
