#%%
from unicodedata import numeric
import numpy as np
import pandas as pd


# %%
punkt_przewoz_2010_18=pd.read_csv("resources/punkt_przewoz_2010_18.csv",sep=";")
# %%
punkt_przewoz_2010_18=punkt_przewoz_2010_18.dropna(axis=1,how="all")

# %%
from unidecode import unidecode
# %%
punkt_przewoz_2010_18.columns=['rok', 'Arriva', 'Koleje Dolnoslaskie', 'Koleje Malopolskie', 
 'Koleje Mazowieckie', 'Koleje Slaskie', 'Koleje Wielkopolskie', 
 'Leo Express', 'Lodzka Kolej Aglomeracyjna', 'PKP Intercity', 
 'PKP SKM', 'Polregio', 'SKM Warszawa', 'SKPL Cargo', 'UBB', 'WKD']
# %%

punkt_przewoz_2010_18.to_csv("resources/punkt_przewoz_2010_18.csv",index=False,encoding="utf-8")

# %%
punkt_przewoz_2019_24=pd.read_csv("resources/punkt_przewoz_2019-24.csv",sep=";",encoding="utf-8")

# %%
punkt_przewoz_2019_24=punkt_przewoz_2019_24.dropna(axis=1,how="all")

# %%
punkt_przewoz_2019_24.columns=['rok', 'miesiac', 'Arriva', 'Cargo Master', 'Ceske Drahy',
 'Koleje Dolnoslaskie', 'Koleje Malopolskie', 'Koleje Mazowieckie',
 'Koleje Slaskie', 'Koleje Wielkopolskie', 'Leo Express',
 'Lodzka Kolej Aglomeracyjna', 'ODEG Ostdeutsche',
 'Parowozownia Wolsztyn', 'PKP Cargo', 'PKP Intercity',
 'PKP SKM Trojmiasto', 'Polregio', 'RegioJet', 'SKM Warszawa',
 'SKPL Cargo', 'UBB', 'WKD']


# %%
punkt_przewoz_2019_24.to_csv("resources/punkt_przewoz_2019_24.csv",encoding="utf-8",index=False)
# %%
punkt_przewoz_2019_24.columns
# %%
punkt_przewoz_2025=pd.read_csv("resources/punkt_przewoz_2025.csv",sep=";",encoding="utf-8")

# %%
punkt_przewoz_2025.columns=['rok', 'miesiac', 'Arriva', 'Cargo Master', 'Ceske Drahy',
 'Koleje Dolnoslaskie', 'Koleje Malopolskie', 'Koleje Mazowieckie',
 'Koleje Slaskie', 'Koleje Wielkopolskie', 'Leo Express',
 'Lodzka Kolej Aglomeracyjna', 'ODEG Ostdeutsche',
 'Parowozownia Wolsztyn', 'PKP Cargo', 'PKP Intercity',
 'PKP SKM Trojmiasto', 'Polregio', 'RegioJet', 'SKM Warszawa',
 'SKPL Cargo', 'UBB', 'WKD']



# %%
punkt_przewoz_2025=punkt_przewoz_2025.iloc[:,2:].apply(lambda x: x.str.rstrip('%'))

# %%
punkt_przewoz_2025=punkt_przewoz_2025.apply(lambda x: x.str.replace(',', '.', regex=False))
# %%
punkt_przewoz_2025 = punkt_przewoz_2025.loc[:, ~punkt_przewoz_2025.isin(['#DZIEL/0!']).any()]

# %%
punkt_przewoz_2025 = punkt_przewoz_2025.apply(lambda x: x.astype(float))

# %%
punkt_przewoz_2025=punkt_przewoz_2025.apply(lambda row: np.round(np.mean(row),2))
# %%
punkt_przewoz_2025.to_csv("resources/punkt_przewoz_2025.csv",encoding="utf-8")
# %%
