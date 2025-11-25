# %%
import pandas as pd
import numpy as np
from main import *
import matplotlib.pyplot as plt
import geopandas as gpd
conn, cursor = parse_data()

# %%
ulice = pd.read_sql("SELECT SIMC, nazwa FROM ulic", conn)
simc = pd.read_sql("SELECT SIMC, TERC FROM simc", conn)
woj = pd.read_sql("SELECT WOJ, NAZWA AS woj_nazwa FROM wojewodztwa", conn)

ulice_simc = pd.merge(ulice, simc, on='SIMC', how='left')

ulice_simc['WOJ'] = ulice_simc['TERC'].str[:2]  

ulice_simc = pd.merge(ulice_simc, woj, on='WOJ', how='left')

counts = ulice_simc.groupby(['woj_nazwa', 'nazwa']).size().reset_index(name='liczba')

c_sort = counts.sort_values(['woj_nazwa', 'liczba'], ascending=[True, False])
top = c_sort.groupby('woj_nazwa').head(4).reset_index()
top['pozycja'] = top.groupby('woj_nazwa').cumcount() + 1

# save to csv