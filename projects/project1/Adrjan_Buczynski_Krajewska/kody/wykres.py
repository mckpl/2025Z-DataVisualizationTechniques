# %%
import pandas as pd
import numpy as np
from main import *
import matplotlib.pyplot as plt
import geopandas as gpd
sql, cursor = parse_data()
# %%
count_raw = pd.read_sql_query(
    """
    SELECT AVG(lat) as lat, AVG(long) as long, COUNT(*) AS count, 
    SIMC, ULIC
    FROM budynki
    GROUP BY SIMC, ULIC
    """, sql
)
count = count_raw.loc[count_raw['count']<np.quantile(count_raw['count'], 0.99),:]
count_most = count_raw.loc[count_raw['count']>=np.quantile(count_raw['count'], 0.99),:]
# %%
plt.hist(count['count'], bins=max(count['count'])-1)
plt.title("Rozkład liczby budynków przy ulicy")
# %%
gpd.read_file("./data/Year_1914.shp").set_crs(epsg=3857).to_crs(epsg=4326).plot(color='white', edgecolor='black', alpha=0.9, linewidth=1)
plt.scatter(
    *count.loc[count['count']==1,['long', 'lat']].to_numpy().T,
    alpha=0.1,
    s=3
)
plt.ylim(49, 55)
plt.xlim(14, 24)
plt.title("Rozmieszczenie ulic o 1 budynku na tle granic z 1914")
# %%
gpd.read_file("./data/wojewodztwa.shp").set_crs(epsg=2180).to_crs(epsg=4326).plot(color='white', edgecolor='black', alpha=0.9, linewidth=1)
plt.scatter(
    *count_most.loc[:,['long', 'lat']].to_numpy().T,
    s=np.log10(count_most['count'])*2,
    alpha=0.1
)
plt.title("Rozmieszczenie ulic z największą liczbą budynków na tle granic województw")
# %%
gpd.read_file("./data/Year_1914.shp").set_crs(epsg=3857).to_crs(epsg=4326).plot(color='white', edgecolor='black', alpha=0.9, linewidth=1)
plt.scatter(
    *count_most.loc[:,['long', 'lat']].to_numpy().T,
    s=np.log10(count_most['count'])*2,
    alpha=0.1
)
plt.ylim(49, 55)
plt.xlim(14, 24)
plt.title("Rozmieszczenie ulic z największą liczbą budynków na tle granic z 1914")
# %%
papiez = pd.read_sql_query(
    """
    SELECT SIMC, ULIC
    FROM ulic
    WHERE (ulic.nazwa LIKE "%Jana Pawła%");
    """, sql
)
# %%
gpd.read_file("./data/wojewodztwa.shp").set_crs(epsg=2180).to_crs(epsg=4326).plot(color='white', edgecolor='black', alpha=0.9, linewidth=1)
plt.scatter(
    *papiez.merge(count_raw, how='inner').loc[:,['long', 'lat']].to_numpy().T,
    alpha=0.3,
    s=3
)
plt.title("Rozmieszczenie ulic Jana Pawła II (i I) na tle granic województw")
# %%
plt.scatter(
    *count.loc[:,['long', 'lat']].to_numpy().T,
    s=np.log10(count['count']),
    alpha=np.log10(count['count'])/(max(np.log10(count['count']))*100)
)
# %%
