# %%
import numpy as np
import pandas as pd
import plotly.express as px
# %%
df = pd.read_csv("stats.csv")
# %%
df["foreigners_percent"] = np.round((df["Foreigners"].astype("Int64"))/(df["Team_size"].astype("Int64")), 2)
# %%
filtered = df[(df["Club"]=="Jagiellonia Białystok") | (df["Club"]=="Pogoń Szczecin") | (df["Club"]=="Górnik Zabrze") | (df["Club"]=="Legia Warszawa") | (df["Club"]=="Lech Poznań")]
filtered = filtered[~filtered["Marketvalue"].isna()]
filtered = filtered.reset_index()
# %%
fig = px.line(
    filtered,
    x = "first_season_year",
    y  = "Marketvalue",
    color = "Club",
    color_discrete_map={
        "Jagiellonia Białystok": "#000000",
        "Górnik Zabrze": "#ffffff",
        "Pogoń Szczecin": "#ac1117",
        "Legia Warszawa": "#fb7b5b",
        "Lech Poznań": "#9b9b9b"
    },
    
)
fig.update_layout(
    plot_bgcolor='rgba(0,0,0,0)',
    paper_bgcolor='rgba(0,0,0,0)',
    title="Marketvalues of some of the best polish clubs",
    font_family = "Antonio",
    font_color = "white",
)
fig.update_xaxes(
    title="Year",
    tickmode='array'
    )
fig.update_yaxes(title="Marketvalue (€)")
fig.update_traces(line_width=4)
fig.show()
# %%
fig.write_image("wykres.svg")