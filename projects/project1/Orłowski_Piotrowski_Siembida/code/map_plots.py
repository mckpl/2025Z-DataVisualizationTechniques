import numpy as np
import pandas as pd
import geopandas as gdp
import matplotlib.pyplot as plt

wojewodztwa = gdp.read_file("/Users/maciek/Desktop/twd_projekt_1/PRG_jednostki_administracyjne_2024/A01_Granice_wojewodztw.shp")
miasta = pd.read_csv("miasta2.csv")
mecze = pd.read_csv("poland_1.csv", sep=';')


wojewodztwa.loc[1, "JPT_NAZWA_"] = "małopolskie"
wojewodztwa.loc[3, "JPT_NAZWA_"] = "łódzkie"
wojewodztwa.loc[7, "JPT_NAZWA_"] = "dolnośląskie"
wojewodztwa.loc[8, "JPT_NAZWA_"] = "świętokrzyskie"
wojewodztwa.loc[11, "JPT_NAZWA_"] = "śląskie"
wojewodztwa.loc[15, "JPT_NAZWA_"] = "warmińsko-mazurskie"

wojewodztwa1=wojewodztwa.rename(columns={"JPT_NAZWA_" : "region"})


mecze_slimmed = mecze.drop(columns=["Unnamed: 0", "note", "IsCancelled", "IsWalkover"])
mecze_slimmed[["year", "month", "day"]] = mecze_slimmed.date.str.split("-", expand=True)
mecze_slimmed["winh"] = mecze_slimmed.apply(lambda x: 1 if x.gh > x.ga else 0, axis=1)
mecze_slimmed["wina"] = mecze_slimmed.apply(lambda x: 1 if x.gh < x.ga else 0, axis=1)

mecze_slimmed2 = mecze_slimmed.loc[pd.to_numeric(mecze_slimmed.year) >= 2000, :].reset_index(drop=True)

mecze2000_2010 = mecze_slimmed2.loc[((pd.to_numeric(mecze_slimmed2.year) >= 2000) & (pd.to_numeric(mecze_slimmed2.year) <= 2010)), :].reset_index(drop=True)
mecze2011_teraz = mecze_slimmed2.loc[(pd.to_numeric(mecze_slimmed2.year) > 2010), :].reset_index(drop=True)

home_wins_2010 = mecze2000_2010.groupby("home").agg(wins = ("winh", "sum"), games_count = ("winh", "count")).reset_index().rename(columns={"home" : "team"})
away_wins_2010 = mecze2000_2010.groupby("away").agg(wins = ("winh", "sum"), games_count = ("winh", "count")).reset_index().rename(columns={"away" : "team"})

home_wins_now = mecze2011_teraz.groupby("home").agg(wins = ("winh", "sum"), games_count = ("winh", "count")).reset_index().rename(columns={"home" : "team"})
away_wins_now = mecze2011_teraz.groupby("away").agg(wins = ("winh", "sum"), games_count = ("winh", "count")).reset_index().rename(columns={"away" : "team"})

match_count_2010 = pd.merge(home_wins_2010, away_wins_2010, how='inner', on="team")
match_count_2010["wins"] = match_count_2010.wins_x + match_count_2010.wins_y
match_count_2010["games_count"] = match_count_2010.games_count_x + match_count_2010.games_count_y
match_count2_2010 = match_count_2010.drop(columns=["wins_x", "games_count_x", "wins_y", "games_count_y"])

match_count_now = pd.merge(home_wins_now, away_wins_now, how='inner', on="team")
match_count_now["wins"] = match_count_now.wins_x + match_count_now.wins_y
match_count_now["games_count"] = match_count_now.games_count_x + match_count_now.games_count_y
match_count2_now = match_count_now.drop(columns=["wins_x", "games_count_x", "wins_y", "games_count_y"])


def add_region_col(df):

    df["region"] = df.team.apply(lambda x: 
                                                 "wielkopolskie" if x == "Amica Wronki" else 
                                                 "pomorskie" if x == "Arka Gdynia" else 
                                                 "małopolskie" if x == "Cracovia" else
                                                 "wielkopolskie" if x == "Dyskobolia Grodzisk Wielkopolski" else
                                                "łódzkie" if x == "GKS Bełchatów" else
                                                "śląskie" if x == "GKS Katowice" else
                                                "dolnośląskie" if x == "Górnik Polkowice" else
                                                "śląskie" if x == "Górnik Zabrze" else
                                                "lubelskie" if x == "Górnik Łęczna" else
                                                "podlaskie" if x == "Jagiellonia Białystok" else
                                                "świętokrzyskie" if x == "KSZO Ostrowiec Świętokrzyski" else 
                                                "świętokrzyskie" if x == "Korona Kielce" else 
                                                "wielkopolskie" if x == "Lech Poznań" else
                                                "pomorskie" if x == "Lechia Gdańsk" else
                                                "mazowieckie" if x == "Legia Warszawa" else
                                                "dolnośląskie" if x == "Miedź Legnica" else 
                                                "lubelskie" if x == "Motor Lublin" else
                                                "śląskie" if x == "Odra Wodzisław Śląski" else
                                                "śląskie" if x == "Piast Gliwice" else 
                                                "śląskie" if x == "Podbeskidzie Bielsko-Biała" else
                                                "zachodniopomorskie" if x == "Pogoń Szczecin" else
                                                "śląskie" if x == "Polonia Bytom" else
                                                "mazowieckie" if x == "Polonia Warszawa" else 
                                                "małopolskie" if x == "Puszcza Niepołomice" else
                                                "łódzkie" if x == "RKS Radomsko" else
                                                "mazowieckie" if x == "Radomiak Radom" else
                                                "śląskie" if x == "Raków Częstochowa" else
                                                "śląskie" if x == "Ruch Chorzów" else 
                                                "śląskie" if x == "Ruch Radzionków" else 
                                                "małopolskie" if x == "Sandecja Nowy Sącz" else
                                                "podkarpackie" if x == "Stal Mielec" else
                                                "warmińsko-mazurskie" if x == "Stomil Olsztyn" else
                                                "śląskie" if x == "Szczakowianka Jaworzno" else
                                                "małopolskie" if x == "Termalica Bruk-Bet Nieciecza" else
                                                "wielkopolskie" if x == "Warta Poznań" else
                                                "łódzkie" if x == "Widzew Łódź" else 
                                                "małopolskie" if x == "Wisła Kraków" else 
                                                "mazowieckie" if x == "Wisła Płock" else
                                                "dolnośląskie" if x == "Zagłębie Lubin" else
                                                "śląskie" if x == "Zagłębie Sosnowiec" else
                                                "kujawsko-pomorskie" if x == "Zawisza Bydgoszcz" else
                                                "łódzkie" if x == "ŁKS Łódź" else
                                                "dolnośląskie" if x == "Śląsk Wrocław" else
                                                "mazowieckie" if x == "Świt Nowy Dwór Mazowiecki" else x)
    
    return df


add_region_col(match_count2_2010)
add_region_col(match_count2_now)

cities_now = match_count2_now.merge(miasta, how='left', left_on='team', right_on="name")
cities_then = match_count2_2010.merge(miasta, how='left', left_on='team', right_on="name")

match_wins_pointer_2010 = match_count2_2010.groupby('region')[['wins', 'games_count']].sum().reset_index()
match_wins_pointer_2010["wins_percent"] = np.round((match_wins_pointer_2010.wins / match_wins_pointer_2010.games_count)*100, 2)

match_wins_pointer_now = match_count2_now.groupby('region')[['wins', 'games_count']].sum().reset_index()
match_wins_pointer_now["wins_percent"] = np.round((match_wins_pointer_now.wins / match_wins_pointer_now.games_count)*100, 2)

wojewodztwa_2010 = pd.merge(wojewodztwa1, match_wins_pointer_2010, how='left', on='region')
wojewodztwa_now = pd.merge(wojewodztwa1, match_wins_pointer_now, how='left', on='region')

def draw_map_plot(df):
    
    fig, ax = plt.subplots()

    df.plot(
        column="wins_percent", 
        legend=True,
        cmap = "Reds",
        vmin=40, vmax=60,
        edgecolor="#000000",
        linewidths=0.3,
        legend_kwds={
            "label": "Percent of wins"
        },
        missing_kwds={          
            "color": "#9B9B9B",
        }, ax=ax)

    cbar = ax.get_figure().axes[-1] 
    cbar.set_ylabel("%", 
                    color="#ffffff",
                    font="Antonio")
    for tick in cbar.get_yticklabels():
        tick.set_color("#FFFFFF")
        tick.set_font("Antonio")

    if df is wojewodztwa_now: 
        ax.set_title("Percent of wins in each region (2010-2025)",
                fontname="Antonio",
                color = "#ffffff")
        x = 1
    else:
        ax.set_title("Percent of wins in each region (2000-2010)",
                fontname="Antonio",
                color = "#ffffff")
        x = 2     

    ax.set_axis_off()

    ax.scatter(x = cities_now.long, y= cities_now.lat, s = 10, c = "#000000", alpha=0.7, label="Club")
    ax.scatter([], [], c="#9B9B9B", alpha=1, s=50, label="Region without clubs", marker="s", edgecolor="black", linewidth=0.3)

    ax.legend(loc="lower left", labelcolor="#ffffff", facecolor="none", edgecolor='none', handletextpad=0.01)

    plt.savefig(f"mapa{x}.png", transparent=True, dpi=1200)


draw_map_plot(wojewodztwa_now)
draw_map_plot(wojewodztwa_2010)