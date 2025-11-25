import pandas as pd 
import numpy as np
import plotly.express as px 

data = {
    'sport_type' : ['Volleyball', 'Football', 'Athletics', 'Tennis', 'Cycling', 'Ski jumping', 'Running', 'Basketball', 'Handball', 'Swimming', 'Box', 'Speedway', 'Table tennis', 'Martial arts', 'Weightlifting'],
    'people_intrested' : [0.58, 0.53, 0.23, 0.21, 0.11, 0.11, 0.10, 0.08, 0.07, 0.06, 0.04, 0.04, 0.03, 0.02, 0.02]
}
df = pd.DataFrame(data)

fig1 = px.bar(df, y = "sport_type", x = "people_intrested", color_discrete_sequence=["#cf2810"], text=[f"{v:.0%}" for v in df.people_intrested])
fig1.update_layout(
    plot_bgcolor='rgba(0,0,0,0)',
    paper_bgcolor='rgba(0,0,0,0)',
    title = "Percentage of people intrested in particular sport",
    font_family = "Antonio",
    font_color = "white",
    font_size = 15,
    xaxis_tickformat=".0%")
fig1.update_yaxes(title="Type of sport",
                  ticks="outside", 
                  autorange="reversed")
fig1.update_xaxes(title="Percent of people")
fig1.update_traces(
    textfont_size=100,     
)
fig1.show()

fig1.write_image("wykres.svg")