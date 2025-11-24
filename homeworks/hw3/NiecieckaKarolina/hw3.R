install.packages("WDI")
install.packages("maps")
install.packages("mapdata")
install.packages("RColorBrewer")

library(dplyr)
library(ggplot2)
library(WDI)
library(maps)
library(mapdata)
library(RColorBrewer)

#WDIsearch(string ='SL.UEM.TOTL.ZS', field = 'indicator')
data <- WDI(indicator = 'SL.UEM.TOTL.ZS', start=2024, end=2024)
data$country <- recode(data$country,
                       "United States" = "USA",
                       "Russian Federation" = "Russia",
                       "Egypt, Arab Rep." = "Egypt",
                       "Iran, Islamic Rep." = "Iran",
                       "Yemen, Rep." = "Yemen",
                       "Venezuela, RB" = "Venezuela",
                       "Gambia, The" = "Gambia",
                       "Korea, Rep." = "South Korea",
                       "Korea, Dem. People's Rep." = "North Korea",
                       "Bahamas, The" = "Bahamas",
                       "Cabo Verde" = "Cape Verde",
                       "Congo, Dem. Rep." = "Democratic Republic of the Congo",
                       "Congo, Rep." = "Republic of Congo",
                       "Cote d'Ivoire" = "Ivory Coast",
                       "Czechia" = "Czech Republic",
                       "Egypt, Arab Rep." = "Egypt",
                       "Gambia, The" = "Gambia",
                       "Eswatini" = "Swaziland",
                       "Korea, Rep." = "South Korea",
                       "Korea, Dem. People's Rep." = "North Korea",
                       "Kyrgyz Republic" = "Kyrgyzstan",
                       "Lao PDR" = "Laos",
                       "Micronesia, Fed. Sts." = "Micronesia",
                       "St. Lucia" = "Saint Lucia",
                       "Slovak Republic" = "Slovakia",
                       "Turkiye" = "Turkey",
                       "Viet Nam" = "Vietnam",
                       "Virgin Islands (U.S.)" = "Virgin Islands",
                       "West Bank and Gaza" = "Palestine",
                       "Yemen, Rep." = "Yemen",
                       "United Kingdom" = "UK"
)

world_map <-  map_data('world')
world_data <- world_map %>% 
  left_join(data, by = c('region' = 'country')) %>% 
  rename(unemployment = SL.UEM.TOTL.ZS) %>% 
  filter(region != 'Antarctica') %>% 
  mutate(box = cut(
    unemployment,
    breaks = seq(0, 50, by = 5),     
    include.lowest = TRUE,
    right = FALSE ))

max_val <- world_data %>% 
  filter(unemployment == max(unemployment, na.rm = TRUE)) %>% 
  pull(region) %>% 
  unique()


min_val <- world_data %>% 
  filter(unemployment == min(unemployment, na.rm = TRUE)) %>% 
  pull(region) %>% 
  unique()

points <- data.frame(
  long = c(51.531, 31.1367 ),
  lat = c(25.2854, -26.3054),
  names = c('Katar - 0.12%', 'Eswatini - 34.4%'),
  stringsAsFactors = FALSE
)
text_points <- data.frame(
  long = c(51.531, 31.1367 ),
  lat = c(30.2854, -30.3054),
  names = c('Katar - 0.12%', 'Eswatini - 34.4%'),
  stringsAsFactors = FALSE
)

kolor <- RColorBrewer::brewer.pal(n=7, name = 'Blues')
mapa_bezrobocie <- ggplot()+
  geom_polygon(data = world_data, aes(x=long, y=lat, group = group, fill = box), color = 'black',linewidth = 0.1)+
  theme_minimal()+
  coord_fixed(1.3)+
  labs(title = 'Stopa bezrobocia na świecie w 2024 roku',
       fill='Stopa bezrobocia (%)')+
  scale_fill_manual(values = kolor, na.value = 'gray', labels = c("0-5%", "5-10%","10-15%", "15-20%", "20-25%", "25-30%","30-35%", "Brak danych"))+
  geom_point(data= points, aes(x=long, y=lat), color = 'black', size = 4)+
  geom_point(data= points, aes(x=long, y=lat), color = 'green', size = 3)+
  geom_label(data=text_points, aes(x=long, y=lat, label=names), fill = 'white', color='black')
ggsave("bezrobocie_na_swiecie.png", mapa_bezrobocie)





