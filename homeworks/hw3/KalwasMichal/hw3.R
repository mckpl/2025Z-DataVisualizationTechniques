library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(patchwork)
library(leaflet)
library(geojsonio)
library(rnaturalearth)


  

CO2 <- read.csv("C:/TWD/PracaDomowa3/co2-emissions-per-capita.csv",sep= ",") %>% 
  filter(Year=="2022")
GDP <- read.csv("C:/TWD/PracaDomowa3/gdp-per-capita-worldbank.csv",sep= ",")%>% 
  filter(Year=="2022")

world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3, name, geometry)

CO2 <- GDP %>% 
  select(Entity,Code) %>% 
  right_join(CO2,by="Entity")

map_dataGDP <- left_join(world_map, GDP, by = c("iso_a3" = "Code")) %>% 
  drop_na()
map_dataCO2 <- left_join(world_map, CO2, by = c("iso_a3" = "Code")) %>% 
  drop_na()

co2pal <- colorNumeric(palette = c("#b2fdb9","darkgreen"),domain = map_dataCO2$Annual.CO..emissions..per.capita.)
gdppal <- colorNumeric(palette = c("#fdcd7c","red"),domain = map_dataGDP$GDP.per.capita..PPP..constant.2021.international...)



p <- leaflet(map_dataGDP) %>% 
  addTiles() %>% 
  addPolygons(
    data=map_dataGDP,
    fillColor = ~gdppal(GDP.per.capita..PPP..constant.2021.international...),
    group = "PKB per capita", 
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    popup = ~paste("PKB per capita (w USD):", round(GDP.per.capita..PPP..constant.2021.international...)),
  ) %>%
  addPolygons(
    data = map_dataCO2,
    fillColor = ~co2pal(Annual.CO..emissions..per.capita.),
    group = "Emisje CO2 per capita", 
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    popup = ~paste("Emisja C02 (w tonach):", round(Annual.CO..emissions..per.capita.,2)),
  ) %>%
  addLayersControl(
    baseGroups = c("PKB per capita","Emisje CO2 per capita"),
    options = layersControlOptions(collapsed = FALSE)
  )
