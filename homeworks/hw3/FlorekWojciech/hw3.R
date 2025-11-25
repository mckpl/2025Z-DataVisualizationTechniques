install.packages("rnaturalearthdata")
library(maps)
library(tidyr)
library(dplyr)
library(rnaturalearth)
library(ggplot2)
library(sf)
library(readr)
library(mapdata)
#kraje europy
kraje <- c("Albania","Andorra","Armenia","Austria","Azerbaijan","Belarus","Belgium","Bosnia and Herzegovina","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Georgia","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Liechtenstein","Lithuania","Luxembourg","Malta","Moldova","Monaco","Montenegro","Netherlands","North Macedonia","Norway","Poland","Portugal","Romania","San Marino","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Türkiye","Ukraine","United Kingdom","Vatican City")
#pobieranie mapy
shp <- st_read("https://gisco-services.ec.europa.eu/distribution/v2/countries/gpkg/CNTR_RG_10M_2024_3857.gpkg")
shp <- shp %>% filter(NAME_ENGL %in% kraje)
#dane
dane <- read.csv("C:\\Users\\wojte\\OneDrive\\Desktop\\projekty\\semestr3\\TWD\\ZadanieDomowe3\\tps00001_linear.csv", sep = ",")
unique(dane$DATAFLOW)#tylko 1
unique(dane$LAST.UPDATE)#tylko 1
unique(dane$freq)#tylko 1
unique(dane$indic_de)#tylko 1
unique(dane$CONF_STATUS)#tylko 1
#wywalamy te z tylko 1 wartością
dane <- dane %>% 
  select(c(geo,TIME_PERIOD,OBS_VALUE,OBS_FLAG))
#bierzemy najnowszy rok w danych
dane <- dane %>% 
  filter(TIME_PERIOD==2024) %>% 
  select(-OBS_FLAG)
unique(dane$geo)#sprawdzamy nazwy krajów
dane[14,1]#euro-area
dane[15,1]#euro-area
dane[19,1]#European Union
#usuwamy dziwne nazwy krajów
dane <- dane[-c(14,15,19), ]
#porównamy nazwy krajów w dane i shp
setdiff(unique(dane$geo), unique(shp$NAME_ENGL))  # kraje tylko w dane
setdiff(unique(shp$NAME_ENGL), unique(dane$geo))  # kraje tylko w shp
#wyznaczamy skrajne wartości
minmax <- shp %>% 
  filter(NAME_ENGL %in% c(dane$geo[which.min(dane$OBS_VALUE)],dane$geo[which.max(dane$OBS_VALUE)])) %>% 
  select(NAME_ENGL) %>% 
  pull(NAME_ENGL)
#czyszcze od notacji naukowej
options(scipen=100000000)
#dodajemy kolumne aby móc wypisać nazwy krajów o skrajnych wartościach
shp$labels = ifelse(shp$NAME_ENGL %in% minmax, shp$NAME_ENGL, "")
#znając geograficzne krańce europy (i zachodni kraniec islandii) wyznaczamy obszar mapy
shp <- st_crop(shp, st_transform(st_as_sfc(st_bbox(c(xmin = -25, ymin = 34, xmax = 69, ymax = 72), crs = st_crs(4326))), 3857))
#wykres
shp %>% 
  left_join(dane, by = join_by(NAME_ENGL == geo)) %>% 
  ggplot() + 
  geom_sf(aes(fill = OBS_VALUE),color = "black") + 
  theme_void() +
  scale_fill_gradient(low = "white", high = "red", na.value = "grey") + 
  labs(title = "Population in European countries 2024",fill="Population") +
  geom_sf_text(mapping=aes(label=labels))
