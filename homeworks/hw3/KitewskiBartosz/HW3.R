library(giscoR)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyverse)


tmp <- tempfile()
download.file("https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_20M_2016_3035_LEVL_1.geojson", tmp)
nuts1 <- read_sf(tmp)

nuts_agg <- nuts1 %>% 
  filter(!NUTS_ID %in% c("FRY", "PT2", "PT3", "ES7") & CNTR_CODE != "TR") %>% 
  count(CNTR_CODE)

emission <- read.csv("https://ourworldindata.org/grapher/annual-co2-emissions-per-country.csv?v=1&csvType=full&useColumnShortNames=true")
emission <- emission %>% filter(Year == 2024)

nuts_agg[nuts_agg$CNTR_CODE == "EL", "CNTR_CODE"] <- "GR"
nuts_agg[nuts_agg$CNTR_CODE == "UK", "CNTR_CODE"] <- "GB"

nuts_agg$Code <- countrycode::countrycode(nuts_agg$CNTR_CODE, "iso2c", "iso3c")
nuts_agg$Name <- countrycode::countrycode(nuts_agg$CNTR_CODE, "iso2c", "country.name")

populacja <- read.csv("tps00001_page_linear.csv") #dane z eurostatu
populacja <- populacja %>% select(geo, OBS_VALUE)

df <- left_join(nuts_agg, emission, by=join_by(Code))
df <- left_join(df, populacja, by=join_by(Name==geo))

df_merc <- st_transform(df, 3857)
tmp <- df$emissions_total / df$OBS_VALUE
df_merc["Emisja dwutlenku węgla\n na 1 mieszkańca"] <- tmp

maxval <- max(tmp, na.rm = TRUE)
minval <- min(tmp, na.rm = TRUE)

zaznacz <- df_merc[tmp %in% c(maxval, minval), ]

centroids <- st_centroid(zaznacz) + c(2,2)

df_merc %>% ggplot() +
  geom_sf(mapping = aes(fill = `Emisja dwutlenku węgla
 na 1 mieszkańca`), color = "white")+
  theme_void() + 
  scale_fill_continuous(palette = "Oranges") +
  ggrepel::geom_label_repel(data = centroids,
                            aes(x = st_coordinates(centroids)[,1],
                                y = st_coordinates(centroids)[,2],
                                label = Name,
                                color = Name),
                            nudge_x = c(0, -10),
                            nudge_y = c(-5, 10), 
                            arrow = arrow(length = unit(0.02, "npc")),  
                            min.segment.length = 0,                     
                            segment.color = "navyblue",                      
                            segment.size = 0.6,
                            fill = alpha("white", 0.55),
                            size=3) +
  scale_color_manual(values = c("darkgreen", "red"), name = "", labels = c("minimalny", 'maksymalny')) +
  labs(x = "", y = "", title = "Wskaźnik emisji dwutlenku węgla na 1 mieszkańca \nw Europie w 2024 roku")
  

