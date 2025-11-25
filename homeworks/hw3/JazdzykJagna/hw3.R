# dane ze strony https://www.fao.org/faostat/en/#data/QCL
# wybrane kategorie: countries-> all, elements-> Stocks, Items-> Live Animals-> Cattle, Years-> 2023

cattle_stocks<- read.csv("FAOSTAT_cattle_stock_2023.csv")

library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)


mapa_świata<-map_data('world')

# poprawiam, aby nazwy krajów w ramkach danych się pokrywały:

mapa_świata$region[mapa_świata$region == "Czech Republic"] <- "Czechia"
mapa_świata$region[mapa_świata$region == "Kosovo"] <- "Serbia"
mapa_świata$region[mapa_świata$region == "Ireland"] <- "United Kingdom of Great Britain and Northern Ireland"
mapa_świata$region[mapa_świata$region == "UK"] <- "United Kingdom of Great Britain and Northern Ireland"
cattle_stocks$Area[cattle_stocks$Area == "Netherlands (Kingdom of the)"]<-"Netherlands"

mapa_świata %>% 
  summarise(region = n())

cattle_stocks %>% 
  summarise(Area = n())

# zacieśniam rozważany obszar->
# w danych cattle_stock nie ma wielu krajów stąd zmniejszenie interesującego mnie obszaru

cattle_stocks<-cattle_stocks %>% 
  filter(Area %in% c(
    "Austria","Belgium","Bulgaria","Croatia","Luxembourg","Liechtenstein",
    "Denmark","Estonia","Finland","France","Germany","Greece","Andorra",
    "Hungary","Italy","Latvia","Lithuania",
    "Luxembourg","Netherlands","Norway","Poland","Portugal",
    "Romania","Slovakia","Spain","Sweden","Switzerland",
    "UK","Czechia","Slovenia","Bosnia and Herzegovina","Serbia","Albania","North Macedonia","Montenegro",
    "United Kingdom of Great Britain and Northern Ireland","Netherlands","Monco","Belarus","Ukraine"
  ))

mapa_świata<-mapa_świata %>% 
 left_join(cattle_stocks, by = c("region" = "Area"))

europe_1 <- mapa_świata %>% 
  mutate(Value = Value/1000)


europa_dane <- europe_1 %>% filter(!is.na(Value))

maxx_1 <- max(europa_dane$Value)
minn_1 <- min(europa_dane$Value)

kraj_max_1 <- europa_dane$region[europa_dane$Value == maxx_1][1]
kraj_min_1 <- europa_dane$region[europa_dane$Value == minn_1][1]

środek_lat_max_1 <- mean(europa_dane$lat[europa_dane$region == kraj_max_1])
środek_long_max_1 <- mean(europa_dane$long[europa_dane$region == kraj_max_1])

środek_lat_min_1 <- mean(europa_dane$lat[europa_dane$region == kraj_min_1])
środek_long_min_1 <- mean(europa_dane$long[europa_dane$region == kraj_min_1])

# sprawdzam i przypisuje zmiennym nazwy polskie-pod wykres

kraj_max_1[1]
kraj_min_1[1]
kraj_max_1 = "Francja"
kraj_min_1 = "Czarnogóra"

e_1<- ggplot()+
  geom_polygon(data = europe_1, aes(x = long, y = lat, group = group, fill = Value), 
               color = "white",
               linewidth = 0.5 )+
  
  coord_cartesian(xlim = c(-12, 35), ylim = c(35, 72)) +
  scale_x_continuous(breaks = seq(-10, 35, by = 10),
                     labels = function(x) {
    ifelse(x < 0, paste0(abs(x), "°W"), paste0(x, "°E"))}) +
  scale_y_continuous(breaks = seq(35, 75, by = 10),
                     labels = function(y) paste0(y, "°N")) +
  labs(title = "Liczebność bydła w krajach europejskch w roku 2023" ,
       fill = "Ilość w tys.",
       x = NULL,
       y = NULL)+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  geom_text(aes(x = środek_long_max_1- 0.3, y = środek_lat_max_1, label = kraj_max_1),
            size = 2, fontface = "bold", color = "white")+
  geom_text(aes(x = środek_long_min_1-2, y = środek_lat_min_1-0.5, label = kraj_min_1),
            size = 2, color = "black", fontface = "bold")+
  geom_point(aes(x = NA, y = NA, shape = "min"),
             color = "black", fill = "black", size = 3) +
  geom_point(aes(x = NA, y = NA, shape = "max"),
             color = "black", fill = "white", size = 3)+
  scale_shape_manual(
    name = "",
    values = c("max" = 21, "min" = 21),
    labels = c("max", "min")
  ) +
  guides(
    fill = guide_colorbar(title.position = "top"),
    shape = guide_legend(override.aes = list(size = 4))
  )+

  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue",
    na.value = "grey80"
  )+
  theme_minimal()+
  geom_point(aes(x =środek_long_max_1, y = środek_lat_max_1+0.5 ), color = "white", size = 1)+
  geom_point(aes(x =środek_long_min_1, y = środek_lat_min_1 ), color = "black", size = 1)

e_1
  