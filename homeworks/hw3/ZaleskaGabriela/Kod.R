# potrzebne pakiety
library(readxl)
library(ggplot2)
library(maps)
library(dplyr)

# ustawienie sciezki
setwd("C:/Users/gabri/OneDrive/Pulpit/TechnikiWizualizacjiDanych/ZaleskaGabriela")

# dane zostaly pobrane ze strony https://www.netflix.com/tudum/top10/most-popular
netflix <- as.data.frame(read_excel("netflix.xlsx",
                  sheet = "Top10"))

# mozna latwo zmienic na inny film/serial
tytul <- "Stranger Things"

# wczytujemy dane wspolrzednych panstw
world_map <- map_data("world")


# liczymy dla kazdego kraju w ilu tygodniach w calym badanym okresie serial/film wybrany przez nas pojawil sie w rankingach
netflix_td <- netflix %>% 
  filter(show_title == tytul) %>% 
  group_by(country_name) %>% 
  summarise(numb=n()) %>% 
  arrange(numb)

# wspolrzedne potrzebne do zazanczenia punktow
cmin <- netflix_td[[1,1]]
cmax <- netflix_td[[nrow(netflix_td),1]]

max_n_country <- world_map %>% 
  group_by(region) %>% 
  summarise(long = mean(long), lat = mean(lat)) %>% 
  filter(region == cmax) 

min_n_country <- world_map %>% 
  group_by(region) %>% 
  summarise(long = mean(long), lat = mean(lat)) %>% 
  filter(region == cmin) 

points_df <- bind_rows(
  max_n_country %>% mutate(type = "Najwyższa liczba tygodni"),
  min_n_country %>% mutate(type = "Najniższa liczba tygodni")
)


# kraje ktorych nazwy sa rozne w danych wspolrzednych i danych netflix
netflix_td$country_name <- recode(netflix_td$country_name,
                                  'United States' = 'USA', 
                                  'United Kingdom' = 'UK',
                                  "Trinidad and Tobago" = "Trinidad")

# laczymy dane wspolrzednych panstw i netflix
map_data <- left_join(world_map, netflix_td, by = c("region" = "country_name"))

# wykres
ggplot(map_data, aes(x = long, y = lat, group = group, fill = numb)) +
  geom_polygon(color = NA) +  
  scale_fill_gradient2(low = "white", mid = "lightblue", high = "blue4", na.value = "transparent", midpoint = 30) +  
  coord_fixed(ratio = 1.1) +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "black", color = NA),   
    legend.box.background = element_rect(fill = "black", color = NA),
    legend.title = element_text(color = "white"),  
    legend.text  = element_text(color = "white"),
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank()
  ) +  
  ggtitle("Liczba tygodni, w których serial Stranger Things znajdował się w rankingu\nTop 10 Najpopularniejszych Seriali Netflixa dla danego kraju", subtitle = "w okresie 29.05.2022 - 09.11.2025") +  
  labs(fill = "Liczba tygodni") +
  geom_point(data = points_df, aes(x = long, y = lat, color = type), size = 4, inherit.aes = FALSE) +
  geom_text(
    data = points_df,
    aes(x = long, y = lat, label = region, color = type),
    vjust = -1,
    size = 3,
    inherit.aes = FALSE,
    color = "white",
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Najwyższa liczba tygodni" = "#8B0000",
      "Najniższa liczba tygodni" = "#FF6666"
    ) ) +
  labs(fill = "Liczba tygodni", color = " ") 