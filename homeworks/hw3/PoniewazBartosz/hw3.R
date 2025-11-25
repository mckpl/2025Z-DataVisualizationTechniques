# twd - praca domowa nr.3 , Bartosz Ponieważ, nr.339016
library(ggplot2)
library(dplyr)
library(maps)
library(ggrepel)


# wczytanie i przygotowanie surowych danych
raw_tea <- read.csv("tea-consumption-by-country-2022.csv")
world_map <- map_data("world")


#----------------------------------------------------------

# obróbka danych
tea <- raw_tea %>%
  select(country, value = TeaConsumptionAnnualPerCapita_2022) %>%
  filter(!is.na(value)) %>%
  mutate(country_matched = case_when( # żeby oznaczenia zgadzały się z tymi w world_map
    country == "United States" ~ "USA",
    country == "United Kingdom" ~ "UK",
    country == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
    country == "Republic of the Congo" ~ "Republic of Congo",
    country == "Antigua and Barbuda" ~ "Antigua",
    country == "Trinidad and Tobago" ~ "Trinidad",
    TRUE ~ country
  )) %>% 
  mutate(category = cut(value, 
    breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 100),
    labels = c("< 0.5 kg", "0.5 - 1 kg","1 - 1.5 kg", "1.5 - 2 kg","2 - 2.5 kg", "2.5 - 3 kg", "> 3 kg"),
    include.lowest = TRUE))

# łączenie danych i min/max wartości żeby znaleźć później skarjne obszary
tea_map <- left_join(world_map, tea, by = c("region" = "country_matched"))
min_val <- min(tea$value)
max_val <- max(tea$value)

# obliczanie środków krajów aby móc przyczepić etykiety
centroids <- world_map %>%
  group_by(region) %>%
  summarise(long = mean(range(long)), lat = mean(range(lat)))


#------------------------------------------------------------

# tworzenie etykiet dla skrajnych obszarów/państw
labels_data <- tea %>%
  filter(value == min_val | value == max_val) %>%
  inner_join(centroids, by = c("country_matched" = "region")) %>%
  mutate(
    nudge_x = case_when( # tutaj przesunięcia, ale przy większej rozdzielczości w zasadzie nie są aż tak potrzebne bo etykiety nie zasłaniają
      country == "Mexico" ~ -25,    
      country == "Nicaragua" ~ -25, 
      country == "Madagascar" ~ 20,   
      country == "Sri Lanka" ~ -25,    
      country == "Micronesia" ~ 10,   
      country == "Serbia" ~ 10,       
      TRUE ~ 0
    ),
    nudge_y = case_when(
      country == "Mexico" ~ -5,
      country == "Sri Lanka" ~ -13,
      country == "Madagascar" ~ -5,
      TRUE ~ 0
    )
  )


#------------------------------------------------------------------------------

# generacja mapy
tea_map_final <- ggplot(tea_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = category), color = "white", size = 0.1) +
  # stosuję skalę virdis bo jest czytelna dla wszystkich dobrze,
  # w tym dla daltonistów i po wydrukowaniu w czerni i bieli
  scale_fill_viridis_d( 
    name = "Spożycie roczne\n(na osobę)",
    na.value = "grey90",
    direction = -1
  ) +
  geom_label_repel(
    data = labels_data,
    aes(x = long, y = lat, label = paste0(country, "\n", value, " kg"), group = NULL),
    inherit.aes = FALSE,
    nudge_x = labels_data$nudge_x, #stosujemy ustawione przesuniecia 
    nudge_y = labels_data$nudge_y, #ale przy skalowaniu mapy i tak nie są zbytnio potrzebna
    
    #etykieta i linia łącząca etykiety z punktem centralnym aka centroidem
    segment.color = "black",  
    segment.size = 0.5,       
    min.segment.length = 0,   
    fill = "white",
    color = "black",
    fontface = "bold",
    size = 2,
    box.padding = 0.5
  ) +
  
  labs(
    title = "Spożycie herbaty per capita na świecie (2022 rok)",
    subtitle = "Etykiety wskazują wartości skrajne (max/min)",
    caption = "autor: Bartosz Ponieważ nr.339016"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = 0.5, color = "grey50")
  )

print(tea_map_final)

