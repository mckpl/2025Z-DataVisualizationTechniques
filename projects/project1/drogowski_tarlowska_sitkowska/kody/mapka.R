library(sf)
library(dplyr)
library(ggplot2)
library(giscoR)

df <- read.csv("tabela_klastry_predkosc.csv")
punkty <- st_as_sf(df, coords = c("x", "y"), crs = 2180)

powiaty <- gisco_get_nuts(
  country = "PL",
  nuts_level = 3,
  resolution = "1"
)

punkty <- st_transform(punkty, st_crs(powiaty))

joined <- st_join(punkty, powiaty, join = st_within, left = TRUE)

agg <- joined %>%
  st_drop_geometry() %>%
  group_by(NUTS_NAME) %>%
  summarise(count = sum(clusterCount, na.rm = TRUE))

agg$count_clipped <- ifelse(agg$count > 20000, 20000, agg$count) # aby mniejsze wyniki miały znaczenie musiałem ograniczyć do 20k
# czasem klastry były po 59k i wtedy 3,8 wyglądało jak 10k, więc 

map_data <- powiaty %>%
  left_join(agg, by = "NUTS_NAME")

top5_regions <- agg %>% # top5 to nazwa robocza
  arrange(desc(count)) %>%
  slice(1:200) %>%
  pull(NUTS_NAME)

map_labels <- map_data %>% # numerki
  filter(NUTS_NAME %in% top5_regions) %>%
  mutate(label = count)


p <- ggplot(map_data) +
  geom_sf(aes(fill = count_clipped), color = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#ecc8bb", 
    high = "#c5647b",
    name = "Liczba zgłoszeń",
    na.value = "grey90",
    limits = c(0, 20000),  
    breaks = seq(0, 20000, by = 5000) 
  ) +
  labs(
    title = "Zgłoszenia o przekroczeniu prędkości wg powiatów",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) 

# tutaj dodałem te numery o których Pani pisała, jednak poszliśmy na kompromis niżej opisane jak

p <- p +
  geom_sf_text(
    data = map_labels,
    aes(label = label),
    color = "black",
    size = 3,
    fontface = "bold"
  )

ggsave("wykres.png",plot = p, bg = 'transparent')




p









# clean mapa która znajduje sie na plakacie

p <- ggplot(map_data) +
  geom_sf(aes(fill = count_clipped), color = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#ecc8bb", 
    high = "#c5647b",
    name = "Liczba zgłoszeń",
    na.value = "#ecc8bb",
    limits = c(0, 20000)
  ) +
  theme_void() + 
  theme(
    legend.position = "none",  
    panel.background = element_rect(fill = "transparent", color = NA),  
    plot.background = element_rect(fill = "transparent", color = NA), 
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("wykres.png", plot = p, bg = "transparent", width = 8, height = 6, dpi = 300)

# uznalismy ze najladniej bedzie podpisac pare wojewodztw (te na wykresie powyżej na plakacie), dzielac mape na wojewodztwa ta biblioteka robi 17
# i nie wygladalo tak ladnie jak powiaty, bo jednak wartosci mimo iz ogromne, byly w miare zblizone więc
# przy standaryzacji kolorki byly gorsze :(

wojewodztwa_counts <- map_data %>%
  st_drop_geometry() %>%
  group_by(NUTS_ID = substr(NUTS_ID, 1, 4)) %>%  
  summarise(total = sum(count, na.rm = TRUE)) %>%
  arrange(desc(total))

print(wojewodztwa_counts)

wojew_nazwy <- gisco_get_nuts(country = "PL", nuts_level = 2) %>%
  st_drop_geometry() %>%
  select(NUTS_ID, NUTS_NAME)

wojewodztwa_counts <- wojewodztwa_counts %>%
  left_join(wojew_nazwy, by = "NUTS_ID") %>%
  select(Wojewodztwo = NUTS_NAME, total) %>%
  arrange(desc(total))

wojewodztwa_counts











