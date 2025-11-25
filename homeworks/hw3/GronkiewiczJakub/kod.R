library(tidyverse)
library(sf)
library(eurostat)
library(giscoR)
library(scales)


regions <- gisco_get_nuts(
  nuts_level = 2, 
  year = "2021", 
  resolution = "20"
)

gdp_data_raw <- get_eurostat("nama_10r_2gdp")


gdp_processed <- gdp_data_raw %>%
  rename_with(tolower) %>%
  rename(time = any_of(c("time_period"))) %>%
  mutate(
    time_str = as.character(time),
    year_num = as.numeric(substr(time_str, 1, 4))
  ) %>%
  filter(unit == "EUR_HAB", year_num == 2021) %>%
  rename(gdp_euro = values) %>%
  select(geo, gdp_euro)

map_data <- regions %>%
  left_join(gdp_processed, by = c("NUTS_ID" = "geo")) %>%
  filter(!is.na(gdp_euro))


labels_nuts <- map_data %>%
  filter(gdp_euro == min(gdp_euro) | gdp_euro == max(gdp_euro)) %>%
  mutate(
    label_txt = paste0(NUTS_NAME, "\n", number(gdp_euro/1000, 0.1), " tys. €"),
    centr_x = st_coordinates(st_centroid(geometry))[,1],
    centr_y = st_coordinates(st_centroid(geometry))[,2]
  )


ggplot(data = map_data) +
  geom_sf(aes(fill = gdp_euro), color = NA) +
  geom_sf(data = gisco_get_countries(resolution = "20"), 
          fill = NA, color = "black", size = 0.2) +
  
  geom_point(data = labels_nuts, aes(x = centr_x, y = centr_y), 
             color = "black", size = 1.5) +
  geom_segment(data = labels_nuts,
               aes(x = centr_x, y = centr_y,
                   xend = centr_x - 2, yend = centr_y + 2), 
               color = "black", size = 0.6) +
  geom_label(data = labels_nuts,
             aes(x = centr_x - 2, y = centr_y + 2, label = label_txt),
             size = 3, fontface = "bold", fill = "white", alpha = 0.9) +
  
  scale_fill_viridis_c(
    option = "magma", 
    direction = -1, # ODWRÓCENIE SKALI KOLORÓW
    name = "PKB regionu (2021)\n(Euro na osobę)",
    labels = label_number(scale = 1/1000, suffix = "k")
  ) +
  
  coord_sf(xlim = c(-12, 45), ylim = c(34, 70)) +
  
  labs(
    title = "Dysproporcje regionalne w Europie (NUTS 2)",
    subtitle = "Najbogatszy vs najbiedniejszy region wg danych Eurostatu",
    caption = "Źródło: Eurostat [nama_10r_2gdp] | Geometria: giscoR",
    x = NULL, y = NULL
  ) +
  theme_void() +
  theme(legend.position = c(0.15, 0.2))

ggsave("mapa_eurosta.png")

