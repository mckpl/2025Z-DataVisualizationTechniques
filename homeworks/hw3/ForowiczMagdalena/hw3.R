library(eurostat)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)


# przejrzenie dostępnych danych
toc <- get_eurostat_toc()
View(toc)
View(toc[toc$title == "Death due to suicide, by sex", ])

# dane
df <- get_eurostat("tps00122", time_format = "date")

data <- df %>% group_by(geo, sex) %>% 
  summarise(rate = mean(values)) %>% 
  filter(sex == "T") %>% 
  select(geo, rate)

# mapka
world <- ne_countries(scale = "medium", returnclass = "sf")

# polączone dane z mapką
map <- left_join(world, data, by = c("iso_a2_eh" = "geo")) %>% 
  select(name_long, iso_a2, rate, geometry)


min_country <- map %>%
  filter(rate == min(rate, na.rm = TRUE)) %>%
  mutate(centroid = st_centroid(geometry),
    x = st_coordinates(centroid)[,1],
    y = st_coordinates(centroid)[,2])

max_country <- map %>%
  filter(rate == max(rate, na.rm = TRUE)) %>%
  mutate(centroid = st_centroid(geometry),
    x = st_coordinates(centroid)[,1],
    y = st_coordinates(centroid)[,2])


ggplot(data = map) +
  geom_sf(aes(fill = rate), color = "black") +
  scale_fill_viridis_c(option = "magma", na.value = "#d3d3d3", direction = -1, name = "Wskaźnik") +
  geom_label(
    data = min_country,
    aes(x = x, y = y,
      label = paste0(name_long, "\n", round(rate, 1))),
    color = "black",
    fill = "white",
    fontface = "bold",
    size = 3,
    label.padding = unit(0.15, "lines"),
    label.size = 0.5) +
  geom_label(
    data = max_country,
    aes(x = x + 5,y = y - 1.5,
      label = paste0(name_long, "\n", round(rate, 1))),
    color = "black",
    fill = "white",
    fontface = "bold",
    size = 3,
    label.padding = unit(0.15, "lines"),
    label.size = 0.5) +
  coord_sf(xlim = c(-22, 45), ylim = c(35, 70)) +
  labs(title = "Wskaźnik samobójstw w krajach Unii Europejskiej",
       caption = "Źródło: Eurostat. Wskaźnik zgonów jest liczony na 100 000 mieszkańców i dostosowany do standardowego rozkładu wieku.",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5))

