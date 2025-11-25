library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(rnaturalearth)
library(sf)
library(ggrepel) 
library(viridis)

# dane z eurostat
life_expectancy <- read.csv("life_expectancy.csv", sep = ",", dec = ",")

life_expectancy1 <- life_expectancy %>% 
  mutate(sovereignt = substr(geo, 4, length(geo))) %>% 
  select(sovereignt, OBS_VALUE)

world <- ne_countries(scale = "medium", returnclass = "sf")
europe <- world[world$region_un == "Europe", ]

le_in_europe <- left_join(europe, life_expectancy1, by = "sovereignt") %>% 
  mutate(life_exp = as.numeric(OBS_VALUE)) %>% 
  select(sovereignt, geometry, life_exp)  

top3 <- le_in_europe %>%
  arrange(desc(life_exp)) %>%
  'head' (3)
  
bottom3 <- le_in_europe %>%
  arrange(life_exp) %>%
  'head' (3)

top3$centroid <- st_centroid(top3$geometry)
bottom3$centroid <- st_centroid(bottom3$geometry)

top3_coords <- st_coordinates(top3$centroid)
bottom3_coords <- st_coordinates(bottom3$centroid)

top3$lon <- top3_coords[,1]
top3$lat <- top3_coords[,2]

bottom3$lon <- bottom3_coords[,1]
bottom3$lat <- bottom3_coords[,2]
  

p1 <- ggplot(data = le_in_europe) +
  geom_rect(aes(xmin = -40, xmax = 50, ymin = 30, ymax = 75), fill = "#F0F8FF") +
  geom_sf(aes(fill = life_exp), color = "white", size = 0.2) +
  
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    na.value = "#E5E5E5",
    name = "Liczba lat"
  ) +
  
  geom_label_repel(
    data = top3,
    aes(x = lon, y = lat, label = paste0(sovereignt, "\n", round(life_exp, 1))),
    fill = "white",
    color = "#0B4068",
    fontface = "bold",
    size = 2.4,
    segment.color = "#0B4068",
    alpha = 0.9 
  ) +
  
  geom_label_repel(
    data = bottom3,
    aes(x = lon, y = lat, label = paste0(sovereignt, "\n", round(life_exp, 1))),
    fill = "#800000",
    color = "white",
    fontface = "bold",
    size = 2.4,
    segment.color = "#800000",
    alpha = 0.9
  ) +
  
  coord_sf(xlim = c(-23, 35), ylim = c(35, 70), expand = FALSE) + 
  
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "black", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 15, hjust = 0.5, color = "#333", margin = margin(b = 10)),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"), 
    legend.key.height = unit(0.3, "cm"),
    legend.title = element_text(vjust = 1, size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.margin = margin(
      t = 1,
      r = 0.5,
      b = 1,
      l = 0.5,
      unit = "cm"
  )) +
  labs(
    title = "Life Expectancy in Europe (2023)",
    subtitle = "Analysis of countries with the highest and lowest rates",
    fill = "Age (years)"
  )

ggsave("map.pdf", 
       plot = p1, 
       width = 10, 
       height = 9, 
       units = "in")


