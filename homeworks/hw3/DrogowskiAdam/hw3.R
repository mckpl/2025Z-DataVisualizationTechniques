library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

df <- read.csv("List of countries by carbon dioxide emissions.csv",
               skip = 6, header = TRUE)

names(df)[1] <- "Country"
df$CO2 <- as.numeric(df[[2]])

europe <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent == "Europe" | admin %in% c("Turkey", "Cyprus"))

map <- europe %>%
  left_join(df, by = c("admin" = "Country"))

max_label <- map %>%
  filter(!is.na(CO2)) %>%
  arrange(desc(CO2)) %>%
  slice(1) %>%
  st_centroid() %>%
  mutate(label = paste0("Najwyżesze:\n", admin, "\n", round(CO2, 1)))

min_label <- map %>%
  filter(!is.na(CO2), CO2 > 0) %>%       
  arrange(CO2) %>%
  slice(1) %>%
  st_centroid() %>%
  mutate(label = paste0("Najniższe:\n", admin, "\n", round(CO2, 1)))

ggplot(map) +
  geom_sf(aes(fill = CO2), color = "black", linewidth = 0.3) +
  scale_fill_gradient(
    low = "#ffcccc",   
    high = "#800000",  
    name = 
  ) +
  theme_void() +
  labs(
    title = "Emisje CO2 w Europie",
  ) +
  geom_label_repel(
    data = max_label,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    size = 4.5,
    fontface = "bold",
    fill = alpha("white", 0.85),
    color = "black",
    label.size = 1,
    box.padding = 0.4,
    point.padding = 0.4,
    seed = 43
  ) +
  
  geom_label_repel(
    data = min_label,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    size = 4.5,
    fontface = "bold",
    fill = alpha("white", 0.85),
    color = "black",
    label.size = 1,
    box.padding = 1,
    point.padding = 0.4,
    seed = 44
  ) +
  coord_sf(xlim = c(-25, 45), ylim = c(34, 72), expand = FALSE)

ggsave(
  filename = "mapa.png",
  plot = last_plot(),
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

# przepraszam że po czasie, chciałem zrobić coś ciekawszego ale mi nie wyszło :(
