library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(ggrepel)
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("eurostat")
library(eurostat)
library(rnaturalearth)
library(rnaturalearthdata)

# Pomysł: mapa europy ze wskaźnikiem starzenia 
# (liczba ludzi w wieku 65+/liczba ludzi w wieku 0-14) * 100
# wykorzystam bibliotekę eurostat

df <- get_eurostat("demo_pjanind", time_format = "num")

# wskaźniki:
# PC_Y0_14 w indic_de - Odsetek populacji 0-14 lat
# PC_Y65_MAX w indic_de - Odsetek populacji 65+ lat
# trzeba wybrać jakiś rok
df %>%
  filter(indic_de %in% c("PC_Y0_14", "PC_Y65_MAX")) %>%
  group_by(TIME_PERIOD) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  View()

# jeżeli chodzi o ilość braków danych najlepiej wybrać 2019


df_clean <- df %>%
  filter(TIME_PERIOD == 2019,
         indic_de %in% c("PC_Y0_14", "PC_Y65_MAX")) %>%
  select(geo, indic_de, values) %>%
  pivot_wider(names_from = indic_de, values_from = values) %>%
  mutate(Aging_Index = (PC_Y65_MAX / PC_Y0_14) * 100)

# pobieram mapę z biblioteki 
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# filtruję kraje europy
europe_map <- world_map %>%
  filter(continent == "Europe" | admin %in% c("Turkey", "Cyprus"))


# poprawiam oznaczenia Eurostatu dla Wielkiej Brytanii i Grecji
df_clean <- df_clean %>%
  mutate(geo = case_when(
    geo == "EL" ~ "GR",
    geo == "UK" ~ "GB",
    TRUE ~ geo))

# łącze dane
map <- europe_map %>%
  left_join(df_clean, by = c("iso_a2_eh" = "geo"))

# znajduję skrajne wartości
min_val <- min(map$Aging_Index, na.rm = TRUE)
max_val <- max(map$Aging_Index, na.rm = TRUE)

# wybieram państwa ze skrajnymi wartościami
punkty_skrajne <- map %>%
  filter(!is.na(Aging_Index)) %>% 
  filter(Aging_Index == min_val | Aging_Index == max_val) %>%
  mutate(label_text = paste0(name, "\n", round(Aging_Index, 1)))


map_plot <- ggplot(data = map) +
  # państwa bez danych wypełniam na szaro
  geom_sf(data = filter(map, is.na(Aging_Index)),
          aes(color = "No Data"), fill = "grey80", linewidth = 0.2) +
  geom_sf(aes(fill = Aging_Index), color = "black", linewidth = 0.5) +
  scale_fill_distiller(palette = "RdYlBu",
                       direction = -1,
                       name = "Aging Index",
                       na.value = "grey80") + 
  scale_color_manual(name = NULL,
                     values = "white",
                     guide = guide_legend(override.aes = list(
                       color = "grey80"))) +
  geom_label_repel(data = punkty_skrajne,
                   aes(label = label_text, geometry = geometry),
                   stat = "sf_coordinates",
                   min.segment.length = 0,
                   box.padding = 0.5,
                   fontface = "bold",
                   fill = alpha("white", 0.8)) +
  coord_sf(xlim = c(-25, 45), ylim = c(34, 72), expand = FALSE) +
  labs(title = "Population Aging Index in Europe (2019)",
       subtitle = "Number of people aged 65+ per 100 children (0-14 years old)",
       colour = "") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#5d6d7e"),
    legend.box.margin = margin(l = 10),
    legend.title = element_text(face = "bold", size = 10, color = "#2c3e50"),
    legend.text = element_text(size = 9, color = "#5d6d7e"))

print(map_plot)

ggsave("Mapa_Wskaznik_Starzenia_w_Europie.png", plot = map_plot, width = 10, height = 8, dpi = 600, bg = "white")

