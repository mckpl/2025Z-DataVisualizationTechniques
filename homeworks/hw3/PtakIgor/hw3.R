library(dplyr)
library(ggplot2)
library(sf)
library(eurostat)
library(giscoR)
library(ggrepel)
library(scales)
library(forcats)

try(while(!is.null(dev.list())) dev.off(), silent = TRUE)

# --- 1. DANE I GEOMETRIA ---
unemployment_raw <- get_eurostat("lfst_r_lfu3rt", time_format = "num")

unemployment_data <- unemployment_raw %>%
  filter(age == "Y15-74", sex == "T", unit == "PC", TIME_PERIOD == 2023) %>%
  select(geo, unemployment_rate = values) %>%
  distinct() %>%
  mutate(unemployment_rate = ifelse(unemployment_rate > 40, NA, unemployment_rate))

crs_europe <- 3035
nuts2 <- gisco_get_nuts(year = 2021, resolution = "10", nuts_level = 2) %>%
  st_transform(crs_europe) %>%
  filter(!grepl("^FRY", NUTS_ID))

countries_all <- gisco_get_countries(year = 2020, resolution = "10") %>%
  st_transform(crs_europe)

countries_no_data <- countries_all %>%
  filter(NAME_ENGL %in% c("Belarus", "Ukraine", "Moldova", "Russian Federation", "Bosnia and Herzegovina") | grepl("Kosov", NAME_ENGL))

# --- 2. PRZETWARZANIE ---
map_data <- nuts2 %>%
  left_join(unemployment_data, by = c("NUTS_ID" = "geo"))

breaks_vals <- c(0, 2.5, 5, 7.5, 10, 15, 25, 100)
labels_vals <- c("< 2,5%", "2,5–5,0", "5,0–7,5", "7,5–10,0", "10,0–15,0", "15,0–25,0", "> 25%")

map_data <- map_data %>%
  mutate(
    unemployment_cat = cut(unemployment_rate, breaks = breaks_vals, labels = labels_vals, include.lowest = TRUE, right = FALSE),
    unemployment_cat = fct_explicit_na(unemployment_cat, na_level = "Brak danych")
  )

valid_data <- map_data %>% filter(!is.na(unemployment_rate))
min_val <- min(valid_data$unemployment_rate, na.rm = TRUE)
max_val <- max(valid_data$unemployment_rate, na.rm = TRUE)

labels_df <- valid_data %>%
  filter(near(unemployment_rate, min_val) | near(unemployment_rate, max_val)) %>%
  mutate(
    coords = st_coordinates(st_point_on_surface(geometry)),
    X = coords[, 1], Y = coords[, 2],
    label_text = paste0(NAME_LATN, "\n", format(unemployment_rate, decimal.mark = ",", nsmall = 1), " %")
  )

# --- 3. RYSOWANIE MAPY ---
pal_cols <- c("#E0E0F5", "#BCBDDC", "#807DBA", "#FDBE85", "#FD8D3C", "#E6550D", "#A63603")
names(pal_cols) <- labels_vals
bbox <- c(xmin = 2600000, ymin = 1350000, xmax = 7600000, ymax = 5400000)

p <- ggplot() +
  geom_sf(data = countries_all, fill = "white", color = NA) +
  geom_sf(data = countries_no_data, fill = "#F0F0F0", color = "white") +
  geom_sf(data = map_data, aes(fill = unemployment_cat), color = "white", linewidth = 0.05) +
  geom_sf(data = countries_all, fill = NA, color = "grey50", linewidth = 0.2) +
  
  scale_fill_manual(
    values = c(pal_cols, "Brak danych" = "grey90"),
    name = "Stopa bezrobocia",
    guide = guide_legend(direction = "horizontal", title.position = "top", label.position = "bottom", keywidth = unit(5, "lines"), keyheight = unit(1.5, "lines"), nrow = 1)
  ) +
  
  geom_label_repel(
    data = labels_df, aes(x = X, y = Y, label = label_text),
    fontface = "bold", 
    size = 9,                       
    fill = alpha("white", 0.95), color = "#222222",
    label.padding = unit(0.4, "lines"), 
    box.padding = 0.8,              
    min.segment.length = 0, max.overlaps = Inf
  ) +

coord_sf(crs = crs_europe, xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
  
  labs(
    title = "Bezrobocie w regionach Europy (2023)",
    subtitle = "Poziom NUTS-2. Odfiltrowano błędy danych powyżej 40%.",
    caption = "Źródło: Eurostat [lfst_r_lfu3rt] | Autor: Igor Ptak"
  ) +
  
  theme_void() +
  theme(
    plot.margin = margin(50, 50, 50, 50),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 60, face = "bold", color = "#222222", hjust = 0.5),
    plot.subtitle = element_text(size = 36, color = "grey40", margin = margin(b = 30), hjust = 0.5),
    plot.caption = element_text(size = 20, color = "grey60", margin = margin(t = 30)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 28),
    legend.text = element_text(size = 24),
    legend.box.margin = margin(t = 40)
  )

# --- 4. ZAPIS ---
ggsave(
  filename = "mapa_bezrobocie_europa_2023.png",
  plot = p,
  width = 3000 / 72,
  height = 2400 / 72,
  dpi = 72,
  units = "in",
  bg = "white",
  limitsize = FALSE
)

