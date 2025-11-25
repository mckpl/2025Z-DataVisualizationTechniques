library(eurostat)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(scales)
library(ggrepel)

dane <- get_eurostat(id = "tour_occ_nim", time_format = "date", filters = list(unit = "NR", c_resid = "FOR", nace_r2 = "I551-I553"))

dane_okresowe_2020 <- dane %>%
  filter(lubridate::year(time) == 2020) %>%
  group_by(panstwo = geo) %>%
  summarise(liczba_przyjazdow_w_okresie = sum(values, na.rm = TRUE)) %>%
  filter(liczba_przyjazdow_w_okresie > 0) %>%
  filter(nchar(as.character(panstwo)) == 2) %>%
  mutate(panstwo = case_when(
    panstwo == "EL" ~ "GR",           #rozne kody w danych z eurostatu niz docelowe na mapie
    panstwo == "UK" ~ "GB",
    TRUE ~ as.character(panstwo)
  ))

mapa_europy_sf <- ne_countries(scale = 'medium', returnclass = 'sf') %>%
  select(name, iso_a2, continent) %>%
  filter(continent == "Europe" | name %in% c("Turkey", "Cyprus")) %>%
  mutate(iso_a2 = case_when(name == "France" ~ "FR", name == "Norway" ~ "NO", name == "Kosovo" ~ "XK", TRUE ~ iso_a2))

mapa_z_danymi <- mapa_europy_sf %>%
  left_join(dane_okresowe_2020, by = c("iso_a2" = "panstwo")) %>%
  st_transform(3035)

panstwo_min <- mapa_z_danymi %>% filter(!is.na(liczba_przyjazdow_w_okresie)) %>% slice_min(liczba_przyjazdow_w_okresie, n = 1)
panstwo_max <- mapa_z_danymi %>% filter(!is.na(liczba_przyjazdow_w_okresie)) %>% slice_max(liczba_przyjazdow_w_okresie, n = 1)

etykiety_coords <- bind_rows(panstwo_min, panstwo_max) %>%
  mutate(
    X = st_coordinates(st_centroid(geometry))[,1],
    Y = st_coordinates(st_centroid(geometry))[,2],
    label_text = paste0(name, "\n", number(liczba_przyjazdow_w_okresie, scale_cut = cut_short_scale())),
    nudge_x = case_when(
      iso_a2 == "IT" ~ -600000,
      iso_a2 == "LI" ~ -900000,
      TRUE ~ 0
    ),
    nudge_y = case_when(
      iso_a2 == "IT" ~ -200000,
      iso_a2 == "LI" ~ 0,
      TRUE ~ 0
    )
  )

ggplot(data = mapa_z_danymi) +
  geom_sf(aes(fill = liczba_przyjazdow_w_okresie), color = "white", linewidth = 0.2) +
  geom_point(aes(x = 0, y = 0, color = "No data"), alpha = 0) +
  geom_label_repel(
    data = etykiety_coords,
    aes(x = X, y = Y, label = label_text),
    nudge_x = etykiety_coords$nudge_x,
    nudge_y = etykiety_coords$nudge_y,
    fill = alpha("white", 0.7),
    fontface = "bold",
    size = 3.5,
    box.padding = 0.5,
    segment.size = 0.8
  ) +
  scale_fill_gradientn(
    colors = c("#FFFFE5", "#78C679", "#31A354", "#005A32"),
    trans = "log10",
    na.value = "#e0e0e0",
    labels = label_number(scale_cut = cut_short_scale()),
    name = "Nights spent"
  ) +
  scale_color_manual(name = NULL, values = "transparent") +
  guides(
    fill = guide_colorbar(
      order = 1,
      barheight = unit(6, "cm"),
      barwidth = unit(0.5, "cm")
    ),
    color = guide_legend(
      order = 2,
      override.aes = list(shape = 15, size = 7, color = "#e0e0e0", alpha = 1)
    )
  ) +
  labs(
    title = "Tourism in Europe (2020)",
    subtitle = "Nights spent by foreign tourists (logarithmic scale)",
    caption = "Source: Eurostat [tour_occ_nim]"
  ) +
  coord_sf(xlim = c(2600000, 6500000), ylim = c(1350000, 5000000), datum = NA) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#f8f9fa", color = NA),
    panel.background = element_rect(fill = "#f8f9fa", color = NA),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10, margin = margin(b = 10)),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 20, b = 5)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#555555", margin = margin(b = 20)),
    plot.caption = element_text(size = 8, color = "#777777", hjust = 1, margin = margin(r = 10, b = 10))
  )

