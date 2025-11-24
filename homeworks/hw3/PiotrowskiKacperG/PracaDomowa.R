library(eurostat)
library(dplyr)
library(ggplot2)
library(sf)
library(ggrepel)
library(stringr)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(tidyr)
library(showtext)

font_add_google("Roboto Condensed", "roboto_c")
font_add_google("Roboto", "roboto")
font_add_google("Merriweather", "merriweather")
showtext_auto()

year_start <- 2004 # lata w ktorych odbywa sie pomiar
year_end <- 2023
id_zbioru_danych <- "nrg_ind_ren"

df_raw <- get_eurostat(id_zbioru_danych, time_format = "num", type = "label", lang = "en")

df_clean <- df_raw %>%
  mutate(TIME_PERIOD = as.integer(TIME_PERIOD)) %>%
  filter(
    TIME_PERIOD >= year_start,
    TIME_PERIOD <= year_end,
    str_detect(nrg_bal, "overall"),
    unit == "Percentage"
  ) %>%
  mutate(geo = as.character(geo)) %>%
  mutate(geo = case_when(
    geo == "Czechia" ~ "Czech Republic",
    geo == "Kosovo*" ~ "Kosovo",
    TRUE ~ geo
  )) %>%
  group_by(geo) %>%
  filter(!is.na(values)) %>%
  summarise(
    val_start = values[which.min(TIME_PERIOD)],
    val_end = values[which.max(TIME_PERIOD)]
  ) %>%
  ungroup() %>%
  mutate(Zmiana_PP = val_end - val_start) %>%
  select(geo, Zmiana_PP)

map_sf_raw <- ne_countries(scale = "medium", returnclass = "sf")

map_sf_raw <- st_transform(map_sf_raw, crs = 3035) 

map_sf <- map_sf_raw %>%
  select(name_long, continent, geometry) %>%
  rename(name = name_long) %>%
  filter(continent == "Europe" | name %in% c("Turkey", "Cyprus",
                                             "Vatican", "Monaco",
                                             "Liechtenstein", "Morocco",
                                             "Algeria", "Libya", "Tunisia")) %>% 
  # dodaje zeby mapa byla realistyczna
  select(name, geometry)

map_final_all <- map_sf %>%
  left_join(df_clean, by = c("name" = "geo"))

point_max <- map_final_all %>% filter(Zmiana_PP == max(map_final_all$Zmiana_PP, na.rm = TRUE))
point_min <- map_final_all %>% filter(Zmiana_PP == min(map_final_all$Zmiana_PP, na.rm = TRUE))

# wybieram maksymalne punkty z konwencja matematyczna dla ciaglej legendy
# czyli biore jeden maksymalny i minimalny, dla dyskretnej bylaby
# potencjalnie jeszcz szwecja

points_labels <- bind_rows(point_max, point_min) %>%
  mutate(
    label_txt = paste0(name, "\n(", scales::number(Zmiana_PP, accuracy = 0.1,
                                                   style_positive = "plus",
                                                   suffix = " pp"), ")")) %>%
  
  mutate(
    nudge_y = case_when(
      name == "Denmark" ~ 100000,
      name == "Kosovo" ~ 150000
    ),
    nudge_x = case_when(
      name == "Denmark" ~ -300000,
      name == "Kosovo" ~ 200000
    )
  ) %>% 
  st_centroid()

# przesuwam zeby nie zaslanialo bardzo innych panstw

target_box <- st_bbox(c(xmin = -15, xmax = 33.5, ymin = 32, ymax = 71), crs = 4326)
target_box_projected <- st_bbox(st_transform(st_as_sfc(target_box), 3035))

# ucinam manualnie zeby wylaczyc clipa i uzyc annotate dla kwadratu no data

map_cropped <- st_crop(map_final_all, target_box_projected)

p <- ggplot(data = map_cropped) +
  
  geom_sf(aes(fill = Zmiana_PP), color = "white", linewidth = 0.45) +
  
  scale_fill_gradientn(
    # colors = c("#d73027", "#f7f7f7", "#a1d99b", "#1a9850"),
    colors = c("#B85B14", "#FFC077", "#f7f7f7", "#C3ACD3", "#745D94"),

    # najlepsza paleta barw aby bylo CVD-safe
    
    values = scales::rescale(c(-5, -2.5, 0, 5, 30), from = c(-5, 30)),
    
    name = NULL,
    limits = c(-5, 30),
    breaks = c(-5, 0, 10, 20, 30),
    labels = scales::comma_format(suffix = " pp"),
    na.value = "gray90",
    oob = scales::squish,
    
    guide = guide_colorbar(
      direction = "vertical",
      barwidth = unit(2.7, "cm"),
      barheight = unit(36, "cm"),
      ticks.colour = NA,
      ticks.linewidth = 0,
      ticks = NA,
      frame.colour = NA,
      frame.linewidth = 0
    )
  ) +
  
  annotate("rect", xmin = 6757000, xmax = 6916000,
           ymin = 2108000, ymax = 2266000,
           fill="gray90",
           linewidth = 0.88) +
  
  
  annotate("text", label = "No data",
           x = 7087000, y = 2187000,
           size = 9.5, color="gray30",
           family="roboto_c") +
  
  coord_sf(
    xlim = c(target_box_projected["xmin"], target_box_projected["xmax"]),
    ylim = c(target_box_projected["ymin"], target_box_projected["ymax"]),
    expand = FALSE,
    datum = NA,
    clip = "off"
  ) +
  
  geom_text_repel(
    data = points_labels,
    aes(
      label = label_txt,
      geometry = geometry),
    
    nudge_x = points_labels$nudge_x,
    nudge_y = points_labels$nudge_y,
    stat = "sf_coordinates",
    
    box.padding = 2.0,
    point.padding = 2.0,
    
    min.segment.length = 0,
    segment.color = "gray20",
    segment.size = 2.0,
    
    bg.color = "white",
    bg.r = 0.15,
    color = "gray20",
    fontface = "bold",
    family = "roboto_c",
    size = 10,
  ) +
  
  labs(
    title = paste0("Renewable Energy Share Dynamics in Europe (", year_start, " - ", year_end, ")"),
    subtitle = "Change in share of gross final energy consumption (pp)",
    caption = "Source: Eurostat. Earliest and latest available data years were used for each country within the selected period."
  ) +
  
  theme_void() +
  theme(
    plot.margin = margin(t = 80, r = 40, b = 80, l = 80),
    
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    panel.background = element_rect(fill = "#eaf2f4", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    plot.title = element_text(
      family = "merriweather", 
      face = "bold",
      color = "black",
      size = 44, 
      hjust = 0, 
      margin = margin(b = 20)
    ),
    
    plot.subtitle = element_text(
      family = "roboto", 
      face = "italic",
      color = "gray30",
      size = 32, 
      hjust = 0, 
      margin = margin(b = 60)
    ),
    
    plot.caption = element_text(
      family = "roboto_c",
      color = "gray50", 
      size = 25, 
      hjust = 0, 
      margin = margin(t = 40)
    ),
    
    legend.position = "right",
    legend.margin = margin(l = 100),
    legend.text = element_text(family = "roboto_c", size = 27,
                               hjust = 1, color = "gray30",
                               margin = margin(l = 20)),
    legend.key = element_blank(),
    legend.justification = c(0, 0.53)
  )

# musialem sporo pozmieniac z rozmiarami bo mapka sie rozjedzala
# (jakis blad z R dotyczacy zapisu, innego dnia zapisywalo sie inaczej
# po zmianie koloru na CVD-safe)
# rowniez wystepuje problem z geom_text_repel dla dpi=300
# tekst sie rozjezdza

print(p)

ggsave("map.png", plot = p, width = 3000, height = 2400, dpi = 72, units="px", bg = "white")
