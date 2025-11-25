library(sf)
library(dplyr)
library(ggplot2)

# Wczytanie granic województw
woj_sf <- st_read("https://raw.githubusercontent.com/andilabs/polska-wojewodztwa-geojson/master/polska-wojewodztwa.geojson")

# Dane 2024
dane_lesistosc_2024 <- data.frame(
  wojewodztwo = c("dolnoslaskie","kujawsko-pomorskie","lubelskie","lubuskie",
                  "lódzkie","malopolskie","mazowieckie","opolskie",
                  "podkarpackie","podlaskie","pomorskie","slaskie",
                  "swietokrzyskie","warminsko-mazurskie","wielkopolskie","zachodniopomorskie"),
  lesistosc_pct = c(28.3,23.5,23.5,49.5,
                    21.4,28.6,23.4,26.7,
                    38.4,31.3,34.2,32.1,
                    28.3,31.9,25.8,35.9)
)

# Dane 2002
dane_lesistosc_2002 <- data.frame(
  wojewodztwo = c("dolnoslaskie","kujawsko-pomorskie","lubelskie","lubuskie",
                  "lódzkie","malopolskie","mazowieckie","opolskie",
                  "podkarpackie","podlaskie","pomorskie","slaskie",
                  "swietokrzyskie","warminsko-mazurskie","wielkopolskie","zachodniopomorskie"),
  lesistosc_pct = c(28.4,23.0,22.3,48.2,
                    20.6,28.4,22.0,26.2,
                    36.4,29.6,35.7,31.6,
                    27.0,29.7,25.3,34.5)
)

# Obliczenie różnicy
dane_lesistosc <- merge(
  dane_lesistosc_2002, dane_lesistosc_2024,
  by = "wojewodztwo",
  suffixes = c("_2002", "_2024")
) %>% 
  mutate(roznica = lesistosc_pct_2024 - lesistosc_pct_2002)

# Przedziały dla danych
progi <- c(-Inf, -1.0, 0.0, 0.5, 1.0, 1.5, 2.0, Inf)

etykiety <- c(
  "< -1.0",
  "[-1.0, 0)",
  "[0, 0.5)",
  "[0.5, 1.0)",
  "[1.0, 1.5)",
  "[1.5, 2.0)",
  "≥ 2.0"
)

# Łączenie mapy z danymi
mapa_roznica <- woj_sf %>%
  mutate(name = tolower(name)) %>%
  left_join(dane_lesistosc, by = c("name" = "wojewodztwo")) %>%
  mutate(
    przedzial = cut(
      roznica,
      breaks = progi,
      labels = etykiety,
      include.lowest = TRUE,
      right = FALSE
    )
  )

# Paleta kolorów

paleta <- c(
  "< -1.0"       = "#C45E68",  
  "[-1.0, 0)"    = "#F0A4A8",  
  "[0, 0.5)"     = "#F8FCF7",  
  "[0.5, 1.0)"   = "#E3F7DD",  
  "[1.0, 1.5)"   = "#B1E3AD",  
  "[1.5, 2.0)"   = "#5DC770",
  "≥ 2.0"        = "#1E8E3E"  
)
paleta2 <- c(
  "< -1.0"       = "#7F0000",  
  "[-1.0, 0)"    = "#f5a7a9",  
  "[0, 0.5)"     = "#e4e7e3",  
  "[0.5, 1.0)"   = "#c8d5c2",  
  "[1.0, 1.5)"   = "#8eb98a",  
  "[1.5, 2.0)"   = "#4a8f56",
  "≥ 2.0"        = "darkgreen" 
)

# Rysowanie mapy
mapa <- ggplot(mapa_roznica) +
  geom_sf(aes(fill = przedzial), color = "black", size = 0.1) +
  scale_fill_manual(
    values = paleta2,
    name = "Zmiana lesistości (%)",
    drop = FALSE,
    guide = guide_legend(reverse = TRUE)
  ) +
  theme_void() +
  labs(
    title = "Różnica lesistości między 2024 a 2002 rokiem",
    caption = "Źródło: Lasy Państwowe, GUS"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  )

# Zapisanie do png
ggsave(
  filename = "mapa_lesistosc_roznica_przedzialy.png",
  plot = mapa,
  width = 8, height = 6, dpi = 300,
  bg = "transparent"
)

