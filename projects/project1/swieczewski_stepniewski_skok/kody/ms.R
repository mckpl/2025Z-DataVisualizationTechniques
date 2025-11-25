library(dplyr)
library(ggplot2)
library(sf)

dane <- read.csv("all_data.csv")

sport_cols <- dane %>% 
  select(-Region, -ends_with(".k"), -PKB_mieszkanca, -Liczba_ludnosci) %>% 
  colnames()

sport_cols_k <- dane %>% 
  select(ends_with(".k")) %>% 
  colnames()

dane <- dane %>%
  mutate(
    naj_sport = sport_cols[max.col(select(., all_of(sport_cols)), ties.method="first")],
    naj_sport_k = sport_cols_k[max.col(select(., all_of(sport_cols_k)), ties.method="first")]
  )

dane$naj_sport_k <- substr(dane$naj_sport_k, 1, nchar(dane$naj_sport_k) - 2)
dane <- dane[,c("Region","naj_sport","naj_sport_k")]

dane$Region <- tolower(dane$Region)
dane <- dane[dane$Region != "polska", ]

granice <- st_read("PRG_jednostki_administracyjne_2024/A01_Granice_wojewodztw.shp", quiet = TRUE)

granice_df <- granice %>%
  mutate(region_tolower = tolower(JPT_NAZWA_)) %>%
  left_join(dane, by = c("region_tolower" = "Region"))

centroidy <- st_centroid(granice_df)

sport_emoji <- function(sport) {
  case_when(
    sport == "Pilka_nozna" ~ "⚽",
    sport == "Siatkowka" ~ "🏐",
    sport == "Strzelectwo" ~ "🎯",
    sport == "Plywanie" ~ "🏊",
    TRUE ~ "❓"
  )
}

centroidy$emoji_m <- sport_emoji(centroidy$naj_sport)
centroidy$emoji_k <- sport_emoji(centroidy$naj_sport_k)

ggplot() +
  geom_sf(data = granice_df, fill = "#EAF6FF", color = "black", size = 0.4) +
  geom_sf_text(
    data = centroidy,
    aes(label = emoji_m),
    size = 8,
    color = "#3399FF",
    nudge_x = -0.3,
    nudge_y = -0.1
  ) +
  geom_sf_text(
    data = centroidy,
    aes(label = emoji_k),
    size = 8,
    color = "#FF69B4",
    nudge_x = 0.3,
    nudge_y = 0.1
  ) +
  labs(
    title = "Sporty trenowane najczęściej w województwach",
    subtitle = "Mężczyźni | Kobiety",
    caption = "Źródło: https://stat.gov.pl/obszary-tematyczne/\nkultura-turystyka-sport/sport"
  ) +
  coord_sf(
    xlim = c(14, 27.1),
    ylim = c(49, 55),
    expand = TRUE
  ) +
  annotate("text", x = 25, y = 51.25, label = "⚽ Piłka nożna", size = 5, hjust = 0) +
  annotate("text", x = 25, y = 51.75, label = "🏐 Siatkówka", size = 5, hjust = 0) +
  annotate("text", x = 25, y = 52.25, label = "🏊 Pływanie", size = 5, hjust = 0) +
  annotate("text", x = 25, y = 52.75, label = "🎯 Strzelectwo", size = 5, hjust = 0) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12))


