library(readr)
library(dplyr)
library(sf)
library(stringr)
library(ggplot2)

locale_do_wczytania <- locale(encoding = "UTF-8")

tryCatch({
  df_salary <- read_csv2("srednie wynagrodzenie.csv", locale = locale_do_wczytania)
  df_price <- read_csv2("srednia cena za m2.csv", locale = locale_do_wczytania)
}, error = function(e) {
  message("Wystąpił błąd podczas wczytywania plików CSV.")
  stop(e)
})


price_clean <- df_price %>%
  select(Kod, Nazwa, Cena_m2 = `ogółem;ogółem;2023;[zł]`) %>%
  mutate(Kod = as.character(Kod))

salary_clean <- df_salary %>%
  select(Kod, Wynagrodzenie = `ogółem;2023;[zł]`) %>%
  mutate(Kod = as.character(Kod))

df_final <- inner_join(price_clean, salary_clean, by = "Kod") %>%
  filter(Kod != "0") %>%
  filter(Cena_m2 > 0) %>%
  mutate(
    Stosunek_Wyn_Do_Ceny = Wynagrodzenie / Cena_m2
  ) %>%
  arrange(desc(Stosunek_Wyn_Do_Ceny))

tryCatch({
  mapa_sf <- sf::read_sf("powiaty-min.geojson")
}, error = function(e) {
  stop("Nie udało się wczytać pliku powiaty-min.geojson.")
})

clean_name_regex <- function(nazwa) {
  nazwa <- tolower(nazwa)
  nazwa <- stringr::str_replace(nazwa, "^powiat ", "")
  nazwa <- stringr::str_replace(nazwa, "^(m\\. st\\. |m\\. )", "")
  nazwa <- stringr::str_replace(nazwa, "( od 2013)$", "")
  nazwa <- stringr::str_trim(nazwa)
  return(nazwa)
}

df_final_clean <- df_final %>%
  mutate(clean_nazwa = clean_name_regex(Nazwa)) 

mapa_sf_clean <- mapa_sf %>%
  mutate(clean_nazwa = clean_name_regex(nazwa)) 

mapa_sf_clean <- mapa_sf_clean %>%
  mutate(
    clean_nazwa = if_else(
      clean_nazwa == "jeleniogórski", 
      "karkonoski", 
      clean_nazwa
    )
  )

mapa_finalna <- inner_join(
  mapa_sf_clean,
  df_final_clean,
  by = "clean_nazwa"
)


print("--- Wynik łączenia z GeoJSON ---")
unmatched_csv <- anti_join(df_final_clean, mapa_sf_clean, by = "clean_nazwa")
if (nrow(unmatched_csv) > 0) {
  print("--- Powiaty z CSV, których NIE udało się połączyć: ---")
  print(unmatched_csv %>% select(Nazwa, clean_nazwa))
} else {
  print(paste("Wszystkie", nrow(mapa_finalna), "powiaty zostały poprawnie połączone z geometrią."))
}


output_geojson_filename <- "mapa_finalna_z_danymi.geojson"
tryCatch({
  sf::st_write(mapa_finalna, output_geojson_filename, delete_dsn = TRUE)
  print(paste("Sukces! Zapisano połączone dane i mapę do:", output_geojson_filename))
}, error = function(e) {
  message("Wystąpił błąd podczas zapisywania pliku GeoJSON.")
  message(e)
})

breaks <- c(0, 0.5, 0.75, 1.0, 1.5, Inf) 

labels <- c("Krytyczny (poniżej 0.5 m²)", 
            "Niski (0.5 - 0.75 m²)", 
            "Średni (0.75 - 1.0 m²)", 
            "Wysoki (1.0 - 1.5 m²)", 
            "Bardzo wysoki (powyżej 1.5 m²)")

mapa_finalna <- mapa_finalna %>%
  mutate(
    Stosunek_Kategorie = cut(Stosunek_Wyn_Do_Ceny, 
                             breaks = breaks, 
                             labels = labels, 
                             right = FALSE, 
                             include.lowest = TRUE)
  )

mapa_z_kontrastem <- ggplot(data = mapa_finalna) +
  
  geom_sf(aes(fill = Stosunek_Kategorie),
          color = "black",
          linewidth = 0.2) +
  scale_fill_brewer(
    palette = "Blues",
    direction = -1,
    name = "Ile m² można kupić\nza 1 pensję:"
  ) +

  labs(
    title = "Dostępność mieszkań w Polsce (2023)",
    subtitle = "Stosunek średniego wynagrodzenia do średniej ceny 1 m² w powiatach",
    caption = "Źródło: GUS | Opracowanie: własne"
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "#f2f1ec", color = NA),   
    panel.background = element_rect(fill = "#f2f1ec", color = NA),   
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5, color = "#2c3e50", margin = margin(t = 20, b = 10)),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "#34495e", margin = margin(b = 20)),
    plot.caption = element_text(size = 10, hjust = 0.95, vjust = 1, color = "#7f8c8d"),
    
    legend.position = "right", 
    legend.key.size = unit(1.2, "cm"),      # Powiększenie kwadracików z kolorami
    legend.title = element_text(face = "bold", size = 14, lineheight = 1.2, color = "#2c3e50"), # Większy tytuł legendy
    legend.text = element_text(size = 12, color = "#2c3e50"),     # Większy opis kategorii
    legend.margin = margin(10, 20, 10, 0)   # Marginesy wokół legendy dla oddechu
  )

ggsave("mapa.png", 
       plot = mapa_z_kontrastem, 
       width = 11, 
       height = 10, 
       dpi = 300,
       bg = "white") 

print(mapa_z_kontrastem)
