# ------------------ 1. pakiety -------------------------

library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(stringr)
library(ggpattern)
library(ggrepel)
library(ggtext) 

# ------------------ 2. czyszczenie i poprawki w danych -------------------------

dane <- read_delim("dane/MARKI_POLACZONE_DATA.csv", delim = ";")

lista_poprawek <- c(
  "DAIMLERCHRYSLER" = "MERCEDES-BENZ",
  "MERCEDES" = "MERCEDES-BENZ",
  "RENAULT/CARPOL" = "RENAULT",
  "MERCEDES-AMG" = "MERCEDES-BENZ",
  "FORD-CNG-TECHNIK" = "FORD",
  "TOYOTA/CARPOL" = "TOYOTA",
  "VOLKSWAGEN/CARPOL" = "VOLKSWAGEN",
  "DAIMLER-BENZ" = "MERCEDES-BENZ",
  "CITROEN/CARPOL" = "CITROEN"
)

mapa_krajow <- c(
  "TOYOTA" = "Japonia",
  "VOLKSWAGEN" = "Niemcy",
  "AUDI" = "Niemcy",
  "FORD" = "USA",
  "OPEL" = "Niemcy",
  "BMW" = "Niemcy",
  "SKODA" = "Czechy",
  "HYUNDAI" = "Korea Południowa",
  "MERCEDES-BENZ" = "Niemcy",
  "KIA" = "Korea Południowa",
  "RENAULT" = "Francja",
  "PEUGEOT" = "Francja",
  "VOLVO" = "Szwecja",
  "CITROEN" = "Francja",
  "NISSAN" = "Japonia",
  "SEAT" = "Hiszpania",
  "DACIA" = "Rumunia",
  "FIAT" = "Włochy",
  "MAZDA" = "Japonia",
  "SUZUKI" = "Japonia",
  "HONDA" = "Japonia",
  "LEXUS" = "Japonia",
  "MITSUBISHI" = "Japonia",
  "JEEP" = "USA",
  "CUPRA" = "Hiszpania",
  "MINI" = "Wielka Brytania",
  "CHEVROLET" = "USA",
  "ALFA ROMEO" = "Włochy",
  "PORSCHE" = "Niemcy",
  "DODGE" = "USA",
  "SUBARU" = "Japonia",
  "LAND ROVER" = "Wielka Brytania",
  "MG" = "Chiny",
  "TESLA" = "USA",
  "CHRYSLER" = "USA",
  "JAGUAR" = "Wielka Brytania",
  "SSANGYONG" = "Korea Południowa",
  "SMART" = "Niemcy",
  "DS" = "Francja",
  "OMODA" = "Chiny",
  "SAAB" = "Szwecja",
  "BAIC" = "Chiny",
  "LANCIA" = "Włochy",
  "JAECOO" = "Chiny",
  "DAIHATSU" = "Japonia",
  "BYD" = "Chiny",
  "MASERATI" = "Włochy",
  "INFINITI" = "Japonia",
  "BUICK" = "USA",
  "RAM" = "USA",
  "CADILLAC" = "USA",
  "KG MOBILITY" = "Korea Południowa",
  "LEAPMOTOR" = "Chiny",
  "VAUXHALL" = "Wielka Brytania",
  "BENTLEY" = "Wielka Brytania",
  "DAEWOO" = "Korea Południowa",
  "FERRARI" = "Włochy",
  "LINCOLN" = "USA",
  "LADA" = "Rosja",
  "FORTHING" = "Chiny",
  "ROVER" = "Wielka Brytania",
  "LAMBORGHINI" = "Włochy",
  "ASTON MARTIN" = "Wielka Brytania",
  "PONTIAC" = "USA",
  "ACURA" = "Japonia",
  "HUMMER" = "USA",
  "DFSK" = "Chiny",
  "GMC" = "USA"
)

df_poprawek <- data.frame(MARKA = names(lista_poprawek), MARKA_CLEAN = lista_poprawek, row.names = NULL)
df_krajow <- data.frame(MARKA_CLEAN = names(mapa_krajow), KRAJ_POCHODZENIA = mapa_krajow, row.names = NULL)

# ---------------------------- 3. dane do mapy - final -------------------------

dane_finalne_do_mapy <- dane %>%
  left_join(df_poprawek, by = "MARKA") %>%
  mutate(MARKA_CLEAN = coalesce(MARKA_CLEAN, MARKA)) %>%
  left_join(df_krajow, by = "MARKA_CLEAN") %>%
  filter(!is.na(KRAJ_POCHODZENIA)) %>%
  group_by(WOJEWODZTWO, MARKA_CLEAN, KRAJ_POCHODZENIA) %>%
  summarise(Total_Liczba_Marki = sum(LICZBA, na.rm = TRUE), .groups = 'drop') %>%
  group_by(WOJEWODZTWO) %>%
  slice_max(order_by = Total_Liczba_Marki, n = 1, with_ties = FALSE) %>%
  ungroup()

wojewodztwa_sf <- st_read("wojewodztwa-max.geojson")
nazwa_kolumny_w_mapie <- "nazwa"

wojewodztwa_sf_std <- wojewodztwa_sf %>%
  mutate(WOJEWODZTWO_JOIN = str_to_upper(!!sym(nazwa_kolumny_w_mapie)))

dane_do_mapy_std <- dane_finalne_do_mapy %>%
  mutate(WOJEWODZTWO_JOIN = str_to_upper(WOJEWODZTWO))
mapa_data <- wojewodztwa_sf_std %>%
  left_join(dane_do_mapy_std, by = "WOJEWODZTWO_JOIN")



# ---------------------------- 4. mapa -------------------------

font_rodzina <- "Arial"
kolor_tla <- "transparent"

mapa_plot <- ggplot(data = mapa_data) +
  geom_sf(aes(fill = MARKA_CLEAN), color = "white", size = 0.3) +
  scale_fill_manual(
    values = c(
      "VOLKSWAGEN" = "#222222",  
      "AUDI"       = "#555555",        
      "OPEL"       = "#888888",        
      "TOYOTA"     = "#69c1ff",   
      "SUZUKI"     = "#69c1ff",      
      "MAZDA"      = "#69c1ff"        
    ),
    breaks = c("VOLKSWAGEN", "AUDI", "OPEL", "TOYOTA", "SUZUKI", "MAZDA"),
    na.value = "grey90",
    name = "Dominująca marka:"  
  ) +
  labs(
    title = paste0("Bitwa Gigantów: <span style='color:\"#666666\";'>Niemcy</span> vs <span style='color:\"", color_japan_main, "\";'>Japonia</span>"),
    subtitle = "Dominująca marka wsród nowo rejestrowanych samochodów \nw każdym województwie (Dane 2021-2024)",
    caption = NULL
  ) +
  theme_void() +
  theme(
    text = element_text(family = font_rodzina, color = "black"),  
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_markdown(  
      size = 22,
      face = "bold",  
      color = "black",  
      hjust = 0.5,  
      margin = margin(t = 15, b = 5)  
    ),
    plot.subtitle = element_text(
      size = 14,  
      color = "black",  
      hjust = 0.5,  
      margin = margin(b = 20)
    ),
    legend.position = "bottom",  
    legend.direction = "horizontal",  
    legend.title = element_text(size = 10, face = "bold", color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    legend.title.align = 0.5,
    plot.background = element_rect(fill = kolor_tla, color = NA),
    panel.background = element_rect(fill = kolor_tla, color = NA),
    legend.background = element_rect(fill = kolor_tla, color = NA),
    legend.key = element_rect(fill = kolor_tla, color = NA)
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,  
      title.position = "top",  
      title.hjust = 0.5  
    )
  )

# ---------------------------- 5. zapis do png  -------------------------
ggsave(
  "mapa_plakat_final.png",  
  plot = mapa_plot,
  width = 8,
  height = 10,
  dpi = 300,
  bg = "transparent"
)