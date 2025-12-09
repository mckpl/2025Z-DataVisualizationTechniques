# Link do oryginalnej wizualizacji: https://x.com/AdityaDahiyaIAS/status/1959462038268428526
# Dane do wykresu: https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-12/attribution_studies.csv

#Co wymagało poprawy?
# 1. Na mapę nałożono wykresy kołowe dla każdego kraju, co sprawia,
# że cała wizualizacja jest przeładowana i chaotyczna.
# 2. Użycie wykresów kołowych dodatkowo utrudnia poprawną percepcję danych, 
# ponieważ ludzkie oko słabo radzi sobie z oceną kątów i pól.
# Dodatkowo liczba badań została zakodowana rozmiarem wykresów kołowych,
# co może prowadzić do zniekształconej oceny wielkości.
# W przypadku bardzo małych kółek odczyt segmentów staje się praktycznie niemożliwy.
# 3. W wielu miejscach (np. w Europie) wykresy kołowe nachodziły na siebie, 
# co uniemożliwia poprawne odczytanie informacji.
# 4. Zastosowane kolory są miejscami zbyt podobne do siebie i słabo się wyróżniają.

# Co zostało zmienione?
# 1. Mapa została podzielona na dwie osobne wizualizacje, dzięki czemu unikamy
# przytłaczającej liczby informacji na jednym wykresie.
# 2. Nowa mapa pokazuje, gdzie prowadzi się badania i jak dużo ich jest,
# a zastosowanie skali logarytmicznej pozwala wyrównać duże dysproporcje w ich liczbie.
# Natomiast wykresy kołowe zostały zamienione na stacked bar chart, 
# który pokazuje podział otrzymanych wyników badań w krajach o ich największej liczbie. 
# Pozwala to na ukazanie zarówno globalnego kontekstu na mapie,
# jak i kluczowych szczegółów na wykresue słupkowym.

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-12/attribution_studies.csv")

df_clean <- df %>%
  filter(!is.na(iso_country_code)) %>%
  separate_rows(iso_country_code, sep = ",") %>%
  mutate(iso_country_code = trimws(iso_country_code))

df_country_counts <- df_clean %>%
  count(iso_country_code, name = "n_studies")

top10 <- df_country_counts %>%
  slice_max(n_studies, n = 10) %>%
  pull(iso_country_code)

#Wybieram 10 krajów, dla których liczba badań jest największa.
df_top10 <- df_clean %>%
  filter(iso_country_code %in% top10) %>%
  count(country = iso_country_code, classification, name = "n") %>%
  mutate(country = fct_reorder(country, n, .fun = sum))

# Łączę dane z rnaturalearth z df_country_counts, ponieważ rnaturalearth 
# zawiera kody ISO3 (iso_a3), które odpowiadają zmiennej iso_country_code 
# w naszych danych.
world_data <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3, name, geometry) %>% 
  left_join(df_country_counts, by = c("iso_a3" = "iso_country_code"))

map <- ggplot(world_data) +
  geom_sf(aes(fill = n_studies)) +
  scale_fill_viridis_c(option = "plasma", trans = "log10", na.value = "grey90") +
  labs(title = "Number of attribution studies by country",
    subtitle = "Grey areas indicate countries for which no data were available.",
    fill = "Number of studies\n(log scale)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
    legend.position = "right")

plot <- ggplot(df_top10, aes(x = country, y = n, fill = classification)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 countries by number of attribution studies",
    x = "Country (ISO3)",
    y = "Number of studies",
    fill = "Classification") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 10))

ggsave("hw4_plot.png", plot, width = 10, height = 6, dpi = 300)
ggsave("hw4_map.png", map, width = 12, height = 6, dpi = 300)

