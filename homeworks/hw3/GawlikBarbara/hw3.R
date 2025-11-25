# ==============================================================================
# 0. WCZYTANIE BIBLIOTEK I DANYCH

library(countrycode)
library(dplyr)
library(eurostat)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidyr)
library(zoo)

df_raw <- get_eurostat("edat_lfse_03", cache=FALSE)



# ==============================================================================
# 1. EKSPLORACJA

View(df_raw)
head(df_raw)
dim(df_raw)

# Sprawdzamy, czy są kolumny z tylko 1 wartością unikatową.
df_raw %>%
  summarise(across(everything(), ~ n_distinct(.))) %>%
  pivot_longer(cols = everything(),
               names_to = "kolumna",
               values_to = "liczba_unikatow")

# Sprawdzamy, czy typy danych wszędzie się zgadzają.
str(df_raw)

# Sprawdzamy, czy (i jeśli tak, to gdzie) występują braki danych.
df_raw %>%
  summarise(across(everything(), ~ 100 * mean(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "kolumna",
               values_to = "procent_pusty")
# Musimy się upewnić, że na.spline() nie wypełni dużej części zbioru danych.
# Inaczej byłyby to dane sztucznie wygenerowane.


# ==============================================================================
# 2. TRANSFORMACJA

df <- df_raw %>%

  # kolumny z 1 unikatem nic nie wnoszą
  select(-freq, -unit) %>%

  # wszędzie jest ta sama data, więc wystarczy nam rok
  mutate(year = as.numeric(format(TIME_PERIOD, "%Y"))) %>%
  select(-TIME_PERIOD) %>%
  
  # zmieniamy wartości w isced11 na bardziej czytelne
  mutate(isced11 = case_when(
    isced11 == "ED0-2"    ~ "Podstawowe i średnie I st",
    isced11 == "ED3-8"    ~ "Średnie II st, policealne i wyższe",
    isced11 == "ED3_4"    ~ "Średnie II st i policealne",
    isced11 == "ED3_4GEN" ~ "Średnie II st i policealne - ogólne",
    isced11 == "ED3_4VOC" ~ "Średnie II st i policealne - zawodowe",
    isced11 == "ED5-8"    ~ "Wyższe",
    TRUE                  ~ "Nieustalone"
  )) %>%
  
  # trzeba wypełnić braki - tak, aby jak najlepiej oddawały, co tam było
  arrange(sex, age, isced11, geo, year) %>%
  group_by(sex, age, isced11, geo) %>%
  mutate(
    values = if(sum(!is.na(values)) >= 2) {
      na.spline(values, x = year, method = "natural")
    } else {
      values
    }
  ) %>%
  ungroup() %>%
  group_by(sex, age, isced11, year) %>%
  mutate(
    values = ifelse(is.na(values), mean(values, na.rm = TRUE), values)
  ) %>%
  ungroup() %>%
  mutate(values = pmax(values, 0)) %>%
  mutate(values = pmin(values, 100))



# ==============================================================================
# 3. WERYFIKACJA

sum(is.na(df$values))

View(df)



# ==============================================================================
# 4. EKSPLORACJA


# 4.01. Jak zmienił się % kobiet z wyższym wykształceniem w każdym z krajów?

df_F <- df %>%
  filter(
    sex == "F",
    isced11 == "Wyższe",
    age == "Y15-64",
    nchar(as.character(geo)) == 2,
    year >= 2004
  )

ggplot(df_F, aes(x = year, y = values, group = geo)) +
  geom_line(alpha = 0.4, color = "purple") +
  geom_smooth(aes(group = 1), method = "loess", color = "red", se = FALSE) +
  scale_x_continuous(
    breaks = seq(2004, 2024, by = 4),
    labels = seq(2004, 2024, by = 4),
    expand = c(0, 0),
    limits = c(2003.7, 2024.3)) +
  labs(
    title = "Ewolucja wykształcenia wyższego kobiet (15-64) w Europie",
    subtitle = "Każda linia to jeden kraj. Czerwona linia to średni trend.",
    y = "Odesetek kobiet z wyższym wykształceniem",
    x = "Rok"
  ) +
  theme_minimal()

# Odesetek kobiet z wyższym wykształceniem stale rośnie w krajach europejskich.
# Na przestrzeni 20 lat, odsetek ten wzrósł z ok. 20% do ok. 37%, co znaczy
# ogromną zmianę pod tym kątem w skali europejskiego społeczeństwa.


# 4.02. Zachód vs Wschód Europy - jak różnimy się pod tym kątem?

df_F_completed_base <- df_F %>%
  select(geo, year, values) %>%
  complete(geo, year = 2004:2024) %>%
  arrange(geo, year)

# Sprawdzamy, ile mamy pustych pól - czy możemy użyć na.spline().
100 * sum(is.na(df_F_completed_base$values)) / dim(df_F_completed_base)[1]

df_F_full <- df_F_completed_base %>%
  group_by(geo) %>%
  mutate(values = na.spline(values, method = "natural", na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(values = pmax(pmin(values, 100), 0))

zachod_kraje <- c("AT", "BE", "CH", "DE", "DK", "ES", "FI", "FR", 
                  "IE", "IS", "IT", "LU", "NL", "NO", "PT", "SE", "UK")

df_agg <- df_F_full %>%
  arrange(geo, year) %>%
  group_by(geo) %>%
  mutate(delta = values - lag(values)) %>%
  ungroup() %>%
  drop_na() %>%
  mutate(region = if_else(geo %in% zachod_kraje, "Zachód", "Wschód")) %>%
  group_by(year, region) %>%
  summarise(srednia_delta = mean(delta, na.rm = TRUE), .groups = 'drop')

ggplot(df_agg, aes(x = year, y = srednia_delta, color = region)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  labs(title = "Roczna zmiana odsetka kobiet z wyższym wykształceniem",
       subtitle = "Porównanie średniej zmiany w Europie Zachodniej i Wschodniej (w p.p.).",
       x = "Rok",
       y = "Zmiana odestka",
       color = "Region") +
  scale_x_continuous(
    breaks = seq(2006, 2024, by = 3),
    labels = seq(2006, 2024, by = 3),
    expand = c(0, 0),
    limits = c(2004.5, 2024.5)) +
  scale_color_manual(values = c("Zachód" = "blue", "Wschód" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Niezależnie od regionu, odsetek kobiet z wyższym wykształceniem rośnie z roku 
# na rok. Nie ma wyraźnej różnicy w trendach zmian między regionami - niekiedy 
# większa średnia zmiana w p.p jest na Zachodzie, a niekiedy na Wschodzie.
# Interesujący jest jednak rok 2023, z dużym dropem w średniej zmianie odsetka
# w obydwu regionach. Może to konsekwencja pandemii z 2019/2020 roku?


# 4.03. Zmiana odsetka kobiet z wyższym wykształceniem wg kraju

df_F_map_data <- df_F_full %>%
  pivot_wider(names_from = year, values_from = values, names_prefix = "y_") %>%
  mutate(change_pp = y_2024 - y_2004) %>%
  mutate(
    geo_name = countrycode(
      sourcevar = geo,
      origin = "eurostat",
      destination = "country.name"
    )) %>%
  mutate(geo_name = case_when(
    geo_name == "Macedonia" ~ "North Macedonia",
    geo_name == "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
    geo_name == "Czechia" ~ "Czech Republic",
    TRUE ~ geo_name
  )) %>%
  select(-geo)

df_F_map_data %>%
  mutate(geo_name = countrycode(
    sourcevar = geo_name,
    origin = "country.name",
    destination = "cldr.short.pl"
  )) %>%
  ggplot(aes(x = reorder(geo_name, change_pp), y = change_pp)) +
  geom_segment(aes(xend = geo_name, yend = 0), color = "grey") +
  geom_point(size = 3, color = "purple") +
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, 35)) +
  coord_flip() +
  labs(
    title = "Zmiana odsetka kobiet z wyższym wykształceniem (2004-2024)",
    subtitle = "Zmiana w punktach procentowych.",
    x = "Kraj",
    y = "Zmiana"
  ) +
  theme_minimal()

# Nie widać szczególnej różnicy w regionach Europy w wartości zmianie odestka.
# Nie mniej jednak, zmiana ta wynosi w zdecydowanej większości krajów >15% -
# znaczy to ogromny wzrost liczby kobiet z wyższym wykształceniem w skali
# społeczeństw każdego z tych krajów.


# 4.04. Czy kraje z mniejszym odsetkiem kobiet z wyższym wykształceniem w 2004
#       gonią kraje z bardzo wysokim odsetkiem z 2024?

df_F_map_data %>%
  mutate(geo_name = countrycode(
    sourcevar = geo_name,
    origin = "country.name",
    destination = "cldr.short.pl"
  )) %>%
  ggplot(aes(x = y_2004, y = change_pp, label = geo_name)) +
    geom_point(color = "purple", size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    geom_text(vjust = -0.5, size = 3) +
    labs(
      title = "Czy słabsi gonią lepszych?",
      subtitle = "Im niższy wynik na osi OX, tym wyższy powinien być wynik na osi OY.",
      x = "Odsetek kobiet z wyższym wykształceniem w 2004",
      y = "Wzrost odestka (p.p.) 2004-2024"
    ) +
    theme_minimal()

# Jest lekka ujemna korelacja między % kobiet z wyższym wykształceniem a
# wzrostem tego odestka w p.p. na przestrzeni 20 lat. Nie mniej jednak, jest to
# bardzo słaba korelacja, co oznaczałoby, że w praktyce kraje z mniejszym 
# odsetkiem kobiet z wyższym wykształceniem w 2004 zasaniczo nie gonią krajów z 
# wysokim odsetkiem w 2004. 



# ==============================================================================
# 5. WIZUALIZACJA

world <- ne_countries(scale = "medium", returnclass = "sf", continent = "Europe")
world_full <- ne_countries(scale = "medium", returnclass = "sf")

europe_map <- world_full %>%
  left_join(df_F_map_data, by = c("name_long" = "geo_name"))

max_countries <- df_F_map_data %>%
  filter(change_pp == max(change_pp, na.rm = TRUE)) %>%
  pull(geo_name) %>%
  countrycode(origin = 'country.name', destination = 'cldr.short.pl')
min_countries <- df_F_map_data %>%
  filter(change_pp == min(change_pp, na.rm = TRUE)) %>%
  pull(geo_name) %>%
  countrycode(origin = 'country.name', destination = 'cldr.short.pl')

mapka <- ggplot(europe_map) +
  geom_sf(fill = "grey85", color = "white", linewidth = 0.3) +
  geom_sf(aes(fill = change_pp), color = "white", linewidth = 0.3) +
  scale_fill_distiller(
    palette = "PuRd",
    direction = 1,
    name = "Zmiana (p.p.)\n2004 – 2024",
    na.value = "grey85",
    limits = c(0, max(europe_map$change_pp, na.rm=TRUE)),
    breaks = seq(0, max(europe_map$change_pp, na.rm=TRUE), by = 5),
    guide = guide_colorbar(
      frame.colour = "black",
      frame.linewidth = 0.5,
      ticks.colour = "black",
      barwidth = 2.5,
      barheight = 15
    )
  ) +
  labs(
    title = "Zmiana odsetka kobiet z wyższym wykształceniem (2004 – 2024)",
    subtitle = paste0(
      "Największy wzrost: ", paste(max_countries, collapse = ", "), "\n",
      "Najmniejszy wzrost: ", paste(min_countries, collapse = ", ")
    ),
    caption = "Źródło danych: Eurostat (edat_lfse_03)"
  ) +
  coord_sf(
    crs = 3035,
    xlim = c(2600000, 6800000),
    ylim = c(1300000, 5200000),
    expand = FALSE
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.background = element_blank(), 
    legend.ticks.length = unit(0.25, "cm"),
    plot.title = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(color = "grey30", size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "white", linetype = "dotted")
  )

mapka
ggsave("mapa.png", plot = mapka, width = 15, height = 15, dpi = 300, bg = "white")