df_gdp <- read.csv("gdp.csv")


library(dplyr)
library(maps)
library(ggplot2)
library(tidyverse)


df_gdp <- df_gdp %>% 
  mutate(across(everything(), ~na_if(., ":")))


# Będę chciał wyznaczyć procentowy wzrost gospodarczy (GDP per capita) każdego państwa w Europie w latach 2000 - 2004


convert_to_numeric <- function(x) {
  as.numeric(gsub("[^0-9.-]", "", x))  # Potrzebne bo dane się wczytały jako stringi 
}

df_przyrost <- df_gdp %>%
  mutate(across(-Country, convert_to_numeric)) %>%
  mutate(przyrost = round((X2024 - X2000) / X2000 * 100, 2)) %>%
  filter(!is.na(przyrost)) %>%  # Tu wsm odpada tylko Czarnogóra dla której nie ma statystyk do 2006 r.
  select(Country, przyrost)

max_przyrost <- df_przyrost %>% 
  filter(przyrost == max(przyrost, na.rm = TRUE)) %>%   # Tak na wypadek jakby była >1 taka wartość
  pull(Country)

min_przyrost <- df_przyrost %>% 
  filter(przyrost == min(przyrost, na.rm = TRUE)) %>% 
  pull(Country)


eur_map <- map_data("world") %>% 
  filter(region %in% df_przyrost$Country)   # Tylko te państwa nas interesują 

eur_map_data <- eur_map %>%
  left_join(df_przyrost, by = c("region" = "Country"))


ggplot() +
  geom_polygon(data = eur_map_data, 
               aes(x = long, y = lat, group = group, fill = przyrost),
               color = "#881288", size = 0.2) +
  geom_polygon(data = eur_map_data %>% filter(region %in% max_przyrost),
               aes(x = long, y = lat, group = group),
               fill = NA, color = "green", size = 0.65) +
  geom_point(data = eur_map_data %>% 
               filter(region %in% max_przyrost) %>%
               group_by(region) %>%
               summarize(long = mean(long), lat = mean(lat)),
             aes(x = long+1.5, y = lat-0.5), size = 1, fill = "black",
             shape = 21, stroke = 2) +
  geom_point(data = eur_map_data %>% 
               filter(region %in% min_przyrost) %>%
               group_by(region) %>%
               summarize(long = mean(long), lat = mean(lat)),
             aes(x = long+1.5, y = lat), size = 1, fill = "black",   # Trochę przesunąłem w prawo żeby kropka była na lądzie
             shape = 21, stroke = 2)+
  geom_polygon(data = eur_map_data %>% filter(region %in% min_przyrost),
               aes(x = long, y = lat, group = group),
               fill = NA, color = "red", size = 0.65) +
  geom_text(data = eur_map_data %>% 
              filter(region %in% c(max_przyrost, min_przyrost)) %>%
              group_by(region) %>%
              summarize(long = mean(long), lat = mean(lat)),
            aes(x = long+1.5, y = lat-0.5, label = region),   # Tu napisy przesunięte w prawo żeby odpowiadały kropkom
            size = 6, color = "black", fontface = "bold",
            nudge_y = 1.5) +
  coord_fixed(xlim = c(-25, 46), ylim = c(36, 72), ratio = 1.3)  +
  scale_fill_gradient(
    low = "#E8F3E9", high = "#1B5E20", 
    name = "Przyrost GDP per capita\n2000-2024 [%]",
    breaks = c(10,50,100,150,200, 250)   # Dodałem ręcznie wartości żeby skala była bardziej czytelna 
  ) +
  labs(
    title = "Procentowy przyrost GDP per capita w Europie \nmiędzy 2000 a 2024 rokiem",
    subtitle = " Największy przyrost - Turcja (286.5 %) | Najmniejszy - Włochy (5.8 %)"
  )+
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    plot.background = element_rect(fill = "azure", color = NA),
    plot.subtitle = element_text(size = 17),
    panel.background = element_rect(fill = "azure", color = NA),
    legend.position = "bottom",        # To tak dla estetyki
    legend.key.width = unit(2, "cm"),
    legend.background = element_rect(fill = "azure"), 
    legend.title = element_text(size = 17, hjust = 0.5, vjust = 3)
  )

ggsave("mapa_przyrost.png", width = 13, height = 10, dpi = 300)





