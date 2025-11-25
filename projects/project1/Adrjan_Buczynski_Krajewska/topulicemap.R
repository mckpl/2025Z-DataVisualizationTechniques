library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)

woj <- st_read("projekt1/twd/data/wojewodztwa.shp", quiet = TRUE)
ulice <- read.csv("projekt1/twd/najczestsze_ulice.csv", stringsAsFactors = F)

w <- woj %>%
  left_join(ulice, by = c("JPT_NAZWA_" = "woj_nazwa"))

ggplot(w) +
  geom_sf(aes(fill = nazwa), color = "grey30", linewidth = 0.3) +
  scale_fill_brewer(palette = "Set3", name = "Najczęstsza ulica") +
  theme_minimal() +
  labs(title = "Najczęstsze nazwy ulic w województwach") +
  theme(legend.position = "bottom")

top5 <- read.csv("projekt1/twd/top5.csv")

top5_pivot <- top5 %>%
  select(woj_nazwa, nazwa, liczba) %>%
  pivot_wider(names_from = nazwa, values_from = liczba, values_fill = 0)

top5_long <- top5_pivot %>%
  pivot_longer(cols = -woj_nazwa, names_to = "nazwa", values_to = "liczba")

ggplot(top5_long, aes(x = nazwa, y = woj_nazwa, fill = liczba)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Top 5 nazw ulic w województwach",
       x = "Nazwa ulicy", y = "Województwo", fill = "Liczba") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#top4 w zal. od pozycji

top4 <- read.csv("projekt1/twd/newtop4.csv") 

top4_long <- top4 %>%
  select(woj_nazwa, nazwa, pozycja) %>%
  pivot_wider(names_from = nazwa, values_from = pozycja, values_fill = 0) %>% 
  pivot_longer(cols = -woj_nazwa, names_to = "nazwa", values_to = "pozycja")
  

ggplot(top4_long, aes(x = nazwa, y = woj_nazwa, fill = pozycja)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Top 4 nazw ulic w województwach",
       x = "Nazwa ulicy", y = "Województwo", fill = "pozycja") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
