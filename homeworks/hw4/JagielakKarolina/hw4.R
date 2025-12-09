library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
#Wykres który poprawiam wzięty z : https://github.com/xh313/TidyTuesdayWithPython
#Z tamtej strony wybrałam ten: https://user-images.githubusercontent.com/77285010/168933069-18ee2f83-b542-4e1b-99e2-e5cf932eaf88.png

# Na wizualizacji, którą wybrałam linie pokazujące, kto komu przyznał punkty, mają bardzo zbliżone 
# do siebie kolory, przez co kierunek głosów jest trudny do odczytania. Dodatkowo etykiety krajów 
# na mapie są rozmieszczone w sposob nieuporządkowany, częściowo nachodzą na siebie 
# a niektóre są w nieprawidłowych miejscach co utrudnia czytelność i interpretacje

## KOD
# POBRANIE DANYCH
link <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/main/data/2022/2022-05-17/eurovision-votes.csv"
dane <- read_csv(link)

# WYBRANIE KONKRETNYCH POTRZEBNYCH DANYCH
dane <- dane %>%
  filter(year == 2022,
         points == 12,
         semi_final == "f",
         jury_or_televoting == "T")

# LICZBA KRAJI
kraje <- sort(unique(c(dane$from_country, dane$to_country)))

# wszystkie możliwe połączenia kraji każdy z każdym
macierz <- expand_grid(from_country = kraje, to_country   = kraje)

# kto dał komu 12 punktów
gdzie_12 <- dane %>%
  select(from_country, to_country) %>%
  mutate(value = 1)

# łączymy
polaczona <- macierz %>%
  left_join(gdzie_12, by = c("from_country", "to_country")) %>%
  mutate(value = replace_na(value, 0))

polaczona$from_country <- factor(polaczona$from_country, levels = kraje)
polaczona$to_country   <- factor(polaczona$to_country, levels = kraje)

# HEATMAPA
ggplot(polaczona, aes(x = to_country, y = from_country, fill = factor(value))) +
  geom_tile(color = "white") +
  labs(x = "Otrzymujący kraj", y = "Głosujący kraj", title = "Jak widzowie przyznawali 12 punktów w Eurovision Contest 2022") +
  scale_fill_manual(values = c("0" = "grey", "1" = "darkgreen")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      size = 7,
      vjust = 0.5
    ),
    axis.text.y = element_text(size = 7),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  coord_fixed()

# Zaproponowana przeze mnie wizualizacja jest lepszym rozwiązaniem dla danych analizowanych,
# ponieważ eliminuje problem nieczytelnych, nakładających się linii, zastępując je macierzą 
# kraj na kraj, dzięki której jesteśmy w stanie jednoznacznie stwierdzić kierunek przyznawania 
# punktów. Dodatkowo bardzo łatwo w ten sposób jesteśmy wstanie stwierdzić kto otrzymał najwięcej 
# punktów co umożliwia nam jednolita skala kolorów.

