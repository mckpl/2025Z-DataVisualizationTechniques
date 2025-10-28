###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# Ładowanie danych --------------------------------------------------------

reviews <- read.csv("reviews.csv")
listings <- read.csv("listings.csv")

 # Instrukcja --------------------------------------------------------------

# Poniższe zadania wymagają obróbki danych. W tym celu należy wykorzystać pakiet
# dplyr i tidyr. Wynik z każdego zadania należy przypisać do zdefiniowanej nazwy
# ANS_TASK_0X. Kod utworzonych wykresów należy pozostawić w pliku .R, nie należy
# zapisywać wykresów.
# Po wykonaniu wszystkich zdań należy wykonać kod oznaczony nagłówkiem 
# "Zapisanie rozwiązań do plik .RDS".

# Zadanie 1 (0.5) ---------------------------------------------------------
# Ilu recenzentów wystawiło więcej niż 20 opinii?

## Rozwiązanie
odp1 <- reviews %>%
  group_by(reviewer_id) %>%
  summarise(n_reviews = n()) %>%
  filter(n_reviews>20) %>%
  summarise(count_reviewers = n())

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- odp1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
odp2 <- reviews %>%
  mutate(month = format(as.Date(date), "%m")) %>%
  group_by(month) %>%
  summarise(n_reviews = n()) %>%
  arrange(desc(n_reviews))

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- odp2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

odp3 <- listings %>%
  inner_join(
    reviews %>%
      group_by(listing_id) %>%
      summarise(n_reviews = n()), by = c("id" = "listing_id")) %>%
  filter(n_reviews>=100) %>%
  mutate(price = as.numeric(gsub("[$,]", "", price))) %>%
  summarise(max_price = max(price, na.rm = TRUE))

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- odp3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
odp4 <- reviews %>%
  group_by(listing_id) %>%
  summarise(first_review = min(as.Date(date), na.rm = TRUE)) %>%
  mutate(years_on_market = as.numeric(difftime(Sys.Date(), first_review, units = "days"))/365) %>%
  summarise(
    n_listings_over_10 = sum(years_on_market > 10, na.rm = TRUE),
    maks_years_on_market = max(years_on_market, na.rm = TRUE)
  )
## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- odp4

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
center_lat <- mean(listings$latitude, na.rm = TRUE)

odp5 <- listings %>%
  mutate(region = ifelse(latitude > center_lat, "North", "South")) %>%
  group_by(room_type, region) %>%
  summarise(n_listings = n(), .groups = "drop") %>%
  pivot_wider(names_from = region, values_from = n_listings, values_fill = 0) %>%
  mutate(diff_south_vs_north = South - North)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- odp5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
odp6 <- reviews %>%
  group_by(listing_id) %>%
  summarise(n_reviews = n(), .groups = "drop") %>%
  filter(n_reviews >= quantile(n_reviews, 0.4),
         n_reviews <= quantile(n_reviews, 0.5)) %>%
  inner_join(reviews, by = "listing_id") %>%
  mutate(year = as.numeric(format(as.Date(date), "%Y")),
         parity = ifelse(year %% 2 == 0, "even", "odd")) %>%
  group_by(listing_id, parity) %>%
  summarise(total_reviews = n(), .groups = "drop") %>%
  pivot_wider(names_from = parity, values_from = total_reviews, values_fill = 0) %>%
  mutate(diff = odd - even,
         more_in_odd = odd > even) %>%
  arrange(desc(diff))

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- odp6

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

odp7 <- listings %>%
  filter(!is.na(price) & !is.na(room_type))

#wykres
# ggplot(listings_clean, aes(x = room_type, y = price)) +
#   geom_violin(fill = "skyblue", alpha = 0.7) +
#   labs(
#     title = "Rozkład cen apartamentów w zależności od typu pokoju",,
#     x = "Typ pokoju",
#     y = "Cena (USD)"
#   ) +
#   theme_minimal() +
#   scale_y_log10(labels = scales::dollar) # Skala logarytmiczna dla czytelności

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- odp7
# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

odp8 <- listings %>%
  filter(!is.na(host_name)) %>%
  mutate(first_letter = substr(host_name, 1, 1)) %>%
  mutate(first_letter = toupper(first_letter)) %>%
  group_by(first_letter) %>%
  summarise(Liczba_hostow = n())

#wykres
# ggplot(odp8[-1, ], 
#        aes(x = first_letter, y = Liczba_hostow)) +
#   
#   geom_bar(stat = "identity", fill = "red", alpha = 0.6) +
#   
#   labs(
#     title = "Rozkład pierwszych liter imion hostów",
#     x = "Pierwsza litera imienia hosta") +
#   theme_minimal()

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- odp8

# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

odp9 <- listings %>%
  filter(is.na(price))
# #wykres, widac na nim ze rozkład apartamentów nie jest równomierny
# ggplot(odp9, aes(x = longitude, y = latitude)) +
#   geom_density_2d(color = "darkblue") +
#   #geom_point(alpha = 0.3, size = 1, color = "red") +
#   
#   labs(
#     title = "Rozkład geograficzny apartamentów bez podanej ceny",
#     x = "Długość geograficzna (Longitude)",
#     y = "Szerokość geograficzna (Latitude)"
#   ) +
#   theme_minimal() +
#   coord_fixed()

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- odp9


# Zapisanie rozwiązań do plik .RDS ----------------------------------------


# PROSZĘ NIC NIE ZMIENIAĆ  ------------------------------------------------

solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, 
                  ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09)

names(solutions) <- c("Task01", "Task02", "Task03", 
                      "Task04", "Task05", "Task06",
                      "Task07", "Task08", "Task09")


#   -----------------------------------------------------------------------

# Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "StepniewskiIgnacy.rds")
