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

ans1 <- reviews %>%
  group_by(reviewer_id) %>% 
  summarise(n = n()) %>% 
  filter(n > 20) %>% 
  pull(n) %>% 
  length()

ans1

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- ans1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

ans2 <- reviews %>%
  mutate(
    date_formatted = as.Date(date, format="%Y-%m-%d"),
    month_name = format(date_formatted, "%B")
  ) %>%
  group_by(month_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice_head(n = 3) %>%   # Zakładam, że interesują nas top 3 miesiące
  pull(month_name)

ans2

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- ans2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy założeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

ans3 <- reviews %>%
  group_by(listing_id) %>%
  summarise(n_reviews = n()) %>%
  filter(n_reviews >= 100) %>%
  left_join(listings, by = c("listing_id"="id")) %>%
  filter(!is.na(price)) %>%
  summarise(max_price = max(price)) %>%
  pull(max_price)

ans3

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- ans3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

current_date <- as.Date("2025-10-27", format="%Y-%m-%d")

ans4 <- reviews %>%
  group_by(listing_id) %>%
  summarise(first_review_date = min(as.Date(date, format="%Y-%m-%d"))) %>%
  mutate(years_on_market = as.numeric(current_date - first_review_date, units="days") / 365) %>%
  summarise(
    apartments_over_10_years = sum(years_on_market > 10),
    max_years = max(years_on_market)
  )

ans4

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- ans4

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

middle <- mean(listings$latitude)

ans5 <- listings %>%
  mutate(region = case_when(
    latitude > middle ~ "North",
    latitude < middle ~ "South"
  )) %>%
  group_by(region, room_type) %>%
  summarise(n = n(), .groups = 'drop') %>%
  pivot_wider(names_from=region, values_from=n, values_fill=0) %>%
  mutate(diff_south_north = South-North) %>%
  select(room_type, diff_south_north)

ans5

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- ans5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

deciles <- quantile(listings$number_of_reviews, probs=seq(0, 1, 0.1), na.rm=TRUE)
decile4 <- deciles["40%"]
decile5 <- deciles["50%"]

selected_listings_ids <- listings %>%
  filter(number_of_reviews >= decile4 & number_of_reviews <= decile5) %>%
  pull(id)

summary_reviews <- reviews %>%
  filter(listing_id %in% selected_listings_ids) %>%
  mutate(date_formatted = as.Date(date, format="%Y-%m-%d"),
         year = as.numeric(format(date_formatted, "%Y")),
         year_type = ifelse(year%%2 == 1, "odd", "even")) %>%
  group_by(listing_id, year_type) %>%
  summarise(n_reviews = n(), .groups = 'drop') %>%
  pivot_wider(names_from = year_type, values_from = n_reviews, values_fill = 0) %>%
  mutate(diff = odd-even)

ans6 <- summary_reviews %>%
  filter(diff > 0) %>%
  select(listing_id, diff)

ans6
# Na podstawie rozmiaru ramki danych można stwierdzić, że takich apartamentów jest 348.

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- ans6

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

price_limit <- quantile(listings$price, 0.99, na.rm = TRUE)
# Muszę zrobić do 99 centyla, bo inaczej wykres byłby nieczytelny.

plot7 <- listings %>%
  filter(price < price_limit) %>%
  ggplot(aes(x = price, fill = room_type)) +
  geom_histogram(bins = 30, alpha = 0.4, color = "black") +
  labs(title = "Rozkład cen apartamentów wg typu pokoju (poniżej 99 centyla)",
       # robię rozkład poniżej 99 centyla bo inaczej jest nieczytelny
       x = "Cena",
       y = "Liczba apartamentów") +
  scale_fill_manual(values = c("blue", "purple", "darkgreen", "lightblue"))

plot7

# Komentarze do wykresu:
# - Rozkład cen jest silnie prawostronnie skośny.
# - Apartamenty typu "Entire home/apt" dominują wśród średnich i wyższych cen.
# - Apartamenty typu "Private room" stanowią znaczną część najtańszych ofert.
# - Jest bardzo mało ofert "Hotel room" i "Shared room" w każdej klasie cenowej.

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- plot7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

plot8 <- listings %>%
  filter(!is.na(host_name), host_name != "") %>%
  mutate(first_letter = toupper(substr(host_name, 1, 1))) %>%
  filter(first_letter >= "A", first_letter <= "Z") %>%
  ggplot(aes(x = first_letter)) +
  geom_bar(fill = "navy") +
  labs(title = "Rozkład pierwszej litery imienia hosta",
       x = "Pierwsza litera imienia hosta",
       y = "Liczba apartamentów")

plot8

# Komentarze do wykresu:
# - Jako pierwsze litery imion hostów najczęściej występują M, J, A, S oraz B.
# - Jako pierwsze litery imion hostów najrzadziej występują Q, U, X, Z oraz O.
# - Szacując na podstawie wykresu, ok. 47% imion zaczyna się na jedną z najczęściej występujących 5 liter.
# - Szacując na podstawie wykresu, ok. 1% imion zaczyna się na jedną z najrzadziej występujących 5 liter.

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- plot8

# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) apartamentów w mieście Seatle, które nie mają podanej 
# ceny w bazie danych, jest równomierny?

## Rozwiązanie

plot9 <- listings %>%
  mutate(price_missing = is.na(price)) %>%
  filter(price_missing == TRUE) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(color = "navy", alpha = 0.6) +
  labs(title = "Lokalizacja apartamentów z brakującą ceną",
       x = "Długość geograficzna",
       y = "Szerokość geograficzna")

plot9

# Komentarze do wykresu:
# - Rozkład (położenie) apartamentów w Seattle bez podanej ceny nie jest równomierny.
# - Widoczna jest większa koncentracja w centrum oraz w północnej części obszaru.
# - Widoczna jest mniejsza koncentracja na wschodzie i na zachód od centrum.

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- plot9



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
saveRDS(solutions, file = "GawlikBarbara.rds")