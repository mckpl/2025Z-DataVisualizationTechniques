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

head(reviews)
head(listings)

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
morethan20 <- reviews %>%
  group_by(reviewer_id) %>%
  summarise(n_reviews = n()) %>%
  filter(n_reviews > 20) %>%
  summarise(count = n()) %>%
  pull(count)

morethan20


ANS_TASK_01 <- morethan20

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?
reviews$date <- as.Date(reviews$date)

miesiacepokolei <- reviews %>%
  mutate(month = format(date, "%m")) %>%
  group_by(month) %>%
  summarise(n_reviews = n()) %>%
  arrange(desc(n_reviews))

miesiacepokolei

ANS_TASK_02 <- miesiacepokolei[1, ]

ANS_TASK_02
  

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
ANS_TASK_03 <- reviews %>%
  group_by(listing_id) %>%
  summarise(n_reviews = n()) %>%
  filter(n_reviews >= 100) %>%
  inner_join(listings, by = c("listing_id" = "id")) %>%
  summarise(max_price = max(price, na.rm = TRUE)) %>%
  pull(max_price)

ANS_TASK_03



# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
granica <- Sys.Date() - (365.25*10)

ANS_TASK_04_A <- reviews %>%
  filter(date < granica) %>%
  summarise(n_apartamentow = length(unique(listing_id))) %>%
  pull(n_apartamentow)

ANS_TASK_04_A

ANS_TASK_04_B <- reviews %>%
  group_by(listing_id) %>%
  summarise(pierwsza_recenzja = min(date)) %>%
  mutate(lata_na_rynku = as.numeric(difftime(Sys.Date(), pierwsza_recenzja, units = "days")) / 365.25) %>%
  arrange(desc(lata_na_rynku))

max_lat <- max(ANS_TASK_04_B$lata_na_rynku)


head(ANS_TASK_04_B, 5)

ANS_TASK_04 <- c(
  wiecej_niz_10 = ANS_TASK_04_A,
  max_lat = max_lat
)

ANS_TASK_04


# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

srednia <- mean(listings$latitude, na.rm = TRUE)


ANS_TASK_05 <- listings %>%
  mutate(region = case_when(
    latitude > srednia ~ "północ",
    latitude < srednia ~ "południe",
    TRUE ~ "srednia"
  )) %>%
  filter(region != "srednia") %>%
  group_by(region, room_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = region, values_from = n, values_fill = 0) %>%
  mutate(na_poludniu_wiecej_o = południe - północ) %>%
  filter(na_poludniu_wiecej_o > 0)

ANS_TASK_05




# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

reviews$date <- as.Date(reviews$date)


liczba_recenzji <- reviews %>%
  group_by(listing_id) %>%
  summarise(total_reviews = n(), .groups = "drop")


decyl <- quantile(liczba_recenzji$total_reviews, probs = c(0.4, 0.5), na.rm = TRUE)


wybrane_apartamenty <- liczba_recenzji %>%
  filter(total_reviews >= decyl[1], total_reviews <= decyle[2])

reviews <- reviews %>%
  mutate(year = format(date, "%Y") %>% as.numeric())

wynik_szczegolowy <- reviews %>%
  filter(listing_id %in% wybrane_apartamenty$listing_id) %>%
  group_by(listing_id) %>%
  summarise(
    nieparzyste = sum(year %% 2 != 0),
    parzyste = sum(year %% 2 == 0),
    .groups = "drop"
  ) %>%
  mutate(roznica = nieparzyste - parzyste) %>%
  filter(roznica > 0)


liczba_apartamentow <- nrow(wynik_szczegolowy)
rozica_dla_apartamentow <- wynik_szczegolowy %>%
  select(listing_id, roznica)

ANS_TASK_06 <- list(
  ile_apartamentow = liczba_apartamentow,
  roznica = rozica_dla_apartamentow
)

ANS_TASK_06



# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
ANS_TASK_07 <- listings %>%
  group_by(room_type) %>%
  summarise(
    min_price = min(price, na.rm = TRUE),
    max_price = max(price, na.rm = TRUE),
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    above_mean = sum(price > mean(price, na.rm = TRUE), na.rm = TRUE),
    .groups = "drop"
  )

ANS_TASK_07

ggplot(listings, aes(x = room_type, y = price)) +
  geom_violin(fill = "violet") +
  labs(title = "Rozkład cen apartamentów w zależności od typu pokoju",
       x = "Typ pokoju",
       y = "Cena")

ggplot(listings, aes(x = price)) +
  geom_histogram(binwidth = 20, fill = "lightblue", color = "darkblue") +
  facet_wrap(~room_type, scales = "free_y") +
  labs(title = "Rozkład cen apartamentów według typu pokoju",
       x = "Cena ($)",
       y = "Liczba apartamentów")


# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
host_letters <- listings %>%
  distinct(host_id, host_name) %>%
  mutate(first_letter = toupper(substr(host_name, 1, 1))) %>%
  filter(grepl("^[A-Z]$", first_letter)) %>%
  group_by(first_letter) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(first_letter)


ANS_TASK_08 <- host_letters

ANS_TASK_08

ggplot(ANS_TASK_08, aes(x = first_letter, y = count)) +
  geom_col()




# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie
ANS_TASK_09 <- listings %>%
  filter(is.na(price)) %>%
  select(longitude, latitude)

ANS_TASK_09

ggplot(ANS_TASK_09, aes(x = longitude, y = latitude)) +
  geom_point() +
  labs(
    title = "Położenie aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych",
    x = "Długość geograficzna",
    y = "Szerokość geograficzna"
  )
# z wykresu widac, że nie ma konkretnego miejsca skupienia, choc są miejsca gdzie jest gęściej. 
# można próbować urozmaicic tabelę by w niej dało się jakos znalezć ale zdecydowanie łatwiej i lepiej odczytuje się z wykresu 



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
saveRDS(solutions, file = "StrzeleckaKarolina.rds")



