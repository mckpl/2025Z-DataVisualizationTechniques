###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

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

reviews %>% 
  count(reviewer_id, name = "n_opinii") %>% 
  filter(n_opinii > 20) %>% 
  summarise(licz = n()) %>% 
  pull(licz)


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- 32

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

reviews %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(miesiac = month(date, label = TRUE, abbr = FALSE)) %>% 
  count(miesiac, name = "n_opinii") %>% 
  arrange(desc(n_opinii))


## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- "Sierpień, Czerwiec, Lipiec"

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

listingi <- reviews %>% 
  count(listing_id, name = "n_opinii") %>% 
  filter(n_opinii >= 100)

maksymalne_ceny <- listings %>% 
  semi_join(listingi, by = c("id" = "listing_id")) %>% 
  group_by(id) %>% 
  arrange(desc(price))

maksymalne_ceny$price[1]

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- 1012

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

reviews %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(rok = year(date)) %>% 
  filter(rok-2025 < -10) %>% 
  count(id, name = "ile_razy") %>% 
  arrange(desc(ile_razy)) %>% 
  summarise(ile = n()) %>% 
  pull(ile)



## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- 6519

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

srodek <- mean(listings$latitude, na.rm = TRUE)
listings %>%
  mutate(region = ifelse(latitude > srodek, "north", "south")) %>%
  count(region) %>%
  summarise(ile = n[region == "south"] - n[region == "north"]) %>% 
  pull(ile)


## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- 346

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

listings %>%
  filter(number_of_reviews >= quantile(listings$number_of_reviews, 0.4, na.rm = TRUE) & number_of_reviews <= quantile(listings$number_of_reviews, 0.5, na.rm = TRUE)) %>%
  select(id) %>%
  inner_join(reviews, by = c("id" = "listing_id")) %>%
  select(id,date) %>%
  mutate(rok = year(date)) %>%
  mutate(parzysty = ifelse(rok %% 2 == 0, 1, 0)) %>%
  mutate(nparzysty = ifelse(rok %% 2 == 1, 1, 0)) %>%
  group_by(id) %>%
  summarise(suma_parzyste = sum(parzysty, na.rm = TRUE),
    suma_nieparzyste = sum(nparzysty, na.rm = TRUE)) %>%
  filter(suma_parzyste < suma_nieparzyste) %>%
  summarise(ile = n(), o_ile = sum(suma_nieparzyste) - sum(suma_parzyste)) %>% 
  pull(ile)


## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- 348

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

wykres <- ggplot(listings, aes(x = room_type, y = price)) +
  geom_boxplot(na.rm = TRUE, outlier.color = "blue") +
  scale_y_log10() +
  labs(x = "Typ pokoju", y = "Cena apartamentu", title = "Rozklad cen w zależności od typu pokoju")


## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- wykres

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

rozklad <- listings %>%
  mutate(first_letter = toupper(substr(host_name, 1, 1))) %>%  
  count(first_letter) %>%                                      
  arrange(desc(n)) %>%
  head(26)

wykres <- ggplot(rozklad, aes(x = first_letter, y = n)) +
  geom_col(fill = 'blue') +
  labs(title = "Rozkład pierwszej litery imienia hosta",
       x = "Litera",
       y = "Liczba wystąpeń")

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- wykres


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

listings %>%
  filter(is.na(price)) %>%
  summarise(sd_lat = sd(latitude, na.rm = TRUE), sd_long = sd(longitude, na.rm = TRUE)) %>% 
  pull(sd_lat, sd_long)


## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- "Tak"



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
saveRDS(solutions, file = "KrawczykŁukasz.rds")



