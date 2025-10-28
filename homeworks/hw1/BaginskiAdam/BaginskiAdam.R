###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)

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
x <- reviews %>% 
  group_by(reviewer_id) %>% 
  filter(n() > 20) %>% 
  summarise(count = n())
odp_1 <- nrow(x)
## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- odp_1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
x <- as.Date(reviews$date)
y <- as.numeric(format(x, "%m"))
months_counts <- as.vector(table(y))
odp_2 <- c(names(which.max(table(y))), "Najwiecej w 8 miesiącu, ale generalnie najwięcej w miesiącach wiosennych i letnich")

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- odp_2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
x <- reviews %>% 
  group_by(listing_id) %>% 
  filter(n() >= 100)
y <- unique(x$listing_id)

odp_3 <- listings %>% 
  filter(id %in% y) %>% 
  select(price) %>% 
  summarise(max = max(price, na.rm = T))
odpp <- odp_3$max

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- odpp

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
reviews_copy <- reviews %>%
  mutate(
    date = as.Date(date),
    y = as.numeric(format(date, "%y%m")),
    y_lata = as.numeric(format(date, "%Y"))) %>%
  filter(y < 1511) %>% #1511 to listopad 2015 wiec bierzemy te przed listopadem 2015
  summarise(count = length(unique(listing_id)), min = min(y_lata))

odp_4 <- c(reviews_copy$count, 2025 - reviews_copy$min)
names(odp_4) <- c("ilosc apartamentow wstawionych przed listopadem 2015", "pierwsza recenzja jest z 2009 lipiec -> 16 lat")

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- odp_4

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

srednia = mean(listings$latitude)

odp_5 <- listings %>% 
  group_by(room_type) %>% 
  transmute(polnoc = latitude > srednia,
            poludnie = latitude <= srednia) %>% 
  summarise(poludnie_minus_polnoc = sum(poludnie) - sum(polnoc))
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- odp_5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

reviews <- reviews %>% 
  group_by(listing_id) %>% 
  mutate(count_reviews = n())

d4 <- quantile(reviews$count_reviews, 0.4)
d5 <- quantile(reviews$count_reviews, 0.5)

reviews <- reviews %>% 
  filter(count_reviews >= d4 & count_reviews <= d5)

x <- as.Date(reviews$date)
lata <- as.numeric(format(x, "%Y"))
reviews$parzysty = lata %% 2 == 0

reviews <- reviews %>% 
  summarise(ilosc_parzystych = sum(parzysty), ilosc_nieparzystych = length(parzysty) - sum(parzysty)) %>% 
  mutate(wiecej_nieparzystych = ilosc_nieparzystych > ilosc_parzystych,
         o_ile_wiecej_NP = ilosc_nieparzystych - ilosc_parzystych)

ilosc_apartamentow_z_wiecej_NP <- sum(reviews$wiecej_nieparzystych)

przewaga_nieparzystych <- reviews %>% 
  filter(wiecej_nieparzystych == TRUE) %>% 
  summarise(suma = sum(o_ile_wiecej_NP))

odp_6 <- c(ilosc_apartamentow_z_wiecej_NP, przewaga_nieparzystych)
names(odp_6) <- c("ilosc apartamentow z wieksza iloscia recenzji w latach nieparzystych",
                "o tyle więcej recenzji w latach nieparzystych w apartamentach ktore maja wiecej recenzji w latach nieparzystych")

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- odp_6

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

odp_7 <- listings %>%
  select(price, room_type) %>%
  drop_na(price, room_type)

ggplot(odp_7, aes(x = price, y = room_type)) +
  geom_boxplot() +
  scale_x_log10() +
  labs(title = "Cena obiektu w zależności od typu")

#pod względem mediany najdroższe są hotel rooms a najtańsze shared rooms.
#najwiecej obserwacji odstajacych widzimy w przypadku entire home/apt
#prawdopodobnie dlatego, ze wystepuja tam bardzo duze rozstrzały standardów domów/mieszkań

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- odp_7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

listings %>% 
  mutate(first_letter = substr(host_name, 1, 1)) %>% 
  filter(grepl("^[0-9]", first_letter)) %>% 
  select(c(host_name, first_letter))

#sa 4 obserwacje gdzie pierwszym znakiem jest cyfra

odp_8 <- listings %>% 
  transmute(first_letter = if_else(grepl("^[0-9]", host_name),
                                   substr(host_name, 2, 2),
                                   substr(host_name, 1, 1))) %>% 
  drop_na(first_letter)

ggplot(odp_8, aes(x = first_letter)) +
  geom_bar() +
  labs(title = "Rozkład pierwszych liter imiona hosta")


#rozkład bez jakiejkolwiek zależności - tam, gdzie obserwacji jest bardzo mało - bardzo niestandardowe imiona
#najbardziej popularne imie wsrod hostow zaczyna sie na M

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- odp_8


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (połozenie) aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie
odp_9 <- listings %>% 
  filter(grepl("Seattle", neighbourhood_group) & is.na(price)) %>% 
  select(latitude, longitude)

ggplot(odp_9, aes(x = longitude, y = latitude)) +
  geom_point() +
  labs(title = "Położenie apartamentów w Seattle (bez podanej ceny)")

#Rozklad nie jest rownomierny - wiecej takich apartamentow jest położonych realtywnie bardziej na północ

## Odpotitle = ## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- odp_9


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
saveRDS(solutions, file = "BaginskiAdam.rds")


