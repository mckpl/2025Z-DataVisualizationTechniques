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

ans <- reviews %>%
  group_by(reviewer_id) %>%
  summarize(count = n()) %>%
  filter(count > 20) %>%
  nrow()
ans

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- ans

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

ans <- reviews %>%
  mutate(month = month.name[as.numeric(substr(reviews$date, 6, 7))]) %>%
  group_by(month) %>%
  summarize(count = n()) %>%
  arrange(-count)
ans

# Najwięcej w wakacje (lipiec/sierpień), ogólnie im dalej wakacji tym mniej

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- ans

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

ans <- reviews %>%
  group_by(listing_id) %>%
  summarize(review_count = n()) %>%
  filter(review_count >= 100) %>%
  left_join(listings, by = join_by(listing_id == id)) %>%
  arrange(-price) %>%
  first() %>%
  select(price)
ans

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- ans

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

listings_years <- reviews %>%
  group_by(listing_id) %>%
  summarize(first_review = min(date)) %>% # sortowanie alfabetyczne pięknie działa dla tego formatu dat
  left_join(listings, by = join_by(listing_id == id)) %>%
  mutate(years_on_market = as.numeric(difftime(last_review, first_review, units = "days")) / 365)

listings_10y <- listings_years %>%
  filter(years_on_market > 10) %>%
  nrow()

longest_listing <- listings_years %>%
  top_n(1, years_on_market)

ans <- data.frame(listings_10y, floor(longest_listing$years_on_market))
colnames(ans) <- c("ponad 10 lat na rynku", "największa liczba lat na rynku")
ans

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- ans

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

mean_lat <- mean(listings$latitude)
ans <- listings %>%
  mutate(south = latitude < mean_lat) %>%
  group_by(room_type, south) %>%
  summarize(count = n()) %>%
  mutate(count = ifelse(south, count, -count)) %>%
  group_by(room_type) %>%
  summarize(more_in_south_half = sum(count))
ans

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- ans

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

ans <- reviews %>%
  mutate(parity = as.numeric(substr(date, 4, 4)) %% 2) %>%
  group_by(listing_id) %>%
  summarize(all = n(), odd = sum(parity), even = all - odd) %>%
  filter(quantile(all, 0.4) <= all, all <= quantile(all, 0.5)) %>%
  filter(odd > even) %>%
  mutate(diff = odd - even) %>%
  select(listing_id, diff)
ans # różnica liczby recenzji w latach nieparzystych i parzystych dla każdego apartamentu osobno

nrow(ans) # ilość takich apartamentów

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- ans

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

ggplot(listings, aes(price, color = room_type)) +
  geom_density(aes(fill = room_type), alpha = 0.2) +
  scale_x_log10() +
  labs(title = "Rozkład cen według typu pokoju",
       x = "log10 ceny",
       y = "Gęstość")

# Rozkład cen pokojów hotelowych jest najszerszy, pokoje hotelowe są zdecydowaną większością
# zarówno najtańszych, jak i najdroższych pokoi.
# Pokoje prywatne i dzielone mają podobną średnią cenę, z tym że pokoje dzielone mają
# nieco szerszy rozkład, zwłaszcza w stronę niższych cen.
# Rozkład cen całych domów/apartamentów jest podobny, ale przesunięty wyraźnie w stronę wyższych cen.

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- listings # ?? nie mam żadnej specjalnej tabeli

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

i <- regexpr("[a-zA-Z].*", listings$host_name)
ans <- substr(listings$host_name, i, i) %>%
  table() %>%
  data.frame() %>%
  tibble() %>%
  rename("letter" = ".", "count" = Freq) %>%
  arrange(-count) %>%
  mutate(letter = factor(as.character(letter), levels = unique(letter)))
ggplot(ans, aes(x = letter, y = count)) +
  geom_col()

# Mamy kilka przedziałów częstości liter
# M J A S B występują najwięcej razy, ponad 500
# C K L V D E R między 300 a 400 razy, R około 240
# i pozostałe litery około 120 i mniej

# Chyba że weźmiemy każdego hosta tylko raz, niezależnie od liczby apartamentów:

i <- regexpr("[a-zA-Z].*", listings$host_name)
ans2 <- substr(unique(listings$host_name), i, i) %>%
  table() %>%
  data.frame() %>%
  tibble() %>%
  rename("letter" = ".", "count" = Freq) %>%
  arrange(-count) %>%
  mutate(letter = factor(as.character(letter), levels = unique(letter)))
ggplot(ans2, aes(x = letter, y = count)) +
  geom_col()

# Wtedy rozkład jest bardziej liniowo opadający i litery w trochę innej, ale nadal podobnej kolejności

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- ans


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

ans <- listings %>%
  filter(is.na(price))

ggplot(ans, aes(x = longitude, y = latitude)) +
  geom_density_2d_filled()

# Nie jest równomierny, w centrum jest znacznie większe zagęszczenie,
# a południowo-zachodniej części bardzo niskie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- ans



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
saveRDS(solutions, file = "BuczynskiPawel.rds")



