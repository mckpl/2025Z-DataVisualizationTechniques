###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
library(dplyr)
library(tidyr)
library(ggplot2)


# Ładowanie danych --------------------------------------------------------

reviews <- read.csv("reviews.csv")
listings <- read.csv("listings.csv")

View(reviews)
View(listings)
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
answer_1 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(n = n()) %>% 
  filter(n>20) %>% 
  count() %>% 
  pull(n)

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- answer_1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
answer_2 <- reviews %>% 
  mutate(month = substr(date,6,7)) %>% 
  group_by(month) %>% 
  summarise(n = n()) %>% 
  top_n(3) %>% 
  arrange(-n) %>% 
  pull(month) %>% 
  as.numeric()

answer_2 <- month.name[answer_2]


## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- answer_2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
over_100 <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(n=n()) %>% 
  filter(n>=100) %>% 
  pull(listing_id)

answer_3 <- listings %>% 
  filter(id %in% over_100) %>% 
  top_n(1, price) %>% 
  pull(price) 

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- answer_3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

over_10_years <- reviews %>% 
  filter(as.Date(date) < Sys.Date() - (10*365+3)) # +3 bo były 3 lata przestępne

answer_4a <- over_10_years$listing_id %>% 
  unique() %>% 
  length()

answer_4b <- reviews %>% 
  mutate(age = as.numeric(round((Sys.Date() - as.Date(date))/365.25,1))) %>% 
  select(age) %>% 
  max()

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- list(answer_4a, answer_4b)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

mid <- mean(listings$latitude)

answer_5 <- listings %>% 
  mutate(direction = ifelse(latitude > mid, "N", "S")) %>% 
  group_by(room_type, direction) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = direction, values_from = n,
              values_fill = 0) %>% 
  mutate("more_on_south" = S - N) %>% 
  select(room_type, more_on_south)

answer_5

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- answer_5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

Min <- quantile(listings$number_of_reviews, 0.4)
Max <- quantile(listings$number_of_reviews, 0.5)

ids <- listings %>% 
  filter(number_of_reviews >= Min & number_of_reviews <= Max) %>%
  pull(id)
  
answer_6 <- reviews %>% 
  filter(listing_id %in% ids) %>% 
  mutate(year = as.numeric(substr(date,1,4))) %>% 
  mutate(year = ifelse(year %% 2 == 1, "odd", "even")) %>% 
  group_by(listing_id, year) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = year, values_from = n,
              values_fill = 0) %>% 
  filter(odd>even)

answer_6

answer_6a <- length(answer_6$listing_id)
answer_6b <- answer_6 %>% 
  mutate(diff = odd - even) %>% 
  select(listing_id, diff)

answer_6b

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- list(answer_6a, answer_6b)

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

answer_7 <- listings %>% 
  select(room_type, price) %>% 
  filter(!is.na(price))

answer_7

answer_7 %>% 
  ggplot(aes(x = price, y = room_type)) +
  geom_boxplot(outlier.color = "red", color = "navy",
               fill = "skyblue", alpha = 0.5) +
  labs(title = "Rozkład cen apartamentów w zależności od typu pokoju",
       x = "cena",
       y = "typ pokoju") +
  scale_x_log10() + 
  labs(x = "population (log)") +
  theme_bw()

# Mediana cen rośnie w zależności od typu zakwaterowania.
# najtańsze są pokoje dzielone i prywatne, następnie całe mieszkania/domy,
# najdroższe i najbardziej zróżnicowane cenowo są pokoje hotelowe.

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- answer_7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

answer_8 <- listings %>% 
  transmute(first_letter = toupper(substr(host_name, 1, 1))) %>% 
  filter(first_letter %in% LETTERS)

answer_8

answer_8 %>% 
  ggplot(aes(x = first_letter)) +
  geom_bar(fill = "navy") +
  labs(title = "Rozkład pierwszych liter imienia hostów",
       x = "Pierwsza litera imienia hosta",
       y = "częstość wystąpienia") +
  theme_bw()

# Rozkład pierwszych liter imion hostów jest nierównomierny,
# z wyraźną dominacją liter M, J, A, S, B.
# litery takie jak Q, U, X, Z występują śladowo.

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- answer_8


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

answer_9 <- listings %>% 
  filter(is.na(price)) %>% 
  select(longitude, latitude) %>% 
  filter(!is.na(longitude) & !is.na(latitude))

answer_9

answer_9 %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(color = "navy", alpha = 0.5) +
  labs(title = "Rozkład apartamentów z brakującą ceną w Seatle ",
       x = "Długość geograficzna",
       y = "Szerokość geograficzna")

# Rozkład nie jest równomierny, ponieważ apartamenty z brakującą ceną 
# tworzą wyraźne skupisko w centralnej części mapy, 
# podczas gdy w pozostałych obszarach są rozproszone rzadziej.

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- answer_9



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
saveRDS(solutions, file = "DabrowskiKrzysztof.rds")
