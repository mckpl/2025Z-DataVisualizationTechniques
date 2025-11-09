###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Ładowanie danych --------------------------------------------------------

reviews <- read.csv("reviews.csv")
listings <- read.csv("listings.csv")

# Instrukcja --------------------------------------------------------------

# Poniższe zadania wymagają obróbki danych. W tym celu należy wykorzystać pakiet
# dplyr i tidyr. Wynik z każdego zadania należy przypisać do zdefiniowanej nazwy
# ANS_TASK_0X. Kod utworzonych wykresów należy pozostawić w pliku .R, nie należy
# zapisywać wykresów.
# Po wykonaniu wszystkich zadań należy wykonać kod oznaczony nagłówkiem 
# "Zapisanie rozwiązań do plik .RDS".



# Zadanie 1 (0.5) ---------------------------------------------------------
# Ilu recenzentów wystawiło więcej niż 20 opinii?

## Rozwiązanie
ans1 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(n=n()) %>% 
  filter(n>20) %>% 
  summarise(n = n())

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- ans1[[1]]

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

ans2 <- reviews %>% 
  mutate(date.month = str_split(date, "-", simplify = T)[,2]) %>% 
  group_by(date.month) %>% 
  summarise(n=n()) %>% 
  top_n(2, n)

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- ans2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

ids <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(n=n()) %>% 
  filter(n>100)

ans3 <- listings %>% 
  filter(id %in% ids$listing_id) %>% 
  top_n(1, price) %>% 
  select("price")


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- ans3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie


a <- reviews %>% 
  mutate(age = 2025 - as.numeric(str_split(date, "-", simplify = T)[,1])) %>%
  filter(age>10) %>% 
  group_by(listing_id) %>% 
  summarise(oldest.rev = max(age))
  
ans4 <- c(summarise(a,n = n()), summarise(a, max(oldest.rev)))

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- ans4

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
meanLat <- mean(listings$latitude)

ans5 <- listings %>% 
  mutate(NS = ifelse(latitude > meanLat, "north", "south")) %>% 
  group_by(room_type, NS) %>%
  summarise(n=n()) %>% 
  pivot_wider(names_from = NS, values_from = n, values_fill = 0) %>% 
  transmute(ile = south - north)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- ans5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
apt <- listings %>%  
  mutate(percentyl = ntile(number_of_reviews, 100)) %>% 
  filter(percentyl>40 & percentyl<50)

b <- reviews %>% 
  filter(listing_id %in% apt$id) %>% 
  mutate(date.year = as.numeric(str_split(date, "-", simplify = T)[,1])) %>%
  mutate(odd.even = ifelse(date.year %% 2 == 1, "odd", "even")) %>% 
  group_by(listing_id, odd.even) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = odd.even, values_from = n) %>% 
  filter(odd - even > 0) %>% 
  ungroup()

ans6 <- c(summarise(b, ile = n_distinct(listing_id)), transmute(b, diff = odd - even))


## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- ans6

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
listings %>% 
  ggplot(aes(x=room_type, y=price))+
  geom_boxplot()+
  labs(title="Zależność cen apartamentów od typu pokoju", x = "typ pokoju", y = "cena")



## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- listings %>% 
  select(room_type, price)

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

listings %>% 
  mutate(name.firstletter = str_extract(host_name, "^[A-Z]")) %>% 
  filter(!is.na(name.firstletter)) %>%
  ggplot(aes(name.firstletter))+
  geom_bar()
  



## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- listings %>% 
  mutate(name.firstletter = str_extract(host_name, "^[A-Z]")) %>% 
  filter(!is.na(name.firstletter)) %>% 
  select(name.firstletter)

ANS_TASK_08
# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

listings %>% 
  filter(is.na(price)) %>% 
  ggplot(aes(latitude, longitude, color = neighbourhood_group))+
  geom_point()
  


## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- listings %>% 
  filter(is.na(price)) %>% 
  select(latitude, longitude)

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
saveRDS(solutions, file = "KrajewskaNatalia.rds")

