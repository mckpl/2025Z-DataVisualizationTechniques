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
# Po wykonaniu wszystkich zdań należy wykonać kod oznaczony nagłówkiem 
# "Zapisanie rozwiązań do plik .RDS".



# Zadanie 1 (0.5) ---------------------------------------------------------
# Ilu recenzentów wystawiło więcej niż 20 opinii?

## Rozwiązanie

x1 <-reviews %>% 
      group_by(reviewer_id) %>% 
      summarise(n=n()) %>% 
      filter(n>20) %>% 
      count()



## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- x1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
x2 <- reviews %>% 
  mutate(month = lapply(strsplit(date,'-'),'[[',2)) %>% 
  group_by(month) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- x2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

a <-reviews %>% 
  group_by(listing_id) %>% 
  summarise(n=n()) %>% 
  filter(n>100) %>% 
  select(listing_id)

x3 <- listings %>% 
  filter(id %in% a$listing_id) %>% 
  select(price) %>% 
  top_n(1)


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- x3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie




## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- NA

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

x5 <- listings %>% 
  mutate(avg_latitude= mean(latitude)) %>% 
  mutate(position= ifelse(latitude > avg_latitude,'North','South')) %>% 
  group_by(room_type) %>% 
  summarise(southNorthDiff= sum(position=='South')-sum(position=='North'))


## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- x5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie




## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- NA

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
x7<- listings %>% 
  ggplot(aes(x=room_type,y=price))+
  geom_boxplot()+
  coord_flip()+
  labs(title="Rozkład cen apartametnów w zależności od typu pokoju",
       x='cena',y='typ pokoju')


## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- x7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
x8 <-listings %>% 
  mutate(firstLetter= str_extract(host_name,"^[A-Za-z]"))
x8 %>% 
  ggplot(aes(x=firstLetter))+
  geom_bar()



## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- NA


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie




## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- NA



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
saveRDS(solutions, file = "NazwiskoImie.rds")


