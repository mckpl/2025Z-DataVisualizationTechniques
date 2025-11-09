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

zad1 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(n = n()) %>% 
  filter(n > 20) %>% 
  nrow()


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- zad1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

zad2 <- reviews %>% 
  mutate(month = format(as.Date(date), '%B')) %>% 
  group_by(month) %>% 
  summarise(n=n()) %>% 
  top_n(1, n) %>% 
  pull(month)


## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- zad2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

reviews100 <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(n=n()) %>% 
  filter(n>=100) %>% 
  pull(listing_id)

zad3 <- listings %>% 
  filter(id %in% reviews100) %>% 
  top_n(1, price) %>% 
  pull(price)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- zad3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

zad4 <- c()

zad4[1] <- reviews %>% 
  mutate(age = as.numeric(Sys.Date() - as.Date(date))/365.25) %>% 
  filter(age > 10) %>% 
  nrow()

zad4[2] <- reviews %>% 
  mutate(age = as.numeric(Sys.Date() - as.Date(date))/365.25) %>% 
  top_n(1, age) %>% 
  pull(age)



## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- zad4

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

zad5 <- listings %>% 
  group_by(room_type) %>% 
  mutate(center = mean(latitude)) %>% 
  mutate(loc = case_when(latitude>center ~ "North",
                         latitude<center ~ "South",
                         TRUE ~ NA)) %>% 
  group_by(room_type, loc) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = loc, values_from = n) %>% 
  mutate(difference = South-North) %>% 
  pull(difference, room_type)

# Liczba dodatnia będzie tu oznaczała, że o tyle więcej jest apartamentów na południu niż północy, 
# wartość bezwzględna z ujemnej na odwrót - o tyle więcej na pólnocy niz półudniu. 0 oznacza tyle samo.

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- zad5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

zad6 <- c()

reviews_n <- reviews %>% 
  group_by(listing_id) %>% 
  mutate(n=n()) %>% 
  pull(n)

decyle <- quantile(reviews_n, seq(0, 1, 0.1))

reviews_even_odd <- reviews %>% 
  group_by(listing_id) %>% 
  mutate(n=n()) %>% 
  filter(n>decyle[5] & n<decyle[6]) %>% 
  mutate(year = as.numeric(format(as.Date(date), "%Y"))) %>% 
  mutate(even_odd=case_when(year %% 2 == 0 ~ "E",
                            year %% 2 == 1 ~ "O",
                            TRUE ~ NA)) %>% 
  group_by(listing_id, even_odd) %>% 
  summarise(n_e_o = n()) %>% 
  pivot_wider(names_from=even_odd, values_from = n_e_o) %>% 
  mutate(more_in_O = ifelse(O-E>0, 1, 0))

zad6[1] <- sum(reviews_even_odd$more_in_O)

zad6[2] <- sum(reviews_even_odd[reviews_even_odd$more_in_O, "O"])

names(zad6) <- c("Ile apartamentów", "Jaka liczba opini")




## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- zad6

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

zad7_wykres <- listings %>% 
  ggplot(mapping=aes(x = room_type, y=log(price), fill=room_type)) +
  geom_violin()+
  labs(title = "Rozkład cen apartamentów w zależności od typu pokoju", x = "", y = "Logarytm z ceny")+
  scale_fill_discrete(name = "Typ pokoju")


## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- "Rozkład cen za pokoje typu Entire home/apt jest bloski normalnemu, i ceny tych pokoi są w większości wyższe niż pokoi typu Private Room czy Share Room. Pokoi Hotel Room jest dużo w wysokiej cenie (większej niż w pozostałych typach), i wraz ze spadkiem ceny spada liczba pokoi tego typu. Private Roomów jest najwięcej w niższych cenach, a wraz ze wzrostem ceny liczba takich jest mniejsza. Rozkład cen za Shared Room jest niemalże równomierny"

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

zad8_wykres <- listings %>% 
  filter(grepl("^[A-Za-z]", host_name)) %>%
  ggplot(mapping = aes(x=substr(host_name, 1, 1)))+
  geom_bar() +
  labs(title="Rozkład pierwszych liter imion hostów", x="", y="Liczba osób")


## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- "Imiona hostów na A, B, J, M oraz S są najbardziej popularne (ponad 400 osób dla każdej z tych liter), natomiast imiona  na F, G, H, I, N, O, P, Q, U, W, X, Y oraz są zdecydowanie rzadsze (mniej niż 100 hostów dla każdej z tych liter). Imiona na pozostałe litery są średnio popularne"


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

zad9 <- listings[is.na(listings$price), ] %>% 
  ggplot(mapping = aes(x=longitude, y=latitude)) +
  geom_point()



## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- "Nie, mieszkań na południowym zachodzie, które nie mają ceny w bazie danych jest zdecydowanie mniej niż tych położonych w pozostałej częsci miasta."



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
saveRDS(solutions, file = "SiembidaKarol.rds")



