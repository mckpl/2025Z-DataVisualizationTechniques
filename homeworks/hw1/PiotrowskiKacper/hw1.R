###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# Ładowanie danych --------------------------------------------------------

reviews <- read.csv("C:/IAD/Semestr_3/Techniki_Wizualizacji_Danych/Homework/reviews.csv")
listings <- read.csv("C:/IAD/Semestr_3/Techniki_Wizualizacji_Danych/Homework/listings.csv")

# Instrukcja --------------------------------------------------------------

# Poniższe zadania wymagają obróbki danych. W tym celu należy wykorzystać pakiet
# dplyr i tidyr. Wynik z każdego zadania należy przypisać do zdefiniowanej nazwy
# ANS_TASK_0X. Kod utworzonych wykresów należy pozostawić w pliku .R, nie należy
# zapisywać wykresów.
# Po wykonaniu wszystkich zdań należy wykonać kod oznaczony nagłówkiem 
# "Zapisanie rozwiązań do plik .RDS".

View(reviews)
View(listings)
# Zadanie 1 (0.5) ---------------------------------------------------------
# Ilu recenzentów wystawiło więcej niż 20 opinii?

## Rozwiązanie
ans_01 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(liczba_opinii = n()) %>% 
  filter(liczba_opinii > 20) %>% 
  summarise(ile = n())

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- ans_01

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
reviews %>% 
  mutate(month = format(as.Date(date), "%m")) %>% 
  group_by(month) %>% 
  summarise(liczba_opinii = n()) %>% 
  filter(liczba_opinii == max(liczba_opinii))

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- "08"

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
opinie <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(liczba_opinii = n()) %>% 
  filter(liczba_opinii >= 100)
  
joined <- inner_join(opinie, listings, by=c("listing_id"="id"))
max(joined["price"], na.rm=TRUE)


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- 1012

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
reviews %>% 
  filter(date <= as.Date("2015-10-24")) %>% 
  summarise(liczba_apartamentów = n_distinct(listing_id))
  

# Drugą część zadania rozumiem jako różnicę między najstarszą a najnowszą opinią danego apartamentu, bo wybranie po prostu najstarzej recenzji wydaje się zbyt proste
reviews %>% 
  group_by(listing_id) %>% 
  summarise(
    najstarsza = min(as.Date(date), na.rm=TRUE),
    najnowsza = max(as.Date(date), na.rm=TRUE),
    lata_na_rynku = as.numeric(difftime(najnowsza, najstarsza, units='days'))/365
  ) %>% 
  summarise(max = max(lata_na_rynku), na.rm=TRUE)

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- c(392, 14.1)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
center <- mean(listings$latitude, na.rm = TRUE)

listings <- listings %>% 
  mutate(region = ifelse(latitude>=center, "Północ", "Południe"))

ile <- listings %>% 
  group_by(room_type, region) %>% 
  summarise(liczba = n(), .groups="drop")

roznice <- ile %>% 
  pivot_wider(
    names_from = region,
    values_from = liczba,
    values_fill = 0
  ) %>% 
  mutate(roznica = Południe - Północ)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- roznice

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

listings_in_decile <- listings %>% 
  filter(number_of_reviews >= quantile(number_of_reviews, 0.4) & number_of_reviews <= quantile(number_of_reviews, 0.5)) %>% 
  rename(listing_id = id)

needed_ids <- listings_in_decile$listing_id

listings_with_more_odd <- reviews %>% 
  filter(listing_id %in% needed_ids) %>% 
  mutate(year = as.numeric(format(as.Date(date), "%Y"))) %>% 
  mutate(odd_or_even = ifelse(year%%2==0, "even", "odd")) %>% 
  group_by(listing_id, odd_or_even) %>% 
  summarise(ile = n()) %>%
  pivot_wider(names_from = odd_or_even, values_from = ile, values_fill = 0) %>% 
  mutate(difference = odd-even) %>% 
  filter(difference>0)

ile_apartamentow <- nrow(listings_with_more_odd)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- list(ile_apartamentow, listings_with_more_odd)

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
plot_07 <- listings %>% 
  ggplot(aes(x=room_type, y=price))+
  geom_boxplot()+
  labs(
    title = "Rozkład cen apartamentów w zależności od typu pokoju",
    x = "Typ pokoju",
    y = "Cena (log)"
  )+
  scale_y_log10() # Skala logarytmiczna dla czytelności
plot_07
## Odpowiedz przypisana do zmiennej
# Jako odpowiedź podaję plota, bo w tym zadaniu nie robiłem żadnej operacji na ramce danych
ANS_TASK_07 <- plot_07
# Rozkład jest różny w zależności od typu pokoju.
# Entire home/apt ma ceny mocno rozrzucone, jednak w większości są one wysokie
# Hotel room - Najwyższa mediana cen, ceny są przeważnie wysokie, jednak zbliżone do siebie
# Private room - Tańsze z kilkoma zauważalnie droższymi przypadkami
# Shared room - Najtańsze ze wszystkich, wszystkie ceny znajdują się względnie blisko mediany

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
ans_08 <- listings %>% 
  mutate(first_letter = substr(host_name, 1, 1)) %>% 
  filter(grepl("^[A-Z]$", first_letter))

plot_08 <- ans_08 %>% 
  ggplot(aes(x=first_letter))+
  geom_bar()+
  labs(
    title = "Rozkład pierwszych liter hostów",
    x = "Pierwsza litera",
    y = "Ilość"
  )
plot_08

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- ans_08
# Rozkład jest bardzo nierównomierny.
# Dominują litery "A", "J", "M" i "S".
# Litery "Q", "U", "X" i "Z" występują bardzo nielicznie.


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie
ans_09 <- listings %>% 
  filter(is.na(price)) 
  
plot_09 <- ans_09 %>% 
  ggplot(aes(x = longitude, y = latitude))+
  geom_point()+
  labs(
    title = "Rozkład położenia apartamentów w Seatle",
    x = "Długość geograficzna",
    y = "Szerokość geograficzna"
)
plot_09

## Odpowiedz przypisana do zmiennej
# Rozkład nie jest równomierny. Największa ilość apartamentów skumulowana jest w centrum, podczas gdy na obrzeżach jest znacznie mniej.
# Dodatkowo widoczna jest duża pusta przestrzeń odcinająca lewy dolny róg rozkładu punktów.
# Wynika ona z obecności zatoki w Seatle, więc logicznie rzecz biorąc apartamentów tam raczej nie będzie.

ANS_TASK_09 <- ans_09



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
saveRDS(solutions, file = "PiotrowskiKacperK.rds")
