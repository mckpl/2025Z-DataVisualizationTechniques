###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# Ładowanie danych --------------------------------------------------------

# reviews <- read.csv("reviews.csv")
# listings <- read.csv("listings.csv")

reviews <- read.csv("/Users/maciek/Desktop/OrlowskiMaciej/reviews.csv")
listings <- read.csv("/Users/maciek/Desktop/OrlowskiMaciej/listings.csv")

View(reviews)
View(listings)

# Insreviews# Instrukcja --------------------------------------------------------------

# Poniższe zadania wymagają obróbki danych. W tym celu należy wykorzystać pakiet
# dplyr i tidyr. Wynik z każdego zadania należy przypisać do zdefiniowanej nazwy
# ANS_TASK_0X. Kod utworzonych wykresów należy pozostawić w pliku .R, nie należy
# zapisywać wykresów.
# Po wykonaniu wszystkich zdań należy wykonać kod oznaczony nagłówkiem 
# "Zapisanie rozwiązań do plik .RDS".



# Zadanie 1 (0.5) ---------------------------------------------------------
# Ilu recenzentów wystawiło więcej niż 20 opinii?

## Rozwiązanie

ans01 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(reviews_count = n()) %>% 
  filter(reviews_count > 20) %>% 
  count()

ans01 <- ans01[[1]]

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- ans01

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

ans02 <- reviews %>% 
  separate(col = date, into = c("year", "month", "day"), sep = "-") %>% 
  group_by(month) %>% 
  summarise(month_count = n()) %>% 
  arrange(desc(month_count)) %>% 
  head(1)[[1]]

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- ans02

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

reviews_count <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(how_many_reviews = n())

ans03 <- listings %>% 
  left_join(reviews_count, by=c("id" = "listing_id")) %>% 
  filter(how_many_reviews > 100) %>% 
  select(price) %>% 
  arrange(desc(price)) %>% 
  head(1)[[1]]

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- ans03

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

today <- as.Date("2025-10-25", format = "%Y-%m-%d")

more_than_10y <- reviews %>% 
  mutate(date = as.Date(date, format="%Y-%m-%d")) %>% 
  group_by(listing_id) %>% 
  summarise(from_when = min(date)) %>% 
  mutate(years_on_market = as.numeric(today - from_when)/365) 
  
number_of_apartments_more_than_10y_on_market <- more_than_10y %>% 
  filter(years_on_market > 10) %>% 
  summarise(count = n()) %>% 
  head(1)[[1]]

longest_apartment_on_market <- more_than_10y %>% 
  summarise(max_number_of_years = max(years_on_market)) %>% 
  head(1)[[1]] %>% 
  round(0)
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- c(number_of_apartments_more_than_10y_on_market, longest_apartment_on_market)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

middle <- mean(listings$latitude)

ans05 <- listings %>% 
  mutate(
    north_south = case_when(latitude > middle ~ "north",
                            TRUE ~ "south")) %>% 
  group_by(room_type, north_south) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = north_south, values_from = count) %>%
  mutate(north = ifelse(is.na(north), 0, north)) %>% 
  mutate(diff = south - north) %>% 
  select(room_type, diff)
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- ans05

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

apartments_id_between_4_5_decile <- listings %>% 
  filter(number_of_reviews >= quantile(number_of_reviews, 0.4) & number_of_reviews <= quantile(number_of_reviews, 0.5)) %>% 
  select(id)

diff_in_reviews <- reviews %>% 
  separate(col = date, into = c("year", "month", "day"), sep = "-") %>% 
  mutate(year = as.numeric(year),
         even_odd_year = ifelse(year%%2==0, "even", "odd")) %>% 
  group_by(listing_id, even_odd_year) %>% 
  summarise(num_of_reviews = n()) %>% 
  filter(listing_id %in% apartments_id_between_4_5_decile$id) %>% 
  pivot_wider(names_from = even_odd_year, values_from = num_of_reviews) %>% 
  mutate(diff_between_reviews_in_even_odd_years = odd-even) %>% 
  filter(diff_between_reviews_in_even_odd_years > 0) %>% 
  select(listing_id, diff_between_reviews_in_even_odd_years)

number_of_apartments_with_more_reviews_in_odd_year <- nrow(diff_in_reviews)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- c(diff_in_reviews, number_of_apartments_with_more_reviews_in_odd_year)

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

plot_1_input <- listings %>% 
  mutate(room_type_in_polish = case_when(room_type == "Shared room" ~ "Pokój Łączony",
                                         room_type == "Private room" ~ "Pokój prywatny",
                                         room_type == "Hotel room" ~ "Pokój hotelowy",
                                         room_type == "Entire home/apt" ~ "Cały dom/mieszkanie"
  )) %>% 
  select(room_type_in_polish, price)

plot_1_input %>% 
  ggplot(aes(x = price, y = room_type_in_polish)) + 
  geom_boxplot() + 
  labs(x = "Cena apartamentu (log10)",
       y = "Rodzaj pokoju",
       title = "Ceny apartamentu w zależności od typu pokoju") +
  theme_bw() +
  scale_x_log10()

'''
Z wykresu wynika, że średnio najdroższe są pokoje hotelowe,
chociaż najwyższe ceny miały całe mieszkania/domy na wynajem, 
gdzie widać największy rozrzut, czyli ceny za ten rodzaj wynajmu 
były najbardziej różnorodne. 

Widzimy, że średnie ceny za pokój prywatny jak i łączony są bardzo podobne, 
ale przy pokoju łączonym ceny są bardziej skupione (patrząc na ogłoszenia możemy
spodziewać się podobnych cen), natomiast przy pokoju prywatnym te ceny już mogą 
być wyższe (przy poszczególnych pokojach).

Możemy zauważyć jeszcze punkty o bardzo małych wartościach w przypadku pokoju 
hotelowego i całego domu/mieszkania. Może to wynikać z faktu, że podane ceny zostały
uwzględnione dla innej skali wynajmu (np. dziennej, godzinowej) lub jest to po 
prostu bład w danych.
'''

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- plot_1_input

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

plot_2_input <- listings %>% 
  mutate(hosts_name_first_letter = substring(host_name, 1, 1)) %>% 
  select(hosts_name_first_letter) %>% 
  group_by(hosts_name_first_letter) %>% 
  summarise(letters_count = n()) %>% 
  slice(-1) #ręcznie zobaczyłem, że jest tylko jeden wiersz, który pokazywał, że 
            #nazwa hosta zaczyna się na liczbę, nie literę, więc usunąłęm pierwszy wiersz 

plot_2_input %>% 
  ggplot(aes(x = hosts_name_first_letter, y = letters_count)) +
  geom_col() +
  labs(x = "Pierwsza litera nazwy hosta", #oś x nazwałem "nazwy hosta", ponieważ nie wszystkie nazwy hostów to ich imiona 
       y = "Liczba hostów",
       title = "Rozkład pierwszych liter nazw hostów")

'''
Z wykresu wynika, że najwięcej hostów ma nazwy zaczynające się na litery: A, J, M, S,
z kolei niewielka liczba hostów ma imiona, lub nazwy zaczynające się na litery: O, Q, U, X, Z.
Rozkład ten nie jest równomierny. 
'''

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- plot_2_input


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

apartments_without_given_price <- listings %>% 
  filter(is.na(price)) %>% 
  select(latitude, longitude, id)

apartments_without_given_price %>% 
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point() +
  labs(x = "Długość geograficzna",
       y = "Szerokość geograficzna",
       title = "Rozkład apartamentów w Seatle bez podanej ceny")

'''
Z wykresu wynika, że położenie tych apartamentów nie jest do końca równomierne. 
Na północy widać w miarę równo rozmieszczone punkty, ale w centrum widzimy ich zagęszczenie. 
Na połudnu jest ich zdecydowanie najmniej i są rozrzucone bardziej na wschód i zachód.
'''

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- apartments_without_given_price



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
saveRDS(solutions, file = "OrlowskiMaciej.rds")



