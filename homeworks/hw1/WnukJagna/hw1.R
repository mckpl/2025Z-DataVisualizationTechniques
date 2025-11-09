###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# Ładowanie danych --------------------------------------------------------

reviews <- read.csv("/Users/jagnawnuk/Downloads/reviews.csv")
listings <- read.csv("/Users/jagnawnuk/Downloads/listings.csv")

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

length(table(reviews$reviewer_id)[table(reviews$reviewer_id)>20])

ans1 <-
reviews %>%
  group_by(reviewer_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 20) %>% 
  nrow()



## Odpowiedz przypisana do zmiennej
  ANS_TASK_01 <- ans1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
ans2 <-
reviews %>% 
mutate(month = substr(date, 6, 7))%>% 
group_by(month) %>% 
summarise(count = n()) %>% 
arrange(desc(count))

ans2
## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- ans

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
apartments_over_100_reviews <-
reviews %>% 
group_by(listing_id) %>% 
summarise(count = n()) %>% 
filter(count > 100)

apartments_over_100_reviews <- apartments_over_100_reviews$listing_id

ans3 <-
listings %>% 
filter(id %in% apartments_over_100_reviews) %>% 
top_n(1,price)

ans3 <- ans_3$price

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- ans3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?


## Rozwiązanie

  apartments_over_10_years_in_business <-
  reviews %>% 
  group_by(listing_id) %>% 
  summarise(min_date = min(date), max_date = max(date)) %>% 
  mutate(years_in_business = as.numeric(difftime(max_date, min_date, units = "days")) / 365) %>% 
  filter(years_in_business > 10) %>% 
  arrange(desc(years_in_business))

  maksymalna_liczba_lat_na_rynku <-
  apartments_over_10_years_in_business %>% 
  top_n(1,years_in_business) %>% 
  select(years_in_business) %>% 
  floor

  liczba_apartamentow_powyżej_10 <-
  apartments_over_10_years_in_business %>% 
  nrow()
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- "Apartamentów na rynku powyżej 10 lat jest 121. Najdłuższy czas apartamentu na rynku to 14 lat"

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

avg_long = mean(listings$latitude)

ans5 <-
listings %>% 
  mutate(nort_south = ifelse(latitude < avg_long,1,-1)) %>% 
  group_by(room_type) %>% 
  summarise(sum(nort_south))
names(ans5)[2] <- "południe"

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- ans5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

number_of_reviews <-
reviews %>% 
group_by(listing_id) %>% 
summarise(count = n())


decil_4 = quantile(number_of_reviews$count, 0.4)
decil_5 = quantile(number_of_reviews$count, 0.5)

chosen_apartaments <-
number_of_reviews %>% 
  filter(count >= decil_4, count <= decil_5)

ans6 <-
reviews %>% 
  filter(listing_id %in% chosen_apartaments$listing_id) %>% 
  mutate(year = as.integer(substr(date, 1, 4))) %>% 
  mutate(is_not_even = ifelse(year %% 2 == 0,-1,1)) %>% 
  group_by(listing_id) %>% 
  summarise (not_even = sum(is_not_even)) %>% 
  filter(not_even > 0)

nrow(ans6) # ilość apartamentów które miały więcej recenzji w latach nieparzystych (341)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- ans6

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
ggplot(listings, aes(x=room_type, y=price)) +
  geom_violin() + scale_y_log10()

ans7 <- listings %>% 
  select(room_type, price)


#W przypadku całego domu lub mieszkania ceny są bardzo zróżnicowane (najszerszy zakres). Największe zagęszczenie ofert występuje w przedziale około 100–500
#W przypadku pokoi hotelowych rozkład cen jest węższy – większość wartości koncentruje się w przedziale od około 100 do 1000. Pojedyncze wartości odstające wskazują na kilka tańszych ofert.
#Ceny pokoi prywatnych skupiają się w niższym zakresie, głównie około 50–200, z niewielkim rozrzutem w górę.
#Ceny ofert pokoi współdzielonych są najniższe i najmniej zmienne – brak wartości odstających, a rozkład jest w miarę jednolity.

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- ans7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

listings %>% 
  mutate(first_letter = substr(host_name, 1, 1)) %>% 
  filter(first_letter %in% LETTERS) %>% 
  ggplot(mapping = aes(x=first_letter)) + 
  geom_bar()

ans8 <- 
  listings %>% 
  mutate(first_letter = substr(host_name, 1, 1)) %>% 
  filter(first_letter %in% LETTERS) %>% 
  select(first_letter)
    

#Litery takie jak A, B, J, M oraz S występują wyjątkowo często, osiągając wartości w przedziale około 550–650 wystąpień.
#Częstotliwość liter C, D, E, K, L, R i T oscyluje w granicach 250–400 wystąpień.
#Litery Q, U, X oraz Z pojawiają się bardzo rzadko, natomiast pozostałe litery występują zazwyczaj poniżej 100 razy.

#Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- ans8


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?
  listings %>% 
  filter(is.na(price)) %>% 
  ggplot(mapping = aes(x=latitude, y=longitude)) +
  geom_point()

listings %>% 
  filter(is.na(price)) %>% 
ggplot(aes(x = latitude, y = longitude)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white")

ans9 <-
listings %>% 
  filter(is.na(price)) %>% 
  select(latitude, longitude)

#Rozkład ten jest jest równomierny ponieważ punkty są rozproszone z różną gęstością na całym obszarze.

 ## Rozwiązanie


## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- ans9



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
saveRDS(solutions, file = "WnukJagna.rds")


