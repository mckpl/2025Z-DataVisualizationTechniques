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

a_1 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(n = n()) %>% 
  filter(n > 20) %>% 
  dim() %>% 
  .[1] %>% 
  as.numeric()


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- a_1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

a_2 <- reviews %>% 
  transmute(Date = as.Date(date),
            Month = months(Date)) %>% 
  group_by(Month) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(3) %>% 
  select(Month)



## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- a_2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

rec_100 <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(n = n()) %>% 
  filter(n >= 100)  

a_3 <- listings %>% 
  filter(id %in% as.vector(rec_100$listing_id)) %>% 
  arrange(-price) %>% 
  select(price) %>% 
  head(1)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- a_3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie



a_4 <- reviews %>% 
  mutate(date_proper = as.Date(date), 
         years_passed = as.numeric(Sys.Date() - date_proper)/365) %>% 
  filter(years_passed > 10) %>% 
  summarise(max_year = max(years_passed),
            apartments_over_10 = n_distinct(listing_id))

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- a_4

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

local_equator <- mean(listings$latitude)

a_5 <- listings %>% 
  mutate(is_north = (latitude > local_equator)) %>% 
  group_by(room_type) %>% 
  summarise(diff_south_north = sum(!is_north) -  sum(is_north))


## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- a_5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

decile <- quantile(listings$number_of_reviews, probs = seq(0, 1, 0.1), na.rm = TRUE)

selected_ids <- listings %>% 
  filter(decile[5]<= number_of_reviews & number_of_reviews<=decile[6]) %>% 
  pull(id)

 tmp <- reviews %>% 
  filter(listing_id %in% selected_ids) %>% 
  mutate(year_of_review = as.numeric(format(as.Date(date), "%Y")),
         is_even = year_of_review %% 2 == 0) %>% 
  group_by(listing_id) %>% 
  summarise(diff_odd_even = sum(!is_even) -  sum(is_even),
            odd = sum(!is_even) )
 
 
 a_6 <- tmp %>% 
   filter(diff_odd_even > 0) %>% 
   summarise(number_of_reviews_total = sum(odd),
             number_of_apartaments = n())

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- a_6

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

a_7 <-listings %>% 
  select(price, room_type) %>% 
  remove_missing()

ggplot(a_7, aes(x = log(price), y = room_type)) +
  geom_boxplot(color = "black",
               fill = "pink",
               alpha = 0.7)+
  labs(title = "Distribution of apartment prices by room type",
       x = "Price in logarithmic scale",
       y = "Type of room") +
  theme_gray()

# Pokoje współdzielone są ogólnie najtańsze i wszystkie są w mniej więcej podobnym przedziale cenowym.
# Prywatne pokoje są w podobnym przedziale cenowym co pokoje współdzielone, z kilkoma pokojami 
# zdecydowanie droższymi. Mediana cen pokoi hotelowych jest największa, a ich średni przedział cenowy 
# jest najszerszy, ze względu na różne poziomy usług oferowanych przez hotele. Największe ceny 
# i największe zróżnicowanie osiągają całe apartamenty z największą ilością obserwacji odstających.
# Świadczy to o największym zróżnicowaniu ofert dla całych mieszkań.

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- a_7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

a_8 <- listings %>% 
  mutate(first_letter = substr(host_name, 1, 1)) %>% 
  filter(grepl("^[A-Za-z]+$", first_letter)) %>% 
  count(first_letter)

 ggplot(a_8, aes(x = first_letter, y = n))+
  geom_col(color = "black",
           fill = "pink",
           alpha = 0.7) +
  labs( title = "Distribution of the first letter of the names of the hosts",
        x = "Letters",
        y = "Frequency")

# Najpopularniejsze pierwsze litery imienia hosta to M, J, A, które wszystkie przekraczają 600 obserwacji.
# Najmniej popularne są imiona zaczynające się na X, U, Q, każda mająca mniej niż 10 obserwacji. Pozostałe
# popularne litery znajdują się na początku alfabetu między A i E, w środku alfabetu, między J i M, a także pod
# koniec między R i T.

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- a_8


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?


## Rozwiązanie

a_9 <- listings %>% 
  filter(is.na(price))

ggplot(a_9, aes(x = latitude, y = longitude)) +
  geom_point(color = "black",
             fill = "pink",
             alpha = 0.7) +
  labs(title = "Distribution of apartments in Seattle, which do not show thier price",
       x = "Latitude",
       y = "Longitude")

# Rozkład apartamentów w Seattle, które nie mają pokazanej ceny nie jest równomierny, najwięcej z nich jest 
# znajduje się w centrum miasta, czyli w najbardziej pożądanym przez turystów obszarze. Może to świadczyć
# o celowym zabiegu hostów, którzy korzystając z wysokiej konkurencji w obszarze, mogą sobie pozwolić na mniejszą
# transparentość w zakresie ceny.

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- a_9



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
saveRDS(solutions, file = "DmitrukAleksandra.rds")

