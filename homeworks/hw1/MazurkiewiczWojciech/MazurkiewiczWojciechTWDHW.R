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


head(reviews)
head(listings)
# Zadanie 1 (0.5) ---------------------------------------------------------
# Ilu recenzentów wystawiło więcej niż 20 opinii?

## Rozwiązanie
wynik1 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(n=n()) %>% 
  filter(n>20) %>% 
  nrow()
wyn1


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- wynik1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

wynik2 <- (reviews %>% 
        mutate(month = format(as.Date(date), "%B")) %>% 
        count(month) %>% 
        top_n(1,n) %>% 
        select(month))[[1]]



## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- wynik2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

wynik3 <- reviews %>%
  count(listing_id) %>%
  filter(n > 100) %>%
  inner_join(listings, by = c("listing_id" = "id")) %>%
  slice_max(price, n = 1) %>%
  pull(price)

wynik3
  


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- wynik3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
head(reviews)
wynik4<- reviews %>%
  group_by(listing_id) %>%
  summarise(
    first_review = min(as.Date(date)),             
    years_on_market = as.numeric(difftime(Sys.Date(), first_review, units = "days")) / 365
  ) 
wynik4a<-wynik4 %>% 
  filter(years_on_market>10) %>% 
  nrow()
wynik4a

wynik4b<-max(wynik4$years_on_market)
wynik4b


## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- "jest 394 takich apartamentów i maksymalny wiek apartamentu na rynku to trochę ponad 16,2 lat"

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
avg_coord = mean(listings$latitude)
wynik5<- listings %>% 
  mutate(N_S = ifelse(latitude > avg_coord,1,-1)) %>% 
  group_by(room_type) %>% 
  summarise(sum(N_S))
wynik5






## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- wynik5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

filtered_id <- (listings %>% 
  filter(number_of_reviews > quantile(number_of_reviews, 0.4) & number_of_reviews < quantile(number_of_reviews, 0.5)))$id
wynik6<-reviews %>% 
  filter(listing_id %in% filtered_id) %>% 
  mutate(
    year = as.integer(format(as.Date(date), "%Y")),
    odd_even = if_else(year %% 2L == 1L, 1L, -1L)) %>% 
  group_by(listing_id) %>% 
  summarise(balance = sum(odd_even), .groups = "drop") %>%
  filter(balance>0)
wynik6a<-nrow(wynik6)
wynik6b<-reviews %>%
  filter(listing_id %in% wynik6$listing_id,
         as.integer(format(as.Date(date), "%Y")) %% 2L == 1L) %>%
  nrow()



wynik6a
wynik6b
## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- "jest to 300 apartamentów i recenzji jest 4658"

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

listings_clean <- listings %>%
  filter(!is.na(price))

wynik7<-ggplot(listings_clean, aes(x = room_type, y = price, fill = room_type)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_continuous(trans = "log10") +
  labs(
    title = "Rozkład cen apartamentów w zależności od typu pokoju",
    x = "Typ pokoju",
    y = "Cena [USD, skala log10]"
  ) +
  theme_minimal() 


#wykres mówi nam że:
#najdroższe są hotel room, najwyższa mediana, rozrzut cen spory jednak bezsprzecznie najdroższe
#entire home/apt również drogie jednak tansze od hotel room, rorzut cen duży 
#private room niższa mediana, ewidentnie tańsze od poprzednich, mniejszy rozrzut
#shared room ewidentnie najtańszy, najmniejsza mediana i mały rozrzut w stosunku do reszty

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- wynik7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

letters_table <- listings %>%
  filter(!is.na(host_name)) %>%                             
  mutate(first_letter = substr(host_name, 1, 1)) %>%  
  count(first_letter, sort = TRUE) 
letters_table
wynik8<-ggplot(letters_table, aes(x = reorder(first_letter, -n), y = n, fill = first_letter)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "rozkład pierwszych liter imienia hosta",
    x = "Pierwsza litera imienia hosta",
    y = "liczba hostów"
  ) +
  theme_minimal()
#Ewidentnie rozkład jest nierównomierny z literami jak M J czy A wiodącymi prym a literami jak X U Q będącymi pomijalną częścią wyniku
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- wynik8


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie
no_price <- listings %>% 
  filter(is.na(price))

wynik9<-ggplot(listings, aes(x = longitude, y = latitude)) +
  geom_point(color = "salmon", size = 1.5) +                     
  geom_point(data = no_price, color = "black", size = 2, alpha = 0.8) + 
  labs(
    title = "Położenie apartamentów w Seattle – brakujące ceny zaznaczone na czarno",
    x = "Długość geograficzna",
    y = "Szerokość geograficzna"
  ) +
  theme_minimal()
#rozłożenie jest stosunkowo równomierne jednak widzimy nieprawidłowości
#duże skupisko w centrum oraz znaczne rozżedzenie na południowych obszarach miasta

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- wynik9



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
saveRDS(solutions, file = "MazurkiewiczWojciech.rds")


