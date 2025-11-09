###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# Ładowanie danych --------------------------------------------------------

reviews <- read.csv("C:\\PDU\\reviews.csv")
listings <- read.csv("C:\\PDU\\listings.csv")

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



## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- reviews %>%
  group_by(reviewer_id) %>%
  summarise(reviews_number = n()) %>%
  filter(reviews_number>20) %>%
  summarise(n())


# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?


## Rozwiązanie


ans02 <- reviews %>%
  transmute(month = format(as.Date(date), "%m")) %>%
  group_by(month) %>%
  summarise(reviews_number = n()) %>%
  top_n(3, reviews_number) %>%
  arrange(desc(reviews_number))
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <-c("3 miesiace w ktorych jest wystawiane najwiecej opinii", ans02)


# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie





## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- reviews %>%
  group_by(listing_id) %>%
  summarise(reviews_number = n()) %>%
  filter(reviews_number >99) %>%
  left_join(listings, by = c("listing_id" = "id") ) %>%
  top_n(1, price) %>%
  select(price)

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
today <- as.Date("2025-10-18")
apps_over10 <- reviews %>% 
  mutate(date = as.Date(date)) %>%
  group_by(listing_id) %>%
  summarise(date_difference = as.numeric(today - min(date, na.rm = TRUE))/365) %>%
  filter(date_difference > 10.0)




## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- apps_over10 %>%
  summarise(how_many = n(), max_age = max(date_difference))

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

average_lat <- listings %>%
  summarise(avg = mean(latitude))
average_lat <- as.numeric(average_lat)
  
  
    

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- listings %>%
  transmute(room_type, where = ifelse(latitude>=average_lat, "north", "south")) %>%
  group_by(where, room_type) %>% 
  summarise(number = n()) %>%
  pivot_wider(names_from = where, values_from = number, values_fill = 0)  %>%
  mutate(diff = south - north) %>%
  select(diff, room_type)

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
decyle <- quantile(listings$number_of_reviews, probs = c(0.4, 0.5), na.rm = TRUE)
 

idx <- listings %>%
  filter(number_of_reviews >= decyle[1] & number_of_reviews <= decyle[2]) %>%
  pull(id)

  more_odd <- reviews%>%
  filter(listing_id %in% idx) %>%
  mutate(even_odd = ifelse(as.numeric(format(as.Date(date), "%Y")) %% 2 == 0, "even", "odd"))%>%
    group_by(listing_id)%>%
    summarize(odd = sum(even_odd == "odd", na.rm = TRUE), even = sum(even_odd == "even", na.rm=TRUE)) %>%
    mutate(diff = odd - even)%>%
    filter(diff>0)

  ans1 <- more_odd %>%
    group_by(listing_id)%>%
    summarize(count = n())%>%
    summarize(ans = sum(count)) %>%
    pull(ans)
  
  ans2 <- more_odd %>%
    summarize(sum = sum(odd))


## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- c(ans1, ans2)

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

plot1 <- listings %>%
  ggplot(aes(x = price)) +
  geom_histogram(bins = 20, fill = "green", color = "black") +
  facet_wrap(~ room_type, scales = "free_y") + 
  scale_x_continuous(limits = c(0,1000)) +
  labs(x = "Price per night", y = "Number of listings", title = "Distribution of Apartment Prices by Room Type" )
plot1
  



## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- listings

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

letters <-listings %>%
  mutate(first_letter = substr(host_name, 1, 1))  %>%
  filter(grepl("^[A-Za-z]$", first_letter)) %>%
  group_by(first_letter) %>%
  summarize(count = n()) 

  letters %>%
  ggplot(aes(x = first_letter, y = count)) +
  geom_col(fill = "darkred") +
  labs(title = "First Letter of Host Names", x = "first letter", y = "count")




## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- letters


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

missing_price <- 
  listings %>%
  filter(is.na(price))
missing_price %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(color = "blue", alpha = 0.4 ) +
  labs(title = "Location of Apartments with Missing Price")




## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <-c("na podstawie wykresu - rozklad nie jest rownomierny",  missing_price)



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
saveRDS(solutions, file = "ZalewskiHubert.rds")


