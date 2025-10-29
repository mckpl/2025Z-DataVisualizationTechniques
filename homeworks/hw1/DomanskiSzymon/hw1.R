###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# Ładowanie danych --------------------------------------------------------

reviews <- read.csv("C:/Users/Szef/Desktop/polibuda/TWD/reviews.csv")
listings <- read.csv("C:/Users/Szef/Desktop/polibuda/TWD/listings.csv")

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
output<-reviews %>% 
  group_by(reviewer_id) %>% 
  summarize(count = n()) %>% 
  filter(count > 20) %>% 
  summarize(n = n()) %>% 
  '[['(1)


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- output

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
output<-reviews %>% 
  mutate(month = substr(date,6,7)) %>% 
  group_by(month) %>% 
  summarize(count = n()) %>% 
  arrange(-count) %>% 
  head(3) %>% 
  '[['(1)



## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- output

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
review_df <- data.frame(reviews %>% 
  group_by(listing_id) %>% 
  summarize(count = n()))
review_listings <- merge(listings, review_df, by.x  = 'id', by.y = 'listing_id')

output <- review_listings %>% 
  filter(count>100) %>% 
  top_n(1, price) %>% 
  select(price) %>% 
  '[['(1)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- output

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
output<-reviews %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(listing_id) %>% 
  transmute(oldest_review = min(date)) %>% 
  unique(by = listing_id) %>% 
  mutate(age = as.numeric(Sys.Date() - oldest_review)) %>% 
  filter(age > 10*365) 

output1 <- output %>% 
  dim() %>% 
  '[['(1)

output2 <- max(output[["age"]])/365
output <- c(output1, output2)

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- output

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
mean_latitude = mean(listings$latitude)
output<-listings %>% 
  mutate(up_or_down = ifelse(latitude>=mean_latitude, "UP", "DOWN")) %>% 
  group_by(room_type, up_or_down) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = up_or_down,
              values_from = count) %>% 
  mutate(UP = replace_na(UP, 0), DOWN = replace_na(DOWN,0)) %>% 
  mutate(difference = DOWN - UP) %>% 
  select(room_type, difference)  
  

  


## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- output

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
decyle <- quantile(listings$number_of_reviews, probs = c(0.4, 0.5), na.rm = TRUE)

filtered_listings <- listings %>% 
  filter(number_of_reviews >= decyle[1] & number_of_reviews <= decyle[2])

filtered_listings <- reviews[reviews$listing_id %in% filtered_listings$id,  ]

output<-filtered_listings %>% 
  mutate(year = as.numeric(substr(date,1,4))) %>% 
  mutate(odd_or_even = ifelse(year%%2==1, "odd", "even")) %>% 
  group_by(listing_id, odd_or_even) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = odd_or_even,
              values_from = count, 
              values_fill = 0) %>% 
  mutate(difference = odd - even) %>% 
  filter(difference > 0)
  
output1 <- output %>% 
  dim() %>% 
  '[['(1)
output2 <- output %>% 
  select(listing_id, difference)
output <- list(output1, output2)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- output

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
#cena - ilościowa, ilorazowa
dane7 <- listings %>% 
  select(price, room_type)

ggplot(dane7, aes(x = price, y = room_type)) +
  geom_boxplot(na.rm = TRUE, color = 'navy', fill = 'blue', alpha = 0.1, median.colour = 'darkblue', outlier.color = 'red', outlier.alpha = 0.3) + 
  scale_x_log10() +
  labs(title = 'Rozkład cen apartamentów w zależności od typu pokoju',
       x = 'Cena (log)',
       y = 'Typ pokoju')

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- dane7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
first_letters <- listings %>% 
  mutate(first_letter = sub("^[^A-Za-z]*([A-Za-z]).*", "\\1", host_name)) %>%
  group_by(first_letter) %>% 
  summarize(count = n())

ggplot(first_letters, aes(x = first_letter, y = count)) +
  geom_col(color = 'darkblue', fill = 'blue', alpha = 0.2) +
  labs(title = 'rozkład pierwszych liter hostów',
       x = "Pierwsza litera",
       y = "Liczba wystąpień")

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- first_letters


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie
listings_Seattle <- listings %>% 
  filter(is.na(price)) %>% 
  select(neighbourhood_group, latitude, longitude, price)

ggplot(listings_Seattle, aes(x = longitude, y = latitude)) +
  geom_point(color = 'navy', alpha = 0.5, size = 3) +
  geom_density_2d(color = "red", linetype = "dashed", size = 0.8) + 
  labs(title = 'Rozkład położenia apartamentów bez ceny w Seattle',
       x = "Długość geograficzna",
       y = "Szerokość geograficzna")

# rozkład nie jest równomierny

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- listings_Seattle



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
saveRDS(solutions, file = "DomanskiSzymon.rds")

