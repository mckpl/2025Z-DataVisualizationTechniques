###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# Ładowanie danych --------------------------------------------------------

reviews <- read.csv("C:\\Users\\Piotr\\Desktop\\reviews.csv")
listings <- read.csv("C:\\Users\\Piotr\\Desktop\\listings.csv")

# Instrukcja --------------------------------------------------------------

# Poniższe zadania wymagają obróbki danych. W tym celu należy wykorzystać pakiet
# dplyr i tidyr. Wynik z każdego zadania należy przypisać do zdefiniowanej nazwy
# ANS_TASK_0X. Kod utworzonych wykresów należy pozostawić w pliku .R, nie należy
# zapisywać wykresów.
# Po wykonaniu wszystkich zdań należy wykonać kod oznaczony nagłówkiem 
# "Zapisanie rozwiązań do plik .RDS".



# Zadanie 1 (0.5) ---------------------------------------------------------
# Ilu recenzentów wystawiło więcej niż 20 opinii?

z1 <- reviews %>%
  count(reviewer_id) %>%
  filter(n>20) %>%
  summarise(n()) %>%
  pull()

## Rozwiązanie

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- z1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

help(mutate)

z2 <- reviews %>%
  mutate(
    month_name = format(as.Date(date,"%Y-%m-%d"), "%B")
  ) %>%
  count(month_name) %>%
  arrange(desc(n))
  
z2 <- head(z2$month_name,3)

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- z2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

#bazując na reviews 
t1 <- reviews %>%
  count(listing_id) %>%
  filter(n>100)

max_price <- listings %>%
  filter(id %in% t1$listing_id) %>%
  summarise(max_p = max(price,na.rm=TRUE)) %>%
  pull(max_p)


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- max_price

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
oldest <- reviews %>%
  mutate(date = as.Date(date,"%Y-%m-%d")) %>%
  group_by(listing_id) %>%
  summarise(oldest_date=min(date,na.rm=TRUE)) %>%
  filter(oldest_date <= Sys.Date()-3650) ## trochę "na oko" bo przestępne...

n <- nrow(oldest)
oldest_date <- min(oldest$oldest_date,na.rm=TRUE)

n
oldest_date

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- c(n,oldest_date)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

center <- mean(listings$latitude,na.rm=TRUE)

t2 <- listings %>%
  mutate(region= ifelse(latitude<center, "pln", "pld")) %>%
  group_by(room_type,region) %>%
  summarise(num_apartments = n()) %>%
  pivot_wider(
    names_from = region,
    values_from = num_apartments,
    values_fill = 0
  ) %>%
  mutate(diff=pld-pln)


t2

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- t2

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
head(listings)

d4 = quantile(listings$number_of_reviews, 0.4, na.rm=TRUE)
d5 = quantile(listings$number_of_reviews, 0.5, na.rm=TRUE)

z6 <- listings %>%
  filter( d5 >= number_of_reviews & number_of_reviews > d4)

z7 <- reviews %>%
  mutate (
    year = as.integer(format(as.Date(date,"%Y-%m-%d"), "%Y")),
    ) %>%
    filter(year %% 2 ==0) %>%
    count(listing_id,name="reviews_even")
  
z8 <- z6 %>%
  left_join(z7, by = c("id" = "listing_id")) %>%
  mutate(
    reviews_even = ifelse(is.na(reviews_even), 0, reviews_even),  
    reviews_odd = number_of_reviews - reviews_even,              
    more_odd = reviews_odd > reviews_even                        
  ) %>%
  filter(more_odd == TRUE) %>%
  summarise(
    liczba_apartamentow = n(),
    liczba_recenzji_odd = sum(reviews_odd)
  )

z8


## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- z8

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

ggplot(listings,aes(x=room_type,y=price)) + 
  geom_boxplot() +
  labs(title="Ceny wg typu pokoju" ,x = "typ" , y="cena")

t7 <- listings %>%
  group_by(room_type) %>%
  summarise(
    mediana = median(price,na.rm=TRUE),
    IQR = IQR(price,na.rm=TRUE)
  )

t7
# caly dom - najwiekszy "rozstrzal" 
# hotel - najwyzsza mediana 
# shared i private podobny rozklad wokol mediany 

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- t7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

tempp <- listings %>%
  mutate(first_letter = substr(host_name,1,1)) %>%
  count(first_letter) %>%
  arrange(first_letter)

tempp

ggplot(tempp,aes(x=first_letter,y=n)) + 
  geom_bar(stat="identity") +
  labs(title="Liczba hostow wg liter",x="1. litera",y="n hostow")

# najwiecej; A, J, M , S ; najmniej ; Q, U,X

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- tempp


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

seattle_no_price <- listings %>%
  filter(is.na(price))

ggplot(seattle_no_price, aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.5, color = "red") 

# rozklad nie jest rownomierny - najwiecej w centrum ; malo w lewym dolnym rogu miasta 


## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- seattle_no_price



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
saveRDS(solutions, file = "SwieczewskiPiotr.rds")




