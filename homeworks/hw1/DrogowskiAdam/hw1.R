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
str(reviews)
str(listings)
View(reviews)
reviews$date <- as.Date(reviews$date) # trzeba sobie ułatwić życie

# Zadanie 1 (0.5) ---------------------------------------------------------
# Ilu recenzentów wystawiło więcej niż 20 opinii?

## Rozwiązanie
  
z1 <- reviews %>%
  count(reviewer_id) %>%
  filter(n > 20) %>%
  nrow()


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- z1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

z2 <- reviews %>%
  mutate(month = format(date, "%m")) %>%
  count(month) %>%                                
  arrange(desc(n)) 

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- z2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

z3 <- reviews %>%
  count(listing_id) %>%                       
  filter(n >= 100) %>%                        
  inner_join(listings, by = c("listing_id" = "id")) %>%  
  summarise(max(price, na.rm = TRUE))

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- z3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

z4 <- reviews %>%
  mutate(date = as.Date(date)) %>%            
  group_by(listing_id) %>%                      
  summarise(first_review = min(date, na.rm = TRUE)) %>%  
  mutate(years_on_market = as.numeric(as.Date("2025-10-28")- first_review) / 365) %>%  
  summarise(
    ile_ponad_10_lat = sum(years_on_market > 10, na.rm = TRUE),  
    najdluzej = max(years_on_market, na.rm = TRUE)             
  )


## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- z4

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

srodek <- mean(listings$latitude, na.rm = TRUE)

z5 <- listings %>%
  mutate(region = ifelse(latitude > srodek, "north", "south")) %>%
  count(region) %>%
  summarise(odp = n[region == "south"] - n[region == "north"])

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- z5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie


z4 <- listings %>%
  filter(number_of_reviews >= quantile(listings$number_of_reviews, 0.4, na.rm = TRUE) & number_of_reviews <= quantile(listings$number_of_reviews, 0.5, na.rm = TRUE)) %>%
  select(id) %>%
  inner_join(reviews, by = c("id" = "listing_id")) %>%
  select(id,date) %>%
  mutate(year = as.numeric(format(date, "%y"))) %>%
  mutate(parzysty = ifelse(year %% 2 == 0, 1, 0)) %>%
  mutate(nparzysty = ifelse(year %% 2 == 1, 1, 0)) %>%
  group_by(id) %>%
    summarise(
      suma_parzyste = sum(parzysty, na.rm = TRUE),
      suma_nieparzyste = sum(nparzysty, na.rm = TRUE)
    ) %>%
  filter(suma_parzyste < suma_nieparzyste) %>%
  summarise(ile = n(), o_ile = sum(suma_nieparzyste) - sum(suma_parzyste))
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- z4

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

z7 <- ggplot(listings, aes(x = room_type, y = price)) +
  geom_boxplot(na.rm = TRUE, outlier.color = "green") +
  scale_y_log10() +
  labs(x = "Typ pokoju", y = "Cena", title = "Rozklad cen wg. typu pokoju")

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- z7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie


dane8 <- listings %>%
  mutate(first_letter = toupper(substr(host_name, 1, 1))) %>%  
  count(first_letter) %>%                                      
  arrange(desc(n)) %>%
  head(26)

z8 <- ggplot(dane8, aes(x = first_letter, y = n)) +
  geom_col(fill = 'green') +
  labs(title = "Rozkład pierwszej litery",
       x = "Pierwsza litera",
       y = "Liczba hostów")

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- z8


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

z9 <- listings %>%
  filter(is.na(price)) %>%
  summarise(
    sd_lat = sd(latitude, na.rm = TRUE),
    sd_long = sd(longitude, na.rm = TRUE)
  )

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- z9 # ja bym powiedział, że jest, bo odchylenie standardowe jest dosc niskie



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
saveRDS(solutions, file = "DrogowskiAdam.rds")



