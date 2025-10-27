###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
install.packages("tidyr")
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

a <- reviews %>% 
  count(reviewer_id) %>% 
  filter(n > 20) %>% 
  summarise(n = n()) %>% 
  pull(n)


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- a

# więcej niż 200 opinii wystawiło 32 recenzentów



# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
b <- reviews %>% 
  mutate(date = as.Date(date)) %>% 
  transmute(month = format(date, "%m")) %>% 
  count(month) %>% 
  arrange(desc(n))


## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- b

### najwięcej recenzji jest wystawianych w sierpniu i czerwcu



# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
apartments <- reviews %>% 
                count(listing_id) %>% 
                filter(n >= 100)
c <- listings %>%
  filter(id %in% apartments$listing_id) %>%
  arrange(desc(price)) %>%
  slice_head(n = 1) %>%  
  pull(price)


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- c

### Najwyższa cena za apartament przy założeniu, że musi mieć co najmniej 100 recenzji wynosi 1012



# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

d <- reviews %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(listing_id) %>%
  summarise(first_date = min(date, na.rm = TRUE)) %>% 
  mutate(years_on_market = as.numeric(difftime(Sys.Date(), first_date, units = "days")) / 365) %>%
  summarise(
    n_over_10_years = sum(years_on_market > 10),
    max_years = max(years_on_market, na.rm = TRUE)
  )

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- d

#### Dłużej niż 10 lat na rynku są 394 apartamenty, a maksymalna liczba lat apartamentu na rynku to około 16 lat. 




# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
center <- mean(listings$latitude)

f <- listings %>% 
  mutate(direction = ifelse(latitude >= center, "north", "south")) %>% 
  group_by(room_type, direction) %>% 
  count() %>% 
  pivot_wider(names_from = direction, values_from = n, values_fill = 0) %>%
  mutate(diff_south_north = south - north)


## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- f

### W kategorii Entire home/apt apartamentów na południu jest więcej niż tych na północy o 363,
### w kategorii Hotel room o 21, 
### w kategorii Private room tych na południu jest mniej o 38,
### w kategorii Shared rooom apartamentów na południu i na północy jest tyle samo


# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
quantile_45 <- listings %>% 
  filter(number_of_reviews > quantile(number_of_reviews, 0.4, na.rm = TRUE),
         number_of_reviews <= quantile(number_of_reviews, 0.5, na.rm = TRUE))

## kod do polecenia na githubie ###
# g <- reviews %>% 
#   filter(listing_id %in% quantile_45$id) %>% 
#   mutate(date = as.Date(date)) %>% 
#   mutate(year = ifelse(as.numeric(format(date, "%y")) %% 2 == 0, "even", "not_even")) %>% 
#   group_by(listing_id, year) %>% 
#   count() %>% 
#   pivot_wider(names_from = year, values_from = n, values_fill = 0) %>%
#   filter(not_even > even) %>%
#   ungroup() %>% 
#   summarise(
#     n_apartments = n(),
#     total_reviews = sum(not_even)
#   )

### kod po sprostowaniu w issues ###

g <- reviews %>% 
  filter(listing_id %in% quantile_45$id) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = ifelse(as.numeric(format(date, "%Y")) %% 2 == 0, "even", "not_even")) %>% 
  group_by(listing_id, year) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  pivot_wider(
    names_from = year, 
    values_from = n, 
    values_fill = 0
  ) %>% 
  mutate(difference = not_even - even) %>% 
  filter(difference > 0) 

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- g %>% 
  mutate(more_noteven_number = nrow(g))

### więcej recenzji w latach nieparzystych niz parzystych mialy 324 apartamenty ####




# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

ggplot(listings, aes(x = room_type, y = price)) +
  geom_boxplot(outlier.color = 'tomato') +
  labs(title = "Apartment prices vs room type")



## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- "Najdroższe z reguły są pokoje hotelowe(Hotel room), ale jest wiele apartamentów z kategorii Entire home/apt  do wynajcia, których
ceny są znacznie wyższe i odstają od reszty ofert zarówno w tej kategorii jak i innych"





# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

first_letters <- listings %>% 
  mutate(first_letter = substr(host_name, 1, 1)) %>% 
  filter(str_detect(first_letter, "^[A-Za-z]"))

ggplot(first_letters, aes(x = first_letter)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Hosts - first letters of their names",
    x = "First letter of host name",
    y = "Count"
  ) +
  theme_minimal()


## Odpowiedz przypisana do zmiennej

ANS_TASK_08 <- first_letters

### Rozkład nie jest równomierny, najwięcej hostów ma imię rozpoczynające się na litery A, J i M, najmniej na - Q, U, X.




# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie
seattle <- listings %>% 
  filter(is.na(price))

k <- ggplot(seattle, aes(x = neighbourhood_group)) +
  geom_bar() +
  labs(title = "Apartments with missing price in Seattle") +
  theme_minimal()


## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- seattle

### Nie, jeśli bierzemy pod uwagę dzielnice to rozkład nie jest równomierny.
### W dzielnicy Downtown jest znacznie wiecej apartamentów na wynajem niż w dzielnicach takich jak Interbay, Seward Park, czy Magnolia.
### Może to wynikać np. z powierzchni poszczególnych dzielnic, ale takich danych nie mamy.


# Zapisanie rozwiązań do plik .RDS ----------------------------------------


# PROSZĘ NIC NIE ZMIENIAĆ  ------------------------------------------------

solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, 
                  ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09)

names(solutions) <- c("Task01", "Task02", "Task03", 
                      "Task04", "Task05", "Task06",
                      "Task07", "Task08", "Task09")

solutions
#   -----------------------------------------------------------------------

# Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "SitkowskaAleksandra.rds")


