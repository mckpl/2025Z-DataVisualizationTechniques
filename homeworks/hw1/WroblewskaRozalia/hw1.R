###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
library(dplyr)
library(tidyr)
library(ggplot2)

# Ładowanie danych --------------------------------------------------------

reviews <- read.csv("reviews.csv")
listings <- read.csv("listings.csv")
View(reviews)
View(listings)

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
t <- table(reviews$reviewer_id)
ans1 <- count(as.data.frame(t[t > 20]))



## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- ans1[1,1]

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

t <- table(months(as.Date(reviews$date)))
t <- sort(t,decreasing = TRUE)
ans2 <- names(head(t))

## Odpowiedz przypisTRUE## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- ans2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

appartaments_with_more_than_100_reviews <- names(table(reviews$listing_id)[table(reviews$listing_id) > 100] )

listings %>% 
  filter(id %in% appartaments_with_more_than_100_reviews) %>% 
  arrange(price) %>% 
  select(price) -> ans3



## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- ans3[1,]

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartamentów na rynku?

## Rozwiązanie
reviews %>% 
  filter(as.numeric(format(as.Date(date), "%Y")) < 2015) %>% 
  select(listing_id) %>% 
  unique() %>% 
  count() -> how_many_appartments_longer_than_10_years

reviews %>% 
  mutate(year = as.numeric(format(as.Date(date), "%Y"))) %>% 
  select(year) %>% 
  arrange(year) -> appartament_years

max_appartament_year <- appartament_years[[1,1]]


## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- c(how_many_appartments_longer_than_10_years, max_appartament_year)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
middle <- mean(listings$latitude)
listings %>% 
  filter(latitude > middle) -> north_appartments
listings %>% 
  filter(latitude < middle) -> south_appartments

table(south_appartments$room_type) -> south_appartments
table(north_appartments$room_type) -> north_appartments
north_appartments[["Hotel room"]] <- 0
difference <- c()
for (col_i in names(south_appartments)){
  difference[col_i] = south_appartments[col_i] - north_appartments[col_i]
}

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- difference

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

t <- table(reviews$listing_id)

reviews %>% 
  mutate(count = t[as.character(listing_id)]) %>% 
  mutate(decil = ntile(count, 10)) %>% 
  filter(decil %in% c(4,5)) %>% 
  mutate(year = as.numeric(format(as.Date(date), "%Y"))) %>% 
  mutate(is_even = ifelse(year%%2 == 0, "even", "odd")) -> appartments_we_want


appartments_we_want %>%
  group_by(listing_id) %>%
  summarise(total_reviews = n(),
            odd_year_reviews  = sum(year %% 2 == 1),
            even_year_reviews = sum(year %% 2 == 0)) -> reviews_count

reviews_count %>% 
  filter(odd_year_reviews > even_year_reviews) %>% 
  mutate(difference = odd_year_reviews - even_year_reviews) %>% 
  select(difference) -> reviews_count

# tych apartamentów jest 383 (długość reviews_count)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- reviews_count

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie


listings %>% 
  ggplot(aes(x = room_type, y = price, fill = room_type)) +
  geom_violin() +
  ylim(0, 2000) +
  labs(title = "Rozkład cen apartamentów w zależności od typu pokoju",
       x = "typ pokoju",
       y = "cena") -> wykres1

wykres1

# wykres1 to wykres badania rozkładu jednej zmiennej, dlatego violin plot
# nie wiem jaką tabelę dać jako odpowiedź, sam wykres wydaje mi się w tym zadaniu jedyną odpowiedzią

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- wykres1

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

# "wyciągam" z host_name, a później odrzucam liczby
listings %>% 
  mutate(first_letter = substr(host_name,1,1)) %>% 
  filter(!(first_letter %in% as.character(c(0:9)))) %>% 
  select(first_letter) -> first_letters

# zliczam liczbę wystąpień poszczególnych liter
first_letters <- table(first_letters)

# mam tabelę, dlatego geom_col to dobre rozwiązanie
data.frame(first_letters) %>% 
  ggplot(aes(x = first_letter, y = Freq)) +
  geom_col() +
  labs(title = "Rozkład pierwszych liter hostów",
       y = "frequency") +
  theme_minimal()


##geom_col()## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <-first_letters


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

listings %>% 
  filter(is.na(price)) %>% 
  select(c(latitude, longitude, neighbourhood_group)) -> without_price

#wykres badania rozkładu dwóch zmiennych - geom_point
without_price %>% 
  ggplot(aes(x = latitude, y = longitude, colour = neighbourhood_group)) +
  geom_point() +
  labs(title = "Położenie apartamentów w mieście bez podanej ceny")
# trudno ocenić, czy położenie apartamentów jest równomierne - raczej nie, ale też nie jest bardzo rozproszone

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- without_price



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
saveRDS(solutions, file = "WroblewskaRozalia.rds")

