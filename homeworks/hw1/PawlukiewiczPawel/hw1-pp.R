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

head(listings)
head(reviews)

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


odp1 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(n=n()) %>% 
  filter(n>20) %>% 
  nrow()
  
odp1


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- odp1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
odp2 <- (reviews %>% 
  mutate(month = format(as.Date(date), "%B")) %>% 
  count(month) %>% 
  top_n(1,n) %>% 
  select(month))[[1]]

odp2


## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- odp2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie


id_to_keep <- reviews %>%
  count(listing_id) %>% 
  filter(n>=100)


odp3 <- (listings %>% 
  filter(id %in% c(id_to_keep$listing_id)) %>% 
  top_n(1, price) %>% 
  select(price))[[1]]

odp3


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- odp3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

apart_age <- reviews %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(listing_id) %>% 
  summarise(review1 = min(date)) %>% 
  mutate(years_since_review1 = as.numeric(Sys.Date() - review1)/365) %>% 
  filter(years_since_review1>10)

number_old_aparts <- apart_age %>%     
  nrow()            # liczba apartamentów, które są na
                    # rynku dłużej niż 10 lat
                    # 394

oldest_apart_age <- round(max(apart_age$years_since_review1),0)



              #maksymalna liczba lat apartametów na rynku
              # 16 lat

number_old_aparts
oldest_apart_age

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- c(number_old_aparts,oldest_apart_age)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie


avg_lat <- (listings %>%  
               summarise(avg_lat = mean(latitude)))[[1]]

odp5 <-  listings %>% 
  mutate(n_or_s = ifelse(latitude<avg_lat, "South", "North")) %>% 
  group_by(room_type, n_or_s) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = n_or_s, values_from = n, values_fill = 0) %>% 
  mutate(South_vs_North = South - North)

odp5


## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- odp5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
filtered_aparts <- listings %>% 
  filter(number_of_reviews > quantile(number_of_reviews, 0.4) & number_of_reviews < quantile(number_of_reviews, 0.5))

filtered_aparts_id <- filtered_aparts$id

aparts_oddly_reviewed <- reviews %>% 
  filter(listing_id %in% filtered_aparts_id) %>% 
  mutate(year = as.numeric(format(as.Date(date), "%Y")), odd_or_even = ifelse(year %% 2 == 0, "even", "odd")) %>% 
  group_by(listing_id, odd_or_even)  %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = odd_or_even, values_from = n, values_fill = 0) %>% 
  filter(odd > even) %>% 
  mutate(how_many_more_odd = odd - even)
  
n_of_aparts_oddly_reviewed <- aparts_oddly_reviewed %>% 
  nrow()

aparts_oddly_reviewed

n_of_aparts_oddly_reviewed

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- c(n_of_aparts_oddly_reviewed, aparts_oddly_reviewed)

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie



odp7 <- ggplot(listings, aes(x = room_type, y = price)) +
  geom_boxplot() +
  labs(
    title = "Rozkład cen apartamentów według typu pokoju",
    x = "Typ pokoju",
    y = "Cena za pokój (w skali logarytmicznej)") +
  scale_y_log10() + # Używam log scale dla lepszej czytelności wykresu 
  theme_minimal() 
      

odp7
## Komentarz do wykresu: 
# Rozkład cen apartamentów znacznie różni się w zależności od typu pokoju:
# 
# 1. Entire home/apt - ogólnie dość wysokie ceny (wysoka mediana), jest też dużo 
# skrajności, szczególnie tych bardzo drogich apartamentów odstających od normy,
# największa rozpiętość cen
# 
# 2. Hotel room - zdecydowanie najwyższa mediana, wysokie ceny oraz widać dużą
# rozpiętość cen
# 
# 3. Private room - niska mediana i stosunkowo niskie ceny, jest też kilka pokoi
# o wysokiej cenie, odstających od normy 
# 
# 4. Shared room - najniższa mediana, niskie ceny, ceny tych pokoi stosunkowo mało
# różnią się od siebie, można powiedzieć że są skupione



## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- odp7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

help_df <- listings %>% 
  mutate(host_first_letter = substr(host_name, 1, 1)) %>% 
  group_by(host_first_letter) %>% 
  summarise(n=n()) %>% 
  filter(grepl("[A-Z]", host_first_letter))   # litery, nie cyfry !
  
odp8 <- ggplot(help_df, aes(x = host_first_letter, y = n)) + 
  geom_col(color = "navy", fill = "salmon") + 
  labs(title = "Rozkład pierwszych liter nazw hostów",
        x = "Pierwsza litera nazwy hosta",
        y = "Liczba wystąpień") + 
  theme_minimal()
    

odp8
## Komentarz do wykresu:
# Rozkład pierwszych liter hostów jest bardzo nierównomierny, znacznie odstają od
# reszty i mają najwięcej wystąpień litery A, B, J, M, S, a litery Q, U, Y, Z mają
# najmniej wystąpień (jako pierwsze litery hostów oczywiśćie)







## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- odp8


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

seatle_is_na <- listings %>% 
  filter(is.na(price))

odp9 <- ggplot(seatle_is_na, aes(x = longitude, y = latitude)) +
  geom_point(color = "tomato") + 
  labs(title="Położenie apartamentów (bez ceny) w Seatle",
       x = "Długość geograficzna",
       y = "Szerokość geograficzna") + 
  theme_minimal()

##Komentarz do wykresu: 
# NIE, rozkład apartamentów w Seatle które nie mają podanej ceny nie jest 
# równomierny - dominują apartamenty w cenrum i na północy miasta, z kolei na 
# południowym zachodzie jest ich bardzo mało, zatem NIE można powiedzieć, że 
# rozkład jest równomierny


odp9
## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- odp9



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
saveRDS(solutions, file = "PawlukiewiczPawel.rds")


