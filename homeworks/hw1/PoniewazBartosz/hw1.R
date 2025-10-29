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

over20_reviews <- nrow(reviews %>% group_by(reviewer_id) %>%
  summarise(review_count=n()) %>%
  filter(review_count>20))

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- over20_reviews
ANS_TASK_01 #just to see it :)

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
reviews <- mutate(reviews, date = as.Date(date), month = format(date, "%B"))

most_reviewed_month <- (reviews %>% 
  group_by(month) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count)) %>% 
  select(month) %>% 
  head(1))[[1]]

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- most_reviewed_month

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

number_of_reviews <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(cnt = n()) %>% 
  rename(id = listing_id)

biggest_price <- (listings %>% 
  left_join(number_of_reviews, by = 'id') %>% 
  filter(cnt > 100) %>% 
  arrange(desc(price)) %>% 
  select(price) %>% 
  head(1))[[1]]

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- biggest_price

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

older_than_10_years <- reviews %>% 
  inner_join(listings, join_by(listing_id == id)) %>% 
  group_by(id) %>% 
  summarise(min_date = min(date)) %>% 
  mutate(years = as.numeric(as.Date(Sys.Date(), format="%Y-%m-%d")- as.Date(min_date, format="%Y-%m-%d"))/365) %>% 
  # uzylem Sys.date zeby po prostu porownac do wyniku z dnia odpalenia programu
  filter(years > 10) %>% 
  arrange(desc(years))

how_many_older_than_10 <-   summarise(older_than_10_years ,count = n()) #ile takich ktore byly dluzej niz 10 lat
max_years_on_market <- summarise(older_than_10_years ,longest = max(as.integer(years))) #najdluzej ile jakis apartament byl na rynku


## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- c(how_many_older_than_10, max_years_on_market)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

s_n_centre <- summarise(listings, mean_latitude = mean(latitude))[[1]]

how_many_south_over_north <- listings %>% 
  mutate(side = ifelse(latitude < s_n_centre,"south", "north")) %>% 
  group_by(room_type, side) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = side, values_from = count, values_fill = 0) %>% 
  mutate(south_over_north = south - north)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- how_many_south_over_north

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

review_limits <- quantile(listings$number_of_reviews, probs = c(0.4, 0.5))
correct_apts_id <- listings %>%
  filter(number_of_reviews >= review_limits[1] & number_of_reviews <= review_limits[2]) %>%
  pull(id) # wyciagamy kolumne jako wektor

apt_with_more_odd_reviews <- reviews %>%
  filter(listing_id %in% correct_apts_id) %>%
  mutate(is_odd_year = as.numeric(format(date, "%Y")) %% 2 != 0) %>%
  group_by(listing_id) %>%
  summarise(
    odd_count = sum(is_odd_year),
    even_count = sum(!is_odd_year)) %>%
  ungroup() %>% 
  filter(odd_count > even_count) %>%
  mutate(
    odd_difference = odd_count - even_count,
    total_reviews = odd_count + even_count)

how_many_odd_more_than_even <- (apt_with_more_odd_reviews %>%
  summarise(
    how_many = n(), # Correctly counts the number of apartments
    total_number_of_reviews = sum(total_reviews)
  ))[[1]]

# ramka danych rozpisuje ile jest opinii w nieparzystych i parzystych latach
# dla tych apartamentów dla których jest więcej tych nieparzystych opinii,
# różnice w ilości oraz całkowitą liczbę opinii dla danego apartamentu
# natomiast zmienna how_many_odd_more_than_even zawiera informację
# ile apartamentów spełnia ten warunek

ANS_TASK_06 <- c(how_many_odd_more_than_even, apt_with_more_odd_reviews)

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

wykres_rozkladu_07 <- listings %>% 
  select(id, room_type, price) %>% 
  ggplot(aes(x = room_type, y = price, fill = room_type)) + 
    geom_violin() + 
    labs(title = "Rozkład cen apartamentów w zależności od typu pokoju", 
         x = "typ pokoju", 
         y = "cena apartamentu (log)") +
  scale_y_log10() +
  coord_flip() +
  guides(fill = "none") #dla ładnych kolorów

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- wykres_rozkladu_07

'''
Rozkład prezentuje się w różny sposób w zależności od typu pokoju.

shared room - Ceny są średnie i skupione, nie ma skrajności,
a rynek jest dość stabilny bo wszystkie apartamenty mają podobne ceny

private room - najwięcej pokoji jest dostępna w cenach średnich na rynku,
podobnych do shared room ale rozstrzał jest większy, mały wybór tańszych prywatnych pokoi
ale za to można znaleźć taki droższy

hotel room - duży rozstrzał, mało pokoji o średniej cenie, jest dużo drogich pokoic
i mniej ale nadal dużo tych tanich

entire home/apt - większość zbliżona do średnich cen rynkowych,
największy rozstrzał pomiędzy skrajnie drogimi i skrajnie tanimi domami/apartamentami
skrajności tych jest jednak mało

'''

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

wykres_rozkladu_08 <- listings %>% 
  mutate(first_name_letter = substr(host_name,1, 1)) %>% 
  group_by(first_name_letter) %>%  
  filter(grepl("[A-Z]", first_name_letter)) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x = first_name_letter, y = count)) + 
    geom_col(color = "mediumseagreen", fill = "lightgreen") + 
    labs(title = "Rozkład pierwszych liter imion hostów", 
         x = "Pierwsza litera imienia hosta", 
         y = "Ilość wystąpnień litery")


## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- wykres_rozkladu_08
'''
niektóre litery występują częściej, np A,B,J,M,S, a np Q,U,X praktycznie nie występują
'''

# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

wykres_rozkladu_09 <- filter(listings,is.na(price)) %>% 
  ggplot(aes(x = longitude, y = latitude)) + 
  geom_hex() + 
  labs(title = "Rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych", 
       x = "Szerokość geograficzna", 
       y = "Długość geograficzna")

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- wykres_rozkladu_09

'''
Wykres pozornie pokazuje nierównomierny rozkład mieszkań bez podanej ceny w Seatle, 
ale po porównaniu współrzędnych z realną mapą, przestzrzeń na wykresie
na której jest pusto, po lewej stronie na dole (taki pas) to akwen wodny
gdzie nie ma mieszkań, co w rozsądny sposób tłumaczy nierównomierność rozkładu.
Widać też zagęszczenie w centrum wykresu, realnie są to okolice centrum miasta
'''

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
saveRDS(solutions, file = "PoniewazBartosz.rds")


