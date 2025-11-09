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

reviewers_count <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(n = n()) %>%
  filter(n > 20) %>% 
  nrow()


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- reviewers_count

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

str(reviews) # widzimy, że kolumna date jest typu chr, zatem użyję funkcji substr, żeby dostać tylko napis z liczbą porządkową kolejnych miesięcy

months_counts <- reviews %>%
  mutate(month = substr(date, nchar(date) - 4, nchar(date) -3)) %>% 
  group_by(month) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(3)


## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- months_counts$month


# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

id_more_than_100 <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(n = n()) %>% 
  filter(n >= 100) 

str(listings) # widzimy, że kolumna price jest typu int
  
max_price <- listings %>% 
  filter(id %in% id_more_than_100$listing_id) %>% 
  summarise(max = max(price, na.rm = TRUE))
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- as.integer(max_price)

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
today <- as.Date(Sys.Date(), format = "%Y-%m-%d")
ago <- as.Date(paste0(as.integer(format(today, "%Y")) - 10, "-", format(today, "%m-%d")))

first_dates <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(first_date = min(date))

longerthan_10 <- first_dates %>%
  filter(first_date < ago) %>% 
  nrow()

max_years <- max(as.numeric(today - as.Date(first_dates$first_date), units = "days"))/365
 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- c(longerthan_10, max_years)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

center <- mean(listings$latitude, na.rm = TRUE)

difference <- listings %>% 
  mutate(region = ifelse(latitude < center, "południe", "północ")) %>% 
  group_by(room_type) %>%
  summarise(
    diff = sum(region == "południe") - sum(region == "północ"))


## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- difference

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

quant <- quantile(listings$number_of_reviews, c(0.4, 0.5), na.rm = TRUE)

df <- listings %>% 
  filter(number_of_reviews >= quant[1] & number_of_reviews <= quant[2])

filtered <- reviews[reviews$listing_id %in% df$id, ]
  
dff <- filtered %>% 
  mutate(year_type = ifelse(as.numeric(substr(date, 1,4))%%2 == 0, "even", "odd")) %>% 
  group_by(listing_id, year_type) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = year_type, values_from = n, values_fill = 0) %>% 
  mutate(difference = odd - even)

dfff <- dff %>% 
  filter(difference > 0)
  
counts_of_apartments <- dfff %>% 
  nrow()

counts_of_reviews <- sum(dfff$odd)


## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- c(counts_of_apartments, counts_of_reviews)

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

ans7 <- listings[, c("price", "room_type")]

wykres <- ggplot(ans7, aes(x = price))+
  geom_histogram(bins = 50)+
  facet_wrap(~ room_type, scales = "free")+
  labs(title = "Rozkład cen apartamentów w zależności od typu pokoju", x = "Cena", y = "Liczba ofert")+
  xlim(0,1000)

wykres2 <- ggplot(ans7, aes(y = price, x = room_type)) +
  geom_boxplot(outlier.color = 'red', outlier.alpha = 0.3) + 
  scale_y_log10() +
  labs(title = 'Rozkład cen apartamentów w zależności od typu pokoju',
       y = 'Cena (log10)',
       x = 'Typ pokoju')
# na tym wykresie jeszcze lepiej widać rozkład cen apartamentów w porównaniu do innych typów pokojów

# widzimy z wykresów, że dla:
# Entire home/apt - ilość ofert zwiększa się wraz z ceną apartamentów do ok. 125, następnie spada.
# Hotel room - liczba ofert zmienia się w miarę proporcjonalnie do wzrostu ceny
# Private room - liczba ofert rośnie wraz z wzrostem ceny do ok. 70, następnie spada. najwięcej ofert w zakresie argumentów od ok. 50 do 120
# Shared room - liczba ofert podobna dla każdego argumentu



## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- ans7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

p <- listings %>% 
  mutate(first_letter = substr(host_name, 1, 1)) %>%
  filter(grepl("[A-Z]", first_letter)) %>% 
  group_by(first_letter) %>% 
  summarize(n = n())
  
ans8 <- ggplot(p, aes(x = first_letter, y = n)) +
  geom_col() +
  labs(title = "Rozkład pierwszych liter hostów",
     x = "Pierwsza litera",
     y = "Liczba hostów")


## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- p


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład(położenie) aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

without_price <- listings %>% 
  filter(is.na(price) | price == 0)

ggplot(without_price, aes(y = latitude, x = longitude))+
  geom_point(alpha = 0.5)+
  labs(title = "Rozkład apartamentów w Seatle niemających podanej ceny",
       y = "Szerokość geograficzna", x = "Długość geograficzna")

# Widać, że rozkład nie jest równomierny, gdyż punkty nie są jednakowo gęsto rozmieszczone na całym obszarze.
# Na wykresie widać kilka skupisk, i puste obszary.

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- "Nie"





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
saveRDS(solutions, file = "PostekWiktoria.rds")


