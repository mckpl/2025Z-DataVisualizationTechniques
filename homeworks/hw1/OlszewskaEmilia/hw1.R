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
View(reviews)
x1 <- reviews %>%
  count(reviewer_id, name = "count_rewievs") %>%
  filter(count_rewievs>20) %>%
  summarise(n()) %>%
  pull()


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- x1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
glimpse(reviews)
x2 <- reviews %>%
  mutate(month = format(as.Date(date),"%m")) %>%
  count(month,name = "count_month") %>%
  arrange(-count_month) %>%
  select(month) %>%
  head(3) %>%
  pull()

x2

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- x2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
x3 <- listings%>%
  filter(number_of_reviews>=100)%>%
  select(price)%>%
  top_n(1)%>%
  pull()



## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- x3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
x4_1 <- reviews %>%
  select(listing_id,date) %>%
  filter( as.Date(date) < as.Date("2015-10-20"))%>%
  distinct(listing_id) %>%
  count() %>%
  pull(n)

x4_2 <- reviews %>%
  mutate(year = format(as.Date(date),"%Y"),number = 2025-as.integer(year))%>%
  arrange(-number)%>%
  #select(number)%>%
  head(1)%>%
  pull(number)

x4 <- c(x4_1,x4_2)
x4

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- x4

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
x5 <- listings %>%
  mutate(polozenie = if_else(latitude > mean(latitude,na.rm=TRUE), "north", 'south')) %>%
  group_by(room_type,polozenie) %>%
  summarise(liczba = n(), .groups = "drop") %>%
  pivot_wider(names_from = polozenie, values_from = liczba, values_fill = 0) %>%
  mutate(difference = south - north)%>%
  select(room_type,difference)

x5

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- x5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

str(listings)
q4 <- quantile(listings$number_of_reviews,probs = 0.4, na.rm=TRUE)
q5 <- quantile(listings$number_of_reviews,probs = 0.5, na.rm=TRUE)
quantile(listings$number_of_reviews, probs = seq(0, 1, 0.1), na.rm = TRUE)

id_good <- listings%>%
  filter(number_of_reviews>=q4 & number_of_reviews<=q5)%>%
  pull(id)

df_filtered <- reviews%>%
  filter(listing_id %in% id_good)%>%
  mutate(year = format(as.Date(date),"%Y"),count_by_year = ifelse(as.numeric(year) %% 2 == 0, -1,1))%>%
  select(listing_id, count_by_year)%>%
  group_by(listing_id)%>%
  summarise(roznica = sum(count_by_year),parzyste = sum(count_by_year== -1),niepatzrsyte = sum(count_by_year == 1),.groups = "drop")%>%
  filter(roznica>0)
  
number_of_apart <- df_filtered%>%
  nrow()

numbers_of_difference <- df_filtered%>%
  pull(roznica)
numbers_of_even <- df_filtered%>%
  pull(niepatzrsyte)

number_of_diff_sum <- sum(numbers_of_difference)
number_of_even_sum <- sum(numbers_of_even)


# roznica - apartamenty ktore mialy wiecej recenzji w latach nieparzystych i jest to liczba nadwyzki w latach nieparzystych
x6 <- c(number_of_apart,number_of_diff_sum)
## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- x6

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
x7 <- listings%>%
  select(price,room_type)

ggplot(x7,aes(y=price,x = room_type))+
  geom_boxplot(fill = 'pink',colour = 'deeppink3')+
  scale_y_log10()+
  labs(title = "Rozkład cen apartamentów w zależności od typu pokoju",
    x = "Typ pokoju",
    y = "Cena") +
  theme_minimal()

# rozklad nie jest rownomierny - cena zalezy od typu pokoju
#ceny apartamentów zależą od typu pokoji.
# entire home/apt - ma bardzo duży rozrzut cenowy, jednak mediana jest dosść wysoka, więć można stwierdzić, że ceny są wysokie
# hotel room - średnio najdroższa kategoria, ceny hotei mogą być bardzo wysokie okolo 1000 ale takze wiele wystepuje w cenie 300
# PRIVATE ROOM - ceny sa bardziej skupione nie ma wielu wartosci odstajacych
# shared room - najtanszty typ zakwaterowania, najmniejsza mediana, najmniejszy rozrzut cen, wiekszosc ofert ma zblizone ceny


## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- x7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
x8<-listings%>%
  transmute(first_letter = substr(host_name,1,1))%>%
  filter(grepl("^[A-Z]",first_letter))%>%
  count(first_letter, sort = TRUE, name = 'number')
x8

plot8 <-  ggplot(x8,aes(x =reorder(first_letter,number), y = number)) +
  geom_col(fill = "hotpink3") +
  labs(title = "Rozkład pierwszych liter imion hostów",
    x = "Pierwsza litera imienia",
    y = "Liczba hostów") +
  theme_minimal()
plot8
#rozkład nie jest równomierny, ponieważ dominują głównie litery : M,J,A,S,B. 
# Z kolei litery Q,U,X pojawiają się bardzo żadko(mniej niż 10 wystąpień)


## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- x8


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie
x9<-listings%>%
  filter(is.na(price))

plot9 <- ggplot(x9,aes(x=longitude , y = latitude))+
  geom_point(color = "deeppink")+
  labs(title = "Rozkład apartamentów w Seattle — brakujące ceny",
    x = "Długość geograficzna",
    y = "Szerokość geograficzna") +
  theme_minimal()
plot9

# rozklad apartamentów bez ceny w Seatle NIE jest równomierny. Widać tworzące się skupiska - głównie
#w centrum(47.60-47.63 / -122.35- (-122.30) ) oraz na północy miasta. Natomiast południowy - wschodna część miasta
# jest dużo mniej zagęszczona.
#

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- x9

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
saveRDS(solutions, file = "OlszewskaEmilia.rds")


