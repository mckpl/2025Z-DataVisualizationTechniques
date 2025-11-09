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
wynik1 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarize(dane1 = n()) %>% 
  filter(dane1 > 20) %>% 
  nrow()

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- wynik1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
wynik2 <- reviews %>% 
  mutate(month = format(as.Date(date),"%m")) %>% 
  group_by(month) %>% 
  summarise(dane2 = n()) %>% 
  arrange(-dane2) %>% 
  select(month)#daje wszystkie miesiace bo nie jest stwierdzone ile dokladnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- wynik2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
wynik3_1 <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(dane3 = n()) %>% 
  filter(dane3 > 99)

wynik3_2 <- listings %>% 
  filter(id %in% wynik3_1$listing_id) %>% 
  arrange(-price) %>% 
  select(price) %>% 
  head(1)
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- wynik3_2

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
wynik4 <- reviews %>% 
  mutate(thedate = as.Date(date)) %>% 
  group_by(listing_id) %>% 
  summarise(min = min(thedate, na.rm = TRUE),max = max(thedate, na.rm = TRUE),days = as.numeric(max - min)) %>% 
  filter(days > 3653) %>% #moze to glupie liczyc w taki sposob ale dziala (szacunkowo)
  arrange(-days) #bazujac na wystawionych recenzjach uznalem ze czas na rynku jest miedzy pierwsza a ostatnia recenzja

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- c(nrow(wynik4),wynik4 %>% head(1) %>% select(days) / 365.25)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
wynik5 <- listings %>% 
  mutate(mean_lat = mean(latitude,na.rm = TRUE)) %>% 
  mutate(place = ifelse(latitude < mean_lat,"poludnie","polnoc")) %>% 
  group_by(room_type,place) %>% 
  summarise(dane5 = n(), .groups = "drop") %>% 
  pivot_wider(names_from = place,values_from = dane5,values_fill = 0) %>% 
  mutate(difference = poludnie - polnoc) %>% 
  summarise(room_type,difference)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- wynik5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
wynik6_1 <- listings %>% #wzialem decyle z listingow
  filter(number_of_reviews >= quantile(number_of_reviews,0.4,na.rm = TRUE) & number_of_reviews <= quantile(number_of_reviews,0.5,na.rm = TRUE)) 

wynik6_2 <- reviews %>% 
  filter(listing_id %in% wynik6_1$id) %>% 
  mutate(year = as.numeric(format(as.Date(date), "%Y"))) %>% 
  mutate(odd_year = ifelse(year %% 2 == 0,"Parzysty","Nieparzysty")) %>% 
  group_by(listing_id,odd_year) %>% 
  summarise(dane6 = n()) %>% 
  pivot_wider(names_from = odd_year,values_from = dane6,values_fill = 0) %>% 
  mutate(greater_odd = ifelse(Nieparzysty>Parzysty,1,0)) %>% 
  filter(greater_odd == 1) %>% 
  mutate(difference=Nieparzysty-Parzysty) %>% 
  select(listing_id,difference)#i dalej dzialalismy na review

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- c(nrow(wynik6_2),sum(wynik6_2$difference))

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
wynik7_1 <- listings %>% 
  group_by(room_type) %>% 
  select(room_type,price)
  
wynik7_2 <- wynik7_1 %>% 
  ggplot(aes(x=room_type,y=price))+
  geom_violin()+
  scale_y_log10()+
  labs(title="Rozkład cen apartamentów w zależności od typu pokoju (w skali logarytmicznej)")

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- wynik7_1

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
wynik8_1 <- listings %>% 
  mutate(first_letter = substr(host_name, 1, 1)) %>% 
  filter(grepl("^[A-Za-z]$", first_letter)) %>% 
  group_by(first_letter) %>% 
  summarise(dane8 = n())

wynik8_2 <- wynik8_1 %>% 
  ggplot(aes(x=first_letter,y=dane8))+
  geom_bar(stat="identity")+
  labs(title="Rozkład pierwszych liter hostów",y="ilość")

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- wynik8_1


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie
wynik9_1 <- listings %>% 
  filter(is.na(price)) %>% 
  filter(room_type=="Entire home/apt") %>% 
  select(latitude,longitude)

wynik9_2 <- wynik9_1 %>% 
  ggplot(aes(x=latitude,y=longitude))+
  geom_point()+
  labs(title="Rozkład apartamentów w Seatle, które nie mają podanej ceny")
  #jak widać z tego wykresu nie jest  

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- wynik9_1



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
saveRDS(solutions, file = "FlorekWojciech.rds")


