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
pom1 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(count=n()) %>% 
  filter(count>20)

odp1<-nrow(pom1)

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- odp1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
pom2<- reviews %>% 
  mutate(miesiac=format(as.Date(date), "%m")) %>% 
  group_by(miesiac) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

odp2<- head(pom2$miesiac,3)

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- odp2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
pom3a<- reviews %>% 
  group_by(listing_id) %>% 
  summarise(count=n()) %>% 
  filter(count>100)

pom3b<-pom3a$listing_id

pom3c<- listings %>%
  filter(id %in% pom3b) %>% 
  select(id, price) %>% 
  arrange(desc(price)) %>% 
  head(1)

odp3 <- pom3c$price

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- odp3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
obecna_data <- as.Date("2025-10-20",format="%Y-%m-%d")

pom4a <- reviews %>% 
  mutate(data=as.Date(date,format="%Y-%m-%d"), na_rynku=as.numeric(obecna_data-data, units="days")/365) %>% 
  filter(na_rynku >10) 
odp4a<-length(unique(pom4a$listing_id))
  
odp4b<- pom4a %>% 
  summarise(maks=max(na_rynku)) %>% 
  pull(maks)

odp4<- c(odp4a, odp4b)

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- odp4

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
middle<-listings %>% 
  summarise(srednia =mean(latitude)) %>% 
  pull(srednia)

odp5<- listings %>% 
  mutate(kierunek = ifelse(latitude>=middle,'polnoc', 'poludnie')) %>%
  select(room_type, kierunek) %>% 
  group_by(room_type, kierunek) %>% 
  summarise(count=n()) %>% 
  pivot_wider(names_from = kierunek, values_from = count, values_fill = 0) %>% 
  mutate(roznica = poludnie-polnoc) %>% 
  select(room_type, roznica)


## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- odp5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

decyle <- quantile(listings$number_of_reviews, probs = c(0.4, 0.5), na.rm=TRUE)

#tylko te apartamenty miedzy 4 a 5 decylem
apartamenty_wybrane <- listings%>% 
  filter(number_of_reviews>=decyle[1], number_of_reviews<=decyle[2])

#nowa kolumna rok
recenzje_apart <- reviews %>% 
  mutate(rok = as.numeric(format(as.Date(date), "%Y")))


recenzje_apart <- recenzje_apart %>%
  filter(listing_id %in% apartamenty_wybrane$id) %>% 
  mutate(parzystosc = ifelse(rok%%2==0, "parzysty", "nieparzysty")) %>% 
  group_by(listing_id, parzystosc) %>% 
  summarise(ilosc = n()) %>% 
  pivot_wider(names_from = parzystosc, values_from = ilosc, values_fill = 0) %>% 
  mutate(wiecej_nieparzystych = nieparzysty >parzysty)
  
ile <- recenzje_apart %>% 
  filter(wiecej_nieparzystych) 

#ilosc wierszy to tyle ile jest apartamentow spelniajacych warunki polecenia
odp6a <- nrow(ile)

#tworze data frame, w którym dla każdego z apartamentów spełniających warunki zadania,
#podaję różnicę między ilością nieparzystych a parzystych lat
odp6b <- ile %>% 
  transmute(listing_id=listing_id, roznica = nieparzysty-parzysty)
odp6 <- list(odp6a, odp6b)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- odp6

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
dane7 <- listings %>% 
  select(room_type, price) %>%
  filter(!is.na(price))

wykres7 <- ggplot(dane7, aes(y = price, x = room_type, fill = room_type)) +
  geom_violin(alpha=0.8) +
  scale_y_log10()+
  labs(
    title = "Rozkład cen apartamentów według typu pokoju",
    x = "Typ pokoju",
    y = "Cena"
  )+
  theme_bw()+
  theme(legend.position = "none") #usuwamy legende bo nie jest potrzebna do odczytania wykresu


## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- dane7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
pierwsze_litery <- listings %>% 
  select(id, host_name) %>% 
  mutate(pierwsza_litera = substr(host_name, 1, 1)) %>% 
  filter(!is.na(pierwsza_litera) & grepl("^[A-Za-z]$", pierwsza_litera))

#wizualizacja rozkladu pierwszych liter hostow:
wykres8 <- pierwsze_litery %>% 
  ggplot(aes(x=pierwsza_litera))+
  geom_bar(fill="pink")+
  labs(
    title = "Rozkład pierwszych liter imion hostów",
    y = "Ilość apartamentów",
    x = "Pierwsza litera imienia hosta"
  )+
  theme_bw()
# najwiecej imion hostów zaczyna się na literę M, a najmniej na Q

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- pierwsze_litery


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie
dane9 <- listings %>% 
  filter(is.na(price) | price == "") %>% 
  select(id, longitude, latitude)

wykres9 <- dane9 %>% 
  ggplot(aes(x = longitude, y = latitude))+
  geom_point(alpha = 0.5,color="blue4")+
  labs(x="Długość geograficzna",
       y="Szerokość geograficzna",
       title="Położenie apartamentów z brakującą ceną w Seatle")+
  theme_bw()
# z wykresu da się zauważyć, że rozkład apartamentów w Seatle nie jest równomierny, duża część
# z nich jest skupiona w samym centrum

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- list("NIE",dane9)



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
saveRDS(solutions, file = "MaciatekHelena.rds")



