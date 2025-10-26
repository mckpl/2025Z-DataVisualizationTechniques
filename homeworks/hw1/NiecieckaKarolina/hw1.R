##########################################
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

ans_01 <- as.numeric(nrow(filter(data.frame(table(reviews$reviewer_id)), Freq >20)))

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- ans_01

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

reviews$date <- as.Date(reviews$date)
reviews$month <- format(reviews$date, '%m')
ans_02 <- head(arrange(data.frame(table(reviews$month)), -Freq), 3)
miesiace <- c('styczeń', 'luty', 'marzec', 'kwiecień', 'maj', 'czerwiec', 'lipiec', 'sierpień', 'wrzesień', 'październik', 'listopad', 'grudzień')
ans_02 <- miesiace[as.numeric(ans_02$Var1)]

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- ans_02

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

id_rev <- filter(data.frame(table(reviews$listing_id)), Freq >= 100)
listings %>% 
  filter(id %in% id_rev$Var1) %>% 
  select(price) %>% 
  arrange(-price) %>% 
  head(1) -> ans_03
ans_03 <- as.numeric(ans_03)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- ans_03

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

date_10_years_ago <-Sys.Date() - 3652

reviews %>% 
  group_by(listing_id) %>% 
  summarise(
    first = min(date) ) %>% 
  filter(first < date_10_years_ago) -> ans_04
max_years <- trunc(as.numeric(difftime(Sys.Date(), min(ans_04$first), units = 'days')) / 365)
ans_04 <- data.frame(liczba_apartamentów_dłużej_niż_10_lat = nrow(ans_04), max_liczba_lat = max_years)

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- ans_04

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
mean_lat <- mean(listings$latitude)
listings <- listings %>% 
  mutate(szerokosc_geo = ifelse(latitude < mean_lat, 'południe', 'północ'))
listings %>% 
  group_by(room_type, szerokosc_geo) %>% 
  summarise(liczba = n()) -> ans_05
ans_05 <- ans_05 %>% 
  pivot_wider(names_from = szerokosc_geo, values_from = liczba, values_fill = 0 ) %>% 
  mutate( roznica = południe - północ)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- ans_05

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
recenzje <- data.frame(table(reviews$listing_id))
recenzje <- recenzje %>% 
  filter(Freq > quantile(recenzje$Freq, 0.4) & Freq < quantile(recenzje$Freq, 0.5))
reviews %>% 
  filter(listing_id %in% recenzje$Var1) %>% 
  select(listing_id, date) %>% 
  mutate(parzystosc_roku = ifelse(as.numeric(format(date, '%Y')) %% 2 == 0, 'parzysty', 'nieparzysty')) %>% 
  group_by(listing_id, parzystosc_roku) %>% 
  summarise(liczba = n()) %>% 
  pivot_wider(names_from = parzystosc_roku, values_from = liczba, values_fill = 0) %>% 
  mutate(roznica = nieparzysty - parzysty) %>% 
  filter(roznica > 0) -> tmp
#w tmp zapisane są liczby recenzji w latach nieparzystych dla poszczególnych apartamentów
ans_06 <- data.frame(liczba_apartamentow_o_wiekszej_liczbie_recenzji_w_latach_nieparzystych = nrow(tmp),
                     liczba_recenzji_w_latach_nieparzystych = sum(tmp$nieparzysty))

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- ans_06

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
listings %>% 
  group_by(room_type) %>% 
  summarise(
    liczba = n(),
    cena_min = min(price, na.rm = TRUE),
    cena_max = max(price, na.rm = TRUE),
    mediana = median(price, na.rm = TRUE),
    srednia = mean(price, na.rm = TRUE),
    centyl_25 = quantile(price, 0.25, na.rm = TRUE),
    centyl_75 = quantile(price, 0.75, na.rm = TRUE)
  ) -> rozklad_cen

ggplot(listings, aes(x = room_type, y = price))+
  geom_boxplot()+
  labs(title = 'Rozkład cen apartamentów w zależności od typu pokoju',
       x = 'Typ pokoju',
       y = 'Cena')


## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- rozklad_cen

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
listings %>% 
  mutate(pierwsza_litera = substr(host_name, 1, 1)) %>% 
  select(pierwsza_litera) %>% 
  filter(!grepl('^[0-9]', pierwsza_litera)) %>% 
  table() %>% 
  data.frame() -> rozklad_liter
ggplot(rozklad_liter, aes(x = pierwsza_litera, y = Freq))+
  geom_col()+
  labs(title = 'Rozkład pierwszych liter imienia hosta',
       x = 'Pierwsza litera imienia',
       y = 'Liczba hostów')
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- rozklad_liter


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie
bez_ceny <- listings %>% 
  filter(room_type == 'Entire home/apt') %>% 
  filter(is.na(price))
ggplot(bez_ceny, aes(x = longitude, y= latitude))+
  geom_point(color = 'blue', alpha= 0.5)+
  labs(title = 'Rozmieszczenie apartamentów bez ceny w Seattle',
       x = 'Długość geograficzna',
       y = 'Szerokość geograficzna')
#rozmieszcznie nie jest równomierne, ponieważ:
#na wykresie widać, że występuje jedno większe skupisko apartamentów bez ceny
#za to na obrzeżach oraz na pewnym 'pasie' nie występują takie apartamenty


## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- bez_ceny



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
saveRDS(solutions, file = "NiecieckaKarolina.rds")


