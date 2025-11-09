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
liczba_recenzentow_powyzej_20_opini<- reviews %>%
  group_by(reviewer_id)%>%
  summarise(n = n())%>%
  filter(n>20)%>%
  summarise(ilosc_recenzentow = n())

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- liczba_recenzentow_powyzej_20_opini$ilosc_recenzentow

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
trzy_z_największą <- reviews%>%
  mutate(month = format(as.Date(date),"%m"))%>%
  group_by(month)%>%
  summarise(ilosc_recenzji = n())%>%
  top_n(3,ilosc_recenzji)%>%
  select(month)

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- trzy_z_największą$month

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
wektor <- reviews %>%
  group_by(listing_id)%>%
  summarise(n = n())%>%
  filter(n>=100)%>%
  pull(listing_id)

odp <-listings%>%
  filter(id %in% wektor)%>%
  select(price)%>%
  top_n(1,price)


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <-odp$price

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
dane <- reviews%>%
  mutate(date = as.Date(date))%>%
  group_by(listing_id)%>%
  summarise(oldest_review = min(date,na.rm = TRUE),
            youngest_review =max(date, na.rm = TRUE),
            how_long = youngest_review - oldest_review,
            years = as.numeric(how_long)/365)%>%
  filter(years > 10)

ilosc_apartamentow <- dane%>%
  summarise(ilosc_apartamentow = n())

najdluzej_na_rynku <- dane%>%
  top_n(1,years)%>%
  select(years)
wynik <- bind_cols(ilosc_apartamentow,najdluzej_na_rynku)

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- wynik

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
srodek <-mean(listings$latitude, na.rm = TRUE)
# powyżej tej wartości to północ a poniżej szerokość
kierunek <- listings%>%
  mutate(kierunek = ifelse(latitude>srodek, "Północ", "Południe"))%>%
  select(kierunek, room_type)%>%
  group_by(kierunek, room_type)%>%
  summarise(liczba_typow_mieszkan = n(), .groups = "drop")
kierunek

roznica <- kierunek%>%
  pivot_wider(names_from = kierunek,
              values_from = liczba_typow_mieszkan,
              values_fill = 0)%>%
  mutate(roznica_ilosci = Południe-Północ,
         gdzie_więcej = case_when(
           roznica_ilosci > 0 ~ "Więcej na południu",
           roznica_ilosci < 0 ~ "Więcej na północy",
           roznica_ilosci == 0 ~ "Tyle samo"))%>%
  select(room_type, roznica_ilosci)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- roznica

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
n_reviews <- reviews%>%
  group_by(listing_id)%>%
  summarise(n = n())

decyle_45 <- quantile(n_reviews$n, probs = c(0.4,0.5) )
apartamenty <- n_reviews%>%
  filter(n >= decyle_45[1], n <= decyle_45[2])

recezje_zliczone <- reviews%>%
  mutate(year = format(as.Date(date),"%Y"))%>%
  filter(listing_id %in% apartamenty$listing_id)%>%
  group_by(listing_id)%>%
  summarise(l_r_nieparzystnym_r = sum(as.numeric(year)%%2 == 1, na.rm = TRUE),
            l_r_parzystym_r = sum(as.numeric(year)%%2 ==0, na.rm = TRUE))%>%
  filter(l_r_nieparzystnym_r > l_r_parzystym_r)%>%
  summarise(l_apartamentów = n(),
            l_wszystkich_recenzji_apartamentow_miedzy_4_5 = sum(l_r_nieparzystnym_r+ l_r_parzystym_r),
            l_parzystych_recenzji_apartamentow_miedzy_4_5= sum(l_r_parzystym_r),
            l_nieparzystych_recenzji_apartamentow_miedzy_4_5 = sum(l_r_nieparzystnym_r))

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- recezje_zliczone

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
listings_without_NA <- listings%>%
  drop_na(price, room_type)

lista_do_wykresu <- listings %>%
  drop_na(price, room_type)%>%
  group_by(room_type) %>%
  summarise(liczba_ofert = n(),
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    min_price = min(price, na.rm = TRUE),
    max_price = max(price, na.rm = TRUE))

ggplot(listings_without_NA, aes(x = room_type, y = price))+
  geom_boxplot(color='black', fill='grey') +  
  scale_y_log10()+
  labs(
    title = "Rozkład cen apartamentów w zależności od typu pokoju.",
    x = "Room Type",
    y = "Price (scale log)") 

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- lista_do_wykresu

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
listings_host_do_wykresy <- listings%>%
  drop_na(host_name)%>%
  mutate(pierwsza_litera = toupper(substr(host_name,1,1)))%>%
  filter(grepl("[A-Z]$",pierwsza_litera))%>%
  group_by(pierwsza_litera)%>%
  summarise(ilosc_hostow = n())%>%
  arrange(desc(ilosc_hostow))

ggplot(listings_host_do_wykresy, aes(x = reorder(pierwsza_litera, - ilosc_hostow), y = ilosc_hostow)) +
  geom_col(color = 'black', fill = 'grey')+
  theme_minimal()+
  labs( title = "Rozkład pierwszych liter imion hostów", 
        x = "Pierwsza litera imienia", 
        y = "Liczba hostów")

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- listings_host_do_wykresy


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie
listings_without_price <- listings %>%
  filter(is.na(price))%>%
  group_by(neighbourhood) %>%
  summarise(liczba_apartamentow = n(),  .groups='drop') %>%
  count(liczba_apartamentow, name= "liczba_dzielnic")

print(ggplot(listings_without_price, aes(x = liczba_apartamentow, y = liczba_dzielnic)) +
  geom_col(fill = "grey", color = "black") +
  theme_minimal() +
  labs(
    title = "Rozkład liczby apartamentów bez ceny w dzielnicach",
    x = "Liczba apartamentów w dzielnicy bez cen",
    y = "Liczba dzielnic"
  ))

str(listings_without_price)
## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- listings_without_price



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
saveRDS(solutions, file = "JagielakKarolina.rds")
