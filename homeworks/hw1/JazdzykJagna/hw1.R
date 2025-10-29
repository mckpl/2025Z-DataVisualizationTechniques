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

# Instrukcja --------------------------------------------------------------

# Poniższe zadania wymagają obróbki danych. W tym celu należy wykorzystać pakiet
# dplyr i tidyr. Wynik z każdego zadania należy przypisać do zdefiniowanej nazwy
# ANS_TASK_0X. Kod utworzonych wykresów należy pozostawić w pliku .R, nie należy
# zapisywać wykresów.
# Po wykonaniu wszystkich zdań należy wykonać kod oznaczony nagłówkiem 
# "Zapisanie rozwiązań do plik .RDS".



# Zadanie 1 (0.5) ---------------------------------------------------------
# Ilu recenzentów wystawiło więcej niż 20 opinii?
View(reviews)
View(table(reviews$reviewer_name))
table(table(reviews$reviewer_name)>20)
## wpierw grupuje dane po imionach recenzetów
## zliczam po ilosc recenzji dla konkretnych imion
## filtruje i zliczam ilu ich jest

## Rozwiązanie
reviews%>% 
  group_by(reviewer_name) %>% 
  summarise(ilość_wystawionych_recenzji=n()) %>% 
  filter(ilość_wystawionych_recenzji>20) %>% 
  summarise(ilość=n())



## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- 2097

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?
is.character(reviews$date)
##wpierw tworze kolumne z wyciągniętymi misiącami wystawionych recenzji
##grupuje po tej kolumnie, zliczam recenzje w miesiącu 
## sortuje i pokazuje kilka, gdzie recenzji jest najwiecej
## Rozwiązanie
x<-reviews %>% 
  mutate(miesiac = substr(reviews$date,6,7)) %>% 
  group_by(miesiac) %>% 
  summarise(ilosc_recenzji=n()) %>% 
  arrange(ilosc_recenzji)
tail(x[,"miesiac"])      


## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- tail(x[,"miesiac"])

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).
View(listings)
##wpierw znajduje w reviews, apartementy z conajmniej 100 recenzjami
## połączam dwa zestawy danych po id i tworzę kolumnę z max ceną której szukamy
## Rozwiązanie
y<-reviews %>% 
  group_by(listing_id) %>% 
  summarise(ilosc_recenzji=n()) %>% 
  filter(ilosc_recenzji>=100)

szukana_cena<-y %>% 
  inner_join(listings,by=c("listing_id"="id")) %>% 
  summarise(max_cena=max(price,na.rm=TRUE)) %>% 
  pull(max_cena)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- szukana_cena

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
apartamenty<-reviews %>% 
  group_by(listing_id) %>% 
  arrange(listing_id,date) %>% 
  mutate(date = as.Date(date)) %>% 
  summarise(
    liczba_lat_na_rynku = as.numeric(max(date) - min(date)) / 365) %>% 
  filter(liczba_lat_na_rynku > 10) %>% 
  summarise(liczba_lat_na_rynku = n())

max_lat<-reviews %>% 
  group_by(listing_id) %>% 
  arrange(listing_id,date) %>% 
  mutate(date = as.Date(date)) %>% 
  summarise(
    liczba_lat_na_rynku = as.numeric(max(date) - min(date)) / 365) %>% 
  summarise(max(liczba_lat_na_rynku)) 

View(apartamenty)
View(max_lat)

razem<-bind_cols(apartamenty,max_lat)
View(razem)
## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- razem

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

##znajduje środek położenia geograficznego z danych
## powyżej tej zmiennej są apartamenty na północ, poniżej na południe

srodek<-mean(listings$latitude,na.rm = TRUE)
## wybieram pokoje które są na północy i zliczam ile ich jest i tak samo robię dla pokoi na południu 

pokoje_polnoc<-listings %>% 
  select(room_type,latitude) %>%
  filter(latitude>srodek) %>% 
  group_by(room_type) %>% 
  summarise(latitude = n())
View(pokoje_polnoc) 


pokoje_poludnie<-listings %>% 
  select(room_type,latitude) %>%
  filter(latitude<srodek) %>% 
  group_by(room_type) %>% 
  summarise(latitude = n())
View(pokoje_poludnie)  
## obliczam  ile w kategorii każdego rodzaju pokoju więcej jest pokoi na południu
roznica <- full_join(pokoje_poludnie, pokoje_polnoc, by = "room_type", suffix = c("_poludnie", "_polnoc"))
roznica[is.na(roznica)] <- 0

roznica<-roznica %>% 
  mutate(
         o_ile_wiecej = latitude_poludnie - latitude_polnoc)

View(roznica)
## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- roznica

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
nowe<-reviews %>% 
  group_by(listing_id) %>% 
  mutate(ilosc = n()) %>% 
  ungroup()

decyle <- quantile(nowe$ilosc, probs = c(0.4,0.5))

rozwazane<-nowe %>% 
  filter(ilosc>decyle[1],ilosc<decyle[2])
rozwazane
odpowiedz<-rozwazane %>% 
  mutate(year = format(as.Date(date),"%Y"))
odpowiedz<-odpowiedz %>% 
  mutate(jakie_lata = ifelse(as.numeric(year)%%2 == 0,"parzyste","nieparzyste") )

recenzje_sum <- odpowiedz %>%
  group_by(listing_id, jakie_lata) %>%
  summarise(liczba_recenzji = n()) %>% 
  ungroup()

recenzje_porownanie <- recenzje_sum %>%
  group_by(listing_id) %>%
  summarise(
    recenzje_parzyste = sum(liczba_recenzji[jakie_lata == "parzyste"]),
    recenzje_nieparzyste = sum(liczba_recenzji[jakie_lata == "nieparzyste"]),
    wiecej_w_nieparzystych = recenzje_nieparzyste > recenzje_parzyste,
    
  ) %>% 
  ungroup()
##tutaj sprawdzam ile jest apartamentów, w których wiecej recenzji było w latach nieparzystych

ile = sum(recenzje_porownanie$wiecej_w_nieparzystych==TRUE) 
## tutaj obliczam o ile więcej recenzji było wystawionych w latach nieparzystych niż parzystych
## interesują mnie tylko te apartamenty, gdzie więcej było w nieparzystych

ktore_o_ile<-recenzje_porownanie %>% 
  mutate(roznica = recenzje_nieparzyste - recenzje_parzyste) %>% 
  filter(wiecej_w_nieparzystych) %>% 
  select(listing_id,roznica)
## kolumna ile to liczba recenzji które zostały wystawione w latach nieparzystych

ktore_o_ile$ile<-ile


ANS_TASK_06 <-ktore_o_ile

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

do_wykresu<-listings %>% 
  filter(!is.na(price)) %>% 
  group_by(room_type) %>% 
  mutate(mean_price = mean(price),
         median_price = median(price),
         first_q = quantile(price,0.25),
         third_q = quantile(price,0.75))
View(do_wykresu)
ggplot(do_wykresu, mapping = aes(x = room_type,y = price))+
  geom_boxplot( fill = "lightblue")+
  scale_y_log10()+
  labs(title = "Rozkład cen w zależności od typu pokoju",
       x = "Typ pokoju",
       y = "Cena")

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- do_wykresu

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

od_imienia<-listings %>% 
  mutate(pierwsza_litera = str_to_upper(str_sub(host_name, 1, 1))) %>%
  filter(pierwsza_litera %in% LETTERS) %>% 
  group_by(pierwsza_litera) %>% 
  summarise(ile = n()) %>% 
  arrange(pierwsza_litera)

ggplot(od_imienia, mapping = aes(x = pierwsza_litera, y = ile))+
  geom_col(fill = "darkgreen")+
  labs(title = "Rozkład pierwszych liter hostów apartamentów",
       x = "pierwsza litera imienia hosta",
       y = "ilość występowania tej litery")

View(od_imienia)

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- od_imienia


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

bez_ceny<-listings %>% 
  filter(is.na(price)) %>% 
  group_by(neighbourhood) %>% 
  summarise(ile = n()) %>% 
  arrange(desc(ile))

ggplot(bez_ceny, mapping = aes(x = reorder(neighbourhood, -ile), y = (ile)))+
  geom_col(fill = "black")+
  coord_flip()+
  labs(title = "rozkład",
       x = "sąsiedztwo",
       y = "ilość")
View(bez_ceny)
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
saveRDS(solutions, file = "JazdzykJagna.rds")