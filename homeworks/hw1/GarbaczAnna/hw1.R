###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Ładowanie danych --------------------------------------------------------

reviews <- read.csv("C:\\Dokumenty\\3 semestr\\twd\\reviews.csv")
listings <- read.csv("C:\\Dokumenty\\3 semestr\\twd\\listings.csv")

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
odp1<-reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(n=n()) %>% 
  filter(n>20) %>% 
  nrow()



## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- odp1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

odp2<-reviews %>% 
  mutate(date2=as.Date(date,"%Y-%m-%d")) %>%
  mutate(month_review=month(date2)) %>% 
  group_by(month_review) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))


## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- odp2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

temp3<-reviews %>% 
  group_by(listing_id) %>% 
  summarise(n=n()) %>% 
  filter(n>=100)

odp3<-inner_join(listings,temp3,join_by("id"=="listing_id")) %>% 
  select(price) %>% 
  arrange(desc(price)) %>% 
  top_n(1)


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- odp3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
ilosc_apartamentow_zad4<-reviews %>%
  group_by(listing_id) %>%
  select(listing_id,date) %>%
  mutate(date2=as.Date(date,"%Y-%m-%d")) %>%
  mutate(year_review=year(date2)) %>%
  summarise(min_year=min(year_review)) %>%
  mutate(years=2025-min_year) %>%
  filter(years>10) %>% 
  nrow()

maks_lata_zad4<-reviews %>%
  group_by(listing_id) %>%
  select(listing_id,date) %>%
  mutate(date2=as.Date(date,"%Y-%m-%d")) %>%
  mutate(year_review=year(date2)) %>%
  summarise(min_year=min(year_review)) %>%
  mutate(years=2025-min_year) %>%
  filter(years>10) %>%
  arrange(years) %>%
  top_n(1,years) %>%
  select(years)



## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- c(ilosc_apartamentow_zad4,maks_lata_zad4)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
srednia_szerokosc<-mean(listings$latitude)
polnoc<-listings %>%
  group_by(room_type) %>%
  filter(latitude>srednia_szerokosc) %>%
  summarise(n=n())
poludnie<-listings %>%
  group_by(room_type) %>%
  filter(latitude<srednia_szerokosc) %>%
  summarise(n=n())
porownanie_zad5<-full_join(poludnie,polnoc,by="room_type",suffix=c("_poludnie","_polnoc"))

porownanie_zad5<-porownanie_zad5 %>%
  mutate( n_poludnie = coalesce(n_poludnie, 0),
          n_polnoc = coalesce(n_polnoc, 0),
          roznica=n_poludnie-n_polnoc
  )



## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <-porownanie_zad5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

decyle<-quantile(listings$number_of_reviews,c(0.4,0.5))
decyle
ids<-listings %>%
  filter(room_type=="Entire home/apt", number_of_reviews>decyle[1], number_of_reviews<decyle[2]) %>% 
  select(id)
reviews_temp<-inner_join(reviews,ids,join_by("listing_id"=="id"))
  

reviews_parzyste<- reviews_temp %>%
  mutate(date2=as.Date(date,"%Y-%m-%d"),year_review=year(date2),czy_nieparzysty=year_review%%2) %>%
  group_by(listing_id) %>%
  filter(czy_nieparzysty==0) %>%
  summarise(n=n())
reviews_nieparzyste<- reviews_temp %>%
  mutate(date2=as.Date(date,"%Y-%m-%d"),year_review=year(date2),czy_nieparzysty=year_review%%2) %>%
  group_by(listing_id) %>%
  filter(czy_nieparzysty==1) %>%
  summarise(n=n())

porownanie_zad6<-full_join(reviews_nieparzyste,reviews_parzyste,join_by(listing_id),suffix=c("_nieparzyste","_parzyste")) %>%
  mutate(roznica=n_nieparzyste-n_parzyste) %>%
  filter(roznica>0)

ilosc_ap_zad6 <-nrow(porownanie_zad6)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <-list(ilosc_ap_zad6,porownanie_zad6)

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
dane_zad7<-listings %>% 
  group_by(room_type) %>% 
  select(room_type,price)

ggplot(dane_zad7, aes(y=room_type,
       x=price))+
  geom_boxplot(outlier.colour = "red")+
  labs(title="Rozkład cen apartamentów w zależności od typu pokoju")+
  xlim(0,4000)



## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- dane_zad7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
tab_litery<-listings%>%
  mutate(litery=substring(host_name,1,1)) %>% 
  filter(as.character(litery)>=65)
  

ggplot(tab_litery,aes(x=litery))+
  geom_bar()+
  labs(title="Rozkład pierwszych liter hostów")
  
  



## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- tab_litery


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

wspolrzedne<-listings %>% 
  filter(is.na(price)) %>% 
  select(latitude,longitude)

ggplot()+
  geom_point(data=listings,aes(x=longitude,y=latitude,color="Wszystkie apartamenty"))+
  geom_point(data=wspolrzedne,aes(x=longitude,y=latitude, color="Brak ceny"))+
  labs(title="Rozkład aparmatemtów w Seattle nieposiadających ceny w bazie danych")+
  scale_color_manual(name="Legenda",values=c("Wszystkie apartamenty"="grey","Brak ceny"="red"))

#Rozkład jest równomierny.

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- wspolrzedne



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
saveRDS(solutions, file = "GarbaczAnna.rds")


