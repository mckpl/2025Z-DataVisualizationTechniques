###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringi)

# Ładowanie danych --------------------------------------------------------

reviews <- read.csv("reviews.csv")
listings <- read.csv("listings.csv")
View(reviews)
View(listings)

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

a <- reviews %>% select(reviewer_id,id) %>%group_by(reviewer_id) %>% summarise(n = n()) %>% filter(n>20) %>% 
  summarise(Liczba_poszukiwanych_recenzentów = n())
a

## Odptop_n()## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- a
# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
b<-reviews %>% mutate(month = stri_split_fixed(date,"-",simplify = TRUE)[,2]) %>% group_by(month) %>% 
  select(id,month) %>% summarise(liczba_recenzji=n()) %>% arrange(desc(liczba_recenzji))

b



## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- b

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
ponad_100_recenzji<- reviews %>% group_by(listing_id) %>% select(id,listing_id) %>% summarise(liczba_recenzji=n()) %>% 
  filter(liczba_recenzji>99) %>% select(listing_id)
ponad_100_recenzji <- pull(ponad_100_recenzji,listing_id)

z<-listings %>% filter(id %in% ponad_100_recenzji) %>%select(price) %>% top_n(1)



## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- z

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
w<- reviews %>% mutate(year = 2025 -strtoi(stri_split_fixed(date,"-",simplify = TRUE)[,1])) %>% select(listing_id,year) %>%
  group_by(listing_id)%>% summarise(ile_lat= max(year)) %>% filter(ile_lat>10) %>% arrange(desc(ile_lat))



## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- c(dluzej_niz_10_lat= 230,maks= 16)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
q<-listings %>% mutate(czy_polnoc = ifelse(latitude>mean(latitude,na.rm=TRUE),"Północ","Południe")) %>% select(czy_polnoc,id,room_type) %>% 
  group_by(room_type,czy_polnoc) %>% summarise(ile = n()) %>% arrange(ile)
q



## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- q

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
szukane_apart<-listings %>% filter((quantile(number_of_reviews,prob=c(0.4))<=number_of_reviews)&(number_of_reviews<=quantile(number_of_reviews,prob=c(0.5)))) %>% 
  select(id)
szukane_apart<-pull(szukane_apart,id)

reviews %>% filter(listing_id %in% szukane_apart) %>% mutate(czy_parzysty_rok= ifelse(strtoi(stri_split_fixed(date,"-",simplify = TRUE)[,1])%%2==0,"parzyste lata","nieparzyste lata")) %>% 
  select(czy_parzysty_rok,id) %>% group_by(czy_parzysty_rok)%>%  summarise(n = n())

reviews %>% filter(listing_id %in% szukane_apart) %>% mutate(czy_parzysty_rok= ifelse(strtoi(stri_split_fixed(date,"-",simplify = TRUE)[,1])%%2==0,"parzyste_lata","nieparzyste_lata")) %>% 
  select(czy_parzysty_rok,listing_id,id)%>% group_by(listing_id,czy_parzysty_rok) %>% summarise(n=n()) %>% pivot_wider(names_from = czy_parzysty_rok,values_from = n) %>% filter(nieparzyste_lata>parzyste_lata) %>% 
  select(listing_id) %>% pivot_longer(listing_id) %>% summarise(n=n())

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- c(ile_wiecej_apart=334,roznica_ilosci_recenzji= 11)

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
w<- listings %>% select(price,room_type) 

w %>% ggplot(aes(x=price)) +
  geom_density(adjust=2,fill="lightblue")+
  facet_wrap(~room_type)+
  coord_cartesian(xlim=c(0,2500))


###### Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- w

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
z<- listings %>% mutate(first= stri_sub(stri_extract_all_regex(host_name, "[^0-9]+"),1,1)) %>% select(first,id) %>% 
  group_by(first) %>% summarise(n=n()) %>% mutate(first=forcats::fct_reorder(first,-n))
z

z %>% ggplot(aes(x=first,y=n))+
  geom_col()



## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- z


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

q <- listings %>% mutate(czy_cena = ifelse(is.na(price),"Cena nie podana","Cena podana")) %>% select(czy_cena,longitude,latitude)
q

q %>% ggplot(aes(x= latitude,y=longitude,colour = czy_cena,alpha=czy_cena))+
  geom_point()+
  scale_alpha_manual(values = c(1,0.1))
q %>% ggplot(aes(x= latitude,y=longitude))+
  geom_density_2d_filled()+
  facet_wrap(~czy_cena)
#tak rozkład jest dość podobny do rozkładu położenia apartamentów z podaną ceną


## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- q



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
saveRDS(solutions, file = "LorencLeonard.rds")


