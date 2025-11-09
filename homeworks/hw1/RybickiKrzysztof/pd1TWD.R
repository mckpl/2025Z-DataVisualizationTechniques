###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# Ładowanie danych --------------------------------------------------------

reviews <- read.csv("C:\\Users\\krzys\\OneDrive\\Pulpit\\reviews.zip\\reviews.csv")
listings <- read.csv("C:\\Users\\krzys\\OneDrive\\Pulpit\\listings.zip\\listings.csv")
#View(reviews)
#View(listings)
# Instrukcja --------------------------------------------------------------

# Poniższe zadania wymagają obróbki danych. W tym celu należy wykorzystać pakiet
# dplyr i tidyr. Wynik z każdego zadania należy przypisać do zdefiniowanej nazwy
# ANS_TASK_0X. Kod utworzonych wykresów należy pozostawić w pliku .R, nie należy
# zapisywać wykresów.
# Po wykonaniu wszystkich zdań należy wykonać kod oznaczony nagłówkiem 
# "Zapisanie rozwiązań do plik .RDS".



# Zadanie 1 (0.5) ---------------------------------------------------------
# Ilu recenzentów wystawiło więcej niż 20 opinii?

x1<-reviews %>% count(reviewer_id) %>% filter(n>20) %>% nrow()

x1

#32 recenzentów wystawiło więcej niż 20 opinii


ANS_TASK_01 <- x1

# Zadanie 2 (0.5) ---------------------------------------------------------


## Rozwiązanie
reviews<-reviews %>% mutate(Daty=as.Date(date)) 
x2 <- reviews %>%
 mutate(miesiąc=format(Daty,"%m")) %>%
 group_by(miesiąc) %>% summarize(licznosc_opinii=n()) %>% arrange(desc(licznosc_opinii))
x2

#Najwięcej opinii wystawianych jest kolejno w sierpniu, czerwcu, lipcu itd jak
# ramce danych.
ANS_TASK_02 <- x2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

wynik<-listings %>% filter(number_of_reviews>=100) %>%
  select(price) %>% summarise(najwyzsza_cena = max(price, na.rm = TRUE))
max_cena<-wynik$najwyzsza_cena
max_cena

#Największa cena za apartament spełniający takie kryteria to 1012
ANS_TASK_03 <- max_cena

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

x<-reviews  %>% filter(as.numeric(Sys.Date()-Daty)>365*10) %>% 
  arrange(as.numeric(Daty)) %>% distinct(listing_id) %>% nrow()
y<-reviews %>% mutate(dawnosc=as.numeric(Sys.Date()-Daty)) %>% 
  summarise(najstarszy=round(max(dawnosc)/365,2))
y
x
c(x,y)
#Takich apartamentów jest 394, najstarszy jest od ok 16.3 roku na rynku
ANS_TASK_04 <- c(x,y)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
sr_szerekosc<-listings %>% summarise(srszerekosc=mean(latitude)) %>% pull()

wynik5 <- listings %>%
  filter(!is.na(latitude)) %>%                
  mutate(region = case_when(
    latitude <  sr_szerekosc ~ "poludnie",
    latitude >=  sr_szerekosc ~ "polnoc"
  )) %>%
  group_by(room_type) %>%
  summarise(
    polnoc   = sum(region == "polnoc", na.rm = TRUE),
    poludnie   = sum(region == "poludnie", na.rm = TRUE),
    roznica = poludnie-polnoc,                 
  )
wynik5
#Więcej jest apartamentów na południu niż na północy dla apartamentów typu
#Entire home/apt, hotel room, z kolei private roomów jest więcej na północy.
ANS_TASK_05 <- wynik5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
decyle<-listings %>% select(number_of_reviews) %>% quantile(probs=c(0.4,0.5),na.rm=TRUE)
szukane<-listings %>% filter( number_of_reviews>decyle[1] 
                    & number_of_reviews<decyle[2]) %>% 
  select(id)
  
reviews <-reviews %>% 
  mutate(parzystosc=ifelse(as.numeric(format(Daty,"%Y"))%%2==0,"parzysty","nieparzysty"))
a<-reviews %>% group_by(listing_id) %>% 
  summarise(l_parz=sum(parzystosc=="parzysty"),
            l_nparz=sum(parzystosc=="nieparzysty")) %>% 
  mutate(np=ifelse(l_nparz>l_parz,"tak","nie"))
a <- a %>% filter(np=="tak") %>% mutate(roznica=l_nparz-l_parz)
a
wynik6 <- a %>%
  filter(listing_id %in% szukane$id) 
ile<-wynik6 %>% nrow()
ile
wynik6
#300 apartamentów spełniających takie kryteria miało więcej opinii w latach nieparzystych
# niż w parzystych. Liczba takich apartamentów to 300. Jako odpowiedź przypisana jest tabelka
# wyjściowa z różnicami między latami nieparzystymi a parzystymi.

ANS_TASK_06 <- wynik6

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

x7<-listings %>%filter(is.na(price)==FALSE) %>%  select(id,room_type,price)
x7
x7%>% 
  ggplot(mapping=aes(x=room_type,y=price,fill=room_type))+geom_boxplot()+scale_y_log10()+
  labs(title="Rozkład cen dla poszczególnych typów pokoju",
       x="Typ pokoju",
       y="Cena ( w skali logarytmicznej")
#Skala logarytmiczna została nadana dla czytelności, bez niej pojedyńcze bardzo wysokie
# wartości cen bardzo zaburzają jego czytelność. Inną opcją byłoby obcięcie wykresów
# do np. 0.995 kwantyla cen, ale wtedy mimo wszystko tracimy pewne dane. 
# Rozkład jest mocno zróżnicowany, np Entire home/apt ma dosc wysokie ceny, ale
# tez duzo wartosci odstajacych, skrajnie wysokich na tle ogólnie wszystkich typów
# apartamentów.Hotel room ma zdecydowanie najwyższa mediane cen, ale mniej jest wartości skrajnych.
# Shared room sa zdecydowanie najtansze i mało jest cen bardzo odstających, a private room
# również posiada kilka wartości cen mocno odstających od mediany, lecz rozrzut ten jest 
# mniejszy niż u Entire home/apt i mediana cen tez jest nizsza.
ANS_TASK_07 <- x7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
x8<-listings %>% mutate(pierwsza_litera = substr(host_name, 1, 1))  %>% 
  filter(pierwsza_litera!="2") %>% 
group_by(pierwsza_litera) %>% summarise(ilosc=n())
x8 %>% ggplot(mapping=aes(x=pierwsza_litera,y=ilosc))+geom_col()+
  labs(title="Rozkład pierwszych liter imion hostów",
       x="Litery",
       y="Ilość")
x8
#Rozkład pierwszych imion hostów jest nierównomierny, dominują takie litery jak
# A, J, M, S, B, za to bardzo mało jest takich jak F,G,H,I,Q,O,Z
ANS_TASK_08 <- x8


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

x9<-listings %>% 
  filter( (is.na(price)==TRUE | price==""| price==" ") ) %>% select (latitude, longitude)
x9 %>%
  ggplot( aes(x = latitude, y = longitude))+geom_point()+
  labs(title="Rozkład położen apartamentowców w Seatlle",
       x="Szerekośc geograficzna",
       y="Długość geograficzna")
x9
         


#Rozkład zdecydowanie nie jest równomierny, w centrum jest ich zdecydowanie najwięcej,
# więcej ich jest również na północnym-wschodzie, zdecydowanie mniej jest na południu i zachodzie.
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
saveRDS(solutions, file = "RybickiKrzysztof.rds")


