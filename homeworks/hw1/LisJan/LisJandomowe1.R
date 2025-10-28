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

z1 <-reviews %>% group_by(reviewer_name) %>% summarise(n = n()) %>% filter(n>20) %>% 
  summarise(n1=n())


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- z1


# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

z2 <- reviews %>% mutate(month = substr(date,6,7)) %>% group_by(month) %>% 
  summarise(n=n()) %>% arrange(desc(n)) %>% mutate(month_name = month.name[as.integer(month)])

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- z2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

z3 <- reviews %>% group_by(listing_id) %>% summarise(n=n()) %>% 
  filter(n>=100) %>% left_join(listings, by=c("listing_id"="id")) %>% 
  select(listing_id, name, price) %>% top_n(1,price) %>% select(price)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- z3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

z4<-reviews %>% select(listing_id, date) %>% filter(date < as.Date('2015-10-24')) %>% 
  group_by(listing_id) %>% 
  summarise(minDate=min(date))


liczba_apartamentów <- z4 %>% summarise(apartamenty_powyzej_10_lat=n())

lata <- z4 %>% summarise(miniDate = min(minDate)) %>% 
  transmute(najstarszy_w_latach =floor(as.numeric((as.Date(Sys.Date())-as.Date(miniDate))/365)))

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- c(liczba_apartamentów,lata)


# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

pom<-listings %>% mutate(sfera=ifelse(latitude>mean(latitude), "północ","południe")) %>%
  group_by(room_type, sfera) %>% summarise(n = n())
z5 <- pom %>% pivot_wider(names_from = sfera, values_from = n, values_fill = 0) %>% 
  transmute(room_type=room_type, wiecej_południe_o = południe-północ)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- z5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

z6 <- reviews %>% select(listing_id)%>% summarise(.by=listing_id, ile=n()) %>%
  filter(ile >= quantile(ile, 0.4, na.rm=TRUE),
         ile <quantile(ile,0.5, na.rm = TRUE)) %>% left_join(reviews,by="listing_id") %>% 
  select(listing_id,date)  %>% 
  mutate(parity = if_else( as.numeric(substr(date,1,4))%%2==0,"parzysty","nieparzysty")) %>% 
  group_by(listing_id, parity) %>% summarise(liczba=n()) %>% 
  pivot_wider(names_from=parity, values_from = liczba) %>% 
  mutate(diff = nieparzysty - parzysty) %>% filter(diff>0)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- z6

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

z7 <-listings %>% select(room_type, price) %>% filter(!is.na(price))

plot7a <- z7 %>% group_by(room_type) %>% filter(price>=quantile(price, 0.01),
                                      price<= quantile(price, 0.99)) %>% ungroup() %>%  
  ggplot(aes(x=price)) + geom_density(alpha=0.4, fill="blue") + 
  facet_wrap(~ room_type, scales="free") +
  labs(title="Rozkład cen w zależności od typu pokoju",
        x = "Cena", y="Częstość")

plot7b <- z7 %>% ggplot(aes(x=price)) + geom_density(alpha=0.4, fill="red") + 
  facet_wrap(~ room_type, scales="free") +
  labs(title="Rozkład cen w zależności od typu pokoju",
       x = "Cena", y="Częstość")

#Opis
#plot7a
# Są to 4 wykresy gęstości każdy względem danego typu pokoju pokazujące rozkład ceny po odrzuceniu
# wartości najbardziej skrajnych.
# Wnioski:
# Najwięcej apartementów typu: Entire home/apt jest w zakresie ok. 200$.
# Dla typu Private room ceny układają się przeważnie ok 60$
# W Hotel room można zobaczyć wyrażny podział na grupy 200$ i 1000$, które są w podobnym rozmiarze
# Shared room jest tendencja spadkowa dla wyższych cen, jednocześne ten rozkład jest najbardziej
# równomierny

#plot7b
#Osie takie same jak w plot7a. Po uwzględnieniu wartości skrajnych widać, że:
# - Kategorie Hotel romm i Shared room uległa niewielkiej zmianie
# - Kategorie Entire home/apt i Private room są mocno zmienione na skutek wartości skrajnych
# (Dla Entire home/apt cena ok. 9000$, a dla private room 1600$)


## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- z7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

z8 <- listings %>% select(host_name) %>% mutate(first_let = substr(host_name,0,1)) %>%
  filter(!grepl("[0-9]",first_let)) %>% group_by(first_let) %>% summarise(litery=n())

ggplot(z8, aes(x=first_let, y=litery))+geom_col()+
  labs(title = "Rozkład pierwszych liter hostów",
       x="Pierwsza litera", y="Ilość wystąpień")

#Opis
#Wykres kolumnowy przedstawia na osi y ilość wystąpień danych na osi x pierwszych liter
# z nazw hostów.
#W rozkładzie ewidentnie wybijają się lotery A, B, J, M, S. WIdać też, że pierwsze litery alfabetu
# były częściej używane (A,B,C,D,E). Odnotowana bardzo mało wystąpień liter: Q, U, X, Z

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- z8


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

z9 <- listings %>% select(neighbourhood_group, price) %>% filter(is.na(price)) %>% 
  group_by(neighbourhood_group) %>% summarise(neighbourhood_number=n())

plot9a <- ggplot(z9, aes(y=neighbourhood_group, x=neighbourhood_number)) + geom_col() +
  labs(title = "Wykres zależności apartamentów bez podanej ceny\n w zależności od dzielnicy",
       x = "Ilość obserwacji", y="Dzielnica")

#Opis plot9a
#Jest to wykres słupkowy, na którym na osi y są wypisane dzielnice miasta, w których odnotowano
#wystąpienie apartementów bez podanej ceny, a na osi x jest ilość zliczonych wystąpień.
#Położenie apartamentów w Seatle bez podanej ceny jest nierównomierne ze wzgledu na dzielnice.
#W pomnieszych dzielnicach (Other neighborhoods) oraz Downtown jest znaczna część wszystkich obserwacji. 

plot9b <- listings %>% select(neighbourhood_group,price,latitude,longitude) %>% filter(is.na(price)) %>% 
  ggplot(aes(x=longitude, y=latitude, color =neighbourhood_group)) + geom_point() +
  labs(title="Rozkład apartamentów bez podanej \nceny w zależności od położenia",
       x = "Długość geograficzna", y="Szerokość geograficzna",
       color="Dzielnica")

#Opis plot9b
#Jest to wykres punktowy, który na osi x ma długość geograficzną, a na osi
# y szerokość geograficzną apartamentów w Seattle, które nie mają podane ceny.
# Kolorem oznaczono dzielnice
# Widać, że potwierdza się duża liczba obserwacji z Other neighborhoods.
# Do tego widzimy duże zagęszczenie w dzielnicy Downtown.
# Ogólne położenenie obesrwacji nie jest równomiernie ułożone.

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- z9

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
saveRDS(solutions, file = "LisJan.rds")


