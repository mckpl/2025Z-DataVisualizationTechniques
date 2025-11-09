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

odp_1 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(n = n()) %>% 
  filter(n>20) %>% 
  summarise(liczba_rec=n())


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- odp_1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
odp_2 <- reviews %>%
  group_by(months(as.Date(date))) %>% 
  summarise(n=n()) %>% 
  top_n(3)



## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- odp_2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie


odp_3 <- listings %>% 
  filter(number_of_reviews>=100) %>% 
  select(price) %>% 
  top_n(1)



## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- odp_3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
odp_4 <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(najstarsza_rec=min(as.Date(date))) %>% 
  mutate(szac_czas_rynek=Sys.Date()-as.Date(najstarsza_rec)) %>% 
  filter(szac_czas_rynek>365*10) %>% 
  summarise(n=n())



## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- odp_4

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
srodek_lat <- mean(listings$latitude)

odp_5 <- listings %>% 
  mutate(polozenie=ifelse(latitude<srodek_lat,"pld","pln")) %>% 
  group_by(room_type,polozenie) %>% 
  summarise(n=n()) %>% 
  mutate(ile_wiecej_pld=ifelse(polozenie=="pln",(-1)*n,n)) %>% 
  group_by(room_type) %>% 
  summarise(wiecej_pld=sum(ile_wiecej_pld))

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- odp_5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

decyle <- quantile(listings$number_of_reviews,probs=c(0.4,0.5), na.rm=TRUE)
rec_miedzy_dec <- listings %>% 
  filter(number_of_reviews>decyle[1] & number_of_reviews<decyle[2])

merged <- merge(rec_miedzy_dec,reviews, by.x="id",by.y="listing_id")
odp_6 <- merged %>% 
  mutate(p_np=ifelse(as.integer(format(as.Date(date),"%Y"))%%2==1,"np","p")) %>% 
  group_by(id,p_np) %>% 
  summarise(l_rec=n()) %>% 
  mutate(l_rec2=ifelse(p_np=="np",l_rec,(-1)*l_rec)) %>% 
  group_by(id) %>% 
  summarise(o_ile_nparz=sum(l_rec2)) %>% 
  filter(o_ile_nparz>0)

#ile apartamentów?
ile_apart <- odp_6 %>% 
  summarise(n=n())


## Odpowiedz przypisana do zmiennej

ANS_TASK_06 <- odp_6

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

  
dane7 <- listings %>% 
  select(price,room_type)
dane7 %>% 
  ggplot(aes(x=price, color = room_type, fill= room_type))+
  geom_density(alpha=0.3)+
  scale_x_continuous(trans = "log10")+
  labs(title="Wykres zależności ceny od typu pokoju", y="zagęszczenie", x="cena (log10)")
#Na wykresie widać, że pokoje typu "Private room" i "Shared room" są w większości droższe od "Entire home/apt"
#wszytskie powyżej wymienione typy mają dość wyraźnie zarysowane przedziały cenowe, z kolei "Hotel room"
#jest bardziej rozłożony cenowo, możliwe, że ze względu na standard
## Odpowiedz przypisana do zmiennej
 
ANS_TASK_07 <- dane7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
dane8 <- listings %>%
  mutate(pier_lit_host= substr(host_name,1,1)) %>% 
  filter(pier_lit_host>="A") %>% 
  select(pier_lit_host)
dane8 %>% 
  ggplot(aes(x=pier_lit_host))+
  geom_bar()+
  labs(title="Rozkład pierwszych liter imion hostów",x="pierwsze litery",y="liczba wystąpień")


## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- dane8


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

dane9 <- listings %>% 
  filter(is.na(price)) %>% 
  select(longitude,latitude)
dane9 %>% 
  ggplot(aes(x=longitude,y=latitude))+
  geom_density_2d_filled()+
  labs(title="Rozkład apartamentów bez podanej ceny", x="szerokość geograficzna",y="długość geograficzna")

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- dane9

#wykres nie jest równomierny, widać koncentrację punktów w środku i fakt, że więcej apartamentów bez ceny jest bardziej na północ od środka

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
saveRDS(solutions, file = "RapaczWioletta.rds")


