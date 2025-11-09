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

ans <- reviews %>% group_by(reviewer_id) %>% summarise(n = n()) %>% filter(n > 20) %>% 
  nrow()

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- ans

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

#Jako rozwiązanie jest wektor z posortowanymi miesiącami w kolejności malejącej
# ilości recenzji wystawianych w danym miesiącu

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- reviews %>% 
  mutate(month = stri_sub(date, from=6, to=7)) %>% 
  count(month) %>% 
  arrange(desc(n)) %>% 
  select(month)

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

apartments_id <- reviews %>% count(listing_id) %>% 
  filter(n > 100) %>% 
  select(listing_id)

ans <- listings %>% 
  select(id, price) %>% 
  filter(id %in% apartments_id$listing_id) %>% 
  top_n(price,n= 1) %>% select(price)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- ans

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

tmp <- reviews %>% mutate(
  year = as.numeric(stri_sub(date, from=1, to=4)),
  month = as.numeric(stri_sub(date, from=6, to=7)),
  day = as.numeric(stri_sub(date, from=9, to=10)),
) %>% mutate(
  czy_stary = case_when(
    year < 2015 ~ TRUE,
    year == 2015 & month < 10 ~ TRUE,
    year == 2015 & month == 10 & day < 27 ~ TRUE,
    .default = FALSE
  )
) 

ile <- tmp %>% filter(czy_stary==TRUE) %>% count(listing_id)
najdluzej <- tmp %>% group_by(listing_id) %>% summarise(ile_lat=(max(year)-min(year))) %>% 
  ungroup() %>% summarise(max=max(ile_lat))

#W zadaniu założyłem, że każdy apartament jest nadal na rynku.

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- data.frame(liczba_apartamentów_będących_na_rynku_dłużej_niż_10_lat = ile,
                          maksymalna_liczba_lat_apartamentów_na_rynku = najdluzej)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

tmp <- listings %>% 
  mutate(kierunek = ifelse(latitude < mean(latitude),"płd","płn")) %>% 
  group_by(room_type, kierunek) %>% 
  summarise(n = n()) %>% 
  ungroup(kierunek) 

tmp1 <- tmp %>% filter(kierunek == "płd") %>%
  transmute(
    room_type = room_type,
    pld = n
  )
 
tmp2 <- tmp %>% filter(kierunek == "płn") %>% 
  transmute(
    room_type = room_type,
    pln = n
  )

ans <- full_join(tmp1, tmp2) %>% mutate(
  pld = replace(pld, is.na(pld), 0),
  pln = replace(pln, is.na(pln), 0)
) %>% transmute(
  room_type = room_type,
  roznica = pld - pln
)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- ans

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

filtr <- reviews %>% 
  count(listing_id) %>% 
  filter(quantile(n, probs=0.4) < n & quantile(n, probs=0.5) > n) %>% 
  select(listing_id)

ans <- reviews %>% 
  filter(listing_id %in% filtr$listing_id) %>% 
  mutate(
    parity = as.numeric(stri_sub(date, from=4, to=4)) %% 2
  ) %>% group_by(listing_id, parity) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = parity, values_from = n) %>% 
  rename(p='0', np='1') %>% mutate(
    wiecej_np = np - p
  ) %>% filter(wiecej_np > 0) %>% select(listing_id, wiecej_np)

tmp <- ans %>% nrow()

ans$ilosc <- tmp

#wiecej_np - pokazuje roznice dla danego apartamentu o ile więcej 
#            recenzji było napisanych w latach nieparzystych niż parzystych
#ilosc - ile jest wszystkich apartamentów, dla których jest więcej recenzji
#        w latach nieparzystych niż parzystych(jest to jedna wartość dla wszystkich
#        wierszy)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- ans

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

listings %>% 
  ggplot(aes(x =price, colour = room_type))+
  stat_density(aes(fill = room_type), alpha = 0.2) +
  facet_wrap(~room_type) +
  scale_x_continuous(limits = c(0, 1500)) + 
  labs(
    title = "Rozkład cen apartamentów w zależności od rodzaju pokoju",
    x = "cena",
    y = "gęstość",
    colour = "Rodzaj pokoju",
    fill = "Rodzaj pokoju"
  ) + theme_bw() +
  theme(legend.position = 'none')

# Do narysowania wykresu potrzebne mi były dwie kolumny: price i room_type.
# Na każdym z wykresów widzimy rozkład ceny w zależności od danego typu pokoju.


## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- listings %>% select(price, room_type)

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

tmp <- listings %>% mutate(
  pierwsza_litera = ifelse(host_name == "2Plus", "P", stri_sub(host_name, from=1, to=1))
) %>% count(pierwsza_litera)  # Nazwa 2Plus jest wyjątkiem i jedynym przypadkiem, że nie zaczyna
# się na literę, więc zostało odfiltrowane odrębnie


tmp %>% ggplot(aes(x = forcats::fct_reorder(pierwsza_litera, -n), y=n)) +
  geom_col(fill="orange", show.legend = FALSE) + 
  theme(legend.title = element_blank()) + 
  labs(
    title= "Rozkład pierwszych liter imienia hosta",
    x ="Litera",
    y = "Ilość"
  ) + theme_light()



## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- tmp


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

p <- listings %>% filter(is.na(price)) %>% 
  ggplot(aes(x =  longitude, y=latitude)) +
  geom_point(color = "darkblue", show.legend = FALSE)+
  labs(
    title= "Rozkład położenia apartamentów",
    x = "Szerokość geograficzna",
    y = "Długość geograficzna"
  ) + theme_light()


ggExtra::ggMarginal(p,fill="lightblue", type="histogram")

# Widać po wykresie punktowym, że istnieje zagęszczenie w centrum.
# Do tego po histogramach na osiach OX i OY widać, że istnieją przedział wartości,
# w którym jest o wiele więcej apartamentów niż w innych. Zatem nie jest 
# to rozkład równomierny.

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- listings %>% filter(is.na(price)) %>% select(longitude, latitude)



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
saveRDS(solutions, file = "KitewskiBartosz.rds")
