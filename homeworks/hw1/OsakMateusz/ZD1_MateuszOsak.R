###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# Ładowanie danych --------------------------------------------------------

reviews <- read.csv("/Users/mateuszosak/Documents/OsakMateusz/reviews.csv")
listings <- read.csv("/Users/mateuszosak/Documents/OsakMateusz/listings.csv")

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

ilu <- reviews %>%
  group_by(reviewer_id) %>%
  summarise(liczba_opinii = n()) %>%
  filter(liczba_opinii >20) %>%
  nrow()


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- ilu

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
reviews_2 <- reviews
reviews_2$date <- as.Date(reviews_2$date)
reviews_2$month <- months(reviews_2$date)
miesiac <- reviews_2%>%
  group_by(month) %>%
  summarise(ile_miesiecy = n()) %>%
  arrange(desc(ile_miesiecy))

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- miesiac

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
apartamenty <- (reviews %>%
  group_by(listing_id) %>%
  summarise(liczba_opinii = n()) %>%
  filter(liczba_opinii >=100))$listing_id

najw_cena <- listings %>%
  filter(id %in% apartamenty) %>%
  summarise(najw_cena = max(price, na.rm = TRUE))
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- najw_cena 

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
apartamenty_10lat <- reviews %>%
  mutate(date = as.Date(date)) %>%
  group_by(listing_id) %>% 
  summarise(min_date = min(date), max_date = max(date)) %>% 
  mutate(difference = max_date - min_date,
         longer_10years = difference > 3652) %>% 
  summarise(
    listings_over_10y = sum(longer_10years),
    max_years = as.numeric(max(difference / 365.2, na.rm = TRUE))
  )
## w moim rozwiązaniu uwzględniałem lata przestępne 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- apartamenty_10lat
# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

srednia <- mean(listings$latitude, na.rm = TRUE)
roznica <- listings %>%
  mutate(polozenie = ifelse(latitude > srednia, "polnoc", "poludnie")) %>%
  group_by(room_type, polozenie) %>%
  summarise(ile = n(), .groups = "drop") %>%
  pivot_wider(names_from = polozenie, values_from = ile, values_fill = 0) %>% 
  mutate(roznica = poludnie - polnoc)
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- roznica

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
apartamenty_filtr <- reviews %>%
                  group_by(listing_id) %>%
                  summarise(liczba_opinii = n())
granice <- quantile(apartamenty_filtr$liczba_opinii,probs= seq(0.4,0.5, by =0.1))
apartamenty_filtr <- apartamenty_filtr[(apartamenty_filtr$liczba_opinii > granice[1]) & (apartamenty_filtr$liczba_opinii  < granice[2]), ]$listing_id
wynik_6 <- reviews %>%
  filter(listing_id %in% apartamenty_filtr) %>%
  mutate(
    rok = as.numeric(format(as.Date(date), "%Y")),
    typ_roku = if_else(rok %% 2 == 0, "parzysty", "nieparzysty")
  )  %>%
  group_by(listing_id, typ_roku) %>%
  summarise(liczba = n(), .groups = "drop") %>% 
  group_by(listing_id) %>%
  summarise(
    nieparzyste = sum(liczba[typ_roku == "nieparzysty"]),
    parzyste = sum(liczba[typ_roku == "parzysty"]),
    .groups = "drop"
  ) %>%
  mutate(wiecej_nieparzystych = nieparzyste > parzyste)


wynik_6 <- wynik_6 %>%
  filter(wiecej_nieparzystych) %>%
  summarise(
    liczba_apartamentow = n(),
    ile_wiecej_nieparzystych = sum(nieparzyste) - sum(parzyste),
    ile_nieparzystych = sum(nieparzyste)
  )

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- wynik_6

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
dane_7 <- listings %>% 
  select(room_type, price) %>%
  filter(!is.na(price))


wykres7 <- ggplot(dane_7, aes(x = room_type, y = price)) +
  geom_boxplot(fill = "skyblue", outlier.color = 'blue') +
  scale_y_log10() +
  labs(
    title = "Rozkład cen apartamentów w zależności od typu pokoju",
    x = "Typ pokoju",
    y = "Cena"
  ) +
  theme_bw()

# Najwyższe ceny są dla pokoi hotelowych, niższe dla całego domu/apartamentu, jednak dla tego typu pokoi mamy również wiele drogich ofert.
# Prawdopodobnie jakieś luksusowe akomodacje. Najniższe ceny są dla private room'ów i shared room'ów. Dla tych typów obserwujemy dużo mniejszy
# roz®zut cen niż dla innych typów.

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- dane_7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?





## Rozwiązanie
dane_8 <- listings %>%
  mutate(
    pierwsza_litera = substr(host_name, 1, 1)) %>%
  filter(!is.na(pierwsza_litera)) %>%
  group_by(pierwsza_litera) %>%
  summarise(ile = n(), .groups = "drop") %>%
  filter(grepl("^[A-ZĄĆĘŁŃÓŚŹŻ]", pierwsza_litera)) 


wykres_8 <- ggplot(litery, aes(x = pierwsza_litera, y = ile, fill = pierwsza_litera)) +
  geom_col() +
  labs(
    title = "Rozkład pierwszych liter imion gospodarzy",
    x = "Litera",
    y = "Liczba wystąpień"
  ) +
  theme_minimal() + 
  theme(legend.position = "none")
  
# najczęsciej hosci mają imiona rozpoczynajace sie na 'M', 'J', 'A', czy 'S'.
# Najrzadziej 'Q', 'U', 'X', 'Z'.
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- dane_8


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?
# chodzi o współrzędne goegraficzne
## Rozwiązanie
dane_9 <- listings %>%
  filter(is.na(price)) %>% 
  select(id, longitude, latitude)


wykres_9 <- ggplot(apartamenty_brak_ceny, aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.5, size = 3) +
  labs(
    title = "Rozmieszczenie apartamentów bez podanej ceny w Seattle",
    x = "Długość geograficzna",
    y = "Szerokość geograficzna"
  ) +
  theme_bw()

# Nie, zaobserwować możemy znaczące skupienie apartamentów w centrum. Sporo z nich znajduje się również na północy od niego.

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- dane_9



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
saveRDS(solutions, file = "OsakMateusz.rds")


