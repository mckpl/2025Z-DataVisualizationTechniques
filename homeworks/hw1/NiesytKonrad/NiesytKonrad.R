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
# Po wykonaniu wszystkich zadań należy wykonać kod oznaczony nagłówkiem 
# "Zapisanie rozwiązań do plik .RDS".



# Zadanie 1 (0.5) ---------------------------------------------------------
# Ilu recenzentów wystawiło więcej niż 20 opinii?

## Rozwiązanie
zad01 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(n_reviews = n()) %>%
  filter(n_reviews > 20) %>% 
  nrow()

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- zad01 # 32 recenzentów wystawiło więcej niż 20 opinii

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

reviews <- reviews %>%
  mutate(date = as.Date(date))


reviews_monthly <- reviews %>%
  mutate(
    month = format(date, "%m"),
  ) %>%
  group_by(month) %>%
  summarise(n_reviews = n()) %>%
  arrange(desc(n_reviews))

reviews_monthly 

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- reviews_monthly # Najwięcej recenzji jest wystawianych w sierpniu, czerwcu oraz lipcu

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
conajmniej100 <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(n_reviews = n()) %>%
  filter(n_reviews >= 100)

zad03 <- merge(conajmniej100, select(listings, id, name, price), by.x ="listing_id",by.y="id") %>% 
  top_n(1, wt=price)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- zad03$price # Największa cena: 1012

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
zad04 <- reviews %>%
  mutate(date = as.Date(date)) %>%
  group_by(listing_id) %>%
  summarise(pierwsza_recenzja = min(date, na.rm = TRUE)) %>%
  mutate(wiek_lat = as.numeric(difftime(Sys.Date(), pierwsza_recenzja, units = "days")) / 365.25) %>%
  summarise(
    liczba_apartamentów_10lat = sum(wiek_lat > 10, na.rm = TRUE),
    maksymalny_wiek_lat = max(wiek_lat, na.rm = TRUE)
  )

zad04
## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- zad04 # 394 apartamenty są na rynku dłużej niż 10 lat, maksymalna liczba lat to 16,3

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
srodek <- mean(listings$latitude)

zad05 <- listings %>%
  mutate(region = ifelse(latitude < srodek, "Południe", "Północ")) %>%
  group_by(room_type, region) %>%
  summarise(liczba = n()) %>%
  pivot_wider(names_from = region, values_from = liczba, values_fill = 0) %>%
  mutate(roznica = Południe - Północ)

zad05

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- zad05


# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

reviews <- reviews %>%
  mutate(date = as.Date(date))


liczby_rec <- reviews %>%
  group_by(listing_id) %>%
  summarise(liczba_recenzji = n())

decyle <- quantile(liczby_rec$liczba_recenzji, probs = c(0.4, 0.5), na.rm = TRUE)

apartamenty_decyl <- liczby_rec %>%
  filter(liczba_recenzji >= decyle[1], liczba_recenzji <= decyle[2]) %>%
  pull(listing_id)

zad06 <- reviews %>%
  filter(listing_id %in% apartamenty_decyl) %>%
  mutate(rok = as.integer(format(date, "%Y")),
         typ_roku = ifelse(rok %% 2 == 0, "parzysty", "nieparzysty")) %>%
  group_by(listing_id, typ_roku) %>%
  summarise(liczba_rec = n(), .groups = "drop") %>%
  pivot_wider(names_from = typ_roku, values_from = liczba_rec, values_fill = 0) %>%
  filter(nieparzysty > parzysty) %>% 
  mutate(roznica = nieparzysty - parzysty)

nrow(zad06)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- zad06 # 341 apartamentów

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

zad07 <- listings %>% 
  select(room_type, price) %>% 
  filter(!is.na(price))

wykres07 <- ggplot(zad07, mapping = aes(y = price, x = room_type, fill = room_type)) +
  geom_violin() +
  labs(
    title = "Rozkład cen apartamentów w zależności od typu pokoju",
    x = "Typ pokoju",
    y = "Cena"
  ) +
  scale_y_log10() +
  theme_bw() + theme(legend.position = "none")

wykres07

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- zad07 #Generalnie najdroższe są apartamenty typu "Hotel room", w typie "Entire home/apt" występują najbardziej skrajne wartości

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

host_letters <- listings %>%
  mutate(
    first_letter = toupper(sub("^[^A-Za-z]*([A-Za-z]).*$", "\\1", host_name))  # pierwsza litera alfabetu, ignorując cyfry
  ) %>%
  group_by(first_letter) %>%
  summarise(liczba = n(), .groups = "drop") %>%
  arrange(desc(liczba))

host_letters

wykres08 <- ggplot(host_letters, mapping = aes(x = first_letter, y = liczba)) +
  geom_col() +
  labs(
    title = "Rozkład pierwszych liter imion hostów",
    x = "Pierwsza litera hosta",
    y = "Liczba apartamentów"
  ) +
  theme_bw()

wykres08

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- host_letters #Najczęściej jako pierwsze litery hostów występują "M", "J" i "A", a najrzadziej "X", "U" oraz "Q".


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie
apartamenty_na <- listings %>%
  filter(is.na(price) | price == 0)

ggplot(apartamenty_na, aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.5, color = "red") +
  coord_fixed() +
  labs(
    title = "Rozkład apartamentów w Seattle bez podanej ceny",
    x = "Długość geograficzna (longitude)",
    y = "Szerokość geograficzna (latitude)"
  ) +
  theme_bw()




## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- apartamenty_na
# Rozkład takich apartamentów nie jest równomiarny - najwięcej występuje w centrum miasta. 


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
saveRDS(solutions, file = "NiesytKonrad.rds")


