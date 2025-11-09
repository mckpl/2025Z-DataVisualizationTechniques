###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# Ładowanie danych --------------------------------------------------------

reviews <- read.csv("homeworks/hw1/AdrjanJerzy/reviews.csv")
listings <- read.csv("homeworks/hw1/AdrjanJerzy/listings.csv")

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

# 32
ANS_TASK_01 <- reviews %>%
  group_by(reviewer_id) %>%
  summarise(n=n()) %>%
  filter(n>20) %>%
  select(reviewer_id) %>%
  lengths() %>%
  unname()

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

# data zapisana jest w sposó standardowy, więc można wyciągnąć miesiąc bezpośrednio

# w odpowiedzi umieszczone są miesiące w kolejności od najwięcej do najmniej recenzji

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- reviews %>%
  mutate(month = substring(date, 6, 7), .keep='none') %>%
  group_by(month) %>%
  summarise(n=n()) %>%
  mutate(month = format(ISOdate(2000, 1:12, 1), "%B")[strtoi(month, base=10)]) %>%
  arrange(desc(n)) %>%
  pull(month)

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- left_join(
  listings,
  reviews %>%
    group_by(listing_id) %>%
    summarise(n=n()),
  by=join_by(id == listing_id)
) %>%
  filter(!is.na(price) & n > 100) %>%
  pull(price) %>%
  max()

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
TODAY <- as.POSIXct(Sys.time(), format = "%Y-%m-%d") # stała
YESTERDAY <- TODAY - 86400 # stała

yearfun <- function(dt, start){
  year <- as.numeric(format(start, "%Y"))-as.numeric(format(dt, "%Y"))
  ifelse(format(start, "%m%d") < format(dt, "%m%d"), year-1, year)
}

# years_bias: liczba lat do wczoraj (!) 
# aby odrzucić apartamenty na rynku równo 10 lat (czyli nie dłużej)
# jeżeli years == 10, to może być to 10 lat i zero dni (czyli nie dłużej)

## Odpowiedz przypisana do zmiennej
tmp04 <- reviews %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d")) %>%
  group_by(listing_id) %>%
  summarise(date = min(date)) %>%
  mutate(years_bias = yearfun(date, YESTERDAY), years=yearfun(date, TODAY)) %>%
  filter(years_bias >= 10) %>%
  summarise(ten_years = n(), max_years = max(years))

# format odpowiedzi: 

# Odpowiedź w formacie wektora 2-elementowego.
# Element 1: liczba apartamentów na rynku dłużej niż 10 lat
# Element 2: maksymalna liczba (pełnych) lat na rynku 

ANS_TASK_04 <- as.numeric(tmp04[1,])

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
mean(listings$latitude)
tmp <- listings %>%
  mutate(south = latitude<mean(latitude)) %>%
  group_by(room_type, south) %>%
  summarise(n = n()) %>%
  mutate(n = n*((as.numeric(south)*2)-1)) %>%
  group_by(room_type) %>%
  summarise(wiecej_o=sum(n))

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- tmp # groupby umieści FALSE ponad TRUE

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
# między 4 a 5 decylem <=> kwantyl 0.40-0.50 (5 decyl)
# liczba recenzji pochodzi ze zliczania elementów dataframe reviews

tmp06 <- reviews %>%
  group_by(listing_id) %>%
  summarise(n=n()) %>%
  filter(n > quantile(n, 0.4) & n <= quantile(n, 0.5)) %>% # 27-41
  left_join(reviews, by='listing_id') %>% # zostają tylko recenzje z 5 decyla
  select(listing_id, date)%>%
  mutate(odd = as.logical((strtoi(substr(date, 1, 4), base=10)) %% 2)) %>%
  group_by(listing_id, odd) %>%
  summarise(n=n()) %>%
  # tutaj zmieniamy liczbę recenzji w latach parzystych na ujemną
  # aby je dodać i uzyskać różnicę między liczbą r. w latach parz. i nieparz.
  mutate(temp = n*((as.numeric(odd)*2)-1)) %>%
  select(-odd) %>%
  group_by(listing_id) %>%
  summarise(listings = sum(n), difference=sum(temp), .groups="drop_last") %>%
  filter(difference > 0) %>%
  select(-listings)

## Odpowiedz przypisana do zmiennej
# Ile: 315. Odpowiedź na pyt. 1 zawarta w wyświetlającym się rozmiarze.
# Przypisany jest dataframe składający się z listy apartamentów
# spełniających warunek i różnicy (o ile więcej jest nieparzystych od parzystych)
ANS_TASK_06 <- tmp06

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

tmp07 <- listings %>%
  select(price, room_type)

# Wykres przedstawia rozkład cen apartamentów 
# osobno dla każdego typu pokoju.
tmp07 %>%
  ggplot(mapping=aes(x=price, color=room_type, fill=room_type)) +
  geom_density(alpha=0.3) +
  scale_x_continuous(transform="log10") +
  ylab("Density") +
  xlab("Cena [skala logarytmiczna]")

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- tmp07

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

tmp08 <- listings %>%
  mutate(letter = substr(host_name, 1, 1), .keep='none') %>%
  filter(grepl('^[A-Za-z]$', letter)) # filtrowanie nie-liter ("2")
  
tmp08 %>%
  ggplot(mapping=aes(x=fct_infreq(letter))) +
  geom_bar() +
  xlab("Pierwsza litera imienia") +
  ylab("Liczba")

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- tmp08

# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

tmp09 <- listings %>%
  mutate(price = !is.na(price)) %>%
  select(latitude, longitude, price)

tmp09 %>%
  ggplot(mapping=aes(y=latitude, x=longitude, alpha=price)) +
  geom_point() +
  scale_alpha_manual(values = c("FALSE" = 1.0, "TRUE" = 0.03)) +
  guides(alpha=guide_legend(title="Obecność ceny"))
  
# Odpowiedź na pytanie: Tak, widać na mapie, że punkty z ceną == NA są rozmieszczone
# względnie równomiernie, tj. podobnie do ogólnego rozmieszczenia punktów.
## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- tmp09

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
saveRDS(solutions, file = "homeworks/hw1/JerzyAdrjan/AdrjanJerzy.rds")



