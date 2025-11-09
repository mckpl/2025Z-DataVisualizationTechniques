###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)

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
ANS_TASK_01 <- reviews %>%
  count(reviewer_id) %>%
  filter(n > 20) %>%
  nrow()

## Odpowiedz przypisana do zmiennej
# ANS_TASK_01 <- NA

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
ANS_TASK_02 <- reviews %>%
  mutate(date = as.Date(date),
         month = format(date, "%m")) %>%
  count(month, name='n_reviews') %>%
  arrange(desc(n_reviews))


## Odpowiedz przypisana do zmiennej
#ANS_TASK_02 <- NA

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
review_count <- reviews %>%
  count(listing_id) %>%
  rename(id = listing_id)

ANS_TASK_03 <- listings %>%
  left_join(review_count, by = 'id') %>%
  filter(n >= 100) %>%
  summarise(max_price = max(price, na.rm = TRUE)) %>%
  pull(max_price)


## Odpowiedz przypisana do zmiennej
#ANS_TASK_03 <- NA

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
res_04 <- reviews %>%
  mutate(date = as.Date(date)) %>%
  group_by(listing_id) %>%
  summarise(first_rev = min(date),
         last_rev = max(date)) %>%
  mutate(rev_span = as.numeric(difftime(last_rev, first_rev, unit='days'))/365) %>%
  filter(rev_span > 10)

#reviews %>%
  #filter(is.na(date))
# Brak NA w tej kolumnie

max_years_04 <- res_04 %>%
  summarise(max_last = max(rev_span)) %>%
  pull(max_last)

res_04_1 <- res_04 %>%
  nrow()

# Chyba najrozsądniej patrzec kiedy jest pierwsza a kiedy ostatnia,
# bo jak jakis lokal nie mial opinii od 10 lat to nie zakladalbym ze nadal jest na rynku

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- c(res_04_1, max_years_04)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

center <- listings %>%
  summarise(mean_lat = mean(latitude)) %>%
  pull(mean_lat)
  
res_05 <- listings %>%
  mutate(location = ifelse(latitude > center, "North", "South")) %>%
  count(room_type, location) %>%
  pivot_wider(names_from = location, values_from = n, values_fill = 0) %>%
  mutate(S_vs_N = South - North)


## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- res_05

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
q4 <- quantile(review_count$n, 0.4, na.rm=TRUE)
q5 <- quantile(review_count$n, 0.5, na.rm=TRUE)

q4_5 <- review_count %>% 
  filter(n > q4, n < q5) %>% 
  rename(listing_id = id)

res_06 <- reviews %>%
  semi_join(q4_5, by='listing_id') %>%
  mutate(date = as.Date(date),
         year = as.numeric(format(date, '%Y')),
         even = ifelse(year %% 2 == 0, 'yes', 'no')) %>%
  count(listing_id, even) %>%
  pivot_wider(names_from = even, values_from = n, values_fill = 0) %>%
  mutate(which_more = case_when(yes - no > 0 ~ 'even',
                               yes - no < 0 ~ 'odd',
                               TRUE ~ 'same'))

res_06_2 <- res_06 %>%
  count(which_more)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- c(res_06, res_06_2)

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
res_07 <- listings %>%
  filter(!is.na(price)) %>%
  select(room_type, price)

price_analysis <- listings %>%
  filter(!is.na(price)) %>%
  group_by(room_type) %>% 
  summarise(mean_price = mean(price),
            median_price = median(price),
            max_price = max(price),
            min_price = min(price),
            std_price = sd(price),
            min_max = max_price - min_price
            )



f7 <- ggplot(res_07, mapping = aes(x = room_type, y = price)) +
  geom_boxplot(fill='seagreen', alpha=0.7, outlier.alpha=0.2) +
  scale_y_log10() +
  labs(
    title = "Rozkład cen apartamentów w zależności od typu pokoju",
    x = 'Typ pokoju', y = 'Cena [$]')+ theme_bw()
f7

# Największy max - min występuje w typie Entire home/apt,
# w tym typie znajdują się jedne z droższych ofert jak i te tanie.
# Najwyższa średnia cena występuje w typie Hotel room, 
# jednak mediana jest już zdecydowanie mniejsze, co oznacza, że
# jest dużo hoteli, których wartość jest zdecydowanie większa, przez
# co średnia jest zdecydowanie zawyżona. Odchylenie standardowe jest bardzo wysokie,
# co oznacza, że dużo wartości odstaje od średniej, w tym typie jest największy rozrzut cenowy.
# Z drugiej strony, w innych typach pokoi, rozrzut pomiedzy mediana a srednia jest juz zdecydowanie nizszy.
# Odchylenie standardowe najniższe dla Shared room, rozrzut cenowy niski.

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- c(price_analysis, f7)

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
res_08 <- listings %>%
  mutate(letter = str_to_upper(substr(host_name, 1, 1))) %>%
  filter(str_detect(letter, "[A-Z]")) %>%
  count(letter)

f8 <- ggplot(res_08, aes(x = fct_reorder(letter, n, .desc = TRUE), y = n)) + geom_col(fill='seagreen', alpha=0.7) + labs(
  title = 'Rozkład pierwszych liter imienion hostów',
  x = "Litera", y = "Liczba hostów"
) + theme_bw() + scale_y_continuous(expand = c(0,0), limits = c(0, max(res_08$n)*1.05))

f8

# Najwięcej M, J, A, S, B, C.
# Najmniej O, Z, X, U, Q.
# W ogólności widzimy znaczną przewage spółgłosek nad samogłoskami.
# Dominują spółgłoski (jednym z powodów jest pewnie to, że jest ich wiecej w alfabecie).
# Rozkład jest nierównomierny i prawostronnie skośny.

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- c(res_08, f8)


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie
res_09 <- listings %>%
  filter(is.na(price))
  
f9 <- ggplot(res_09, aes(x = longitude, y = latitude)) + geom_point(colour='seagreen', alpha=0.7) + labs(
  title = 'Czy rozkład apartamentów bez ceny jest równomierny?', 
  x = 'Długość geograficzna', y = 'Szerokość geograficzna'
) + theme_bw()

f9

# Nie, rozkład nie jest równomierny, widzimy podział na skupiska na północy i w centrum.
# Obszary na południu są niemal puste. Zatem apartamenty w centrum i na północy miasta,
# zazwyczaj nie mają podanej ceny w ogłoszeniu, dla tych położonych na południu to rzadkość.

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- c(res_09, f9)



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
saveRDS(solutions, file = "PiotrowskiKacper.rds")

