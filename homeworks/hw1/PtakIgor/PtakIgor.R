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

res1 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(review_count = n()) %>% 
  filter(review_count > 20) %>% 
  nrow()


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- res1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

res2 <- reviews %>% 
  mutate(date = as.Date(date),
         month = format(date, "%m")) %>% 
  group_by(month) %>% 
  summarise(n_reviews = n()) %>% 
  arrange(desc(n_reviews))

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- res2

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

res3 <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(n_reviews = n()) %>% 
  filter(n_reviews >= 100) %>% 
  left_join(listings, by = c("listing_id" = "id")) %>% 
  summarise(max_price = max(price, na.rm = TRUE))


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- res3

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

res4 <- reviews %>%
  mutate(date = as.Date(date)) %>%
  group_by(listing_id) %>%
  summarise(
    first_review = min(date, na.rm = TRUE),
    last_review = max(date, na.rm = TRUE)
  )  %>% 
  mutate(years_on_market = as.numeric(difftime(last_review, first_review, units = "days")) / 365) %>% 
  summarise(
    n_over_10 = sum(years_on_market > 10),
    max_years = max(years_on_market, na.rm = TRUE)
  )


## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- res4

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie


mean_lat <- mean(listings$latitude, na.rm = TRUE)

res5 <- listings %>%
  mutate(region = ifelse(latitude > mean_lat, "north", "south")) %>%
  group_by(room_type, region) %>% 
  summarise(n_listings = n(), .groups = "drop") %>% 
  pivot_wider(names_from = region, values_from = n_listings, values_fill = 0) %>% 
  mutate(diff_south_north = south - north)



## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- res5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

review_count <- reviews %>% 
  count(listing_id) %>% 
  rename(n_reviews = n)

q4 <- quantile(review_count$n_reviews, 0.4, na.rm = TRUE)
q5 <- quantile(review_count$n_reviews, 0.5, na.rm = TRUE)

mid_listings <- review_count %>%
  filter(n_reviews > q4, n_reviews < q5) %>%
  pull(listing_id)

res6 <- reviews %>%
  filter(listing_id %in% mid_listings) %>%
  mutate(date = as.Date(date),
         year = as.numeric(format(date, "%Y")),
         parity = ifelse(year %% 2 == 0, "even", "odd")) %>%
  group_by(listing_id, parity) %>%
  summarise(n_reviews = n(), .groups = "drop") %>%
  pivot_wider(names_from = parity, values_from = n_reviews, values_fill = 0) %>%
  mutate(more_in_odd = odd > even, difference = odd - even) %>%
  filter(more_in_odd) %>%
  summarise(
    n_listings = n(),
    total_reviews = sum(difference) # Tak rozumiem różnice dla poszczególnych wybranych apartamentów
  )



## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- res6

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

res7 <- listings %>%
  filter(!is.na(price)) %>%
  group_by(room_type) %>%
  summarise(
    mean_price = mean(price),
    median_price = median(price),
    min_price = min(price),
    max_price = max(price),
    .groups = "drop"
  )

plot7 <- ggplot(listings, aes(x = room_type, y = price)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.alpha = 0.4) +
  scale_y_log10() + 
  labs(
    title = "Rozkład cen apartamentów w zależności od typu pokoju",
    x = "Typ pokoju",
    y = "Cena [USD]"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# Ceny znacząco różnią się między typami pokoi – najwyższe są dla hotel rooms 
# (wysoka mediana i szeroki rozrzut), nieco niższe dla entire home/apt 
# (duża zmienność i wiele drogich ofert), a najniższe dla private i shared rooms, 
# które charakteryzują się bardziej jednolitym poziomem cen.
# Największy rozrzut cen dla Entire home/apt 


## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- res7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie


res8 <- listings %>%
  filter(!is.na(host_name)) %>%
  mutate(first_letter = toupper(substr(host_name, 1, 1))) %>%
  filter(grepl("^[A-ZĄĆĘŁŃÓŚŹŻ]$", first_letter)) %>% 
  group_by(first_letter) %>%
  summarise(n_hosts = n(), .groups = "drop") %>%
  arrange(desc(n_hosts))

plot8 <- ggplot(res8, aes(x = reorder(first_letter, -n_hosts), y = n_hosts)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Rozkład pierwszych liter imienia hosta",
    x = "Pierwsza litera imienia",
    y = "Liczba hostów"
  ) +
  theme_minimal()

# Najwięcej hostów ma imiona rozpoczynające się na litery M, J, A i S. 
# Kolejne popularne to B, C i K. 
# Litery rzadziej spotykane to np. Q, U, X, Z i O. 
# Rozkład nie jest równomierny – widać wyraźną dominację kilku pierwszych liter alfabetu.

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- res8


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

res9 <- listings %>%
  mutate(missing_price = is.na(price)) %>%
  group_by(missing_price) %>%
  summarise(n = n(), .groups = "drop")

plot9 <- ggplot(listings %>% filter(is.na(price)),
       aes(x = longitude, y = latitude)) +
  geom_point(color = "red", alpha = 0.5, size = 1.5) +
  labs(
    title = "Rozkład apartamentów bez podanej ceny w Seattle",
    x = "Długość geograficzna",
    y = "Szerokość geograficzna"
  ) +
  theme_minimal()

# Apartamenty bez podanej ceny nie są rozmieszczone równomiernie w Seattle. 
# Najwięcej braków cenowych występuje w centralnej części miasta, 
# natomiast na obrzeżach liczba takich ofert jest mniejsza. 
# Może to sugerować, że braki w danych częściej dotyczą obiektów w popularnych lokalizacjach.

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- res9



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
saveRDS(solutions, file = "NazwiskoImie.rds")


