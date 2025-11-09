###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# Ładowanie danych --------------------------------------------------------
setwd("~/Desktop/nie_spacja/wizualizacja/MulewiczAleksandra/")
reviews <- read.csv("reviews.csv")
listings <- read.csv("listings.csv")

# Instrukcja --------------------------------------------------------------

# Poniższe zadania wymagają obróbki danych. W tym celu należy wykorzystać pakiet
# dplyr i tidyr. Wynik z każdego zadania należy przypisać do zdefiniowanej nazwy
# ANS_TASK_0X. Kod utworzonych wykresów należy pozostawić w pliku .R, nie należy
# zapisywać wykresów.
# Po wykonaniu wszystkich zdań należy wykonać kod oznaczony nagłówkiem 
# "Zapisanie rozwiązań do plik .RDS".


View(reviews)
View(listings)
# Zadanie 1 (0.5) ---------------------------------------------------------
# Ilu recenzentów wystawiło więcej niż 20 opinii?

## Rozwiązanie
odp_1 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(n=n()) %>% 
  filter(n>20) %>% 
  summarise(liczba_recenzentow = n())

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- odp_1[[1]]

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
reviews <- reviews %>% 
  mutate(date = as.Date(date), month = format(date, "%B"))

odp_2 <- reviews %>% 
  group_by(month) %>% 
  summarise(n=n()) %>%
  arrange(desc(n))%>% 
  head(3)
  
odp2_sentence <- paste0(
  "Najwięcej recenzji jest w tych miesiącach: ",
  paste0(odp_2$month, " (", odp_2$n, ")", collapse = ", "),
  "."
)

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- odp2_sentence

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
host_id <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(n=n()) %>% 
  rename(id = listing_id)

host_id_100 <- listings %>% 
  left_join(host_id, by = 'id') %>% 
  filter(n > 100) %>% 
  arrange(desc(price)) %>% 
  select(price) %>% 
  head(1)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- host_id_100[[1]]

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
reviews$date <- as.Date(reviews$date)
date_today <- as.Date("2025-10-20")

apartmenty_wiek <- reviews %>% 
  inner_join(listings, join_by(listing_id == id)) %>% 
  group_by(id) %>% 
  summarise(first_review = min(date)) %>% 
  mutate(wiek = as.numeric(difftime(date_today, first_review, units = "days"))/365) %>% 
  filter(wiek>10) %>% 
  arrange(desc(wiek))

n_ponad_10 <- apartmenty_wiek %>% 
  summarise(n = n()) 
wiek <- apartmenty_wiek %>% 
  select(wiek) %>% 
  head(1)
odp3_sentence <- paste0(
  "Na rynku jest ", n_ponad_10[[1]], 
  " apartamentów starszych niż 10 lat na dzień 20 października. ",
  "Maksymalna liczba lat apartamentu na rynku to ",round(wiek[[1]],0), "."
)

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- odp3_sentence

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
srodek <- mean(listings$latitude, na.rm = TRUE)

roznica <- listings %>% 
  mutate(region = if_else(latitude > srodek, "North", "South")) %>% 
  group_by(room_type, region) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  pivot_wider(names_from = region, values_from = n, values_fill = 0) %>% 
  mutate(difference = South - North) %>% 
  select(room_type, difference)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- roznica

# Zadanie 6 (0.5) ---------------------------------------------------------
  # Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
  # 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
  # niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

reviews_n <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(total_reviews = n()) 

q4 <- quantile(reviews_n$total_reviews, 0.4, na.rm = T) 
q5 <- quantile(reviews_n$total_reviews, 0.5, na.rm = T) 

ids <- reviews_n %>% 
  filter(total_reviews >= q4 & total_reviews <= q5 ) %>% 
  pull(listing_id)

wynik <- reviews %>%
  filter(listing_id %in% ids) %>%
  mutate(date  = as.Date(date),year  = as.integer(format(date, "%Y")),parity = if_else(year %% 2L == 0L, "even", "odd")) %>%
  count(listing_id, parity, name = "n") %>%
  pivot_wider(names_from = parity, values_from = n, values_fill = list(n = 0)) %>%
  mutate(total = odd + even, odd_more = odd > even)

odp_6 <- summarise(wynik,
                    more_odd = sum(odd_more),
                    total_reviews = sum(total[odd_more]),
                    total_odd_reviews = sum(odd[odd_more])
)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- odp_6


# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

odp_7 <- listings %>%
  mutate(price = as.numeric(gsub("[$,]", "", price)))  # jeśli kolumna jest tekstowa

plot <- ggplot(odp_7, aes(x = room_type, y = price)) +
  geom_boxplot(fill = "white", color = "pink", outlier.color = "red") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(
    title = "Rozkład cen apartamentów w zależności od typu pokoju",
    x = "Typ pokoju",
    y = "Cena za noc"
  )+
  ylim(0,1800)
plot

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- odp_7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

litery <- listings %>%
  mutate(pierwsza_litera = toupper(substr(host_name, 1, 1))) %>%
  filter(!is.na(pierwsza_litera) & pierwsza_litera != "") %>%
  count(pierwsza_litera, sort = TRUE) %>% 
  filter(grepl("[A-Z]", pierwsza_litera)) 

srednia <- mean(litery$n) 

plot2<-ggplot(litery, aes(x = pierwsza_litera, y = n)) +
  geom_col(fill = "darkblue") +
  geom_hline(yintercept = srednia, color = "red", linetype="dashed") +
  labs(
    title = "Rozkład pierwszych liter imion hostów",
    subtitle = paste("Czerwona linia to średnia liczba hostów (", round(srednia, 1), ")", sep = ""),
    x = "Pierwsza litera imienia hosta",
    y = "Liczba hostów"
  )
plot2
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- litery


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie
bez_ceny <- listings %>%
  filter(is.na(price) | price == "" | price == 0)

srodek_lat =  mean(listings$latitude, na.rm = TRUE)
srodek_long =  mean(listings$longitude, na.rm = TRUE)

ggplot(bez_ceny, aes(x = latitude, y = longitude)) +
  geom_point(color = "pink") +
  geom_vline(xintercept = srodek_lat, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = srodek_long, linetype = "dashed", color = "gray40") +
  labs(
    title = "Rozkład apartamentów bez ceny",
    x = "lat",
    y = "long"
  )

plot3 <- ggplot(bez_ceny, aes(x = latitude, y = longitude)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.4) +
  geom_point(color = "pink", alpha = 0.6, size = 1) +
  scale_fill_viridis_c(option = "plasma") +
  geom_vline(xintercept = srodek_lat, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = srodek_long, linetype = "dashed", color = "gray40") +
  labs(
    title = "Gęstość apartamentów bez ceny w Seattle",
    x = "lat",
    y = "long",
    fill = "Gęstość"
  )

plot3

 # Nie jest równomierny
## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- bez_ceny



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
saveRDS(solutions, file = "MulewiczAleksandra.rds")


