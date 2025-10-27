###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Tomasz Stolarczyk, 333090
# Ładowanie pakietów-----------------------------------------------------
# install.packages(c("dplyr", "tidyr", "ggplot2"))
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
## Odpowiedz przypisana do zmiennej

ANS_TASK_01 <- reviews %>% 
  count(reviewer_id, name = "review_count") %>% 
  filter(review_count > 20) %>% 
  nrow()

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?
## Rozwiązanie
## Odpowiedz przypisana do zmiennej

ANS_TASK_02 <- reviews %>%
  mutate(month = format(as.Date(date), "%m")) %>% # zmieniamy format ze stringa w date
  count(month, name = "review_count") %>%
  mutate(month_name = month.name[as.numeric(month)]) %>% # numer miesiaca w nazwe
  arrange(desc(review_count)) %>% # sortujemy malejaco
  select(month, month_name, review_count)

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).
## Rozwiązanie
## Odpowiedz przypisana do zmiennej

# najpierw znajdzmy id tych ktore maja wiecej niz 100 opinii
populars <- reviews %>%
  count(listing_id) %>%        
  filter(n > 100) %>%     
  pull(listing_id) # wyciagamy pojedyncza kolumne jako wektor

ANS_TASK_03 <- listings %>%
  select(id, price) %>%
  filter(id %in% populars) %>% # bierzemy te ktore sa wsrod popularnych >100 opinii
  arrange(desc(price)) %>%
  slice(1) %>% # bierzemy tylko pierwszy wiersz tabeli - max cena
  pull(price) # bierzemy z tego 1 wiersza wartosc ceny

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?
## Rozwiązanie

# poniewaz dane sa historyczne to ustalmy ze "dzisiaj" to data ostatniej recenzji
today_date <- as.Date(max(reviews$date))

listing_ages <- reviews %>%
  group_by(listing_id) %>% # grupujemy po listing id 
  summarise(first_review_date = as.Date(min(date))) %>% # dla kazdego znajdujemy first review date
  mutate(
    age_in_days = as.numeric(today_date - first_review_date), # ile lat na rynku
    age_in_years = floor(age_in_days / 365.25) # dni -> lata
  )

apartments_over_10_years <- listing_ages %>%
  filter(age_in_years > 10) %>%
  nrow()

max_years_on_market <- max(listing_ages$age_in_years)

ANS_TASK_04 <- tibble(
  ile_ponad_10_lat = apartments_over_10_years, 
  maksymalna_liczba_lat = as.integer(max_years_on_market) 
)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)
## Rozwiązanie
## Odpowiedz przypisana do zmiennej

# środek określamy jako średnia z szerokości geograficznej
mean_latitude <- mean(listings$latitude, na.rm = TRUE)

ANS_TASK_05 <- listings %>%
  mutate(location = ifelse(latitude > mean_latitude, "North", "South")) %>%
  count(room_type, location) %>%
  # uzyjemy do tego pivot_wider z biliobteki tidyr
  pivot_wider(
    names_from = location, 
    values_from = n,
    values_fill = 0 
  ) %>%
  mutate(difference_South_vs_North = abs(South - North))
  
# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?
## Rozwiązanie

# zliczmy ile kazdy aparatament dostal recenzji
review_counts_per_apt <- reviews %>%
  count(listing_id, name = "total_reviews")

# wybieramy decyle, miedzy 4 a 5 
deciles <- quantile(review_counts_per_apt$total_reviews, probs = c(0.4, 0.5), na.rm = TRUE)
decile_4 <- deciles[1] 
decile_5 <- deciles[2]

# filtrujemy aprtamenty ktore nas interesuja
target_apartment_ids <- review_counts_per_apt %>%
  filter(total_reviews >= decile_4 & total_reviews <= decile_5) %>%
  pull(listing_id) # bierzemy tylko ich id 

final_apartments_table <- reviews %>%
  filter(listing_id %in% target_apartment_ids) %>% # te ktore nas interesuja
  mutate(
    date = as.Date(date),
    year = as.numeric(format(date, "%Y")),
    year_type = ifelse(year %% 2 == 0, "Parzysty", "Nieparzysty")
  ) %>%
  count(listing_id, year_type) %>%
  # znow uzyjemy pivot_wider z pakietu tidyr
  pivot_wider(
    names_from = year_type,
    values_from = n,
    values_fill = 0 
  ) %>% 
  filter(Nieparzysty > Parzysty) 

final_count_apartments <- nrow(final_apartments_table)
final_total_reviews <- sum(final_apartments_table$Nieparzysty) + sum(final_apartments_table$Parzysty)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- tibble(
  ile_wiecej_w_latach_nieparzystych = final_count_apartments, 
  liczba_recenzji = as.integer(final_total_reviews) 
)

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
## Odpowiedz przypisana do zmiennej

ANS_TASK_07 <- listings %>%
  filter(!is.na(price), price > 0) %>% # odfiltrowujemy dziwne oferty
  
  ggplot(aes(x = room_type, y = price, fill = room_type)) +
  geom_boxplot(outlier.alpha = 0.2) + # nadajemy outlierom przezroczystosci dla estetyki
  # uzyjemy skali logarytmicznej, analogicznie jak robilismy na labach z populacja tydzien temu :)
  scale_y_log10() +
  labs(
    title = "Rozkład cen apartamentów wg typu pokoju",
    x = "Typ pokoju",
    y = "Cena (w $) - Skala Logarytmiczna"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"), # Tytuł na srodku i pogrubiony
  )

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?
## Rozwiązanie
## Odpowiedz przypisana do zmiennej

ANS_TASK_08 <- listings %>% 
  filter(!is.na(host_name) & host_name != "") %>%
  mutate(first_letter = toupper(substr(host_name, 1, 1))) %>%
  filter(grepl("^[A-Z]$", first_letter)) %>% 
  count(first_letter, name = "count") %>% 
  arrange(desc(count)) %>% 
  ggplot(aes(x = reorder(first_letter, -count), y = count)) + 
  geom_col(fill = "blue") + 
  labs(
    title = "Rozkład pierwszej litery imienia hosta vs liczba apartamentów",
    x = "Pierwsza litera imienia hosta",
    y = "Liczba apartamentów"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))# Tytuł na srodku 


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

# odfiltrujmy te co potrzebujemy
listings_no_price <- listings %>%
  filter(is.na(price)) %>%
  select(id, longitude, latitude)

ANS_TASK_09 <- ggplot(listings_no_price, aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.5, size = 1.5) + # alpha daje nam przezroczysotsocs
  labs(
    title = "Rozkład przestrzenny apartamentów (bez ceny) w Seattle",
    x = "Longitude",
    y = "Latitude",
    caption = "odp do zadania: jak widac rozklad jest nierownomierny"
  ) +
  theme_minimal() +
  # coord_fixed() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5, face = "bold")
  )


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
saveRDS(solutions, file = "StolarczykTomasz.rds")
