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
ans1 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarize(num_reviews = n()) %>% 
  filter(num_reviews > 20) %>% 
  nrow()


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- ans1

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie


#Znajdujemy tutaj 3 miesiąca z najwiekszą ilością wystawianych komentarzy
best_months <- reviews %>%
  mutate(month = as.integer(format(as.Date(reviews$date), "%m"))) %>% 
  group_by(month) %>% 
  summarize(num_reviews = n()) %>% 
  arrange(desc(num_reviews)) %>% 
  top_n(3)

#czyli to są Sierpień(top 1), Czerwiec(top 2), Lipiec(top 3)


## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- best_months


# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie
max_price <- reviews %>% 
  group_by(listing_id) %>% 
  summarize(num_reviews = n()) %>% 
  filter(num_reviews >= 100) %>% 
  inner_join(listings, by=c("listing_id" = "id")) %>% 
  summarize(max_price = max(price, na.rm=TRUE))

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- max_price

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
ans4 <- reviews %>%
  group_by(listing_id) %>%
  summarise(first_review_year = min(as.integer(format(as.Date(date), "%Y"))), .groups = "drop") %>%
  mutate(years_on_market = 2025 - first_review_year) %>%
  summarise(
    apartments_over_10_years = sum(years_on_market > 10, na.rm = TRUE),
    max_years_on_market = max(years_on_market, na.rm = TRUE)
  )


## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- ans4

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
ans5 <- listings %>%
  filter(!is.na(latitude) & !is.na(room_type)) %>%
  mutate(hemisphere = ifelse(latitude > mean(latitude, na.rm = TRUE), "South", "North")) %>%
  group_by(room_type, hemisphere) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = hemisphere, values_from = count, values_fill = 0) %>%
  mutate(difference_south_vs_north = South - North)



## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- ans5

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
reviews_by_listing <- reviews %>%
  group_by(listing_id) %>%
  summarise(n_reviews = n(), .groups = "drop")

deciles <- quantile(reviews_by_listing$n_reviews, probs = c(0.4, 0.5))

ans6 <- reviews %>%
  inner_join(reviews_by_listing, by = "listing_id") %>%
  filter(n_reviews >= deciles[1] & n_reviews <= deciles[2]) %>%
  mutate(year_type = ifelse(as.integer(format(as.Date(date), "%Y")) %% 2 == 1, "Odd", "Even")) %>%
  group_by(listing_id, year_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = year_type, values_from = count, values_fill = 0) %>%
  filter(Odd > Even) %>%
  summarise(
    num_apartments = n(),
    total_reviews = sum(Odd) + sum(Even)
  )



## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- ans6

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
ans7 <- listings %>%
  mutate(price_numeric = as.numeric(gsub("[\\$,]", "", price))) %>%
  filter(!is.na(price_numeric) & price_numeric > 0) %>%  # Remove non-finite values
  ggplot(aes(x = room_type, y = price_numeric)) +
  geom_boxplot(outlier.alpha = 0.5) +  # Adjust outlier transparency
  scale_y_log10(labels = scales::dollar_format()) +
  labs(
    title = "Rozkład cen apartamentów w zależności od typu pokoju",
    x = "Typ pokoju",
    y = "Cena (skala logarytmiczna)"
  ) +
  theme_minimal()



## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- ans7

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
ans8 <- listings %>%
  filter(!is.na(host_name) & host_name != "") %>%
  mutate(first_letter = toupper(substr(host_name, 1, 1))) %>%
  filter(grepl("^[A-Z]$", first_letter)) %>%  # Keep only rows where the first letter is an uppercase letter
  count(first_letter, name = "count") %>%
  mutate(first_letter = reorder(first_letter, -count)) %>%
  ggplot(aes(x = first_letter, y = count, fill = count)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "purple") +
  labs(
    title = "Rozkład pierwszych liter imion hostów",
    x = "Pierwsza litera imienia",
    y = "Liczba hostów"
  ) +
  theme_minimal() +
  theme(legend.position = "right")



## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- ans8


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład (położenie) aparmatemtów w mieście Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

# Filter listings without price
data_no_price <- listings %>%
  filter(is.na(price) | price == "") %>%
  filter(!is.na(longitude) & !is.na(latitude))

# Plot 1: Scatter plot
plot1 <- data_no_price %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Położenie apartamentów bez podanej ceny w Seattle",
    x = "Długość geograficzna",
    y = "Szerokość geograficzna",
    caption = "Widoczne są skupiska, co wskazuje na nierównomierny rozkład."
  ) +
  theme_minimal()

# Plot 2: 2D density plot with normalized levels
plot2 <- data_no_price %>%
  ggplot(aes(x = longitude, y = latitude)) +
  stat_density_2d(aes(fill = ..level../max(..level..)), geom = "polygon", alpha = 0.7) +
  scale_fill_gradient(low = "lightblue", high = "purple", name = "Gęstość (znormalizowana)") +
  labs(
    title = "Gęstość rozkładu apartamentów bez ceny",
    x = "Długość geograficzna",
    y = "Szerokość geograficzna",
  ) +
  theme_minimal()

# Combine the two plots
ans9 <- list(plot1, plot2)

# Na pierwszym wykresie widoczne są skupiska,
# oraz na drugim wykresie widoczne są obszary o wysokiej gęstości
# w porównaniu do innych obszarów, co
# razem wskazuje na nierównomierny rozkład.


## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- ans9



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
saveRDS(solutions, file = "ButsialevichYauheni.rds")



