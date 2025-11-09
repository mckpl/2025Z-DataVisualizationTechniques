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

reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(reviews_count = n()) %>% 
  filter(reviews_count > 20) %>% 
  nrow()
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- reviews %>% 
  group_by(reviewer_id) %>% 
  summarise(reviews_count = n()) %>% 
  filter(reviews_count > 20) %>% 
  nrow()


# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie

reviews %>% 
  separate(date, into = c("year","month","day"), sep = "-") %>% 
  group_by(month) %>% 
  summarise(reviews_count = n()) %>% 
  arrange(desc(reviews_count)) %>%
  select(month) %>%
  head(3) # 3 mięsiące, w których jest wystawiane najwięcej opinii

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- reviews %>% 
  separate(date, into = c("year","month","day"), sep = "-") %>% 
  group_by(month) %>% 
  summarise(reviews_count = n()) %>% 
  arrange(desc(reviews_count)) %>%
  select(month) %>% 
  head(3)


# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie

# wektor id apartamentów, które mają co najmniej 100 opinii
min_100_rev <- reviews %>% 
  group_by(listing_id) %>% 
  summarise(reviews_count = n()) %>% 
  filter(reviews_count >= 100) %>% 
  pull(listing_id) 

listings %>%
  filter(id %in% min_100_rev, !is.na(price)) %>% # szukanie apartamentów, których id znajduje się w powyższym wektorze
  select(price) %>% 
  top_n(1, price) %>%
  as.integer()

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- listings %>% 
  filter(id %in% min_100_rev, !is.na(price)) %>% 
  select(price) %>% 
  top_n(1, price) %>% 
  as.integer()


# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie

# tabelka apartamentów i (przypuszczalnej) ilości ich lat na rynku
over_10_years <- reviews %>%
  mutate(elapsed_time = as.numeric((as.Date(Sys.Date()) - as.Date(date))/365)) %>% # obliczenie jak dawno temu zostały wystawione poszczególne recenzje (w latach z resztą)
  group_by(listing_id) %>% 
  summarise(years_on_the_market = max(elapsed_time)) %>% # wyszukanie najstarszej opinii dla poszczególnych apartamentów
  filter(years_on_the_market > 10)

# ilość apartamentów, które były na rynku dłużej niż 10 lat 
apartments_count_04 <- nrow(over_10_years)

# maksymalna liczba lat apartamentów na rynku
max_years_04 <- as.integer(max(over_10_years$years_on_the_market))

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- data.frame(apartments_count = apartments_count_04, 
                          max_years_on_the_market = max_years_04)


# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie

listings %>% 
  mutate(north_or_south = ifelse(latitude > mean(latitude), "north", "south")) %>% # kolumna pomocnicza, wskazująca czy apartament znajduje się na północy czy południu
  group_by(room_type, north_or_south) %>% 
  summarise(count = n()) %>% # ilość apartamentów na północy i południu w podziale na typ pokoju
  pivot_wider(names_from = north_or_south, values_from = count, values_fill = 0) %>% 
  mutate(south_north_diff = south - north) # różnica między liczbą apartamentów na południu a liczbą apartamentów na północy w podziale na pokój (minusowe wartości oznaczają, że na południu jest więcej apartamentów)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- listings %>% 
  mutate(north_or_south = ifelse(latitude > mean(latitude), "north", "south")) %>% 
  group_by(room_type, north_or_south) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = north_or_south, values_from = count, values_fill = 0) %>% 
  mutate(south_north_diff = south - north)


# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie

# wektor wartości decyli dla ilości recenzji apartamentów
decyles <- quantile(as.vector(listings[,'number_of_reviews']), probs = seq(0.1, 1, 0.1)) 

df_06 <- reviews %>% 
  separate(date, into = c("year","month","day"), sep = "-") %>%
  mutate(even_or_odd = ifelse(as.integer(year) %% 2 == 0, "rev_even_years", "rev_odd_years")) %>% # podział na lata parzyste i nieparzyste
  group_by(listing_id, even_or_odd) %>% 
  summarise(count = n()) %>% # ilość recenzji w latach parzystych i nieparzystych dla poszczególnych apartamentów
  pivot_wider(names_from = even_or_odd, values_from = count, values_fill=0) %>% 
  mutate(sum_even_odd = rev_even_years + rev_odd_years) %>% # suma recenzji dla poszczególnych apartamentów
  filter((sum_even_odd > decyles[4]) & 
           (sum_even_odd < decyles[5]) & 
           (rev_odd_years > rev_even_years)) # wyszukiwanie apartamentów, dla których ilość recenzji jest między 4 a 5 decylem oraz dla których jest więcej recenzji w latach nieparzystych

# ilość apartamentów spełniających warunki zadania
apartments_count_06 <- nrow(df_06)

# różnica ilości recenzji między ich liczbą w latach nieparzystych a parzystych dla wszystkich apartamentów
rev_odd_even_diff_06 <- sum(df_06$rev_odd_years - df_06$rev_even_years)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- data.frame(apartments_count = apartments_count_06, 
                          rev_odd_even_diff = rev_odd_even_diff_06)


# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie

# tabelka cen i typu pokoju dla poszczególnych apartamentów
df_07 <- listings %>% 
  select(room_type, price) %>% 
  filter(!is.na(price))
  
wykres_07 <- df_07 %>% 
  ggplot(aes(x = room_type, y = price, color = room_type, fill = room_type))+
  geom_violin(alpha = 0.3)+
  geom_boxplot(width = 0.01, outlier.size = 0.5, color = '#453643')+ # chciałam, aby widoczne były odstające wartości
  labs(title = 'Wykres rozkładu cen w zależności od typu pokoju', 
       subtitle = "Seattle",  
       x = 'Typ pokoju', 
       y= 'Cena')+
  scale_color_manual(values = c('#8DAA91', '#788475','#5E5D5C','#453643'))+
  scale_fill_manual(values = c('#8DAA91', '#788475','#5E5D5C','#453643'))+ 
  theme(legend.position = 'none')+ # uznałam, że legenda kolorów jest zbędna w tym przypadku
  scale_y_log10() # większa czytelność rozkładu cen

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_07


# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

# tableka pierwszych liter imion hostów i ich częstości występowania
df_08 <- listings %>% 
  mutate(first_letter = substring(host_name,1,1)) %>% 
  group_by(first_letter) %>% 
  summarise(count = n()) %>% 
  filter(first_letter != '2')

wykres_08 <- df_08 %>% 
  ggplot(aes(x=first_letter, y = count))+
  geom_col(fill='#8DAA91', alpha=0.6)+
  labs(title = "Wykres rozkładu pierwszych liter imion hostów", 
       x = "Pierwsza litera imienia hosta", 
       y = 'Częstość występowania')

## Odpowiedz przypisana do zmiennej## Odpowiefirst_letterdz przypisana do zmiennej
ANS_TASK_08 <- df_08


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

# tabelka szerokości i długości geograficznej dla poszczególnych apartamentów
df_09 <- listings %>% 
  filter(is.na(price)) %>% 
  select(longitude, latitude)

wykres_09 <- df_09 %>% 
  ggplot(aes(x=longitude, y=latitude))+
  geom_point(color='#8DAA91')+
  labs(title = 'Wykres rozkładu apartamentów, które nie mają podanej ceny', 
       subtitle = "Seattle", 
       x = "Długość geograficzna", 
       y = "Szerokość geograficzna")
# Rozkład apartamentów nie jest równomierny, zagęszczenie w centralnej części Seattle i przerzedzenie w południowo-centralnej części Seattle


## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_09



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
saveRDS(solutions, file = "OnyszczukNina.rds")


