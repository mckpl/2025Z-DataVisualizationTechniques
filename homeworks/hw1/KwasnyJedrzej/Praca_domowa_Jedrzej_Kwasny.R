###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################

# Ładowanie pakietów-----------------------------------------------------
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
library(dplyr)
library(tidyr)
library(ggplot2)

# Ładowanie danych --------------------------------------------------------

reviews <- read.csv("reviews.csv")
listings <- read.csv("listings.csv")

View(reviews)
View(listings)

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


x<-count(reviews, reviewer_id)
nrow(x[x["n"]>20,])


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- nrow(x[x["n"]>20,])

# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
reviews2 <- reviews %>%
  mutate(date = as.Date(date)) %>%
  mutate(miesiac = format(date, "%m")) %>%
  group_by(miesiac)
  mutate(miesiac = format( date, "%m"))


c(01,02,03,04,05,06,07,08,09,10,11,12)[max(table(x$miesiac))==table(x$miesiac)]

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- c(01,02,03,04,05,06,07,08,09,10,11,12)[max(table(x$miesiac))==table(x$miesiac)]

# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).

## Rozwiązanie


max(listings[listings["number_of_reviews"]>=100,]["price"], na.rm=TRUE)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- max(listings[listings["number_of_reviews"]>=100,]["price"], na.rm=TRUE)

# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
stare <- reviews2 %>%
  filter(date < Sys.Date() - 10*365)
nrow(unique(stare["listing_id"]))  #ilość 10 lat temu
min(reviews2$date) # najstarszy






## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- c(nrow(unique(stare["listing_id"])),16)

# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie


x<-listings[listings["latitude"]>=mean(listings$latitude),] %>%
  count(room_type)
y<-listings[listings["latitude"]<mean(listings$latitude),] %>%
  count(room_type)

z <- merge(y, x, by = "room_type", all = TRUE, suffixes = c("y", "x"))
z[is.na(z)] <- 0
z$roznica <- z$nx - z$ny
z[c("room_type","roznica")]


## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- z[c("room_type","roznica")]

# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
decyle <- quantile(listings$number_of_reviews, probs = seq(0,1,0.1), na.rm = TRUE)


interesujace <- listings %>%
  filter(!is.na(number_of_reviews) & number_of_reviews >= as.numeric(decyle["40%"]) & number_of_reviews <= as.numeric(decyle["50%"]))


x <- reviews[reviews$listing_id %in% interesujace$id, ] %>%
  mutate(
    rok = as.integer(format(as.Date(date), "%Y")),
    parzysty = if_else(rok %% 2 == 0, 1, -1)
  )  %>%
  group_by(listing_id) %>%
  summarise(parzysty_odjac_nieparzysty = sum(parzysty)) 
sum(x$parzysty_odjac_nieparzysty < 0)





## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- x[x$parzysty_odjac_nieparzysty > 0,]

# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?

## Rozwiązanie
listings2 <- listings %>%
  filter(price <= quantile(listings$price, 0.99, na.rm = TRUE))

ggplot(listings2, aes(x = room_type, y = price)) +
  geom_boxplot() +
  labs(
    title = "Rozkład cen apartamentów w zależności od typu pokoju",
    x = "Typ pokoju",
    y = "Cena za noc"
  ) 


listings %>% 
  select(room_type, price)

# Private room oraz Entire home/apt stanowią największą grupę wśród drogich ogłoszeń
# Ceny Hotel room oraz Shared room mają stosnukowo małe odchylenia w stosunku do reszty rodzajów ogłoszeń
## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- listings %>%
  select(price, room_type)

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie

listings2 <- listings %>%
  mutate(pierwsza =substr(host_name, 1, 1)) %>%
  count(pierwsza)%>%
  filter(grepl('^[A-Za-z]$', pierwsza))
View(listings2)

ggplot(listings2, aes(x = pierwsza, y = n)) +
  geom_col() +
  labs(
    title = "Rozkład pierwszych liter hostów",
    x = "Litera",
    y = "Ilość"
  ) 

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- listings2


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie

brak_ceny <- listings %>%
  filter(is.na(price))


ggplot() +
  geom_point(data = listings, aes(x = longitude, y = latitude),
             color = "green", size = 1, alpha = 0.5) +
  geom_point(data = brak_ceny, aes(x = longitude, y = latitude),
             color = "red", size = 2, alpha = 0.7) +
  labs(
    title = "Rozkład apartamentów w Seattle bez podanej ceny (położenie)",
    x = "Długość geograficzna",
    y = "Szerokość geograficzna"
  )

#Ogłoszenia bez ceny są rozmieszczone równomiernie względem wszystkich ogłoszeń
## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- brak_ceny[c("latitude", "longitude")]



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
saveRDS(solutions, file = "KwasnyJedrzej.rds")

