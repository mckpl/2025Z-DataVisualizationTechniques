###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           PRACA DOMOWA 1            ###
###########################################
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")}
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
tmp <-  reviews %>% 
          select(reviewer_id) %>% 
          table() %>% 
          data.frame() %>% 
          filter(Freq>20) %>% 
          dim() %>% 
          .[1]


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- tmp
cat(ANS_TASK_01, " recenzentów wystawiło więcej niż 20 opinii")
# Zadanie 2 (0.5) ---------------------------------------------------------
# W jakich miesiącach jest wystawiane najwięcej recenzji?

## Rozwiązanie
tmp <- substr(select(reviews,date)$date,6,7) %>% 
      table() %>%
      data.frame() %>% 
      arrange(by=-Freq) %>% 
      top_n(3)
tmp <- month.name[tmp[[1]]]


#arrange## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- tmp
cat("Najwięcej rezenzji jest wystawiane odpowiednio w miesiącach :",ANS_TASK_02)
# Zadanie 3 (0.5) ---------------------------------------------------------
# Jaka jest największa cena za apartament przy złożeniu, że musi
# mieć co najmniej 100 recenzji (bazujemy na tabeli reviews).
## Rozwiązanie
tmp <- select(reviews,listing_id) %>% 
  table() %>%
  data.frame() %>%
  rename(id=listing_id) %>% 
  filter(Freq>=100) %>%
  .[1] %>% 
  merge(.,select(listings,id,price)) %>% 
  arrange(price) %>% 
  top_n(1) %>% 
  .[[2]]

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- tmp
cat("Największa cena za apartament przy założeniu, że musi
mieć co najmniej 100 recenzji wynosi :",ANS_TASK_03)
# Zadanie 4 (0.5) ---------------------------------------------------------
# Bazując na wystawionych recenzjach oblicz ile jest apartamentów, które są na
# rynku dłużej niż 10 lat. Jaka jest maksymalna liczba lat apartametów na rynku?

## Rozwiązanie
tmp <- select(reviews,listing_id,date) %>% 
  transmute(listing_id,as.Date(.$date)) %>%
  rename(date=names(.)[2]) %>% 
  group_by(listing_id) %>% 
  summarise(Najstarsza_recenzja = min(date)) %>% 
  arrange(Najstarsza_recenzja) 

tmp <- mutate(tmp,Jak_długo=(Sys.Date()-tmp$Najstarsza_recenzja))
tmp <- c(sum(tmp$Jak_długo>=(365*10+2)),floor(tmp$Jak_długo[1]/365.25))

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- tmp
cat("Apartamentów które są dłużej na rynku niż 10 lat jest :",ANS_TASK_04[1])
cat("Maksymalna liczba lat apartamentu licząć do dzisiaj wynosi :",ANS_TASK_04[2])
# Zadanie 5 (0.5) ---------------------------------------------------------
# O ile więcej jest apartamentów na południu niż na północy w podziale na typ pokoju? 
# (środek określamy jako średnia z szerokości geograficznej)

## Rozwiązanie
tmp <- select(listings,room_type,latitude) %>% 
  mutate(mean(.$latitude)) %>%
  transmute(room_type,lat=.$latitude-.$mean) %>% 
  transmute(room_type,lat=ifelse(.$lat<0,1,-1)) %>% 
  group_by(.$room_type) %>% 
  summarise(Ile_wiecej_na_poludnie=sum(lat)) 
## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- tmp
cat("Tabela do zad5 :")
print(ANS_TASK_05)
# Zadanie 6 (0.5) ---------------------------------------------------------
# Rozważmy recencje apartamentów, interesują nas tylko apartamenty, które mają liczbę recenzji między
# 4 a 5 decylem rozkładu recenzji. Ile apartamentów miało więcej recenzji w latach nieparzystych
# niż parzystych i jaka to jest liczba recenzji?

## Rozwiązanie
tmp <- select(reviews,listing_id,date) %>%
  group_by(listing_id) %>% 
  count() %>% 
  mutate(mn=quantile(.$n,c(0.4))) %>% 
  mutate(mx=quantile(.$n,c(0.5))) %>% 
  filter(n>mn & n<mx) %>% 
  select((listing_id))

tmp <- merge(tmp,select(reviews,listing_id,date),by="listing_id") %>% 
  mutate(date=as.numeric(substring(.$date,1,4))%%2) %>%
  group_by(listing_id,date) %>%
  count()

tmp <- merge(filter(tmp,date==1),filter(tmp,date==0),by="listing_id")
tmp <- transmute(tmp,listing_id,ile_wiecej=tmp$n.x-tmp$n.y) %>% 
  rename(Ile_więcej_recenzji_w_nieparzystych_latach_niż_parzystych=ile_wiecej)

tmp <- list(sum(ifelse(tmp$Ile_więcej_recenzji_w_nieparzystych_latach_niż_parzystych>0,1,0)),tmp)
## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- tmp
cat(unlist(ANS_TASK_06)[1],"apartamentów miało więcej recenzji w latach nieparzystych
niż parzystych")
cat("bilanse dla poszczególnych apartamentów")
options(max.print = 20)
print(ANS_TASK_06[2])


# Zadanie 7 (0.5 + 0.5) ---------------------------------------------------
# Jak wygląda rozkład cen apartamentów w zależności od typu pokoju?
## Rozwiązanie
wyk1<-ggplot(listings,aes(y=price,x=room_type,fill=room_type))+
  scale_y_log10()+
  theme(legend.position="none")+
  labs(title="Rozkład cen apartamentów w zależności od typu pokoju,",subtitle = "w skali logarytmicznej",x="Rodzaj pokoju",y="Cena")+
  geom_violin()

wyk1
cat("Najtańsze są wynajmowane pokoje oraz pokoje ze współlokatorem.\nWystępują pojedyńcze drogie wille.\nMieszkania i domy są niewiele droższe od pokojów.\nCeny hoteli kończą się na barierze 1000$")
## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- select(listings,price,room_type)

# Zadanie 8 (0.5 + 0.5) ---------------------------------------------------
#
# Dla każdego apartamentu wybierzmy pierwszą literę imienia hosta. Jak wygląda 
# rozkład pierwszych liter hostów?

## Rozwiązanie
tmp<-select(listings,host_name) %>% 
  .[[1]] %>% 
  substring(first=1,last=1) %>% 
  .[grepl("[a-zA-Z]",.[])] %>% 
  data.frame(.)
wyk2<-ggplot(tmp,aes(x=.))+
  geom_bar()+
  labs(title="Rozkład pierwszych liter hostów",y="Liczba wystąpień",x="Pierwsze litery")
wyk2
cat("Rozkład pierwszych liter nazw hosta nie jest równomierny")
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- select(listings,host_name)


# Zadanie 9 (0.5 + 0.5) ---------------------------------------------------
# Czy rozkład aparmatemtów w Seatle, które nie mają podanej ceny w bazie danych
# jest równomierny?

## Rozwiązanie
tmp<-select(listings,price,latitude,longitude) %>% 
  filter(is.na(.$price)) 
tmp2<-select(listings,price,latitude,longitude) %>% 
  filter(!is.na(.$price)) 
wyk3<-ggplot(tmp,aes(x=longitude,y=latitude,fill="Brak ceny"))+
  geom_point(color = "#E63946")+
  labs(title = "Zmapowane lokalizacje apartamentów bez podanej ceny w Seatle",x="Długość geograficzna",y="Szerokość geograficzna")
wyk3<-wyk3+
  geom_point(data = tmp2,aes(x=longitude,y=latitude,fill="Podana cena"),
             color = "#457B9D",
             size=0.2
             )+
  theme(legend.title = element_blank(),legend.text=element_text(size=14))

wyk3
#Więcej apartamentów bez ceny pojawia się tam gdzie jest więcej apartamentów w bazie. Możemy uznać że rozkład apartamentów bez ceny jest równomierny.
## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- list(tmp,tmp2)
#wersja 2 przy użyciu pakietów związanych z mapą
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")}
if (!requireNamespace("maptiles", quietly = TRUE)) {
    install.packages("maptiles")}
if (!requireNamespace("tidyterra", quietly = TRUE)) {
      install.packages("tidyterra")}
library(sf)
library(maptiles)
library(tidyterra)

seattle_bbox_sf <- st_bbox(
  c(xmin = -122.45, ymin = 47.5, xmax = -122.2, ymax = 47.7),
  crs = st_crs(4326)
) |> st_as_sfc() 

seattle_map_tiles <- get_tiles(
  seattle_bbox_sf,
  provider = "Esri.WorldGrayCanvas",
  zoom = 11,
  crop = TRUE 
)

wyk4<-ggplot() +
  geom_spatraster_rgb(data = seattle_map_tiles) +
  geom_point(
    data = select(tmp,latitude,longitude),
    aes(x = longitude, y = latitude,fill="Brak ceny"),
    color = "#E63946",  
    size = 1,         
    alpha = 1,        
    inherit.aes = FALSE
  ) +
  coord_sf(
    crs = 4326,
    xlim = c(-122.45, -122.2),
    ylim = c(47.5, 47.7),
    expand = FALSE 
  ) +
  theme(legend.title = element_blank(),legend.text=element_text(size=14)) +
  labs(
    title = "Mapa punktowa na tle Seatle",
  )
wyk4 <- wyk4 +
  geom_point(
    data = tmp2, 
    aes(x = longitude, y = latitude,fill="Podana cena"),
    color = "#457B9D", 
    size=0.2,
    alpha = 1,
    inherit.aes = FALSE
  )
print(wyk4)
#remove.packages("sf")
#remove.packages("tidyterra")
#remove.packages("maptiles")

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
saveRDS(solutions, file = "JanusiakKamil.rds")


