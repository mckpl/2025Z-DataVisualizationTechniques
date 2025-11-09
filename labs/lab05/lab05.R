###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 5            ###
###########################################

library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

# Pakiet maps -------------------------------------------------------------

# Pakiet maps zawiera wiele zarysów kontynentów, krajów, państw i hrabstw.
# Na przykład: usa, nz, state, world

usa <- map_data("usa")

dim(usa)
head(usa)

w1 <- map_data("world")

# Wysokiej rozdzielczości mapa świata.
w2hr <- map_data("world2Hires")
dim(w2hr)

## Struktura ramki danych zawierającej dane o mapie:
## long - długość geograficzna, na zachód od południka zerowego wartości są ujemne
## lat - szerokość geograficzna
## order - wyznacza w jakiej kolejności ggplot powinien "połączyć punkty"
## region i subregion - mówią, jaki region lub subregion otacza zbiór punktów
## group - Funkcje ggplot2 mogą przyjąć argument group, który kontroluje (między innymi) czy sąsiadujące punkty powinny być połączone liniami. 
# Jeśli są w tej samej grupie, to zostaną połączone, ale jeśli są w różnych grupach, to nie.
# Zasadniczo, posiadanie punktów w różnych grupach oznacza, że ggplot "podnosi pióro" kiedy przechodzi między nimi.


# Prosta mapa -------------------------------------------------------------

# Gdy zmienimy rozmiar okna to wykres jest rozszerzany lub zwężany - tak nie powinno się dziać 

us <- ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group))
us

world <- ggplot() + 
  geom_polygon(data = w1, aes(x = long, y = lat, group = group))
world

## coord_fixed()
# Naprawia relację między jedną jednostką w kierunku y i jedną jednostką w kierunku x.
# Nawet jeśli zmienisz zewnętrzne wymiary wykresu (np. zmieniając rozmiar okna lub rozmiar pliku pdf, do którego ją zapisujesz (na przykład w ggsave)), proporcje pozostają niezmienione.
# Przy ustaleniu parametów 1.3 daje dobre rezultaty, bliżej biegunów mogą być potrzebne inne wartości.
us + coord_fixed()
us + coord_fixed(1.3)

## coord_map()
# Zastąpiona przez nową funkcję, ale starsza jest prostsza w obsłudze. 
?coord_map 
?coord_sf 
us + coord_map()
us + coord_map("mercator")

us + coord_map("mollweide")

# stożkowe
us + coord_map("albers", lat0 = 25, lat1=50)
us + coord_map("lambert", lat0 = 25, lat1=50)
us + coord_map("conic", lat0 = 40)

us + coord_map("azequidistant")

# nie działa zbyt dobrze :(
world + coord_map("mollweide")
range(w1$long) # błąd spowodowany długościami geograficznymi spoza spodziewanego zakresu (Alaska, Rosja)

w1_prim <- map_data("world") %>% filter(long <= 180)
ggplot() + 
  geom_polygon(data = w1_prim, aes(x = long, y = lat, group = group)) +
  coord_map("mollweide")



# Obrys mapy

ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "red") + 
  coord_map()

# Obrys i wypełnienie

us_fill <- ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = "violet", color = "blue") + 
  coord_map()
us_fill


# Dodanie puntków do mapy 

points <- data.frame(
  long = c(-122.064873, -122.306417),
  lat = c(36.951968, 47.644855),
  names = c("A", "B"),
  stringsAsFactors = FALSE
) 

us_fill + 
  geom_point(data = points, aes(x = long, y = lat), color = "black", size = 5) +
  geom_point(data = points, aes(x = long, y = lat), color = "yellow", size = 4)


# Podpisy punktów

us_fill + 
  geom_point(data = points, aes(x = long, y = lat), color = "black", size = 5) +
  geom_text(data = points, aes(x = long, y = lat, label = names), color = "yellow")


# Co gdy nie dodamy parametu group?

ggplot() + 
  geom_polygon(data = usa, aes(x = long, y = lat), fill = "violet", color = "blue") + 
  geom_point(data = points, aes(x = long, y = lat), color = "black", size = 5) +
  geom_point(data = points, aes(x = long, y = lat), color = "yellow", size = 4) +
  coord_map()


## USA - podział na stany

states <- map_data("state")

dim(states)

ggplot(data = states) +
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") +
  coord_map("albers", 25, 50) +
  theme(legend.position = "None")




# Zadanie 1 ---------------------------------------------------------------

# Przyjrzyjmy się bliżej Kalifornii, Oregon i Waszyngtonowi. Przygotuj mapę zawierającą te stany.
# Wypełnij całość jednym kolorem.
#? Wybierz c("california", "oregon", "washington")  



# Przyjrzyjmy się teraz bliżej Kalifornii

ca_df <- subset(states, region == "california")

head(ca_df)

counties <- map_data("county")
ca_county <- subset(counties, region == "california")

ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_map("albers", 25, 50) +
  geom_polygon(color = "black", fill = "gray")

ca_base + theme_minimal()

ca_base + theme_minimal() + 
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)


# Dodatkowe źródło danych
# Ramka danych na podstawie infomracji z Wikipedii
pop_and_area <- read.csv("pop_and_area.csv")

ca <- ca_county %>% left_join(pop_and_area, by = c("subregion" = "Country"))

ca$people_per_sq_mile <- ca$Population/ca$Area.sq.mi



# Zadanie 2 ---------------------------------------------------------------

# Przygotuj wykres na którym będzie gęstość zaludnienia.
# Zapewnij czytelność przygotowanej mapy.




# Przycinamy obszar mapy
ca_density + xlim(-123, -121.0) + ylim(36, 38)
ca_density + coord_map(xlim = c(-123, -121.0),  ylim = c(36, 38))


# Zadanie 3 ---------------------------------------------------------------

# Narysuj mapę świata i zaznacz na niej wskaźnik utworzony poprzez wylosowanie zmiennej 
# z rozdkładu normalnego o średniej 0 i wariancji 5. Użyj odpowiedniej skali kolorystycznej.



# Mapa Polski -------------------------------------------------------------

# Pierwszym elementem przygotowania mapy Polski jest pobranie danych opisujących granice państwa, 
# granice województw, granice powiatów lub gmin. Pliki te dostępne są pod adresem 
# https://www.geoportal.gov.pl/pl/dane/panstwowy-rejestr-granic-prg/. 
# Korzystając z www.geoportal.gov.pl można wybrać tylko potrzebny podzbiór danych, 
# natomiast ze strony Państwowy Rejestr Granic (PRG) na końcu strony są 
# gotowe do pobrania pliki spakowane w .zip.

# Pobrane dane należy rozpakować. Folder zawiera granice dla państwa/województw/powiatów .. w różnych rozszerzeniach. 
# Interesującym plikiem do rysowania granic w pakiecie ggplot2 jest tak zwany plik shapefile (.shp).

install.packages("sf")
library(sf)

granice_powiatow <-  st_read("PRG_jednostki_administracyjne_2024/A02_Granice_powiatow.shp", quiet = TRUE)

head(granice_powiatow)

# obrys i podział na powiaty
ggplot(data = granice_powiatow) + 
  geom_sf() 

ggplot(data = granice_powiatow) + 
  geom_sf(color = "pink", fill = "blue") 


# theme void - czyści tło pod wykresem
ggplot(data = granice_powiatow) + 
  geom_sf(color = "black") +
  theme_void()

wojewodztwa <- st_read("PRG_jednostki_administracyjne_2024/A01_Granice_wojewodztw.shp", quiet = TRUE)

# Zadanie 4 ---------------------------------------------------------------
# Na mapie Polski z województwami umieść informacje o zdawalności egzaminu maturalnego.

# Dane pobrane z https://bdl.stat.gov.pl/BDL/start, kategoria szkolnictwo (K20), grupa zdawalność egzaminów (G500)
dane <- read.csv("SZKO_3193_CTAB_20251028182008.csv", sep = ";", dec = ",")

