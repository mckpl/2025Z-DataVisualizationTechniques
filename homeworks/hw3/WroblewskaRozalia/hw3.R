#wczytywanie bibliotek
install.packages("openxlsx")
library(openxlsx)
library(dplyr)
library(tidyr)

install.packages(c("sf", "rnaturalearth", "rnaturalearthdata"))
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)

#ladowanie danych
dane <- read.xlsx("chronicgdaybwtotpop.xlsx")
View(dane)

#ustawianie nagłówków, usuwanie zbędnego wiersza
dane <- dane[-1,]
colnames(dane) <- dane[1, ] 
dane <- dane[-1,]

dane <- as.data.frame(dane)

#przygotowaywanie danych do mapy -> chcemy się skupić na spożywaniu owoców morza wśród maloletnich

dane %>% 
  select(`Pop Class`) %>% 
  unique()

dane %>% 
  select(Country) %>% 
  unique()

dane %>% 
  filter(`Pop Class` %in% c('Adolescents', 'Infants', 'Other children', 'Toddlers')) %>% 
  filter(`Foodex L1` == 'Fish and other seafood (including amphibians, rept') %>% 
  select('Country', 'Nr Subjects', 'Nr Consumers') -> maloletnijedzaryby

View(maloletnijedzaryby)

maloletnijedzaryby[, c('Nr Subjects', 'Nr Consumers')] <- lapply(maloletnijedzaryby[, c('Nr Subjects', 'Nr Consumers')], as.numeric)

maloletnijedzaryby <- maloletnijedzaryby %>%
  group_by(Country) %>%
  summarise(
    NrSubjects = sum(`Nr Subjects`, na.rm = TRUE),
    NrConsumers = sum(`Nr Consumers`, na.rm = TRUE)
  ) %>% 
  mutate(Procent = NrConsumers/NrSubjects * 100)

#rysowanie mapy europy
europa <- ne_countries(scale = "medium", returnclass = "sf")

#lista czlonkow UE na 2016, ponieważ to z tego roku są dane
ue_2016 <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
  "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
  "Slovenia", "Spain", "Sweden", "United Kingdom"
)

ue <- europa[ europa$name_long %in% ue_2016 , ]

ue %>% 
  left_join(maloletnijedzaryby, by = join_by(name_long == Country)) %>% 
  ggplot()+
  geom_sf(aes(fill= Procent), color = 'white') +
  coord_sf(xlim = c(-10, 30), ylim = c(35, 72)) +
  theme_void()+
  scale_fill_gradient(low = "lightblue", high = 'navy', na.value = "darkgray") -> baza

#aby dodać napisy Państw, wyliczamy punkty na środku danego Państwa
pts <- st_point_on_surface(ue)
coords <- st_coordinates(pts)
centroids <- cbind(st_drop_geometry(pts), coords) 
View(centroids)

mini <- maloletnijedzaryby %>% 
  filter(Procent == min(Procent)) %>% 
  select(Country)

maksi <- maloletnijedzaryby %>% 
  filter(Procent == max(Procent)) %>% 
  select(Country)

minmaxcentroids <- centroids %>% 
  filter(name_long %in% c(mini, maksi))

#dodajemy napisy i tytuł
baza +
  geom_text(data = minmaxcentroids, aes(x = X, y = Y, label = name_long), color='darkgray') +
  labs(title = "Procent dzieci spożywających ryby i owoce morza
dla państw członkowskich Unii Europejskiej w roku 2016") +
  theme(plot.title = element_text(hjust = 0.5))

