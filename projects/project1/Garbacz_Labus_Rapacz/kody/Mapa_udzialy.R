library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(forcats)
library(cowplot)
library(showtext)
library(sf)

font_add_google("League Spartan", family = "spartan")
showtext_auto()


as_tibble(read.csv("resources/Liczba_zatrzymań_pociągów_pasażerskich_w_2021_r.csv",
         sep = ";",
         header = TRUE,
         fileEncoding = "Windows-1250",
         stringsAsFactors = FALSE)) -> zatrzymania

names(zatrzymania) <- filter(zatrzymania,zatrzymania$X == "Stacja")
zatrzymania <- filter(zatrzymania,zatrzymania$Stacja != "Stacja")



przewoznicy <- as_tibble(read.csv("resources/dane_eksploatacyjne_pasazerskie_udzialy.csv",
                         header = TRUE,
                         stringsAsFactors = FALSE))
names(przewoznicy) <- c("rok", "miesiac", "przewoznik", "udzial")
przewoznicy <- przewoznicy %>% slice(5:n())
przewoznicy$udzial <- as.numeric(substring(przewoznicy$udzial, 1, nchar(przewoznicy$udzial)-1))



przewoznicy %>% filter(przewoznik %in% c("Polregio", "Koleje Mazowieckie - KM",
                                         "PKP Szybka Kolej Miejska w Trójmieście",
                                         "PKP Intercity", "Szybka Kolej Miejska - Warszawa",
                                         "Koleje Śląskie", "Koleje Dolnośląskie",
                                         "Warszawska Kolej Dojazdowa")) -> przewoznicy

przewoznicy %>% 
  filter(miesiac == "styczeń-luty/ January-February" | miesiac == " styczeń-luty/ January-February") %>% 
  ggplot(aes(x = rok, y = udzial, color = przewoznik, group = przewoznik))+
  geom_line(linewidth = 1)+
  theme_light(base_family = "spartan", base_size = 16)+
  labs(title = "Udział przewoźników w rynku w latach 2012-2024",
       subtitle = "pod względem liczby pasażerów",
       x = "Rok", y = "Udział w rynku (%)")+
  scale_color_manual(values = c("Polregio" = "#D52B1E",  
                               "Koleje Mazowieckie - KM" = "#78BE20", 
                               "PKP Szybka Kolej Miejska w Trójmieście" = "#0072CE",
                               "PKP Intercity" = "#F39200",
                               "Szybka Kolej Miejska - Warszawa" = "#FFD700",
                               "Koleje Śląskie" = "#00AEEF",
                               "Koleje Dolnośląskie" = "#7C7C7C",
                               "Warszawska Kolej Dojazdowa" = "#002B5C"))+
  scale_x_discrete(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme(legend.position = "none", plot.margin = margin(10,15,10,10), plot.title = element_text(face ="bold"))


s <- read.csv("resources/c2.csv", sep = ",")
names(s) <- c("Stacja","woj")
s$woj <- tolower(s$woj)

left_join(s, zatrzymania) %>% filter(woj != "")-> zatrzymania_new
names(zatrzymania_new)[16] <- "suma"
zatrzymania_new %>% filter(Przewoźnik != "") -> zatrzymania_new

zatrzymania_new %>% filter(woj %in% c(
  "dolnośląskie",
  "kujawsko-pomorskie",
  "lubelskie",
  "lubuskie",
  "łódzkie",
  "małopolskie",
  "mazowieckie",
  "opolskie",
  "podkarpackie",
  "podlaskie",
  "pomorskie",
  "śląskie",
  "świętokrzyskie",
  "warmińsko-mazurskie",
  "wielkopolskie",
  "zachodniopomorskie"
)) -> zatrzymania_new

zatrzymania_new %>% group_by(Przewoźnik, woj) %>% 
  summarise(sum(suma)) -> new
new %>% filter(Przewoźnik != "") -> new

new %>% group_by(woj) %>%
  slice_max(`sum(suma)`, n = 1) -> df


wojewodztwa <- st_read("resources/PRG_jednostki_administracyjne_2024/A01_Granice_wojewodztw.shp", quiet = TRUE)
names(df) <- c("przew", "JPT_NAZWA_", "count")
df$JPT_NAZWA_ <- tolower(df$JPT_NAZWA_)
left_join(wojewodztwa, df) -> wojewodztwa

ggplot(data = wojewodztwa, aes(fill = przew)) + # Mapa była modyfikowana później w programie graficznym
  geom_sf(color = "black") +
  theme_void(base_family = "spartan", base_size = 16)+
  scale_fill_manual(values = c("POLREGIO Spółka Akcyjna" = "#ff6363",  
                               "Koleje Mazowieckie - KM Spółka z o.o." = "#63ff63", 
                               "PKP Szybka Kolej Miejska w Trójmieście" = "#0072CE",
                               "PKP Intercity" = "#F39200",
                               "Szybka Kolej Miejska - Warszawa" = "#FFD700",
                               "Koleje Śląskie Spółka z o.o." = "#63d3ff",
                               "Koleje Dolnośląskie S.A." = "#ababab",
                               "Warszawska Kolej Dojazdowa" = "#002B5C",
                               "Koleje Małopolskie Sp. z o.o." = "#fc30ab",
                               "Łódzka Kolej Aglomeracyjna Sp. z o.o." = "#effa55",
                               "Koleje Wielk
                               opolskie Sp. z o.o." = "#faaa55"))+
  labs(title = "Najwięksi przewoźnicy kolejowi w województwach",
       subtitle = "pod względem liczby zatrzymań w 2021 roku")+
  theme(legend.position = "none", plot.title = element_text(face ="bold"))

  

