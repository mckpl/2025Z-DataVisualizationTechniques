library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(sf)

# wczytanie danych:
df <- read.csv("https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/sdg_07_40/1.0/*.*.*.*?c[freq]=A&c[nrg_bal]=REN&c[unit]=PC&c[geo]=EU27_2020,EA20,BE,BG,CZ,DK,DE,EE,IE,EL,ES,FR,HR,IT,CY,LV,LT,LU,HU,MT,NL,AT,PL,PT,RO,SI,SK,FI,SE,IS,NO,BA,ME,MD,MK,GE,AL,RS,XK,CH&c[TIME_PERIOD]=2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023&compress=false&format=csvdata&formatVersion=1.0&lang=en&labels=label_only")
df <- df[c("geo", "TIME_PERIOD", "OBS_VALUE")] %>% 
  filter(geo != "Georgia")

# Obliczenie zmiany udziału odnawialnych źródeł energii
difference <- df %>% group_by(geo) %>% 
  summarise("diff" = last(OBS_VALUE) - first(OBS_VALUE))

names(difference) <-c("NAME_ENGL", "diff")

# Wczytanie danych mapy oraz połączenie z danymi
europe <- st_read("Europe/CNTR_RG_20M_2024_3035.shp", quiet = TRUE) 
europe <- left_join(europe, difference)


# Mapa
ggplot(data = europe, aes(fill = diff)) + 
  geom_sf(data = europe %>% filter(NAME_ENGL == "Australia"), 
         aes(color = "Brak danych"), # Dodaję Australię do mapy, bo jest daleko i nie ma danych
         fill = "#7f7f7f",           # Dzięki temu mam na legendzie brak danych
         size = 0.2,
         name = NULL)+
  geom_sf(color = "black") +
  coord_sf(
    crs = st_crs(3035),             # Dodaję faktyczną mapę
    xlim = c(2300000, 6800000),
    ylim = c(1300000, 5400000), 
    expand = FALSE)+                # Paleta kolorystyczna uwzględnia osoby nierozróżniające kolorów
  scale_fill_gradientn(colours = rev(c("#001d29","#005f73","#0a9396","#94d2bd","#e9d8a6","#ee9b00","#ca6702","#ae2012","#9b2226")),
                       name = "Min: Macedonia Północna - 4.5%\nMax: Dania - 29.6%\n\nZmiana [pkt %]", 
                       guide = guide_colorbar(order = 1,
                                              frame.colour = "black", 
                                              frame.linewidth = 0.5, 
                                              ticks.colour = "black", 
                                              ticks.linewidth = 0.5))+
  scale_color_manual(
    name = NULL,
    values = "black",
    guide = guide_legend(
      order = 2, 
      override.aes = list(fill = "#7f7f7f", color = "black"))) +
  theme_void()+
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
        plot.margin = margin(10, 10, 10, 10))+
  labs(title = "Zmiana udziału odnawialnych źródeł energii w miksie energetycznym w latach 2004-2023",
       subtitle = "Dane: Eurostat")
 