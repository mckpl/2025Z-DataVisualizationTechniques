# Tomasz Stolarczyk, 333090

# ------------------------- 0. PAKIETY, DANE CZCIONKI --------------------

library(tidyverse) # wczytuje nam na raz dplyr, tidyr, ggplot i inne
library(maps) # do mapki
library(readxl) # bo mamy dane .xlsx
library(ggrepel)
library(showtext)

font_add_google("Montserrat", "font_title") 
font_add_google("Lato", "font_body")        
showtext_auto()

# Pobierzemy dane ze strony Eurostat - Life expectancy at birth by sex
# link : https://ec.europa.eu/eurostat/databrowser/view/tps00205/default/table

raw_kobiety <- read_excel("LifeExpectancyEUFemales.xlsx")
raw_mezczyzni <- read_excel("LifeExpectancyEUMales.xlsx")

# ------------------------- 1. WYCZYSCMY DANE ----------------------------------

czyscimy_dane <- function(dane, plec_nazwa) {
  dane %>%
    rename(country = 1) %>%
    pivot_longer(cols = -country,names_to = "year", values_to = "srednia_dlugosc_zycia") %>%
    mutate(srednia_dlugosc_zycia = as.numeric(as.character(srednia_dlugosc_zycia))) %>% # dla pewnosci ze to liczba
    mutate(gender = plec_nazwa) %>%
    filter(!is.na(srednia_dlugosc_zycia))
}

kobiety <- czyscimy_dane(raw_kobiety, "Females")
mezczyzni <- czyscimy_dane(raw_mezczyzni, "Males")

# laczymy to w jedna tabele
srednia_dlugosc_zycia_europa_temp <- bind_rows(kobiety, mezczyzni) 


# ------------------------- 2. NAPRAWMY NAZWY KRAJÓW ----------------------------------

poprawne_nazwy_mapy <- unique(map_data("world")$region)
nasze_nazwy <- unique(srednia_dlugosc_zycia_europa_temp$country)
niepasujace <- setdiff(nasze_nazwy, poprawne_nazwy_mapy)
# print(niepasujace)
# w konsoli: "Czechia", "United Kingdom", "Türkiye" 
# naprawmy to
srednia_dlugosc_zycia_europa <- srednia_dlugosc_zycia_europa_temp %>%
  mutate(country = case_when(
    grepl("Czechia", country) ~ "Czech Republic",
    grepl("Türkiye", country) ~ "Turkey",
    grepl("United Kingdom", country) ~ "UK",
    TRUE ~ country
  ))

# ------------------------- 3. OBLICZMY WSKAŹNIK ZMIANY ----------------------------------

dane_do_mapy <- srednia_dlugosc_zycia_europa %>%
  group_by(country, gender) %>%
  summarise(
    rok_start = min(year),
    rok_koniec = max(year),
    wartosc_start = srednia_dlugosc_zycia[year == min(year)],
    wartosc_koniec = srednia_dlugosc_zycia[year == max(year)],
    zmiana = round(wartosc_koniec - wartosc_start, 2),
    # jesli mamy dane z tylko jednego roku to potraktujemy to jako 
    #  brak danych (jest tak w przypadku Kosowa) inaczej wyszloby ze zmiana to 0
    zmiana = if_else(rok_start == rok_koniec, NA_real_, round(wartosc_koniec - wartosc_start, 2)),
    .groups = "drop"
  )

# ------------------- 4. PRZYGOTUJMY MAPE EUROPY (SAMA GEOMETRIA POKI CO) ------------------

mapa_swiata <- map_data("world")
mapa_europa <- mapa_swiata %>%
  filter(long > -25 & long < 45 & lat > 34 & lat < 72) %>%
  rename(country = region)

mapa_z_danymi <- dane_do_mapy %>%
  # right join nam zapewnia ze narysujemy tez kraje bez danych
  right_join(mapa_europa, by = "country") %>% 
  filter(!is.na(lat)) 

# --------------- 5. PRZYGOTUJMY ETYKIETKI DO GGREPEL (MIN / MAX) ---------

# pod lokalizacje etykietek (ggrepl)
srodki_krajow <- mapa_europa %>%
  group_by(country) %>%
  summarise(long = mean(long), lat = mean(lat))

etykiety_min_max <- dane_do_mapy %>%
  filter(!is.na(zmiana)) %>% 
  group_by(gender) %>%
  filter(zmiana == min(zmiana) | zmiana == max(zmiana)) %>%
  left_join(srodki_krajow, by = "country")

# ------------------------- 6. RYSUJEMY WYKRES ---------------

opis_stopka <- paste0(
  "Earliest and latest available data years were used for each country within the selected period. \n",
  "Countries coloured in gray lack sufficient data for comparison. \n",
  "Source: Eurostat \n",
  "Note: Ukraine data available only until 2020. Current figures would differ significantly due to the impact of the war."
)

ggplot() +
  # baza
  geom_polygon(data = mapa_europa, aes(x = long, y = lat, group = group), 
               fill = "#f0f0f0", color = "white", linewidth = 0.15) +
  
  # dane
  geom_polygon(data = mapa_z_danymi %>% filter(!is.na(zmiana)), 
               aes(x = long, y = lat, group = group, fill = zmiana), 
               color = "white", linewidth = 0.15) +
  
  # panele - 2 mapki osobna dla kazdej plci
  facet_wrap(~gender) +
  
  # kolory
  scale_fill_gradient2(
    low = "#d73027",    
    mid = "#ffffbf",    
    high = "#1a9850",   
    midpoint = 0,
    name = ""            
  ) +
  
  # etykietki do ggrepel
  geom_text_repel(
    data = etykiety_min_max,
    aes(x = long, y = lat, label = paste0(country, " (", ifelse(zmiana > 0, "+", ""), zmiana, " y)")),
    family = "font_body",
    size = 3.5,          
    fontface = "bold",   
    color = "#2c3e50",
    bg.color = "white",  
    bg.r = 0.15,         
    box.padding = 0.5,      
    point.padding = 0.1,    
    min.segment.length = 0, 
    max.overlaps = Inf,     
    force = 2               
  ) +
  
  # opisy
  labs(
    title = "How European Longevity is Shifting",
    subtitle = "Change in life expectancy measured in years and divided by gender (2013-2024)",
    caption = opis_stopka
  ) +
  
  # style
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "#ffffff", color = NA),
    
    # tytul
    plot.title = element_text(family = "font_title", face = "bold", size = 26, 
                              color = "#2c3e50", hjust = 0.5, margin = margin(t=20, b = 10)),
    
    # podtytul
    plot.subtitle = element_text(family = "font_body", size = 14, face = "italic",
                                 color = "#7f8c8d", hjust = 0.5, margin = margin(b = 10)),
    
    # stopka
    plot.caption = element_text(family = "font_body", size = 9, color = "#7f8c8d", 
                                hjust = 0, margin = margin(t = 20, b = 10, r = 20, l = 20)), 
    
    # legenda
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm"),   
    legend.key.height = unit(0.3, "cm"),  
    legend.text = element_text(family = "font_body", size = 10, color = "#34495e"),
    
    strip.text = element_text(family = "font_title", size = 20, face = "bold", 
                              color = "#34495e", margin = margin(b = 15)),
    strip.background = element_blank()
  ) +
  
  # zoom
  coord_fixed(xlim = c(-25, 45), ylim = c(34, 71), ratio = 1.3)

