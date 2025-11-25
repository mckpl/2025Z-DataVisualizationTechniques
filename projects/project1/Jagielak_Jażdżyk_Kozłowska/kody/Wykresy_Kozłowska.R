library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork)

#Wykres 1 - Własność lasów (waffle chart)

#Struktury własności i gatunkowe
lasy_statusy_wlasnosci <- read.csv2("Studia/semestr 3/TechnikiWizualizacjiDanych/Projekt1/Lasy_wlasnosc.csv")
gatunek_powierzchnia <- read.csv2("Studia/semestr 3/TechnikiWizualizacjiDanych/Projekt1/sklad_powierzchnia.csv")
gatunek_zasobnosc <- read.csv2("Studia/semestr 3/TechnikiWizualizacjiDanych/Projekt1/sklad_zasobnosc.csv")

#Wykresy:

#Statusy wlasnosci na przestrzeni lat:

lasy_stacked <- lasy_statusy_wlasnosci %>%
  select(Rok, 
         Lasy_panstwowe = w_zarządzie_Lasów_Państwowych,
         Parki_narodowe = parki_narodowe,
         Gminy = własność_gmin,
         Zasob_rolny = w_Zasobie_Własności_Rolnej_Skarbu_Państwa,
         Lasy_prywatne = Lasy_prywatne_w_tys_ha) %>%
  pivot_longer(cols = -Rok, names_to = "Kategoria", values_to = "Powierzchnia")

lasy_stacked$Kategoria <- factor(lasy_stacked$Kategoria, 
                                 levels = c("Lasy_panstwowe", "Parki_narodowe", "Gminy", 
                                            "Zasob_rolny", "Lasy_prywatne"))


# Kolory

kolory <- c(
  "Lasy_panstwowe" = "#003200",
  "Parki_narodowe" = "#d5531a", 
  "Gminy" = "hotpink",
  "Własność Rolna" = "blue",
  "Lasy_prywatne" = "darkred"
)

last_year_data <- lasy_stacked %>% 
  filter(Rok == 2023) %>%
  group_by(Kategoria) %>%
  summarise(Powierzchnia = sum(Powierzchnia))

#Dane do tabeli + zaokrąglanie

last_year_data <- last_year_data %>%
  mutate(liczba_drzew = round(Powierzchnia / sum(Powierzchnia) * 100))

suma <- sum(last_year_data$liczba_drzew)

if(suma != 100) {
  last_year_data$liczba_drzew[which.max(last_year_data$liczba_drzew)] <- 
    last_year_data$liczba_drzew[which.max(last_year_data$liczba_drzew)] + (100 - suma)
}

grid_trees <- expand.grid(row = 1:10, col = 1:10) %>%
  mutate(kategoria = rep(last_year_data$Kategoria, times = last_year_data$liczba_drzew))

# Wykres

mapa <- ggplot(grid_trees, aes(x = col, y = row, color = kategoria)) +
  geom_text(label = "🌳", size = 9) +
  scale_color_manual(
    values = kolory,
    labels = c("Lasy Państwowe", "Parki narodowe", "Gminy", "Lasy prywatne"),
    name = "Kategoria:"
  ) + 
  labs(
    title = "Struktura własności lasów w Polsce (2023)",
    subtitle = "Każde drzewo = 1% powierzchni lasów",
    x = "", y = "",
    caption = "Źródło: Rocznik Statystyczny Leśnictwa 2024"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    legend.background = element_rect(fill = "transparent")
  ) 

mapa

# Wykres 2 - Skład gatunkowy lasów

#Wczytywanie danych

gatunek_powierzchnia <- read.csv2("Studia/semestr 3/TechnikiWizualizacjiDanych/Projekt1/sklad_powierzchnia.csv")

drzewa_gatunki <- gatunek_powierzchnia %>%
  filter(Kategoria == "Skład gatunkowy") %>%
  filter(!Specyfikacja %in% c("Ogółem", "Drzewa iglaste", "Drzewa liściaste"))

drzewa_ilosci <- gatunek_powierzchnia %>%
  filter(Kategoria == "Skład gatunkowy") %>%
  filter(Specyfikacja %in% c("Drzewa iglaste", "Drzewa liściaste"))

kolory_gatunki <- c(
  "Sosna" = "darkred",
  "Świerk" = "darkred", 
  "Jodła" = "darkred",
  "Dąb" = "darkgreen",
  "Brzoza" = "darkgreen",
  "Buk" = "darkgreen",
  "Olsza" = "darkgreen",
  "Grab" = "darkgreen",
  "Osika" = "darkgreen",
  "Topola" = "darkgreen"    
)

#Pierwszy z wykresów

sklad <- ggplot(drzewa_gatunki, aes(x = reorder(Specyfikacja, -Udział_procentowy), y = Udział_procentowy, fill = Specyfikacja)) +
  geom_col() +
  geom_text(aes(label = paste0(Udział_procentowy, "%")), 
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = kolory_gatunki, name = "Gatunek drzewa") +
  labs(
    title = "Skład gatunkowy drzew w polskich lasach",
    subtitle = "Udział procentowy głównych gatunków drzew",
    x = "Gatunek drzewa",
    y = "Udział procentowy (%)",
    caption = "Źródło: Rocznik Statystyczny Leśnictwa 2024",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 14),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 13),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylim(0, max(drzewa_gatunki$Udział_procentowy) * 1.15)

#Mały wykres kołowy

wykres_kolowy <- ggplot(drzewa_ilosci, aes(x = "", y = Powierzchnia_tys_ha, fill = Specyfikacja)) +
  geom_col() +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Udział_procentowy, "%"), color = Specyfikacja),
            position = position_stack(vjust = 0.55), 
            size = 3, fontface = "bold", show.legend = FALSE) +
  scale_fill_manual(values = c("Drzewa iglaste" = "darkred", "Drzewa liściaste" = "darkgreen")) +
  scale_color_manual(values = c("Drzewa iglaste" = "white", "Drzewa liściaste" = "white")) +
  labs(
    title = "Udział drzew iglastych\n i liściastych",
    fill = NULL
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

#Połączenie wykresów

połączony_wykres <- sklad + 
  inset_element(wykres_kolowy, 
                left = 0.50,   
                bottom = 0.50, 
                right = 1.00,  
                top = 1.00) &  
  theme(plot.background = element_rect(fill = "transparent", color = NA))

połączony_wykres
