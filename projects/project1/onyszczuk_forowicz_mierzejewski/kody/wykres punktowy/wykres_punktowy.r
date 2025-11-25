# Ładowanie pakietów-----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(extrafont)
library(ggrepel)

# Ładowanie danych ------------------------------------------------------

przyczyny <- read.csv('przyczyny_pozarow.csv', sep = ';')
opady <- read.csv('opady_pl.csv')

# Pogodowa ramka danych -------------------------------------------------

Rok <- c(2009:2024)
Temp_powietrza <- c(8.5, 7.5, 8.9, 8.5, 8.5, 9.6, 9.7, 9.2, 9.0, 9.8, 10.2, 9.9, 8.7, 9.5, 10.0, 10.9)

Temp_powietrza <- data.frame(Rok = Rok, Temp_rok = Temp_powietrza)

pogoda <- Temp_powietrza %>% left_join(opady, by= c('Rok' = 'rok'))

# Format przyczyn ------------------------------------------------------

liczba_pozarow <- przyczyny %>% 
  filter(Nazwa == 'POLSKA') %>% 
  select(starts_with('pożary.lasów.ogółem.')) %>% 
  pivot_longer(everything(), names_to = 'Rok', names_pattern = 'pożary.lasów.ogółem.(\\d{4})..szt..', values_to = 'Liczba_pozarow') %>% 
  mutate(Rok = as.integer(Rok))

# Gotowa ramka ---------------------------------------------------------
               
pogoda_liczba <- pogoda %>% left_join(liczba_pozarow, by='Rok')

pogoda_liczba$RokiPozary <- paste(pogoda_liczba$Rok, pogoda_liczba$Liczba_pozarow, sep = ",")

breaks = c(4000, 6000, 8000, 10000, 12000)

loadfonts(device="win")

p <- pogoda_liczba %>% 
  ggplot(aes(x = srednie_opady_mm, y = Temp_rok, fill = Liczba_pozarow))+
  geom_point(size = 4, shape = 21, color = "#520519", aes(fill = Liczba_pozarow))+
  labs(x = "Średnie opady [mm]", y = "Średnia temperatura [C]")+
  ggrepel::geom_text_repel(aes(label = Rok), vjust = -0.6, size = 5,
                           family = "Times New Roman",
                           min.segment.length = 0,
                           nudge_y = 0.1,
                           nudge_x = 7,
                           segment.color = "#520519",
                           box.padding = 0.5)+
  scale_fill_steps(
    breaks = breaks,
    n.breaks = length(breaks),
    low = "#f1e6c8",
    high = "#520519",
    name = "Liczba Pożarów",
    guide = guide_coloursteps(
      direction = "horizontal",
      barwidth = 30,
      barheight = 1,
      title.position = "top"
    )) +
  theme_minimal()+
  theme(
    legend.position = "top",
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    axis.text = element_text(size = 15, family = "Times New Roman"),
    axis.title.x = element_text(size=15, family = "Times New Roman"),
    axis.title.y = element_text(size=15, family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman", face = "bold", size = 15, margin = margin(b = 5), hjust = 0.5),
    legend.text = element_text(family = "Times New Roman", size = 15)
)


p
ggsave("wykres_pozarow_pogada.pdf",units = "cm", plot = p, width = 17.6, height = 14.4, dpi = 300, device = cairo_pdf, bg = "transparent")



      