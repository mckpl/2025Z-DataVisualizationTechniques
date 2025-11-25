# Ładowanie pakietów-------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# Ładowanie danych --------------------------------------------------------

przyczyny_pozarow <- read.csv("przyczyny_pozarow.csv", sep = ";")

# Porządkowanie danych ----------------------------------------------------
  
pp <- przyczyny_pozarow %>% 
  select(-Kod) %>% 
  pivot_longer(!Nazwa, names_to = c('Przyczyna','Rok'), names_pattern = 'pożary.lasów.(\\D*).(\\d{4})..szt..', values_to = "Liczba", values_drop_na = TRUE) %>%
  mutate(Rok = as.integer(Rok), Liczba = as.integer(Liczba)) %>% 
  filter(Nazwa == 'POLSKA') %>% 
  select(!Nazwa) %>% 
  pivot_wider(names_from = Przyczyna, values_from = Liczba) %>% 
  mutate(nieostrożność.ludzi = nieostrożność.nieletnich + nieostrożność.dorosłych, 
         wady.i.nieprawidłowa.eksploatacja.urządzeń.oraz.środków.transportu = wady.urządzeń.technicznych.i.ich.nieprawidłowa.eksploatacja + wady.środków.transportu.i.ich.nieprawidłowa.eksploatacja) %>% 
  select(!c(nieostrożność.nieletnich, 
            nieostrożność.dorosłych,
            wady.urządzeń.technicznych.i.ich.nieprawidłowa.eksploatacja,
            wady.środków.transportu.i.ich.nieprawidłowa.eksploatacja)) %>% 
  pivot_longer(!Rok, names_to = 'Przyczyna', values_to = 'Liczba') %>% 
  mutate(Przyczyna = stringr::str_to_sentence(gsub('\\.', ' ', Przyczyna)))


# Wykres ------------------------------------------------------------------

library(extrafont)
font_import()
loadfonts(device="win")
fonts()

p <- pp %>% 
  filter(Przyczyna != 'Ogółem') %>%
  ggplot(aes(x = Rok, y = Liczba, fill = forcats::fct_reorder(Przyczyna, Liczba)))+
  geom_col()+
  labs(y = 'Liczba pożarów',
       fill = 'Przyczyny pożarów')+
  scale_fill_manual(values = c('#6f819b', '#c8ccb9','#f1e6c8', '#caae9d', '#a27671', '#7a3e45'))+
  scale_x_continuous(breaks = seq(2009,2024, by = 1) ,guide = guide_axis(angle = 45))+
  theme_minimal()+
  theme(axis.text = element_text(size = 15, family = "Times New Roman"),
        axis.title = element_text(size=15, family = "Times New Roman"),
        legend.title = element_text(family = "Times New Roman", face = "bold", size = 15, margin = margin(b = 5)),
        legend.text = element_text(family = "Times New Roman", size = 15), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())

p

# Zapisanie jako pdf ------------------------------------------------------

ggsave("przyczyny_pozarow_ost.pdf", plot = p, width = 16.1, height = 11.2, dpi = 300, device = cairo_pdf, bg = "transparent", units = 'cm')




