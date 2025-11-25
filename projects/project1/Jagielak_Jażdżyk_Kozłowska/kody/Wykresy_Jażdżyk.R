# już na czysto projekt i wykresik
# dane z : https://bdl.stat.gov.pl/bdl/dane/podgrup/temat -> 
# pozyskiwanie drewna (grubizny) oraz sadzenie drzew i krzewów w latach 2016- 2024

library(scales) 
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# wycinka grubizny:
sama_grubizna_ogolem<-read.csv("grubizna_ogolem.csv",sep=";")

sama_grubizna_ogolem<-sama_grubizna_ogolem %>% 
  pivot_longer(cols = starts_with("grubizna") ,names_to = "Zmienne", values_to = "Wyciete") %>% 
  mutate(Zmienne = str_replace(Zmienne, "\\.\\.m3\\.$", ""),
         Rok = str_extract(Zmienne, "\\d{4}") %>% as.integer()) %>% 
  select(-c(X,"Kod","Zmienne"))

# zasadzone drzewa i krzewy :
sadzenie_drzew_krzewow<-read.csv("sadzenie_drzew_krzewow.csv",sep=";")

sadzenie_drzew_krzewow<-sadzenie_drzew_krzewow %>% 
  pivot_longer(cols = starts_with("sadzenie") ,names_to = "Zmienne", values_to = "Zasadzone") %>% 
  mutate(Rok = str_extract(Zmienne, "\\d{4}") %>% as.integer()) %>% 
  select(-c(X,"Zmienne","Kod"))


sadzenie_drzew_krzewow<-sadzenie_drzew_krzewow %>% 
  group_by(Nazwa,Rok) %>% 
  summarise(Zasadzone = sum(Zasadzone)) %>% 
  ungroup()

sadzenie_drzew_krzewow %>% 
  select(Nazwa,Zasadzone, Rok )

# Wspólnie:
wspolnie<-sadzenie_drzew_krzewow %>% 
  right_join(sama_grubizna_ogolem, by = c("Rok","Nazwa") )

# bez województw:

bez_wojewodztw<-wspolnie %>%
  group_by(Rok) %>% 
  summarise(Wyciete = sum(Wyciete),Zasadzone = sum(Zasadzone)) %>% 
  ungroup()

bez_wojewodztw_long <- bez_wojewodztw %>%
  pivot_longer(cols = c(Zasadzone, Wyciete),
               names_to = "Typ",
               values_to = "Ilosc")

## KOŃCOWY WYKRES, dwie oddzielne kolumny dla różnych jednostek

############################

 


dobry_plot_facet <- ggplot(bez_wojewodztw_long,
                           aes(x = as.factor(Rok),
                               y = Ilosc,
                               fill = Typ)) +
  geom_col(width = 0.7) +
  facet_grid(~Typ, 
             scales = "free",
             space = "free_x",
             axes = "all",
             labeller = as_labeller(c(
               "Zasadzone" = "Zasadzone drzewa i krzewy w sztukach",
               "Wyciete" = "Wycięta grubizna w m³"
             ))) +
  scale_fill_manual(values = c("Zasadzone" = "darkgreen",
                               "Wyciete" = "darkred")) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +  
  labs(x = "Rok",
       y = "Ilość",
       fill = "Rodzaj",
       title = "Porównanie ilości zasadzonych drzew i krzewów z wycinką grubizny",
       subtitle = "Na przestrzeni lat") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "none",
    plot.subtitle = element_text(size = 11),
    axis.title.y = element_text(margin = margin(r = 15)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray60", linewidth = 0.3),
    panel.grid.minor.x = element_line(color = "gray60", linewidth = 0.3),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(margin = margin(r = 15, t = 10)),
    strip.text = element_text(margin = margin(t = 15,b = 10),size = 11, hjust = 0.08),
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA)
  ) +
  coord_flip()



dobry_plot_facet

ggsave(
  filename = "wycinka_sadzonki_dobry.png",
  plot = dobry_plot_facet,
  width = 8, height = 6, units = "in",
  dpi = 300,
  bg = "transparent"
)
###########################

