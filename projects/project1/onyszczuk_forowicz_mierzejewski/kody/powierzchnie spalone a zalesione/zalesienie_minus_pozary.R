library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

pozary <- read_excel("/home/madzia/Dokumenty/twd/powierzchnia_pozarow.xlsx", sheet = 2)
zalesianie <- read_excel("/home/madzia/Dokumenty/twd/zalesienie_lasow.xlsx", sheet = 2)

# przygotowanie ramki o obszarach zalesionych
zalesianie_PL <- zalesianie[2, c(11:ncol(zalesianie))] %>%
  pivot_longer(
    cols = `2009`:`2024`,  
    names_to = "rok",
    values_to = "powierzchnia_zalesiona"
  ) %>%
  mutate(rok = as.numeric(rok))
zalesianie_PL$powierzchnia_zalesiona <- as.numeric(zalesianie_PL$powierzchnia_zalesiona)
zalesianie_PL$powierzchnia_zalesiona <- zalesianie_PL$powierzchnia_zalesiona / 100

# przygotowanie ramki o obszarach spalonych
pozary_PL <- pozary %>%
  pivot_longer(
    cols = `2009`:`2024`,  # wybiera kolumny od 2009 do 2021
    names_to = "rok",
    values_to = "powierzchnia_spalona"
  ) %>%
  select(rok, powierzchnia_spalona) %>%  # zostawia tylko te dwie kolumny
  mutate(rok = as.numeric(rok)) 
pozary_PL$powierzchnia_spalona <- pozary_PL$powierzchnia_spalona / 100 # zeby bylo w km2

# połączenie dwoch ramek i kolumna z roznicą
dane <- left_join(pozary_PL, zalesianie_PL, by = "rok")
dane <- dane %>%  mutate(roznica = powierzchnia_zalesiona - powierzchnia_spalona)
View(dane)

# wykres

dane$kat <- ifelse(dane$roznica < 0, "ujemne", "dodatnie")

s <- ggplot(dane, aes(x = rok, y = roznica, fill = kat)) +
  geom_col() +
  scale_fill_manual(values = c("ujemne" = "#9f656b","dodatnie" = "#909B6F")) +
  scale_x_continuous(breaks = dane$rok, guide = guide_axis(angle = 45)) +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    #plot.title = element_text(family = "Times New Roman", face = "bold", size = 15, color = "black", hjust = 0.5),
    axis.text.x = element_text(family = "Times New Roman", size = 15),
    axis.text.y = element_text(family = "Times New Roman", size = 15),
    axis.title.y = element_text(family = "Times New Roman", size = 15),
    axis.title.x = element_text(family = "Times New Roman", size = 15),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line()) +
  labs(
    #title = expression("Różnica gruntów zalesionych i spalonych na przestrzeni lat ["*km^2*"]"),
    y = expression("Różnica"),
    x = expression("Rok"))
s

ggsave("wykres_zalesianie_minus_pozary_slupki.pdf",plot = s,
       unit = "cm",
       width = 16,
       height = 10,
       device = cairo_pdf
       ,bg = "white") 






