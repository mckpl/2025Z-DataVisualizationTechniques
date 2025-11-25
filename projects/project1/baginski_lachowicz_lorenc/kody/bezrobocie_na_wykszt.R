library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)

edu <- read.csv("bezrobocie_wg_wyksztalcenia_gus_PL.csv", sep = ";")
edu_clean <- edu %>% select(-Kod, -Nazwa)
edu_clean <- lapply(edu_clean, function(x) as.numeric(str_replace(x, ",", ".")))
edu_clean <- data.frame(edu_clean)

edu_long <- edu_clean %>%
  pivot_longer(
    cols = everything(),
    names_to = "Wyksztalcenie_Rok",
    values_to = "Stopa_bezrobocia") %>%
  mutate(
    Wyksztalcenie = str_extract(Wyksztalcenie_Rok, "^[^.]+"), #wtedy mamy tylko interesującą nas 'nazwę wykształcenia'
    Rok = as.numeric(str_extract(Wyksztalcenie_Rok, "\\d{4}"))) %>%
  filter(!is.na(Rok)) %>%
  select(Rok, Wyksztalcenie, Stopa_bezrobocia)

colors <- c(
  "wyższe" = "#FFF001",
  "średnie" = "#ff0000",
  "zasadnicze" = "#008000"  ,
  "gimnazjalne" = "#ffffff",
  "policealne" = "#ffa756")

ggplot(edu_long, aes(x = Rok, y = Stopa_bezrobocia, color = Wyksztalcenie)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) + #dodajemy znak % do liczb na osi Y
  labs(
    title = "Stopa bezrobocia w Polsce wg poziomu wykształcenia",
    x = "Rok",
    y = "Stopa bezrobocia (%)",
    color = "Wykształcenie",
    caption = "Źródło: GUS (Bank Danych Lokalnych)") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(colour = "black", hjust = 0.5, face = "bold"), #na plakacie white
    #plot.caption = element_text(colour = "white"), #zakomentowane, żeby było widać w konsoli R
    #axis.title = element_text(colour = "white"),
    #axis.text = element_text(colour = "white"),
    #legend.title = element_text(colour = "white"),
    #legend.text = element_text(colour = "white")
  )
