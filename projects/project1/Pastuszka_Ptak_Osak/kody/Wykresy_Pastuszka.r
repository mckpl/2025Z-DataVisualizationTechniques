library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

transakcje_karta <- read_excel("dane/transakcje_liczba.xlsx", 
                               sheet = "KWARTALNIE (od 1999)",
                               n_max = 1)
transakcje_karta <- transakcje_karta[, -c(1:3)]

transakcje_karta <- transakcje_karta %>% 
  select("2015 Q2":last_col())

transakcje_karta <- transakcje_karta %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Kwartal",
    values_to = "liczba_karta"
  )
transakcje_blik <- read_excel("dane/BLIK.xlsx",
                              sheet = "KWARTALNIE ",
                              skip = 8,
                              n_max = 1,
                              col_names = FALSE
)
colnames(transakcje_blik) <- names(read_excel("dane/BLIK.xlsx",
                                              sheet = "KWARTALNIE ",
                                              n_max = 0))
transakcje_blik <- transakcje_blik[, -c(1:2)]

transakcje_blik <- transakcje_blik %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Kwartal",
    values_to = "liczba_blik"
  ) %>% 
  #usuwanie wielokrotnych spacji
  mutate(Kwartal = gsub("\\s+", " ", Kwartal))

transakcje <- full_join(transakcje_karta, transakcje_blik, by="Kwartal") %>% 
  rename(
    BLIK = liczba_blik,
    Karta = liczba_karta
  )
transakcje <- transakcje %>% 
  pivot_longer(
    cols = c(Karta, BLIK),
    names_to = "Typ",
    values_to = "Liczba"
  ) %>% 
  mutate(
    Liczba_mln = Liczba / 10e6 
  )
wykres <- ggplot(transakcje, aes(x = Kwartal, y = Liczba_mln,
                                 color = Typ, group = Typ)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(
    labels = scales::label_number(suffix = " mln", accuracy = 1),
    expand = expansion(mult = c(0, 0.05)),
    limits = c(0, NA)
  ) +
  scale_x_discrete(
    breaks = grep("Q1", unique(transakcje$Kwartal), value = TRUE),
    labels = function(x) substr(x, 1, 4)
  ) +
  scale_color_manual(
    values = c("BLIK" = "#E55812", "Karta" = "#950952"),
    labels = c("BLIK", "Karta")
  ) +
  labs(
    title = "Liczba transakcji kartą vs BLIK (kwartalnie)",
    subtitle = "Jak zmieniają się preferencje w korzystaniu z płatności mobilnych?",
    x = "Data",
    y = "Liczba transakcji",
    color = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(

    text = element_text(color = "white"),
    axis.text = element_text(color = "white", size = 12),

    axis.title = element_text(size = 14, color = "white"),
    axis.title.x = element_text(margin = margin(t = 15)), # Odstęp od dołu
    axis.title.y = element_text(margin = margin(r = 15)), # Odstęp z prawej

    plot.title = element_text(face = "bold", size = 20, hjust = 0),
    plot.subtitle = element_text(size = 14, hjust = 0, margin = margin(b = 15)),

    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),

    panel.grid.minor.y = element_blank(),

    axis.line = element_line(color = "white", linewidth = 0.5),
    axis.ticks = element_line(color = "white", linewidth = 0.5),
    
    axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 10)),
    axis.text.y = element_text(margin = margin(r = 10)),

    legend.position = "top",
    legend.justification = "left",
    legend.direction = "horizontal",
    legend.key.size = unit(1.5, "lines"),
    legend.margin = margin(t = 0, b = 10),

    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )


ggsave("wykres_przezroczysty.png", plot = wykres, width = 12, height = 7, bg = "transparent")
