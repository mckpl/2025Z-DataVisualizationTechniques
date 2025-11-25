library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(svglite)
library(systemfonts)
library(stringr)

rozwody <- read_excel("C:\\PW\\TWD projekt\\rozwody-okrestrwania.xlsx", skip = 8)

colnames(rozwody) <- c("Wiek", "Ogółem", "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-plus")

rozwody <- rozwody %>% 
  filter(!Wiek %in% c("18", "19", "w tym 20 lat  of which 20", "16", "17"))

idx_kobiety <- which(grepl("Kobiety", rozwody$Wiek))

mezczyzni <- rozwody[1:(idx_kobiety - 1), ] %>%
  filter(!is.na(Ogółem), !grepl("Mężczyźni", Wiek)) %>%
  mutate(Płeć = "Mężczyźni")

kobiety <- rozwody[(idx_kobiety + 1):nrow(rozwody), ] %>%
  filter(!is.na(Ogółem), !grepl("Kobiety", Wiek)) %>%
  mutate(Płeć = "Kobiety")

df <- bind_rows(mezczyzni, kobiety)

rozwody_long <- df %>%
  pivot_longer(cols = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-plus"),
               names_to = "Okres_trwania",
               values_to = "Liczba_rozwodów")

rozwody_long$Okres_trwania <- factor(rozwody_long$Okres_trwania,
                                     levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-plus"))

p2 <- ggplot(rozwody_long, aes(y = Okres_trwania, x = Wiek, fill = Liczba_rozwodów)) +
  geom_tile(color = "white") +
  facet_wrap(~ Płeć) +
  scale_fill_gradient(low = "#F3C677", high = "#B33F62") +
  labs(
    title = str_wrap("Liczba rozwodów według wieku małżonków i długości trwania małżeństwa w 2023 roku", width = 55),
    subtitle = str_wrap("Podział na płeć", width = 70),
    y = "Okres trwania małżeństwa (lata)",
    x = "Wiek małżonka w momencie zawarcia małżeństwa",
    fill = "Liczba rozwodów") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "whitesmoke"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "whitesmoke"),
    axis.title = element_text(face = "bold", color = "whitesmoke"),
    axis.text = element_text(color = "whitesmoke"),
    legend.title = element_text(face = "bold", color = "whitesmoke"),
    legend.text = element_text(size = 11, color = "whitesmoke"),
    strip.text = element_text(face = "bold", color = "whitesmoke"),
    strip.background = element_rect(fill = NA, color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    panel.border = element_blank(),
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.box.background = element_rect(fill = NA, color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "rozwody_heatmap.svg",
  plot = p2,
  device = "svg",
  bg = "transparent",
  width = 8,
  height = 5,
  units = "in")

p2


