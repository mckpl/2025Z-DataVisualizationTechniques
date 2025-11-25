# let's roll
# kod do wykresu zespołu 18

library(dplyr)
library(ggplot2)
library(tidyr)
library(showtext)

yearly <- read.csv("yearly.csv")
yearly <- yearly %>%
  filter(year != 2025)

font_add_google("Anonymous Pro", "anonymous")
showtext_auto()
theme_set(theme_minimal( base_size = 20, base_family = "anonymous"))

tmp <- yearly %>% 
  mutate(
    wszystkie = as.numeric(gsub(" ", "", total)) / 1000,
    ulgowe = as.numeric(gsub(" ", "", ulgowe)) / 1000,
    normalne = as.numeric(gsub(" ", "", normalne)) / 1000,
    inne = wszystkie - (ulgowe + normalne)
  ) %>% filter(year != 2025) %>% 
  pivot_longer(cols = c(wszystkie, ulgowe, normalne, inne),
               names_to = "typ",
               values_to = "sprzedaz")

# wykres
p <- ggplot(tmp, aes(x = factor(year), y = sprzedaz, color = typ, group = typ)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Sprzedaż biletów w latach",
    x = "Rok",
    y = "Sprzedaż (tys.)",
    color = "Typ biletu"
  ) +
  theme_minimal(base_size = 25, base_family = "anonymous") +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

p
