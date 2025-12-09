library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# link do posta: https://x.com/brier_gallihugh/status/1762117592548868510?s=46&t=3TqLse1Qw8ul08ltfPtKLA
# link do zestawu danych: https://github.com/rfordatascience/tidytuesday/blob/main/data/2024/2024-02-13/historical_spending.csv

# Błędy w oryginalnej wizualizacji: 
# - brak informacji o jednostce osi OY - brak określonej waluty,
# - zły dobór kolorów (zastosowanie czerwonej czcionki na czerwonym tle) znacząco utrudnia odczytanie prezentowanych danych.

# Przygotowany wykres został uzupełniony o informację o walucie. Poprawiono estetykę i kontrast kolorów.
# Dodatkowo linię przedstawiającą zmianę wydatków względem roku 2010 zastąpiono etykietami pokazującymi różnicę rok do roku,
# co znacznie lepiej pokazuje dynamikę zmian kosztów.


dane <- read.csv("historical_spending.csv")

dane <- dane %>%
  arrange(Year) %>%
  mutate(
    Diff = PerPerson - lag(PerPerson),
    DiffLabel = case_when(
      is.na(Diff) ~ "start",
      Diff > 0 ~ paste0("+", dollar(Diff, accuracy = 0.01)),
      TRUE ~ dollar(Diff, accuracy = 0.01)
    ),
    TotalLabel = dollar(PerPerson, accuracy = 1),
    ChangeType = case_when(
      is.na(Diff) ~ "Neutral",
      Diff > 0 ~ "Increase",
      TRUE ~ "Decrease"
    )
  )

colors <- c("Increase" = "#2BA84A", "Decrease" = "#D64541", "Neutral" = "grey50")

ggplot(dane, aes(x = Year, y = PerPerson)) +
  geom_line(color = "grey80", linewidth = 1) +
  geom_point(aes(color = ChangeType), size = 5) +
  geom_point(color = "white", size = 2) +
  geom_label(
    aes(label = TotalLabel),
    vjust = -0.8,
    fill = 'transparent',
    label.size = 0,
    fontface = "bold",
    size = 4.5,
    color = "grey20"
  ) +
  geom_text(
    aes(label = DiffLabel, color = ChangeType),
    vjust = 1.8,
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(min(dane$Year), max(dane$Year), 1)) +
  scale_y_continuous(
    labels = label_dollar(accuracy = 1),
    expand = expansion(mult = c(0.1, 0.15)) 
  ) +
  
  labs(
    title = "Średnie wydatki walentynkowe na osobę",
    subtitle = "Etykety nad punktami określają wartości wydatków w danym roku,\nnatomiast etykiety pod punktami oznaczają zmianę w stosunku do roku poprzedniego.",
    x =  "Rok",
    y = "Wydatki na osobę (USD)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, color = "grey40", margin = margin(b = 20)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey92"),
    axis.text.x = element_text(color = "grey30", size = 11),
    axis.text.y = element_text(color = "grey60"),
    legend.position = "none"
  )