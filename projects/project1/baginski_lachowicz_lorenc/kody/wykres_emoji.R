library(ggplot2)
library(dplyr)
library(patchwork)
library(emojifont)

load.emojifont()

#Dane pochodzą z baz danych GUS i ZUS
#ale nie były wczytywane w R, ponieważ były potrzebne tylko dwie wartości

#Ostateczny wygląd wykresu został osiągnięty poprzez jego edycję poza R

rows <- 2
cols <- 5
symbol <- emojifont::emoji('heavy_dollar_sign')
colors <- c("A" = "#FFD700", "B" = "#A9A9A9")

#Pierwsza część wykresu

data_top <- data.frame(group = c("A", "B"), value = c(6, 4))

#Budowanie struktury wykresu - 6 wartości A i 4 B
data_plot_top <- data.frame(group = rep(data_top$group, data_top$value))

#W których kolumnach i wierszach będą wartości
data_plot_top$col <- c(1,2,3,4,5,1,2,3,4,5)
data_plot_top$row <- c(2,2,2,2,2,1,1,1,1,1)

wykres_gorny <- ggplot(data_plot_top, aes(x = col, y = row, color = group)) +
  geom_text(label = symbol, family = "EmojiOne", size = 12) +
  scale_color_manual(values = colors) +
  coord_equal() +
  scale_x_continuous(limits = c(0.5, cols + 0.5)) +
  scale_y_continuous(limits = c(0.5, rows + 0.5)) +
  labs(title = "", subtitle = "2022") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0, face = "italic", size = 12, margin = margin(t = -5, b = 1)),
    plot.margin = unit(c(0, 0, 0, 0), "cm"))

#Druga część wyrkesu

data_bottom <- data.frame(group = c("A", "B"), value = c(10, 0))

#Budowanie 'siatki' wykresu - 10 wartości A
data_plot_bottom <- data.frame(
  group = rep(data_bottom$group, data_bottom$value))

data_plot_bottom$col <- c(1,2,3,4,5,1,2,3,4,5)
data_plot_bottom$row <- c(2,2,2,2,2,1,1,1,1,1)

wykres_dolny <- ggplot(data_plot_bottom, aes(x = col, y = row, color = group)) +
  geom_text(label = symbol, family = "EmojiOne", size = 12) +
  scale_color_manual(values = colors) +
  coord_equal() +
  scale_x_continuous(limits = c(0.5, cols + 0.5)) +
  scale_y_continuous(limits = c(0.5, rows)) +
  labs(title = paste0("Wynagrodzenie osób z wyższym wykształceniem", "\n", "vs pensja minimalna na tle roku 2006"),
       subtitle = "2006") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0, face = "italic", size = 12, margin = margin(t = -5, b = 1)),
    plot.margin = unit(c(0, 0, 0, 0), "cm"))

wykres_dolny / wykres_gorny

