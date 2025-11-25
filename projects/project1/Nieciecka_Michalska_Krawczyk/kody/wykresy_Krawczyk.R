library(dplyr)
library(ggplot2)
library(plotly)

df_raw <- read.csv("dane.csv", sep=';')

df_raw <- df_raw %>%
  mutate(across(2:18, ~ {
    x <- gsub(" ", "", .)
    as.numeric(x)
  }))

df_raw <- df_raw %>%
  rowwise() %>%
  mutate(mean_val = mean(c_across(2:18), na.rm = TRUE)) %>%
  ungroup()

df <- data.frame(Tydzien = df_raw[1], mean_val = df_raw[19])

start_year <- 2019

df$week_num <- as.numeric(gsub("[^0-9]", "", df$Tydzien))

df$cycle <- ave(df$week_num, df$week_num, FUN = seq_along)

df$rok <- start_year + (df$cycle - 1)

df$Tydzien_new <- sprintf("%d-%02d", df$rok, df$week_num)

df$week_num <- NULL
df$cycle <- NULL
df$rok <- NULL

n <- nrow(df)

idx <- seq_len(n)

tick_positions <- idx[seq(1, n, by = 13)]

tick_labels <- df$Tydzien_new[tick_positions]

ciemny_niebieski <- "#348382"
jasny_niebieski  <- "#7CCBC9"
ciemny_roz       <- "#8C4066"
jasny_roz        <- "#C581A3"
kolor_osi        <- "#2B3044"
kolor_grid       <- "#B8C0CC"

df$index <- seq_len(nrow(df))

n <- nrow(df)
tick_positions <- seq(1, n, by = 13)
tick_labels <- df$Tydzien_new[tick_positions]

tick_labels <- c("2019-01", "2019-04", "2019-07", "2019-10", "2020-01", "2020-04", "2020-07", "2020-10", "2021-01", "2021-04", "2021-07", "2021-10")

df$mean_val <- df$mean_val/2.15

p <- plot_ly() %>%
  
  add_lines(
    data = df,
    x = ~index,
    y = ~mean_val,
    line = list(color = ciemny_niebieski, width = 4),
    name = "Tygodniowa liczba zgonów",
    opacity = 0.8
  ) %>%
  
  add_markers(
    data = df,
    x = ~index,
    y = ~mean_val,
    marker = list(color = jasny_niebieski, size = 6),
    showlegend = FALSE,
    opacity = 0.8
  ) %>%
  
  add_lines(
    data = df,
    x = ~index,
    y = ~fitted(loess(mean_val ~ seq_along(mean_val))),
    line = list(color = ciemny_roz, width = 4),
    name = "Krzywa wygładzenia wykresu"
  ) %>%
  
  layout(
    xaxis = list(
      title = "Data",
      tickvals = tick_positions,
      ticktext = tick_labels,
      tickangle = -45,
      showline = TRUE,
      linewidth = 3,
      linecolor = kolor_osi,
      gridcolor = kolor_grid,
      zeroline = FALSE
    ),
    yaxis = list(
      title = "Zgony tygodniowo na milion mieszkańców",
      showline = TRUE,
      linewidth = 3,
      linecolor = kolor_osi,
      gridcolor = kolor_grid,
      zeroline = FALSE
    ),
    plot_bgcolor = "rgba(244,235,232,1)",
    paper_bgcolor = "rgba(244,235,232,1)"
  )

p <- p %>% layout(
  shapes = list(
    list(
      type = "line",
      x0 = 63, x1 = 63,
      y0 = 400/2.15, y1 = 950/2.15,
      line = list(color = jasny_roz, width = 4),
      layer = "above"
    ),
    list(
      type = "line",
      x0 = 105, x1 = 105,
      y0 = 400/2.15, y1 = 950/2.15,
      line = list(color = jasny_roz, width = 4),
      layer = "above"
    )
  ),
  
  annotations = list(
    list(
      x = 56,
      y = (950 + 10)/2.15,   # trochę ponad końcem kreski
      text = "Ogłoszenie epidemii",
      showarrow = FALSE,
      font = list(color = kolor_osi, size = 16),
      bgcolor = "rgba(244, 235, 232, 0.8)",
      bordercolor = jasny_roz,
      borderwidth = 1,
      xanchor = "center",
      yanchor = "bottom"
    ),
    list(
      x = 112,
      y = (950 + 10)/2.15,  # trochę ponad końcem kreski
      text = "Początek roku 2021",
      showarrow = FALSE,
      font = list(color = kolor_osi, size = 16),
      bgcolor = "rgba(244, 235, 232, 0.8)",
      bordercolor = jasny_roz,
      borderwidth = 1,
      xanchor = "center",
      yanchor = "bottom"
    )
  )
)