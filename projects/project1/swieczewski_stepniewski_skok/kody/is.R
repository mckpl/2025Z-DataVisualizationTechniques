library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)

dane <- read_csv("estat_hlth_ehis_pe9u.csv")

first_col <- names(dane)[1]
year_cols <- names(dane)[-1]
dane_long <- dane %>%
  separate(
    {{ first_col }},
    into = c("freq","physact","deg_urb","sex","age","unit","geo"),
    sep = ","
  ) %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "rok",
    values_to = "wartosc"
  ) %>%
  mutate(
    rok = str_trim(rok),
    wartosc = na_if(wartosc, ":"),
    wartosc = as.numeric(wartosc)
  )

pl <- dane_long %>%
  filter(geo == "PL")

##################################
age_levels <- c("Y18-24", "Y25-34", "Y35-44",
                "Y45-54", "Y55-64", "Y65-74", "Y_GE75")

pl_pir <- pl %>%
  filter(
    physact == "MV_AERO",
    deg_urb == "TOTAL",
    unit == "PC",
    age %in% age_levels,
    sex %in% c("M", "F"),
    rok == "2019",
    !is.na(wartosc)
  ) %>%
  mutate(
    age = factor(age, levels = age_levels),
    age_label = dplyr::recode(
      age,
      "Y18-24" = "18 - 24",
      "Y25-34" = "25 - 34",
      "Y35-44" = "35 - 44",
      "Y45-54" = "45 - 54",
      "Y55-64" = "55 - 64",
      "Y65-74" = "65 - 74",
      "Y_GE75" = "75+"
    ),
    wartosc_plot = if_else(sex == "M", -wartosc, wartosc)
  )

# zakres osi X zaokrąglony do pełnych 10
max_val <- max(abs(pl_pir$wartosc_plot), na.rm = TRUE)
limit <- ceiling(max_val / 10) * 10




ggplot(pl_pir, aes(x = wartosc_plot, y = age, fill = sex)) +
  geom_col(width = 0.8) +
             geom_vline(xintercept = 0, color = "grey40") +
             geom_text(
               aes(x = 0, label = age_label),
               color = "white",
               size = 3.5,
               vjust = 0.5
             ) +
             scale_x_continuous(
               labels = abs,
               breaks = seq(-limit, limit, by = 10),
               limits = c(-limit, limit)
             ) +
             
             scale_y_discrete(labels = NULL) +
             labs(
               x = "Odsetek osób ćwiczących (%)",
               y = NULL,
               fill = "Płeć",
               title = "Piramida wieku osób ćwiczących"
             ) +
             scale_fill_manual(values = c("M" = "steelblue", "F" = "deeppink")) +
             theme_minimal() +
             theme(
               plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
               axis.text.y = element_text(size = 12)
             )








