sport_inne <- read.csv("sport_inne.csv",sep=";")
sport_inne2 <- read.csv("sport_inne2.csv",sep=";")
sport_walki <-read.csv("sport_walki.csv",sep=";")
sport_walki2 <- read.csv("sport_walki2.csv",sep=";")
sport_wodne <- read.csv("sport_wodne.csv",sep=";")
sport_wodne2 <- read.csv("sport_wodne2.csv",sep=";")
sport_zesp <- read.csv("sport_zesp.csv",sep=";")
sport_zesp2 <- read.csv("sport_zesp2.csv",sep=";")
sport_zima <- read.csv("sport_zima.csv",sep=";")
sport_zima2 <- read.csv("sport_zima2.csv",sep=";")

sport_inne <- cbind(sport_inne,sport_inne2)
sport_walki <- cbind(sport_walki,sport_walki2)
sport_wodne <- cbind(sport_wodne,sport_wodne2)
sport_zesp <- cbind(sport_zesp,sport_zesp2)
sport_zima <- cbind(sport_zima,sport_zima2)

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

process_sport_df <- function(df) {
  
  df[, 3:ncol(df)] <- lapply(df[, 3:ncol(df)], as.numeric)
  df <- df[, -(1:2)]
  df <- df[, !duplicated(colnames(df))]
  
  df %>%
    pivot_longer(
      #ekstracja lat // ~nazw 
      cols = everything(),
      names_to = c("sport","rok"),
      names_pattern = "(.*)\\.ogółem\\.(\\d+).*",
      values_to = "osób_ćwiczących"
    ) %>%
    mutate(rok = as.integer(rok)) %>%
    group_by(sport, rok) %>%
    summarise(osób_ćwiczących = min(osób_ćwiczących, na.rm = TRUE), .groups = "drop")
}

sport_inne_long  <- process_sport_df(sport_inne)  %>% filter(!sport %in% c("jeździectwo","kolarstwo.górskie","kolarstwo.szosowe","kolarstwo.torowe","łucznictwo"))
sport_walki_long <- process_sport_df(sport_walki) %>% filter(!sport %in% c("karate.tradycyjne","szermierka"))
sport_wodne_long <- process_sport_df(sport_wodne) %>% filter(sport == "pływanie")
sport_zesp_long  <- process_sport_df(sport_zesp)  %>% filter(sport != "piłka.nożna..łącznie.z.halową.i.plażową.")
sport_zima_long  <- process_sport_df(sport_zima)

sporty_1 <- rbind(sport_inne_long, sport_zesp_long, sport_wodne_long)

even_years <- seq(2000, 2024, 2)

df1 <- sporty_1 %>%
  filter(!is.na(sport)) %>%
  mutate(
    rok = as.integer(rok),
    sport = droplevels(factor(sport))
  )

df1_data <- df1 %>% filter(!is.na(osób_ćwiczących))
df1_na   <- df1 %>% filter(is.na(osób_ćwiczących))

p1 <- ggplot(df1_data, aes(rok, osób_ćwiczących, color = sport)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_point(data = df1_na, aes(rok, 0), shape = 4, size = 3) +
  scale_x_continuous(breaks = even_years, limits = c(1999, 2024), expand = expansion(mult = 0.01)) +
  scale_color_manual(
    values = RColorBrewer::brewer.pal(8, "Dark2"), 
    labels = c(
      "piłka.ręczna" = "Piłka ręczna",
      "pływanie" = "Pływanie",
      "koszykówka" = "Koszykówka",
      "lekkoatletyka" = "Lekkoatletyka",
      "piłka.siatkowa..łącznie.z.plażową." = "Siatkówka",
      "strzelectwo.sportowe" = "Strzelectwo",
      "tenis" = "Tenis",
      "tenis.stołowy" = "Tenis stołowy"
    )
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linetype = 1, shape = 16, size = 2)
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

df2 <- sport_zima_long %>%
  filter(!is.na(sport)) %>%
  mutate(
    rok = as.integer(rok),
    sport = droplevels(factor(sport))
  )

df2_data <- df2 %>% filter(!is.na(osób_ćwiczących))
df2_na   <- df2 %>% filter(is.na(osób_ćwiczących))

p3 <- ggplot(df2_data, aes(rok, osób_ćwiczących, color = sport)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_point(data = df2_na, aes(rok, 0), shape = 4, size = 3) +
  scale_x_continuous(breaks = even_years, limits = c(1999, 2024), expand = expansion(mult = 0.01)) +
  scale_color_manual(
    values = RColorBrewer::brewer.pal(8, "Set1"),
    labels = c(
      "narciarstwo.alpejskie" = "Narciarstwo szybkie",
      "narciarstwo.klasyczne" = "Narciarstwo biegowe"
    )
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linetype = 1, shape = 16, size = 2)
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )


combined_plot <- (p1 | plot_spacer() | p3) +
  plot_layout(guides = "collect", widths = c(1, 0.2, 1)) +
  plot_annotation(
    title = "Liczba osób ćwicząca w sekcjach sportowych na przestrzeni lat",
    subtitle = "Dane całościowe - wszystkie grupy wiekowe",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 14)
    )
  )

combined_plot

