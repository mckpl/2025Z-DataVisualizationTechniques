library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(zoo)
library(scales)

invisible(try(Sys.setlocale("LC_CTYPE", "pl_PL.UTF-8"), silent = TRUE))

file_blik <- "BLIK (1).xlsx"

theme_set(
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 12, color = "gray30"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
)

parse_number_safely <- function(x) {
  x %>%
    str_replace_all("\\s", "") %>%
    na_if("") %>%
    na_if("-") %>%
    as.numeric()
}

print("--- Przygotowanie danych kwartalnych BLIK... ---")

tryCatch({
  sheet_blik_q <- "KWARTALNIE "
  df_blik_q_raw <- read_excel(file_blik, sheet = sheet_blik_q)
  colnames(df_blik_q_raw)[1:3] <- c("Sekcja_ID", "Miara", "Jednostka")
  df_blik_q_raw <- df_blik_q_raw %>%
    mutate(across(c(Sekcja_ID, Miara, Jednostka), ~ str_squish(as.character(.x))))
  
  quarter_cols_blik <- grep("^\\d{4}\\s+Q[1-4]$", colnames(df_blik_q_raw), value = TRUE)

  df_blik_q <- df_blik_q_raw %>%
    filter(str_to_lower(str_trim(Miara)) == "ogółem", 
           str_to_lower(str_trim(Jednostka)) == "szt.") %>%
    select(-Sekcja_ID, -Jednostka) %>%
    mutate(across(any_of(quarter_cols_blik), as.character)) %>%
    pivot_longer(cols = all_of(quarter_cols_blik), names_to = "Kwartal", values_to = "Liczba_str") %>%
    mutate(Liczba = parse_number_safely(Liczba_str)) %>%
    filter(!is.na(Liczba)) %>%
    mutate(Kwartal = str_replace_all(Kwartal, "\\s+", " ")) %>%
    mutate(
      Data = as.Date(as.yearqtr(Kwartal, format = "%Y Q%q")),
      Typ = "BLIK (wszystkie transakcje)"
    ) %>%
    select(Data, Liczba, Typ) %>%
    filter(!is.na(Data))

  df_blik_mix <- df_blik_q_raw %>%
    mutate(
      Kategoria = if_else(!is.na(Sekcja_ID) & str_detect(Sekcja_ID, "^4"),
                          Miara,
                          NA_character_)
    ) %>%
    fill(Kategoria, .direction = "down") %>%
    filter(
      !is.na(Kategoria),
      str_detect(str_to_lower(Miara), "^liczba$"),
      str_to_lower(str_trim(Jednostka)) == "szt."
    ) %>%
    mutate(
      Kategoria_clean = case_when(
        str_detect(str_to_lower(Kategoria), "internecie") ~ "W internecie",
        str_detect(str_to_lower(Kategoria), "bankomatach") ~ "W bankomatach",
        str_detect(str_to_lower(Kategoria), "kodem") ~ "POS - kod",
        str_detect(str_to_lower(Kategoria), "zbliżeniowe") ~ "POS - zbliżeniowo",
        str_detect(str_to_lower(Kategoria), "p2p") ~ "Przelewy P2P",
        TRUE ~ "Inne kanały"
      )
    ) %>%
    select(-Sekcja_ID, -Jednostka, -Miara, -Kategoria) %>%
    mutate(across(all_of(quarter_cols_blik), as.character)) %>%
    pivot_longer(
      cols = all_of(quarter_cols_blik),
      names_to = "Kwartal",
      values_to = "Liczba_str"
    ) %>%
    mutate(
      Liczba = parse_number_safely(Liczba_str),
      Kwartal = str_replace_all(Kwartal, "\\s+", " "),
      Data = as.Date(as.yearqtr(Kwartal, format = "%Y Q%q"))
    ) %>%
    filter(!is.na(Data), !is.na(Liczba)) %>%
    transmute(Data, Liczba, Miara = Kategoria_clean) %>%
    filter(Data >= as.Date("2017-01-01"))
    
    print("--- Dane kwartalne BLIK przygotowane. ---")

}, error = function(e) {
  print(paste("BŁĄD przy przygotowaniu danych kwartalnych:", e$message))
})


print("--- Generowanie Wykresu 6: Struktura liczby transakcji BLIK (kwartalnie) ---")

tryCatch({
  
  palette_colors <- c("#E8C547",
                      "#E55812",
                      "#87CEEB",
                      "#04724D",
                      "#DA70D6",
                      "#950952")

  n_kat <- length(unique(df_blik_mix$Miara))
  kolory <- rep(palette_colors, length.out = n_kat)
  
  g6 <- ggplot(df_blik_mix, aes(x = Data, y = Liczba, fill = Miara)) +
    geom_area(position = "stack", alpha = 0.9) +
    scale_y_continuous(
      labels = label_number(scale = 1e-6, suffix = "mln", big.mark = " ")
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0,0)) +
    scale_fill_manual(values = kolory) +
    labs(
      title = "Struktura liczby transakcji BLIK (kwartalnie)",
      subtitle = "Jak zmienia się sposób, w jaki korzystamy z BLIKa",
      x = "Data",
      y = "Liczba transakcji",
      fill = "Kanał transakcji"
    ) +
    guides(fill = guide_legend(nrow = 2)) +
    theme(
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.box.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA),
      plot.title = element_text(color = "white"),
      plot.subtitle = element_text(color = "white"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      panel.grid = element_blank(),
      axis.ticks = element_line(color = "white"),
      axis.line = element_line(color = "white")
    )
  
  print(g6)
  ggsave("plakat_wykres6_struktura_blik.png", g6, width = 12, height = 8, bg = "transparent")
  
}, error = function(e) {
  print(paste("BŁĄD przy Wykresie 6:", e$message))
})


print("--- Generowanie Wykresu 8: Sezonowość liczby transakcji BLIK ---")


tryCatch({

  color_low <- "#E8C547"

  color_high <- "#950952"



  df_blik_heat <- df_blik_q %>%

    mutate(

      Rok = as.integer(format(Data, "%Y")),

      Kwartal = factor(quarters(Data), levels = c("Q1", "Q2", "Q3", "Q4")),

      Liczba_mln = Liczba / 1e6

    ) %>%

    filter(Rok >= 2015)



  g8 <- ggplot(df_blik_heat, aes(x = Kwartal, y = factor(Rok), fill = Liczba_mln)) +

    geom_tile(color = "white", linewidth = 0.5) +

    scale_fill_gradient(low = color_low, high = color_high, name = "Liczba (mln)") +

    labs(

      title = "Sezonowość liczby transakcji BLIK",

      subtitle = "Natężenie liczby transakcji (w mln) w układzie rok x kwartał",

      x = "Kwartał",

      y = "Rok"

    ) +

    theme(

      panel.grid = element_blank(),

      axis.text.x = element_text(angle = 0, hjust = 0.5),

      legend.position = "right",

      plot.background = element_rect(fill = "transparent", colour = NA),

      panel.background = element_rect(fill = "transparent", colour = NA),

      legend.background = element_rect(fill = "transparent", colour = NA),

      legend.box.background = element_rect(fill = "transparent", colour = NA),

      plot.title = element_text(color = "white"),

      plot.subtitle = element_text(color = "white"),

      axis.text = element_text(color = "white"),

      axis.title = element_text(color = "white"),

      legend.text = element_text(color = "white"),

      legend.title = element_text(color = "white"),

      axis.ticks = element_line(color = "white"),

      axis.line = element_line(color = "white")

    )



  print(g8)

  ggsave("plakat_wykres8_sezonowosc_blik.png", g8, width = 8, height = 9, bg = "transparent")



}, error = function(e) {

  print(paste("BŁĄD przy Wykresie 8:", e$message))

})
print("--- Zakończono generowanie wykresów do plakatu. ---")
