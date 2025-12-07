library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(forcats)


# Link do posta z oryginalnym wykresem - 
#   https://x.com/simisani10/status/1774290101717094552

#dane do wykresu - https://github.com/rfordatascience/tidytuesday/blob/main/data/2024/2024-03-19/mutant_moneyball.csv

# Co wymaga poprawy w oryginalnym wykresie?
# 1) Imiona i nazwiska postaci nie są odpowiednio sformatowane (np scottSummers zamiast Scott Summers)
# 2) Podpisy wykresów nachodzą na siebie w niektórych przypadkach, przez co się ucinają
# 3) Utworzone osobne wykresy kołowe znacznie utrudniają porównywanie liczby wystąpien między postaciami
# 4) Brak jakichkolwiek wartości liczbowych - nie mamy żadnego punktu odniesienia dla wykresów, nie wiemy co stanowi pełne koło



# Co zostało poprawione?
# 1) Zamiana na uporządkowany wykres kolumnowy - łatwo porównywać wartości między postaciami
# 2) Imiona i nazwiska postaci są poprawnie sformatowane co ułatwia czytelność i wygląda estetyczniej
# 3) Na osi x została stworzona skala, dzięki czemu wiemy na jakich wartościach operujemy na wykresie
# 4) Nowa forma wykresu pozwoliła na jego wzbogacenie o dodatkowe informacje, czyli podział na dekade w jakiej komiks został wydany

xmen_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-19/mutant_moneyball.csv')

xmen_clean <- xmen_data %>%
  mutate(
    Member_Clean = str_replace_all(Member, "([a-z])([A-Z])", "\\1 \\2") %>%
      str_to_title() %>%
      str_replace_all("Le Beau", "LeBeau") %>%
      str_replace_all("Mc Coy", "McCoy")
  )

xmen_long <- xmen_clean %>%
  select(Member_Clean, TotalIssues60s, TotalIssues70s, TotalIssues80s, TotalIssues90s) %>%
  pivot_longer(
    cols = starts_with("Total"),
    names_to = "Decade",
    values_to = "Appearances"
  ) %>%
  mutate(
    Decade = str_remove(Decade, "TotalIssues"),
    Decade = factor(Decade, 
                    labels = c("1963-1969", "1970-1979", "1980-1989", "1990-1992")
    )
  )

ggplot(xmen_long, aes(x = Appearances, y = fct_reorder(Member_Clean, Appearances, .fun = sum), fill = fct_rev(Decade))) +
  geom_col(width = 0.75) +
  geom_text(data = xmen_clean, aes(x = TotalIssues, y = Member_Clean, label = TotalIssues, fill = NULL), 
            hjust = -0.2, size = 3.5, color = "gray30") +
  scale_fill_manual(
    values = c(
      "1990-1992" = "#D55E00",
      "1980-1989" = "#F0E442",
      "1970-1979" = "#56B4E9",
      "1963-1969" = "#0072B2"
    ),
    breaks = c("1963-1969", "1970-1979", "1980-1989", "1990-1992")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Najczęściej występujący członkowie X-Men",
    subtitle = "Liczba komiksów z ich udziałem w latach 1963-1992 z podziałem na dekady ",
    x = "Liczba komiksów",
    y = "Postać",
    fill = "Rok wydania komiksu",
    caption = "Dane: TidyTuesday | 2024-03-19"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_line(color = "gray40", linetype = "dotted"),
    panel.grid.major.x = element_line(color = "gray40", linetype = "dotted"),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(color = "black", size = 10, margin = margin(r = 5)),
    plot.margin = margin(15, 15, 15, 15)
  )
 
