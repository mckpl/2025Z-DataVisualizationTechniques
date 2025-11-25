library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)


d2019 <- read_excel("zgony_2019.xlsx") %>%
  mutate(Rok = 2019)

d2021 <- read_excel("zgony_2021.xlsx") %>%
  mutate(Rok = 2021)

d_all <- bind_rows(d2019, d2021)

d_all <- d_all %>%
  complete(`Przyczyna zgonu`, Rok, fill = list(Zgony = 0))

top_order <- d_all %>%
  filter(Rok == 2021) %>%
  arrange(desc(Zgony)) %>%
  pull(`Przyczyna zgonu`)

d_all$`Przyczyna zgonu` <- factor(d_all$`Przyczyna zgonu`, levels = top_order)

d_all$Zgony <- d_all$Zgony/37000

p <- ggplot(d_all, aes(x = `Przyczyna zgonu`, y = Zgony, fill = factor(Rok))) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  scale_fill_manual(
    values = c(
      "2019" = "#7CCBC9",   # jasny niebieski
      "2021" = "#8C4066"    # ciemny róż
    ),
    name = "Rok"
  ) +
  labs(
    x = "Przyczyna zgonu",
    y = "Zgony na tysiąc mieszkańców"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    # KOLORY OSI
    axis.text = element_text(color = "#2B3044"),
    axis.title = element_text(color = "#2B3044"),
    axis.line = element_line(color = "#2B3044"),
    
    # GRID
    panel.grid.major = element_line(color = "#B8C0CC"),
    panel.grid.minor = element_line(color = "#B8C0CC"),
    
    # TŁA
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA),
    
    # LEGENDA
    legend.background = element_rect(fill = NA, color = NA),
    legend.text = element_text(color = "#2B3044"),
    legend.title = element_text(color = "#2B3044"),
    
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("wykres.png", p, bg = "transparent", width = 10, height = 6, dpi = 300)


