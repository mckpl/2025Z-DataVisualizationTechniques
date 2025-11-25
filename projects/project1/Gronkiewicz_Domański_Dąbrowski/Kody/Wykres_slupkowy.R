install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("scales")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)


ceny_mieszkan <- read.csv("ceny_mieszkan.csv")
View(ceny_mieszkan)
ceny_mieszkan <-ceny_mieszkan %>% 
  mutate(Nazwa = reorder(Nazwa, Cena))



bar_plot <- ggplot(data = ceny_mieszkan, 
                   aes(x = Nazwa, y = Cena)) +
  geom_col(fill = "#4d869c", width = 0.7) +
  geom_text(
    aes(label = scales::number(Cena, big.mark = " ")), 
    vjust = -0.5,size = 3,color = "black") +
  scale_y_continuous(
    breaks = seq(2000, 11000, 2000),
    labels = scales::number_format(big.mark = " ", suffix = " zł"),
    expand = expansion(mult = c(0, 0.1))) +
  labs(title = "",x = "",y = "",) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
      plot.caption = element_text(size = 9, face = "italic", hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f2f1ec", color = NA))

print(bar_plot)
ggsave("wykres_slupkowy_ceny.png", plot = bar_plot, width = 10, height = 7, dpi = 300)
