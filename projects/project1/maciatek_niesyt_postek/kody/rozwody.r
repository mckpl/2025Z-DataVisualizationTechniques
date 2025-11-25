library(readxl)
library(sf)
library(ggplot2)
library(dplyr)
dane <- read_excel("przyczynyrozwodow202.xlsx")

dane_sorted <- dane %>%
  group_by(przyczyna) %>%
  summarise(total = sum(liczba)) %>%
  arrange(total)

dane$przyczyna <- factor(dane$przyczyna, levels = dane_sorted$przyczyna)

wykres <- ggplot(dane, aes(x = przyczyna, y = liczba, fill = orzeczenie_o_winie)) +
  geom_col(position = "fill", width=0.4) +
  coord_flip()+
  labs(
    title = "Przyczyny rozwodów na podstawie orzeczenia o winie \nw roku 2023",
    x = "Przyczyny rozwodu posortowane malejąco\n",
    y = "Procentowy udział orzeczeń o winie",
    fill = "Orzeczenie \no winie"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#8F2F4F", "#F9564F", "#FBAF00"))+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title      = element_text(face = "bold", size = 15, hjust = 0.5, color = "whitesmoke"),
    axis.title      = element_text(face = "bold", color = "whitesmoke"),
    axis.text       = element_text(color = "whitesmoke", size=12),
    legend.title    = element_text(face = "bold", color = "whitesmoke"),
    legend.text     = element_text(size = 11, color = "whitesmoke"),
    panel.grid.major = element_line(color = "whitesmoke"),
    panel.grid.minor = element_line(color = "whitesmoke"),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )
wykres
ggsave("wykresik.png", plot = wykres, bg = "transparent", width = 8 , height = 4, dpi = 300)
