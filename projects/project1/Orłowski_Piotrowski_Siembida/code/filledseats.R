library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(ragg)


font_add_google("Antonio", "antonio")
showtext_auto()

stadiums <- read.csv("stadiums.csv")

filledseats <- stadiums %>%
  filter(Club %in% c("Lech Poznań", "Jagiellonia Białystok", "Górnik Zabrze", "Pogoń Szczecin", "Legia Warszawa")) %>% 
  filter(First.year.of.season > 2003) %>% 
  mutate(usage.better = (Average/Capacity)*100) %>% 
  ggplot(aes(x=First.year.of.season, y=usage.better, color=Club))+
  geom_line(linewidth = 6.5) +
  labs(
    x = "Year",
    y = "Percantage",
    title = "Filled seats in selected Ekstraklasa stadiums",
    color = NULL
  ) +
  scale_x_continuous(limits = c(2004, 2025), 
                     expand = c(0, 0),
                     breaks = seq(2005, 2025, by = 5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme(
    plot.margin = margin(b = 30),
    text = element_text(family = "antonio", colour = "white", size=40),
    axis.text.x = element_text(colour = "white", size=40),
    axis.text.y = element_text(colour = "white", size=40),
    axis.title.x = element_text(vjust = -5),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA),
    axis.line        = element_line(color = NA),
    axis.ticks       = element_line(color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 1.5),
    panel.grid.minor = element_line(color = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "top",
    legend.text = element_text(margin = margin(r = 20))
  )+
  scale_color_manual(
    values = c(
      "Lech Poznań" = "#9b9b9b",
      "Jagiellonia Białystok" = "#000000",
      "Górnik Zabrze" = "#ffffff",
      "Pogoń Szczecin" = "#ac1117",
      "Legia Warszawa" = "#fb7b5b"
    )
  )

agg_png(
  "filledseats.png",
  width = 2100,
  height = 1500,
  units = "px",
  background = "transparent",
  res = 96
)
print(filledseats)
dev.off()
