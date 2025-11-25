library(readxl)
library(dplyr)
library(janitor)
library(ggthemes)
library(lubridate)
install.packages("ggthemes")

mandaty <- read_excel("Dane_z_mandatów_karnych.xlsx") %>% 
  clean_names()

mies_pl <- c("styczeń","luty","marzec","kwiecień","maj","czerwiec",
             "lipiec","sierpień","wrzesień","październik","listopad","grudzień")

p <- mandaty %>%
  mutate(miesiac = match(miesiac, mies_pl),
         data = make_date(rok, miesiac, 1),
         srednia_kwota_zl = round(kwota_mandatow_w_tys_zl / ilosc_mandatow_w_tys)) %>% 
  ggplot(aes(x = data, y = srednia_kwota_zl)) +
  geom_line(color = "#ecc8bb",linewidth = 2) +
  geom_vline(xintercept = as.Date("2022-01-01"),
             linetype = "dashed", linewidth = 1, color = "#AD3A62") +
  annotate("text",
           x = as.Date("2022-01-01"),
           y = max(mandaty$ilosc_mandatow_w_tys, na.rm = TRUE),
           label = "Nowy taryfikator", vjust = 1, hjust = 0.5, size = 5, color = "#AD3A62") +
  scale_x_date(date_breaks = "4 months", date_labels = "%Y-%m", guide = guide_axis(angle = 45)) +
  labs(title = "Średnia wartość mandatu w latach 2020-2025",
       x = "Data",
       y = "Średnia kwota mandatu (zł)") +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(color = "#653600", size = 17),
        axis.title = element_text(color = "#653600", size = 14), 
        axis.text = element_text(color = "#653600", size =13),
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA)) +
  expand_limits(y = 0)
  

ggsave("mandaty.png", plot = p, width = 9, height = 5, dpi = 300, bg = "transparent")


