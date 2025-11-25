library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(ggthemes)
library(ggrepel)

# Wczytanie danych:
wypadki <- read_xlsx("wypadki_w_latach.xlsx")

# Sprowadzamy dane do postaci wąskiej:
wypadki_long <- wypadki %>% 
  pivot_longer('2010':'2024', names_to = "rok", values_to = "liczba_wypadkow") %>% 
  mutate(rok = as.integer(rok))

# Znajdujemy top 5 najbardziej wypadkowych województw do umieszczenia na wykresie:
top5 <- wypadki_long %>% 
  group_by(wojewodztwo)  %>% 
  summarise(suma = sum(liczba_wypadkow, na.rm = TRUE), .groups = "drop")  %>% 
  slice_max(suma, n = 5)  %>% 
  arrange(desc(suma)) %>% 
  pull(wojewodztwo)

wypadki_long$kolor <- ifelse(wypadki_long$wojewodztwo %in% top5,
                             wypadki_long$wojewodztwo,
                             "")
wypadki_long$etykieta <- ifelse((wypadki_long$wojewodztwo %in% top5) &
                                  (wypadki_long$rok == 2024),
                                wypadki_long$wojewodztwo,
                                "")

wykres_wypadkow <- ggplot(wypadki_long,
       aes(x = rok, y = liczba_wypadkow, group = wojewodztwo)) +
  geom_line(color = "#7B5B59", size = 1.5) +
  theme_fivethirtyeight() +
  labs(title = "Liczba wypadków w latach 2010-2024", x = "Rok", y = "Liczba wypadków") +
  geom_line(
    data = wypadki_long[wypadki_long$kolor != "", ],
    aes(x = rok, y = liczba_wypadkow, color = wojewodztwo),
    size = 2.5) +
 # geom_label_repel(data = wypadki_long[wypadki_long$kolor != "", ],
  #                 aes(label = etykieta, color = wojewodztwo, size = 20),
  #                 nudge_x = 0.5) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 20, color = "#653600"),
    axis.text.y = element_text(size = 20, color = "#653600"),
    axis.title.x = element_text(size = 14, color = "#653600"),
    axis.title.y = element_text(size = 14, color = "#653600"),
    title = element_text(size = 17, color = "#653600"),
    plot.background  = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
  ) +
  scale_color_manual(values = c( "#dfb3b0", "#fcf8eb", "#76a2bd", "#BE5D7B", "#caeaff")) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) 

ggsave("wypadki_w_czasie.png", plot = wykres_wypadkow, width = 12, height = 7, dpi = 300, bg = "transparent")
