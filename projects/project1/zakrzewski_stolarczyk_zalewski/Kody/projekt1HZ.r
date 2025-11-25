library(dplyr)
library(readr)
library(ggrepel)
library(scales)
library(stringr)

marki <- read_csv2(
  "dane/MARKI_POLACZONE_DATA.csv")

dane_liczba_woj <- read_csv("dane/ludnosc.csv")
dane_zarobki <- read_csv("dane/zarobki.csv") 

colnames(dane_zarobki) <- c("WOJEWODZTWO", "rok2021", "rok2022")
colnames(dane_liczba_woj) <- c("WOJEWODZTWO", "LUDNOSC")

dane_zarobki <- dane_zarobki %>%
  mutate(zarobki = (rok2021 + rok2022) / 2,
         WOJEWODZTWO = str_to_title(WOJEWODZTWO))

dane_populacja_woj <- dane_liczba_woj %>%
  rename(Populacja = LUDNOSC) %>%
  mutate(WOJEWODZTWO = str_to_title(WOJEWODZTWO))

rejestracje_agg_woj <- marki %>%
  filter(ROK %in% 2021:2025) %>%
  group_by(WOJEWODZTWO) %>%
  summarize(Suma_Rejestracji = sum(LICZBA, na.rm = TRUE)) %>%
  mutate(WOJEWODZTWO = str_to_title(WOJEWODZTWO))

dane_do_scatter <- dane_populacja_woj %>%
  inner_join(dane_zarobki, by = "WOJEWODZTWO") %>%
  inner_join(rejestracje_agg_woj, by = "WOJEWODZTWO")

dane_do_scatter <- dane_do_scatter %>%
  mutate(
    Wskaznik_Aut_1000_Os = (Suma_Rejestracji / Populacja) * 1000
  )

plot_scatter_dochody <- ggplot(
  dane_do_scatter, 
  aes(x = zarobki, y = Wskaznik_Aut_1000_Os)
) +
  geom_point(color = "#0072B2", size = 3, alpha = 0.7) +
  geom_text_repel(aes(label = WOJEWODZTWO), size = 3.5, max.overlaps = Inf,fontface = "bold", min.segment.length = 0.1,box.padding = unit(0.5, "lines"),
                  color ="black",
                  segment.color = "black"
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00", linetype = "dashed") +
  scale_x_continuous(
    labels = scales::dollar_format(suffix = " zł", prefix = ""), 
    expand = expansion(mult = 0.1)
  ) +
  scale_y_continuous(
    labels = scales::comma, 
    expand = expansion(mult = 0.1)
  ) +
  labs(
    title = "Zależność między zamożnością wojewodztwa a liczbą nowych aut (2021-2024)",
    subtitle = "Dla średniego przychodu w wojewodztwie w okresie 2022-2023",
    x = "Średnie wynagrodzenie brutto",
    y = "Liczba nowych rejestracji w latach 2021-2024 na 1000 mieszkańców"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "plain",hjust = 0.5, size = 10),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    text = element_text(face = "bold",color = "black"),
    axis.text = element_text(face = "bold",color = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
    
  )

print(plot_scatter_dochody)
ggsave("wykres.png", plot = plot_scatter_dochody, bg = "transparent")

