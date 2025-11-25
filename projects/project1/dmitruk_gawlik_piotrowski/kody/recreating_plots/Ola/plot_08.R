library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(extrafont)

tab03 <- read.csv("kody//data//2011//tab03_2011.csv", sep = ";", header = TRUE)
tab04 <- read.csv("kody//data//2021//Tab4_JDom_woj-Tabela-1.csv", sep = ";", header = TRUE)

tab04 <- tab04 %>%
  pivot_longer(cols = -1,       
               names_to = "wojewodztwo",
               values_to = "liczba")


tab04 <- tab04 %>%
  pivot_wider(names_from = 1,  
              values_from = liczba) 

num_cols <- names(tab04)[-1] 

tab04[num_cols] <- tab04[num_cols] %>% 
  mutate(across(where(is.character), ~ as.numeric(str_remove_all(.x, "\\s"))))

tab03[ , 1] <- tab04[ , 1]
colnames(tab03) <- jezyki <- c("wojewodztwo",
  "Ogółem", "Polski", "Inny niż polski", "angielski", "białoruski",
  "francuski", "kaszubski", "niemiecki", "romski", "rosyjski", "śląski",
  "ukraiński", "włoski", "inne", "Nieustalony")

tab04 <- tab04[, colnames(tab03)]

dane_2011 <- tab03[1,5:14] %>% 
  pivot_longer(cols = everything(),
               names_to = "Język",
               values_to = "Liczba")
dane_2021 <- tab04[1,5:14] %>% 
  pivot_longer(cols = everything(),
               names_to = "Język",
               values_to = "Liczba")

dane <- dane_2011 %>% left_join(dane_2021, by = "Język", suffix = c("_2011", "_2021"))

options(scipen = 999)

plot_08 <- ggplot(dane)+
  geom_segment(aes(x = Liczba_2011, xend = Liczba_2021, y = Język, yend = Język),
               color="#ffc300", size = 1.5) +
  geom_point(aes(x = Liczba_2011, y = Język, color = "2011"), size = 3) +
  geom_point(aes(x = Liczba_2021, y = Język, color = "2021"), size = 3) +
  scale_color_manual(values = c("2011" = "red", "2021" = "navy")) +
  
  labs(
    title = "Zmiana liczby użytkowników języków innych niż polski używanych w domu",
    subtitle = "Linia łączy liczbę użytkowników z 2011 i 2021 roku dla każdego języka",
    color = "Rok",
    x = "Liczba użytkowników (w skali logarytmicznej)",
    caption = "Przesunięcie punktów w prawo lub w lewo wskazuje na wzrost lub spadek liczby użytkowników danego języka w domu między 2011 a 2021 rokiem."
  ) +
  scale_x_log10() +
  theme(
    legend.title = element_text(size = 10, family = "Cambria"),
    legend.text  = element_text(family = "Cambria"),
    text = element_text(family = "Cambria"),
    axis.text.x = element_text(color = "black", family ="Cambria"),
    axis.text.y = element_text(color = "black", family ="Cambria"),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major.y = element_line(color = NA),
    panel.grid.major.x = element_line(color = "#787676"),
    panel.grid.minor = element_line(color = "#787676"),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(t = 10, r = 15, b = 40, l = 15)
  )





ggsave(plot_08, filename = "kody/plots/Ola/plot_08.png", device = "png", height = 8, width = 8, dpi = 300, bg = "transparent")