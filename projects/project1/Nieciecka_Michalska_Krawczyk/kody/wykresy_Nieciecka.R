install.packages(c("readxl", "tidyverse", "janitor", "ggplot2"))
library(readxl)
library(tidyverse) #dplyr, ggplot2
library(forcats)

#wczytywanie danych z 2021
tablice_trwania_zycia_w_latach_2021 <- read_excel("tablice_trwania_zycia_w_latach_1990-2023.xlsx", sheet = '2021', skip = 3)
#excel_sheets("tablice_trwania_zycia_w_latach_1990-2023.xlsx") - sprawdzanie arkuszy
tablice_trwania_zycia_w_latach_2021 <- tablice_trwania_zycia_w_latach_2021[c(1,2,5,6)]
colnames(tablice_trwania_zycia_w_latach_2021) <- c("płeć","wiek","liczba_zmarłych", "liczba_ludności")
tablice_trwania_zycia_w_latach_2021 <- tablice_trwania_zycia_w_latach_2021 %>%
  mutate(
    płeć = factor(płeć, levels = c(1, 2), labels = c("Mężczyźni", "Kobiety")),
    wiek = as.numeric(wiek),
    liczba_zmarłych = as.numeric(liczba_zmarłych))

#Wczytywanie danych z 2019
tablice_trwania_zycia_w_latach_2019 <- read_excel("tablice_trwania_zycia_w_latach_1990-2023.xlsx", sheet = '2019', skip = 3)
tablice_trwania_zycia_w_latach_2019 <- tablice_trwania_zycia_w_latach_2019[c(1,2,5,6)]
colnames(tablice_trwania_zycia_w_latach_2019) <- c("płeć","wiek","liczba_zmarłych", "liczba_ludności")
#View(tablice_trwania_zycia_w_latach_2019)
tablice_trwania_zycia_w_latach_2019 <- tablice_trwania_zycia_w_latach_2019 %>%
  mutate(
    płeć = factor(płeć, levels = c(1, 2), labels = c("Mężczyźni", "Kobiety")),
    wiek = as.numeric(wiek),
    liczba_zmarłych = as.numeric(liczba_zmarłych))


#połączenie piramid z 2021 i 2019 ze zgonami na 1000 mieszkańców
tablice_trwania_zycia_w_latach_2021 <- tablice_trwania_zycia_w_latach_2021 %>% 
  mutate(zgony_na_1000_miesz = liczba_zmarłych/liczba_ludności * 1000) %>% 
  mutate(rok=2021)
tablice_trwania_zycia_w_latach_2019 <- tablice_trwania_zycia_w_latach_2019 %>% 
  mutate(zgony_na_1000_miesz = liczba_zmarłych/liczba_ludności*1000) %>% 
  mutate(rok = 2019)
tablice_all <- rbind(tablice_trwania_zycia_w_latach_2021, tablice_trwania_zycia_w_latach_2019)
tablice_all <- tablice_all %>%
  mutate(
    kolor = case_when(
      płeć == "Mężczyźni" & rok == 2021 ~ "#348382",
      płeć == "Mężczyźni" & rok == 2019 ~ "#7ccbc9", 
      płeć == "Kobiety" & rok == 2021 ~ "#8c4066", 
      płeć == "Kobiety" & rok == 2019 ~ "#c581a3" 
    )
  )
plot_19_20 <- ggplot(tablice_all, aes(x = wiek,
                        y = ifelse(płeć == 'Mężczyźni', -zgony_na_1000_miesz, zgony_na_1000_miesz),
                        fill = kolor))+
  geom_col(position = 'identity', width = 0.9)+
  scale_y_continuous(labels = abs)+
  scale_fill_identity(
    guide = "legend",
    labels = c(
      "Mężczyźni 2019", "Nadwyżka mężczyżni 2021",
      "Kobiety 2019", "Nadwyżka kobiety 2021"
    ),
    breaks = c("#7ccbc9", "#348382", "#c581a3", "#8c4066"),
    name = "Porównanie lat 2019 i 2021"
  )+
  coord_flip()+
  theme_minimal()+
  labs(x = "Wiek",
       y = 'Zgony na 1000 mieszkańców',
       fill = 'Rok')+
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size=10, hjust=0.5, color = 'gray30'),
    plot.margin = margin(t=40, r=20, b=20, l=20)  
    )+
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.major = element_line(color = "#b8c0cc"),
    panel.grid.minor = element_line(color = "#b8c0cc"),
    axis.text = element_text(color = "#2B3044"),
    axis.title = element_text(color = "#2B3044"),
    axis.line = element_line(color = "#2B3044"),
    plot.title = element_text(color = "#2B3044"),
    plot.subtitle = element_text(color = "#2B3044"),
    plot.caption = element_text(color = "#2B3044"),
    legend.title = element_text(color = "#2B3044"),
    legend.text = element_text(color = "#2B3044")
  )
#install.packages("cowplot")
library(cowplot)
plot_bez_legendy <- plot_19_20 +
  theme(legend.position = 'none')
legenda <- cowplot::get_legend(plot_19_20)

#ggsave("wykres_bez_legendy.png", plot_bez_legendy, bg = "transparent")

#ggsave("legenda.png", legenda, bg = "transparent")




