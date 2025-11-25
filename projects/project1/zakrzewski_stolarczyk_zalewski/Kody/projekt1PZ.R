library(readr)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(ggrepel)

marki <- read_csv2(
  "dane/MARKI_POLACZONE_DATA.csv")
#View(marki)
ludnosc <- read_csv(
  "dane/ludnosc.csv")
#View(ludnosc)

dane <- marki %>%
  group_by(MARKA,ROK) %>%
  summarize(liczba_aut = sum(LICZBA))

dane2021 <- dane %>%
  filter(ROK==2021) %>%
  rename("R2021" = liczba_aut) %>%
  select(MARKA,"R2021")


dane2023 <- dane %>%
  filter(ROK==2023) %>%
  rename("R2023" = liczba_aut) %>%
  select(MARKA,"R2023")

dane2024 <- dane %>%
  filter(ROK==2024) %>%
  rename("R2024" = liczba_aut) %>%
  select(MARKA,"R2024")

dane2022 <- dane %>%
  filter(ROK==2022) %>%
  rename("R2022" = liczba_aut) %>%
  select(MARKA,"R2022")

dane2025 <- dane %>%
  filter(ROK==2025) %>%
  rename("R2025" = liczba_aut) %>%
  select(MARKA,"R2025") %>%
  mutate("R2025" = round(R2025*4/3)) # uzupełnienie danych, więcej w opisie README.md



dane21_25 <- full_join(full_join(full_join(full_join(dane2021,dane2022,by="MARKA"),dane2023,by="MARKA"),dane2024,by="MARKA"),dane2025,by="MARKA") %>%
  arrange(desc(R2025))

dane21_25[is.na(dane21_25)] <- 0

dane21_25 <- dane21_25 %>%
  mutate(sredni_wzrost = round(((R2023-R2022)+(R2024-R2023)+(R2025-R2024))/3)) %>%
  mutate(wskaznik = (R2025/R2022 )^(1/3) - 1  ) 

nowemarki <- dane21_25 %>%
  filter(MARKA %in% c("TESLA", "BYD", "BAIC", "MG", "LEXUS", "JEEP","CUPRA", "OMODA", "JAECOO")) %>%
  select(-c(sredni_wzrost,wskaznik))

colnames(nowemarki) <- gsub("^R", "", colnames(nowemarki))

factormarki <- nowemarki %>%
  pivot_longer(cols=-MARKA, names_to="rok", values_to="liczba_aut", names_transform=list(rok=as.integer)) %>%
  filter(rok == 2025) %>%
  arrange(desc(liczba_aut)) %>%
  pull(MARKA)


wykres <- nowemarki %>%
  pivot_longer(cols=-MARKA, names_to="rok", values_to="liczba_aut", names_transform=list(rok=as.integer)) %>%
  mutate(MARKA = factor(MARKA,factormarki)) %>%
  ggplot(aes(x=rok,y=liczba_aut, color=MARKA)) +
  geom_vline(xintercept=c(2021,2022,2023,2024,2025), color="#91918d", linewidth=0.1) +
  geom_line(linewidth=1.2) +
  scale_color_manual(values=c("#1F77B4",
                              "#0c5920","#9467BD","darkblue",
                              "red", "#14ff00","#0094ff", "magenta", "black"))+
  theme_minimal() +
  scale_x_continuous(expand=c(0,0.3)) +
  scale_x_continuous(expand = c(0,0.1),
                     breaks = seq(2021, 2025, by = 1)) + 
  scale_y_continuous(breaks=seq(0,20000,by=5000)) +
  labs(
    title = "Marki najbardziej zyskujące na popularności na rynku polskim",
    subtitle="Liczba rejestrowanych aut w latach 2021-2025",
    x = "Rok",
    y = "Liczba zarejestrowanych aut") +
  theme_minimal(base_size = 9) +
  theme(plot.title = element_text(face = "bold",hjust=0.5,color="black",size=19), 
        plot.subtitle=element_text(face = "italic",hjust=0.5,color="black",size=12),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(color="black", face="bold", size=10),
        axis.title.x = element_text(color="black",face="bold",size=18, margin=margin(12)),
        axis.title.y = element_text(color="black",face="bold",size=18, margin=margin(r= 13)),
        axis.text.y = element_text(color="black", face="bold", size=10),
        panel.grid.major=element_line(color="#91918d",linewidth=0.1),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.text = element_text(face="bold", size=8),
        legend.title = element_text(face="bold", size=13),
        legend.key.size=unit(1,"cm"),
        text = element_text(color = "black"))


ggsave("wykres.png", plot = wykres, bg = "transparent", width=10, height=8)



 
