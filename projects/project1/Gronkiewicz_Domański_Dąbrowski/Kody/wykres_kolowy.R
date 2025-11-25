install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)

# Wykres 1

dane <- read.csv("dane_o_zamieszkaniu.csv")


View(dane)

dane_do_wykresu <- dane %>%
  select(tenure, OBS_VALUE) %>%
  mutate(
    tenure = case_when(
      tenure == "Owner, with mortgage or loan" ~ "Właściciel (z kredytem)",
      tenure == "Owner, no outstanding mortgage or housing loan" ~ "Właściciel (bez kredytu)",
      tenure == "Tenant, rent at reduced price or free" ~ "Najemca (czynsz obniżony/darmowy)",
      tenure == "Tenant, rent at market price" ~ "Najemca (czynsz rynkowy)"),
    procent = paste0(OBS_VALUE,"%")) %>% 
  arrange(desc(OBS_VALUE)) %>%
  mutate(tenure = factor(tenure, levels = tenure))

# Tworzenie wykresu kołowego
wykres_1 <- ggplot(dane_do_wykresu, aes(x = "", y = OBS_VALUE, fill = tenure)) +
  geom_bar(stat = "identity", width = 1, color = "#f2f1ec") + 
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = procent, x = 1.65), position = position_stack(vjust = 0.5), 
            color = "#365b6d", size = 5, fontface = "bold") +
  scale_fill_brewer(palette = "Paired") + 
  #scale_fill_manual(values = c("#ADD8E6", "#73A7C5", "#3972A4", "#191970")) +
  labs(fill = "Struktura Własności i Najmu") +
  theme(legend.text = element_text(size = 12, color = "#365b6d"),legend.key.size = unit(8, "mm"),
        legend.title = element_text(size = 14, face = "bold", color = "#365b6d"),
        plot.background = element_rect(fill = "#f2f1ec", color = NA))

wykres_1

ggsave("wykres_kołowy.png", plot = wykres_1)

