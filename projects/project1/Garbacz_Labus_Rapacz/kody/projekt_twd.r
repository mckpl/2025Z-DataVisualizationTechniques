library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(patchwork)
library(showtext)
font_add_google("League Spartan", family = "spartan")
showtext_auto()


# 2010-18 dane o przewoznikach
punkt_przewoz_2010_18<-read.csv("resources/punkt_przewoz_2010_18.csv")
punkt_przewoz_2010_18<-data.frame(punkt_przewoz_2010_18)
punkt_przewoz_2010_18<-punkt_przewoz_2010_18 %>%
  pivot_longer(
    cols=-rok,
    names_to = "Przewoznik",
    values_to = "Punktualnosc_str",
  ) %>%
  mutate(
    Punktualnosc=parse_number(Punktualnosc_str,locale = locale(decimal_mark = ","))
  ) %>%
  filter(!is.na(Punktualnosc))
head(punkt_przewoz_2010_18)
glowni_przewoznicy_10_18<-punkt_przewoz_2010_18 %>%
  filter(Przewoznik %in% c("Polregio","Koleje.Mazowieckie","PKP.Intercity","SKM.Warszawa","Koleje.Slaskie","Koleje.Dolnoslaskie","WKD"))


#dane o przewoznikach 2019-24
punkt_przewoz_2019_24<-read.csv("resources/punkt_przewoz_2019_24.csv")
punkt_przewoz_2019_24<-data.frame(punkt_przewoz_2019_24)
punkt_przewoz_2019_24<-punkt_przewoz_2019_24 %>%
  pivot_longer(
    cols=-rok,
    names_to = "Przewoznik",
    values_to = "Punktualnosc_str",
  ) %>%
  mutate(
    Punktualnosc=parse_number(Punktualnosc_str,locale = locale(decimal_mark = ","))
  ) %>%
  filter(!is.na(Punktualnosc))
head(punkt_przewoz_2019_24)
glowni_przewoznicy_19_24<-punkt_przewoz_2019_24 %>%
  filter(Przewoznik %in% c("Polregio","Koleje.Mazowieckie","PKP.Intercity","SKM.Warszawa","Koleje.Slaskie","Koleje.Dolnoslaskie","WKD","PKP.SKM.Trojmiasto"))

punkt_przewoz_2025<-read.csv("resources/punkt_przewoz_2025.csv")
punkt_przewoz_2025<-data.frame(punkt_przewoz_2025)
punkt_przewoz_2025=data.frame(c(2025,punkt_przewoz_2025))
colnames(punkt_przewoz_2025)<-c("rok","Przewoznik","Punktualnosc")
punkt_przewoz_2025$Przewoznik<-gsub(" ",".",punkt_przewoz_2025$Przewoznik)
glowni_przewoznicy_2025<-punkt_przewoz_2025%>%
  filter(Przewoznik %in% c("Polregio","Koleje.Mazowieckie","PKP.Intercity","SKM.Warszawa","Koleje.Slaskie","Koleje.Dolnoslaskie","WKD","PKP.SKM.Trojmiasto"))
punkt_przewoz_2025
glowni_przewoznicy_10_25<-bind_rows(glowni_przewoznicy_10_18,glowni_przewoznicy_19_24,glowni_przewoznicy_2025)
glowni_przewoznicy_10_25
glowni_przewoznicy_10_25<-glowni_przewoznicy_10_25 %>%
  mutate(cat=case_when(Przewoznik=="PKP.Intercity" ~ "dalekobieżne",
                       Przewoznik %in% c("Polregio","Koleje.Mazowieckie","Koleje.Slaskie","Koleje.Dolnoslaskie") ~ "regionalne",
                       TRUE ~ "aglomeracyjne / krótkodystansowe"))

podzial<-glowni_przewoznicy_10_25 %>%
  mutate(cat=factor(cat,levels = c("aglomeracyjne / krótkodystansowe","regionalne","dalekobieżne")))%>%
  group_by(rok,cat) %>%
  mutate(mean=mean(Punktualnosc)) %>%
  select(rok,cat,mean) %>%
  distinct()
ggplot(podzial,aes(x=rok,y=mean,colour =cat))+
  geom_line(linewidth = 1.1)+
  geom_vline(xintercept = 2019, linetype = "dashed", color = "black", linewidth = 0.4)+
  labs(
    title="Punktualność przewoźników w latach 2010-2025",
    subtitle=" w podziale na długości oferowanych tras",
    x = "Rok",
    y = "Punktualność (%)"
  ) +
  scale_x_continuous(breaks = unique(podzial$rok),expand = c(0,0)) +
  scale_color_manual(values = c("dalekobieżne" = "#fc9942",
                                "regionalne"="#b438c2",
                                "aglomeracyjne / krótkodystansowe"="#38c297"))+
  theme_light(base_family = "spartan",base_size = 16)+
  theme(legend.position = "None",
        plot.title = element_text(face = "bold"),
        plot.margin = margin(10,20,10,10))


















