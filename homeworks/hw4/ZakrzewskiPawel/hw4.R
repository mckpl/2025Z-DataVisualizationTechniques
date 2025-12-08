####### Paweł Zakrzewski, hw4
####### Linki
#
# POST:
# https://x.com/NaomiWithNature/status/1701789023021232187/photo/1
# DANE:
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2023/2023-09-12/readme.md
# (2023 TidyTuesday week 37)
#
####### Co wymaga poprawy?
#
# a) Podział wykresów na kraje znacznie utrudnia analizę, zwłaszcza w obecnej formie
# brak linii pomocniczych, niedpowiednie podziały na osi Y sprawiają że praktycznie
# nie da się odczytać wartości
# b) Zmniejszenie legendy, by nie zajmowała  połowy wykresu
# c) Poprawa nazewnictwa osi - oś X jest myląca i nieprawidłowa
# d*) Zmiana kodów iso3 na nazwy państw (bardziej intuicyjne niż iso3) 
# e*) poprawa estetyki tytułu i legendy
#
####### Co zmieniono i w jaki sposób poprawia to wykres?
#
# Zamiast tworzyć 9 wykresów, stworzyłem jeden wykres z 9 słupkami, każdy o wysokości 24h
# podzielony kolorami na odpowiednie typy aktywności oraz przypisanymi do nich dokładnymi liczbami.
# Ta forma zdecydowanie ułatwia zarówno analizę w obrębie jednego kraju oraz porównywanie różnic między państwami.
# Labele umieszczone są dla segmentów o wysokości >0.6 dla przejrzystości wykresu. 
# Legenda nie zajmuje już połowy wykresu, dopasowane tytuły osi (zwłaszcza oś X),
# poprawiłem również ogólną estetykę tytułu i podpisów legendy.
#
####### Paczuszki + dane
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(readr)
library(showtext)
font_add_google("Roboto","roboto")
font_add_google("Merriweather", "merriweather")
showtext_auto()
#wczytywanie danych skopiowane z guide'a
all_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-09-12/all_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-09-12/country_regions.csv')
global_human_day <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-09-12/global_human_day.csv')
global_economic_activity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-09-12/global_economic_activity.csv')
#
####### FILTRACJA DANYCH + WYKRES
#
newdata <- all_countries %>%
  group_by(country_iso3, Category) %>%
  summarise(total_h = sum(hoursPerDayCombined)) %>%
  filter(country_iso3 %in% c("IND", "CHN", "USA", "IDN", "PAK", "NGA", "BRA", "BGD", "RUS")) %>%
  mutate(country= recode(country_iso3,
                         "IND" = "India", "CHN"= "China", "USA"="USA", "IDN" ="Indonesia",
                         "PAK"="Pakistan","NGA"="Nigeria","BRA"="Brazil","BGD"="Bangladesh", "RUS"="Russia"))

newdata$Category <- factor(newdata$Category, levels=c("Somatic maintenance", "Experience oriented", 
                                                      "Food provision","Organization",
                                                      "Deliberate neural restructuring", "Technosphere modification", #Zmieniam kolejność by wyświetlały się
                                                      "Maintenance of surroundings", "Nonfood provision")) # od największego podziału do najmniejszego

wykres <- ggplot(newdata, aes(x=country, y=total_h, fill=Category)) +
  geom_col(width=0.8) +
  geom_text(aes(label=ifelse(total_h > 0.6, round(total_h,1), "")), #>0.6 dla czytelności, labele na dole nachodzą na siebie
            position = position_stack(vjust=0.5),
            size=4,color="black",
            fontface="bold",
            family="roboto") +
  scale_fill_manual(values = c("lightblue","#ff8080","lightgreen","#f0a3eb","#d4d161","#5e9e5d","#f24444","blue")) + 
  theme_minimal() +
  ylab("Hours spent per day on activity type") +
  labs(title="Hours spent on human activities per day",
       subtitle="Data shown for 9 most populated countries",
       fill="Activity type") +
  scale_y_continuous(expand= expansion(mult = c(0.01, 0.03)), breaks=seq(0, 24, by=5)) +
  theme(
    axis.title.x = element_blank(),
    axis.text.y= element_blank(),
    plot.background = element_rect(fill="#F9F5F2"),
    axis.title.y=element_text(hjust = 0.5, size=rel(1.2), color="black", face="bold", family="merriweather"),
    axis.text.x=element_text(size=rel(1.4), color="black", face="bold", family="merriweather"),
    legend.position = "right",
    legend.text = element_text(face="italic", size=rel(1.1), family="merriweather"),
    legend.title = element_text(face="bold", size=rel(1.4), family="merriweather"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.title = element_text(hjust=0, size=rel(3), face="bold", family="merriweather"),
    plot.subtitle = element_text(hjust=0, size=rel(1.6), face="italic", family="merriweather")
  )

ggsave("new_plot.png", plot = wykres, width=12, height=8, units="in",dpi=150)
