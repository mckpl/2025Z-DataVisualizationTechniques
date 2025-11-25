library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(maps)
install.packages("mapdata")
library(mapdata)
library(sf)
library(extrafont)
loadfonts()




tab02 <- read.csv("kody\\data\\2011\\tab02_2011.csv", sep = ";", header = TRUE)

tmp <- tab02 %>%
  select(-c('Procent', 'Kod_terytorialny', 'Województwo')) %>%
  rename(
    identyfikacja = Identyfikacja_narodowo.etniczna,
    powiat = Powiat,
    liczba = Liczba
  ) %>% 
  mutate(liczba = as.integer(liczba),
         identyfikacja = case_when(identyfikacja == "Polska" ~ "polska",
                                   identyfikacja == "Ogółem" ~ "ogolem",
                                   str_detect(identyfikacja, "^Inna niż polska") ~ "to_del",
                                   TRUE ~ "inne")) %>% 
  filter(identyfikacja != "top_del")

tmp <- tmp %>% 
  group_by(powiat, identyfikacja) %>% 
  summarise(liczba = sum(liczba), .groups = "drop") %>%
  filter(!is.na(liczba)) %>% 
  pivot_wider(names_from = identyfikacja,
              values_from = liczba) %>% 
  mutate(ogolem = inne + polska,
         inne = (inne/ogolem)*100,
         polska = (polska/ogolem)*100)

dane <- tmp %>%
  pivot_longer(cols = c(polska, inne),
               names_to = "identyfikacja",
               values_to = "liczba") %>% 
  filter(identyfikacja == "inne")

granice_powiatow <-  st_read("kody/data/granice/A02_Granice_powiatow.shp")

granice_powiatow <- granice_powiatow %>% 
  select(geometry, JPT_NAZWA_) %>% 
  rename(powiat = JPT_NAZWA_) %>% 
  mutate(powiat = gsub("^powiat\\s+", "",powiat, ignore.case = TRUE)) %>% 
  left_join(dane, by ="powiat") %>% 
  select(powiat, liczba, geometry) %>% 
  mutate(bin = cut(liczba,
                   breaks = seq(0, 40, by = 4),
                   include.lowest = TRUE,
                   right = FALSE))

plot_07 <- ggplot(data = granice_powiatow, aes(fill = bin)) +
  geom_sf(color = "white") +
  scale_fill_manual(values = c("#C10020","orange", "#ffc300","yellow", "#dad83d","blue","darkblue","#6e3784","#53178f","#2f0a55"), 
  na.value = "darkgray")+
  theme_void() + 
  labs(fill = "Procent ludności o przynależności\nnarodowo-etnicznej innej niż polska") +
  labs(
    title = "Procent ludności o identyfikacji narodowo-etnicznej innej niż polska (2011)",
    subtitle = "Wartości podane w procentach i wg powiatów, kategorie na legendzie",
    caption = "Kolor powiatu odpowiada procentowi ludności o innej identyfikacji niż polska."
  ) +
  theme(legend.title = element_text(size = 10, family = "Cambria"),
        legend.text  = element_text(family = "Cambria", size = 12),
        legend.position = "bottom",
        plot.margin = margin(t = 10, r = 15, b = 40, l = 15),
        plot.caption = element_text(hjust = 0.5),
        text = element_text(family = "Cambria"))

plot_07

ggsave(plot_07, filename = "kody/plots/Ola/plot_07.png", device = "png",
       height = 8, width = 8, dpi = 300, bg = "transparent")

