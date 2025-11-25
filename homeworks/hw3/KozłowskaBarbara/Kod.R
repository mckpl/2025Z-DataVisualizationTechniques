#Link do danych, z których skorzystałam - https://www.kaggle.com/datasets/rehan497/worldwide-temperature-trends-19612024

data <- read.csv("Studia/semestr 3/TechnikiWizualizacjiDanych/homeworks/3/Environment_Temperature_change_E_All_Data_NOFLAG.csv", 
                 dec = ",", fileEncoding = "ISO-8859-1")

library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(ggnewscale)

poprawione <- data %>% filter(Element == "Temperature change") %>% 
  filter(Months == "Meteorological year" & Area.Code<5000 ) %>% #same panstwa
  mutate(
    Y2019 = as.numeric(Y2019), Y2000 = as.numeric(Y2000), roznica = Y2019 - Y2000) %>%
  select(Area,Y2000,Y2019,roznica) %>% filter(!is.na(Y2019) &!is.na(Y2000)) %>% 
  mutate(min_max = ifelse(roznica %in% c(min(roznica), max(roznica)), Area, "")) %>% 
  
  #Nazwy w obu ramkach są inne dla niektórych krajów
  
    mutate(
      Area = case_when(
        Area == "United States of America" ~ "USA",
        Area == "United Kingdom" ~ "UK",
        Area == "Czech Republic" ~ "Czechia",
        Area == "Venezuela (Bolivarian Republic of)" ~  "Venezuela",
        Area == "Bolivia (Plurinational State of)" ~ "Bolivia",
        Area == "United Republic of Tanzania" ~ "Tanzania",
        Area == "Congo" ~ "Republic of Congo",
        Area == "Côte d'Ivoire" ~ "Ivory Coast",
        Area == "Viet Nam" ~ "Vietnam",
        Area == "Iran (Islamic Republic of)" ~ "Iran",
        Area == "Syrian Arab Republic" ~ "Syria",
        Area == "Russian Federation" ~ "Russia",
        Area == "Czechia" ~ "Czech Republic",
        Area == "Lao People's Democratic Republic" ~ "Laos",
        Area == "Republic of Korea" ~ "South Korea",
        Area == "Democratic People's Republic of Korea" ~ "North Korea",
        Area == "Republic of Moldova" ~ "Moldova",
        TRUE ~ Area))



w1 <- map_data("world")

w2 <- w1 %>%
  left_join(poprawione, by = c("region" = "Area")) %>% 
  select(!c(subregion,Y2000,Y2019))


centroidy <- w2 %>%
  filter(min_max != "") %>%
  group_by(region) %>%
  summarise(
    long = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    roznica = first(roznica),
    min_max = first(min_max)
  ) %>%
  ungroup() %>%
  mutate(nr = 1:n())

world <- w2 %>% 
  mutate(
    fill_category = case_when(
      is.na(roznica) ~ "Brak danych",
      roznica <= -1.0 ~ "-1.5 do -1.0",
      roznica <= -0.5 ~ "-1.0 do -0.5",
      roznica <= 0 ~ "-0.5 do 0.0",
      roznica <= 0.5 ~ "0.0 do 0.5",
      roznica <= 1.0 ~ "0.5 do 1.0",
      roznica <= 1.5 ~ "1.0 do 1.5",
      roznica <= 2.0 ~ "1.5 do 2.0",
      roznica <= 2.5 ~ "2.0 do 2.5"
    ),
    fill_category = factor(fill_category, 
                           levels = c("-1.0 do -0.5", "-0.5 do 0.0",
                                      "0.0 do 0.5", "0.5 do 1.0", "1.0 do 1.5", "1.5 do 2.0", 
                                      "2.0 do 2.5", "Brak danych"))
  ) %>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = fill_category), color = "black", size = 0.1) +
  scale_fill_manual(
    name = "Zmiana temperatury",
    values = c(
      "-1.0 do -0.5" = "#800080",
      "-0.5 do 0.0" = "#FF00FF",
      "0.0 do 0.5" = "#FF1493",
      "0.5 do 1.0" = "#FF6347",
      "1.0 do 1.5" = "#FF8C00",
      "1.5 do 2.0" = "#FFA500",
      "2.0 do 2.5" = "#FFD700",    
     
      "Brak danych" = "gray50"
    ),
    breaks = c("-1.0 do -0.5", "-0.5 do 0.0",
               "0.0 do 0.5", "0.5 do 1.0", "1.0 do 1.5", "1.5 do 2.0", 
               "2.0 do 2.5","Brak danych"),
    drop = FALSE
  ) +
  
  # Punkty
  geom_point(
    data = centroidy,
    aes(x = long, y = lat),
    color = "black",
    size = 5) + 
  
  # Tekst w pkt
  geom_text(
    data = centroidy,
    aes(x = long, y = lat, label = nr),
    color = "white",
    size = 3
  ) +
  
  # Podpisy
  labs(
    title = "Różnica między średnimi rocznymi temperaturami w latach 2000 i 2019 [°C]",
    x = "Długość geograficzna",
    y = "Szerokość geograficzna",
    caption = paste(
      "Skrajna różnica wartości:\n",
      "1.", centroidy$min_max[1], round(centroidy$roznica[1], 1), "°C\n",
      "2.", centroidy$min_max[2], round(centroidy$roznica[2], 1), "°C")
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 7),
    plot.caption = element_text(
      size = 9,
      hjust = 0.9,
      vjust = 5,
      margin = margin(t = 10)
    ), 
    plot.caption.position = "plot"
  )

world
