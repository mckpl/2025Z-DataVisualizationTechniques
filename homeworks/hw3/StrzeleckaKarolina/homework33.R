
library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(mapdata)
library(stringr)
install.packages("ggpattern")
library(ggpattern)


### Wczytanie danych
df <- read.csv("API_IT.NET.USER.ZS_DS2_en_csv_v2_129784.csv", skip = 4)
getwd()
setwd("C:/Users/karol/Desktop/iad_sem_3/techniki_wizualizacji_danych/hw3")
View(df)
df <- df[, colSums(!is.na(df)) > 0]
View(df)

world <- map_data("world")
'''
data_2023 <- df %>%
  select(region = Country.Name, value = X2023)

world_joined <- world %>%
  left_join(data_2023, by = "region") %>% 
  

missing_countries <- world_joined %>%
  filter(is.na(value)) %>%
  distinct(region) %>% 
  arrange(region)# tylko unikalne nazwy

missing_countries

# Kraje w data_2023, których nie ma w mapie
missing_in_map <- data_2023 %>% 
  anti_join(world, by = "region") %>% 
  distinct(region) %>% 
  arrange((region))

missing_in_map

world_joined2 <- world_joined %>%
  mutate(region = recode(region,
                         "United States" = "USA",
                         "Yemen, Rep." = "Yemen",
                         "Virgin Islands (U.S.)" = "Virgin Islands",
                         "Viet Nam" = "Vietnam",
                         "Venezuela, RB" = "Venezuela",
                         "United Kingdom" = "UK",
                         "Turkiye" = "Turkey",
                         "Somalia, Fed. Rep." = "Somalia",
                         "Russian Federation" = "Russia",
                         "Bahamas, The" = "Bahamas",
                         "Czechia" = "Czech Republic"
                         ))
'''
data_2023 <- df %>%
  select(region = Country.Name, value = X2023) %>%
  mutate(region = recode(region,
                         "United States" = "USA",
                         "Yemen, Rep." = "Yemen",
                         "Virgin Islands (U.S.)" = "Virgin Islands",
                         "Viet Nam" = "Vietnam",
                         "Venezuela, RB" = "Venezuela",
                         "United Kingdom" = "UK",
                         "Turkiye" = "Turkey",
                         "Somalia, Fed. Rep." = "Somalia",
                         "Russian Federation" = "Russia",
                         "Bahamas, The" = "Bahamas",
                         "Czechia" = "Czech Republic"))

world_joined2 <- world %>%
  left_join(data_2023, by = "region")

ggplot(world_joined2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = value), color = "black", linewidth = 0.2) +
  coord_map("mollweide") +
  scale_fill_gradient2(
    low = "orange",
    mid = "white",
    high = "purple",
    midpoint = 50,
    na.value = "grey80"
  ) +
  theme_void() +
  theme(legend.position = "right") +
  labs(title="Ile procent osób korzystało z internetu?",subtitle="Ile procent populacji krajów korzystało z internetu w 2023 roku", fill = "Procent osób korzystających z internetu (2023)")


