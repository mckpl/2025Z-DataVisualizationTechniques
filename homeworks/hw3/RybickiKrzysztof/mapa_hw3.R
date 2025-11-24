library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(readxl)
library(ggrepel)



dane<-
  read_excel("C:\\Users\\krzys\\OneDrive\\Pulpit\\RybickiKrzysztof\\P_Popular Indicators.xlsx")


dane<-dane %>%   filter(`Series Code` == "SP.DYN.LE00.IN")

dane<-dane  %>% select(-c("Series Code", "Series Name", "Country Code"))

dane1 <- dane %>% select(-c("Country Name")) %>%
  mutate(across(where(is.character), ~ as.numeric(.x)))

dane1 <- dane1 %>%
  mutate(across(where(is.numeric), ~ round(.x,1)))

extra_cols <- setdiff(names(dane), names(dane1))  

dane_k <- bind_cols(
  dane[extra_cols], 
  dane1             
)


dane_k <- dane_k %>%
  mutate(`Country Name` = if_else(`Country Name` == "Venezuela, RB",
                                  "Venezuela",
                                  `Country Name`))


dane_2015<-dane_k %>% select("Country Name", "2015 [YR2015]")


world<-map_data("world")

south_america <- c(
  "Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador",
  "Guyana","Paraguay","Peru","Suriname","Uruguay","Venezuela","French Guiana"
)

przyciecie_mapy <- world %>%
  filter(
    long > -135, long < -25,  
    lat  > -60,  lat  < 40   
  )

americas_joined <- przyciecie_mapy %>%
  left_join(dane_2015, by = c("region" = "Country Name"))

americas_joined <- americas_joined %>%
  mutate(
    life_exp = ifelse(region %in% south_america,
                     `2015 [YR2015]`,
                     NA_real_)  
  )

sa_values <- americas_joined %>%
  filter(region %in% south_america) %>%
  group_by(region) %>%
  summarise(life = unique(life_exp), .groups = "drop")


max_val <- max(sa_values$life, na.rm = TRUE)
min_val <- min(sa_values$life, na.rm = TRUE)

max_countries <- sa_values$region[sa_values$life == max_val]
min_countries <- sa_values$region[sa_values$life == min_val]

highlight_countries <- c("Chile", "Bolivia")

centroids_two <- americas_joined %>%
  filter(region %in% highlight_countries) %>%
  group_by(region) %>%
  summarise(
    long = mean(long),
    lat  = mean(lat),
    .groups = "drop"
  )



ggplot(americas_joined, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = life_exp), color = "white", linewidth = 0.2)  +
  geom_point(
    data = centroids_two,
    aes(x = long, y = lat),
    color = "red",
    size = 2,
    inherit.aes = FALSE
  ) +
geom_text(
  data = centroids_two,
  aes(x = long, y = lat + 2, label = region), 
  color = "pink",
  size = 3.5,
  fontface = "bold",
  inherit.aes = FALSE
)+
  coord_fixed(xlim = c(-120, -25), ylim = c(-60, 50))+
  scale_fill_viridis_c(
    option    = "magma",
    direction = -1,
    na.value  = "grey",
    name      = "Przewidywana długość życia"
  ) +
  theme_minimal() +
  labs(
    title    = "Przewidywana długość życia
    w Ameryce Południowej (2015)",
       subtitle = paste0(
      "Najniższa wartość: ", round(min_val, 1)," " , min_countries,"\n",
      "Najwyższa wartość: ", round(max_val, 1)," " , max_countries
    ),
    x = "Długość geograficzna",
    y = "Szerokość geograficzna"
  )

