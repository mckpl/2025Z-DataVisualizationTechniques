library(maps)
library(ggplot2)
library(dplyr)
library(tidyr)


samoboje <- read.csv("suicide-rate-by-country-2025.csv")

swiat <- map_data("world")
swiat <- swiat %>% 
  filter(long<=180 & lat>=-60)
swiat_samoboj <- left_join(swiat, samoboje, by=join_by("region"=="country") )

na_regiony <- swiat_samoboj %>% 
  filter(is.na(SuicideRateCountries_2023)) %>% 
  distinct(region)

samoboje2 <- samoboje %>% 
  mutate(country=case_when(
    country=="United States" ~ "USA",
    country=="United Kingdom" ~ "UK",
    country == "Eswatini" ~ "Swaziland",
    country == "Czechia" ~ "Czech Republic",
    country == "DR Congo" ~ "Democratic Republic of the Congo",
    country == "Republic of the Congo" ~ "Republic of Congo",
    country == "United States Virgin Islands" ~ "Virgin Islands",
    TRUE ~ country))

swiat2 <- swiat %>% 
  mutate(region=case_when(
    region %in% c("Trinidad", "Tobago") ~ "Trinidad and Tobago",
    region %in% c("Saint Vincent", "Grenadines") ~ "Saint Vincent and the Grenadines",
    region %in% c("Saint Kitts", "Nevis") ~ "Saint Kitts and Nevis",
    TRUE ~ region))

swiat_samoboj2 <- left_join(swiat2, samoboje2, by=join_by("region"=="country") )

na_regiony2 <- swiat_samoboj2 %>% 
  filter(is.na(SuicideRateCountries_2023)) %>% 
  distinct(region)

wyroznione <- samoboje %>% 
  filter(SuicideRateMaleCountries_2023==max(SuicideRateMaleCountries_2023) | SuicideRateMaleCountries_2023==min(SuicideRateMaleCountries_2023) )
wyr_swiat <- right_join(swiat2, wyroznione, by=join_by("region"=="country") ) %>% 
  group_by(region) %>% 
  summarise(szer=mean(lat), dl=mean(long))

mapa <- ggplot() + 
  geom_polygon(data = swiat_samoboj2, aes(x = long, y = lat, group = group, fill=SuicideRateMaleCountries_2023), color="black")+
  scale_fill_binned(n.breaks=10, palette = "inferno", na.value = "grey50" )+
  labs(title="Liczba samobójstw mężczyzn na 100 tysięcy mieszkańców", subtitle = "W poszczególnych krajach", x="długość geograficzna", y="szerokość geograficzna", fill="samobójstwa/100 tys. ludzi")+
  geom_label(
    data = wyr_swiat,
    aes(y= szer, x = dl, label = region), fill = "black", color = "white", alpha = 0.5, size = 3)
mapa

