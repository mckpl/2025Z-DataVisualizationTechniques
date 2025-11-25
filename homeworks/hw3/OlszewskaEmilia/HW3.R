library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(sf)
sf_use_s2(FALSE)

beer_consumption <- read.csv("/Users/emilia/Downloads/zadanie_domowe3/beer-consumption.csv")
beer_country<- beer_consumption %>%
  rename(BeerPerCapita = BeerConsumptionRate_2022,
         BeerAnually=BeerConsumptionTotal_2022)%>%
  mutate(country_map = recode(country,
                              "United States" = "USA",
                              "United Kingdom" = "UK",
                              "Czechia" = "Czech Republic",
                              .default = country
  ))

world <- map("world", plot = FALSE, fill = TRUE)

world_sf <- st_as_sf(world)

unique(world_sf$ID)

beer_world <- world_sf%>%
  left_join(beer_country, by = c('ID'='country_map'))

europe_countries <- c(
  "Albania", "Andorra", "Austria", "Belgium", "Bosnia and Herzegovina",
  "Bulgaria", "Belarus", "Croatia", "Czech Republic", "Denmark",
  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Liechtenstein",
  "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro",
  "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal",
  "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia",
  "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "UK",
  "Vatican"
)
beer_europe <- beer_world %>%
  filter(ID %in% europe_countries)

extrema <- beer_europe %>%
  filter(!is.na(BeerPerCapita)) %>%
  slice_max(BeerPerCapita, n = 1, with_ties = FALSE) %>%
  bind_rows(
    beer_europe %>%
      filter(!is.na(BeerPerCapita)) %>%
      slice_min(BeerPerCapita, n = 1, with_ties = FALSE)
  )

beer_europe_centers <- st_centroid(beer_europe) %>% 
  cbind(st_coordinates(.))

extrema_with_coords <- beer_europe_centers %>%
  filter(ID %in% extrema$ID)

extrema_with_coords$ID[extrema_with_coords$ID == "Czech Republic"] <- "Czech\nRepublic"

# ---
ggplot(beer_world) +
  geom_sf(aes(fill = BeerPerCapita), color = "grey30", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#FDE2E4",
    high = '#99004C',
    name = "Beer (liters per capita)",
    na.value = "grey90"
  ) +
  geom_text(
    data = extrema_with_coords,
    aes(x = X, y = Y, label = ID),
    size = 3,
    fontface = "bold",
    colour = '#1B1F5A',   
    lineheight = 0.8,
    family = "Times"
  )+
  coord_sf(xlim = c(-25, 42), ylim = c(35, 72), expand = FALSE) +
  theme_void() +
  labs(
    title    = "Per Capita Beer Consumption in Europe",
    subtitle = "Data Source: 2022"
  )+
  theme(plot.title = element_text(hjust = 0.5,size = 20,face = "bold",family = "Times" ),
        plot.subtitle = element_text(hjust = 0.5,size = 14,face = "italic",family = "Times"),
        legend.title = element_text(family = "Times",size = 12,face = "bold"),
        legend.text = element_text(family = "Times",size = 10)
  )
