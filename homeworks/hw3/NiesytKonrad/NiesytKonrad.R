library(ggplot2)
library(dplyr)
library(maps)

coffee <- read.csv('coffee-consumption-by-country-2022.csv', sep=";")
world <- map_data("world")

coffee <- coffee %>%
  mutate(country = recode(country,
                          "United States" = "USA",
                          "United Kingdom" = "UK",
                          "Lao PDR" = "Laos",
                          "Viet Nam" = "Vietnam",
                          "Korea" = "South Korea",
                          "Russian Federation" = "Russia",
                          "Czechia" = "Czech Republic",
                          "Syrian Arab Republic" = "Syria",
                          "Republic of the Congo" = "Republic of Congo",
                          "DR Congo" = "Democratic Republic of the Congo",
                          "Burma" = "Myanmar",
                          "United Republic of Tanzania" = "Tanzania",
                          "Côte d'Ivoire" = "Ivory Coast")) %>%
  filter(!country %in% c("Macau", "Hong Kong", "Trinidad and Tobago",
                         "Antigua and Barbuda", "Saint Vincent and the Grenadines",
                         "Tuvalu", "Total", "Eswatini"))

coffee_map <- world %>%
  left_join(coffee, by = c("region" = "country"))

max_countries <- coffee %>% filter(CoffeeConsumptionRateCountries_2022 == max(CoffeeConsumptionRateCountries_2022, na.rm = TRUE)) %>% pull(country)
min_countries <- coffee %>% filter(CoffeeConsumptionRateCountries_2022 == min(CoffeeConsumptionRateCountries_2022, na.rm = TRUE)) %>% pull(country)

centroids <- coffee_map %>%
  group_by(region) %>%
  summarize(long = mean(long), lat = mean(lat), 
            CoffeeConsumptionRateCountries_2022 = mean(CoffeeConsumptionRateCountries_2022, na.rm = TRUE),
            .groups = "drop")

label_data <- centroids %>% 
  filter(region %in% c(max_countries, min_countries)) %>%
  rename(country = region) %>%
  mutate(
    xend_segment = long,
    yend_segment = lat,
    long = case_when(
      country == "Chad" ~ -2,         
      country == "Afghanistan" ~ 63,  
      country == "Pakistan" ~ 85,     
      country == "Belize" ~ -95,    
      TRUE ~ long 
    ),
    lat = case_when(
      country == "Chad" ~ -1,
      country == "Afghanistan" ~ 13,
      country == "Pakistan" ~ 2,
      country == "Belize" ~ 7,
      TRUE ~ lat
    )
  )
ggplot(coffee_map, aes(x = long, y = lat, group = group, fill = CoffeeConsumptionRateCountries_2022)) +
  geom_polygon(data = filter(coffee_map, is.na(CoffeeConsumptionRateCountries_2022)),
               fill = "gray95", color = "gray70", size = 0.1) +
  geom_polygon(data = filter(coffee_map, !is.na(CoffeeConsumptionRateCountries_2022)),
               color = "black", size = 0.1) +
  coord_fixed(1.3) +
  scale_fill_gradientn(
    colors = c("#FAEBD7", "#C8A67B", "#7F4F24", "#4B2E2E"),
    na.value = "gray95",
    name = "Coffee\nconsumption\n(kg per person)",
    limits = c(0, 25),
    breaks = c(5, 10, 15, 20, 25)
  ) +
  geom_segment(
    data = label_data,
    aes(x = long, y = lat, xend = xend_segment, yend = yend_segment),
    color = "black",
    linewidth = 0.7,
    inherit.aes = FALSE
  ) +
  geom_label(
    data = label_data,
    aes(x = long, y = lat, label = paste(country, "\n", round(CoffeeConsumptionRateCountries_2022, 3))),
    color = "black",
    size = 3,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  labs(
    title = "Total coffee consumption per capita",
    subtitle = "Data for 2022, in kilograms per person",
    caption = "Source: kaggle.com (ICO Coffee Dataset)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(t = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40", margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key.height = unit(1.5, "lines"),
    legend.background = element_rect(fill = "white", color = "gray70", size = 0.5)

  )
