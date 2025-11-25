library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(sf)
library(patchwork)
library(ggrepel)

#https://worldpopulationreview.com/country-rankings/which-country-eats-the-most-chocolate
choco <- read.csv(file = "~/Downloads/chocolate_per_capita.csv",
               encoding = "UTF-8")

total_choco <- read.csv(file = "~/Downloads/total_chocolate.csv",
                        encoding = "UTF-8")

world <- map_data("world")

choco_map <- choco %>%                       
  rename(choco_kg = ChocolateConsumptionGramsPerCapita_2022) %>%
  mutate(
    country_map = recode(country,
                         "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Czechia" = "Czech Republic",
                         .default = country
    )
  )

world_choco <- world %>%
  left_join(choco_map, by = c("region" = "country_map")) %>% 
  filter(region != "Antarctica",
         region != "Greenland")

#https://www.icco.org/wp-content/uploads/Production_QBCS-LI-No.-1.pdf
producers <- tibble::tribble(
  ~country,            ~production_1000t,
  "Cameroon",               270,
  "Cote d'Ivoire",         2241,
  "Ghana",                  654,
  "Nigeria",                315,
  "Brazil",                 220,
  "Ecuador",                454,
  "Indonesia",              160,
  "Papua New Guinea",        43
)

data("world.cities", package = "maps")

prod_coords <- world.cities %>%
  mutate(country_match = dplyr::recode(
    country.etc,
    "Ivory Coast" = "Cote d'Ivoire",
    .default = country.etc
  )) %>%
  group_by(country_match) %>%
  slice_max(pop, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  right_join(producers, by = c("country_match" = "country")) %>%
  mutate(
    country_lab = country_match 
  )

top10 <- choco %>%
  arrange(desc(ChocolateConsumptionGramsPerCapita_2022)) %>%
  filter(country != "Total") %>% 
  head(5)%>% 
  ggplot(aes(x = reorder(country, ChocolateConsumptionGramsPerCapita_2022),
             y = ChocolateConsumptionGramsPerCapita_2022)) +
  geom_col(fill = "#F7E7D2") +
  coord_flip() +
  labs(
    title = "Highest Consumers of Chocolate",
    x = "Country",
    y = "Consumption per capita"
  ) +
  theme(
    panel.grid    = element_blank(),
    legend.position = "bottom",
    plot.title    = element_text(face = "bold", size = 10,hjust = 0),
    panel.background = element_rect(fill = "white", colour = NA)
  )

min10 <- choco %>%
  arrange(desc(ChocolateConsumptionGramsPerCapita_2022)) %>%
  filter(country != "Total") %>% 
  tail(5)%>% 
  ggplot(aes(x = reorder(country, ChocolateConsumptionGramsPerCapita_2022),
             y = ChocolateConsumptionGramsPerCapita_2022)) +
  geom_col(fill = "#F7E7D2") +
  coord_flip() +
  labs(
    title = "Lowest Consumers of Chocolate",
    x = "Country",
    y = "Consumption per capita"
  ) +
  theme(
    panel.grid    = element_blank(),
    legend.position = "bottom",
    plot.title    = element_text(face = "bold", size = 10),
    panel.background = element_rect(fill = "white", colour = NA),
  )

top_producers <- prod_coords %>% 
  arrange(desc(production_1000t)) %>%
  head(5)%>% 
  ggplot(aes(x = reorder(country_lab, production_1000t),
             y = production_1000t)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(
    title = "Top Producers of Cocoa",
    x = "Country",
    y = "Thousands tons produced"
  ) +
  theme(
    panel.grid    = element_blank(),
    legend.position = "bottom",
    plot.title    = element_text(face = "bold", size = 10,hjust = 0),
    panel.background = element_rect(fill = "white", colour = NA),
  )

p_main <- world_choco %>% 
  ggplot( aes(long, lat, group = group)) +
  geom_polygon(aes(fill = choco_kg),
               color = "white", size = 0.2) +
  scale_fill_gradient(
    low = "#F7E7D2",
    high = "#6A3B1E",
    na.value = "grey90",
    name = "Chocolate (kg/capita)"
  ) +
  geom_hline(yintercept = 23.5, color = "chocolate4", size = 0.4, linetype = "dashed") +
  geom_hline(yintercept = -23.5, color = "chocolate4", size = 0.4, linetype = "dashed") +
  annotate("text", x = -160, y = 25.5,  label = "Tropic of Cancer",       hjust = 0, size=3, color="chocolate4", fontface = "bold") +
  annotate("text", x = -160, y = -21.5, label = "Tropic of Capricorn", hjust = 0, size=3, color="chocolate4", fontface = "bold") +
  geom_point(
    data = prod_coords,
    aes(x = long, y = lat, size = production_1000t),
    inherit.aes = FALSE,
    color = "darkred",
    alpha = 0.7,
    show.legend = TRUE
  ) +
  geom_text_repel(
    data = prod_coords,
    aes(x = long, y = lat, label = country_match),
    inherit.aes = FALSE,
    vjust = -0.8,
    size = 3,
    color = "darkred",
    fontface = "bold",
    max.overlaps = Inf,
    box.padding = 1
  ) +                                                
  scale_size_continuous(
    range = c(2.5, 6),
    name  = "Cocoa production\n(thousand tons)"
  ) +                                               
  guides(
    fill = guide_colorbar(order = 1),
    size = guide_legend(order = 2)
  ) +
  labs(
    title   = "Chocolate consumption (kg per capita)",
    subtitle= "Did you know that cocoa beans grow only between Tropic of Cancer and Capricorn?",
    caption = "Author: Aleksandra Mulewicz\nSources: WPR / own calculations · Cocoa production: ICCO 2022/23",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid    = element_blank(),
    legend.position = "bottom",
    plot.title    = element_text(face = "bold", size = 13, hjust=0.5),
    plot.subtitle = element_text(face = "italic", size = 9, hjust=0.5),
    legend.title = element_text(face = "bold", size = 8),
    legend.text  = element_text(size = 8)
  )
  
p_main + (top10 / top_producers/min10) +
  plot_layout(
    ncol = 2, widths = c(0.7, 0.30))

