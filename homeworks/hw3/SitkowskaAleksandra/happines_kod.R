library(ggplot2)
library(maps)
library(dplyr)



world_map <- map_data("world") %>% 
  rename(region = region)

happiness <- read.csv2("happiness.csv")

happiness <- happiness %>%
  select(Country.name, Ladder.score) %>% 
  mutate(
    Country.name = case_when(
      Country.name == "United States" ~ "USA",
      Country.name == "United Kingdom" ~ "UK",
      Country.name == "Czechia" ~ "Czech Republic",
      Country.name == "State of Palestine" ~ "Palestine",
      Country.name == "Congo (Brazzaville)" ~ "Republic of Congo",
      Country.name == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
      Country.name == "Turkiye" ~ "Turkey",
      Country.name == "Taiwan Province of China" ~ "Taiwan",
      TRUE ~ Country.name
    )
  )

data_final <- world_map %>%
  left_join(happiness, by = c("region" = "Country.name"))

## happiest and least happy countries
max_score <- data_final %>%
  arrange(desc(Ladder.score)) %>%
  slice(1)

min_score <- data_final %>%
  arrange(Ladder.score) %>%
  slice(1)

data_max <-  data_final %>%
  filter(region == max_score$region)

data_min <- data_final %>%
  filter(region == min_score$region)


cent_max <- data_max %>%
  summarise(
    long = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    region = first(region),
    Ladder.score = first(Ladder.score)
  )


cent_min <- data_min %>%
  summarise(
    long = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    region = first(region),
    Ladder.score = first(Ladder.score)
  )

points_to_annotate <- bind_rows(cent_max, cent_min) %>%
  mutate(
    label = paste0(region, "\n", "Index: ", round(Ladder.score, 2)),
    label_long = c(-35, 80),
    label_lat = c(50, -10))

  


ggplot(data_final, aes(x = long, y = lat, group = group, fill = Ladder.score)) +
  geom_polygon(color = "gray", size = 0.1) +
  scale_fill_viridis_c(
    option = "C",         
    limits = c(0, 10),    
    na.value = "lightgray", 
    name = "Happiness\nindex"
  ) +
  geom_segment(
    data = points_to_annotate,
    aes(x = long, y = lat, xend = label_long, yend = label_lat),
    inherit.aes = FALSE,
    color = "black",        
    linetype = "solid",     
    size = 0.4) +
  geom_label(
    data = points_to_annotate,
    aes(x = label_long, y = label_lat, label = label),
    inherit.aes = FALSE,
    fill = "white",             
    color = "black",            
    size = 3.5,
    label.padding = unit(0.2, "lines"), 
    label.r = unit(0.1, "lines")) +
  geom_point(
    data = points_to_annotate,
    aes(x = long, y = lat), 
    inherit.aes = FALSE,    
    color = "black",        
    size = 1) +
  theme_void()  +
  coord_fixed(ratio = 1.3)+
  labs(
    title = "World's happiness index in 2023",
    subtitle = "index = 0 - least happy, index = 10 - happiest",
    caption = "Source: https://data.worldhappiness.report") +
  theme(
    plot.title = element_text(
      size = 17,
      face = "bold"))
