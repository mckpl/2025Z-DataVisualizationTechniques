library(ggplot2)
library(dplyr)
library(tidyr)
library(mapdata)
library(ggnewscale)

rain <- read.csv("average-precipitation-per-year.csv")

rain_f <- rain %>% 
  filter(Year == 2024) %>% 
  mutate(Entity = recode(Entity,
                         "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Czechia" = "Czech Republic",
                         "Democratic Republic of Congo" = "Democratic Republic of the Congo",
                         "Congo" ="Republic of Congo"))


world <- left_join(map_data("world"), rain_f, by = c("region" = "Entity"), 
                   relationship = "many-to-one")

max_val <- max(rain_f$Annual.precipitation, na.rm = TRUE)
min_val <- min(rain_f$Annual.precipitation, na.rm = TRUE)

extr <- rain_f %>%
  filter(Annual.precipitation %in% c(max_val, min_val)) %>%
  mutate(extremum = ifelse(Annual.precipitation == max_val, "Max", "Min")) %>% 
  mutate(label_legend = paste0(Entity," ", round(Annual.precipitation, 3),  " (", extremum, ")"))

labels <- world %>%
  group_by(region) %>%
  summarise(long = mean(range(long)), lat = mean(range(lat))) %>%
  inner_join(extr, by = c("region" = "Entity"))


ggplot() + 
  geom_polygon(data = world,
               aes(x=long, y = lat, group = group, fill = Annual.precipitation),
               color = "grey30", linewidth = 0.1)+
  scale_fill_gradientn(colors = c("#FFFFCC", "#A6D96A", "#3288BD", "#5E4FA2"), 
                       na.value = "grey90", name = "Opady [mm/rok]") +
  ggnewscale::new_scale_fill() +
  geom_point(data = labels,
             aes(x = long, y = lat, shape = label_legend, fill = label_legend),
             size = 2) +
  scale_shape_manual(values = c("Egypt 9.76 (Min)" = 25, "Solomon Islands 4365.58 (Max)" = 24)) +
  scale_fill_manual(values = c("Egypt 9.76 (Min)" = "red", "Solomon Islands 4365.58 (Max)" = "blue"), guide = "none") +
  coord_quickmap() +
  theme_minimal()+
  theme(
    legend.key.size = unit(0.6, "lines"),
    legend.text = element_text(size = 8), 
    legend.title = element_text(size = 9)) +
  labs(title = "Roczne opady deszczu w 2024 roku", shape = "Min/Max [mm/rok]")
