library(ggplot2)
library(dplyr)
library(maps)
library(ggrepel)
library(scales)

data("USArrests")

df <- tibble(state = row.names(USArrests),
             region = casefold(state, upper = FALSE),
             urban = USArrests$UrbanPop) %>%
  mutate(urban = urban / 100)

states <- map_data("state")

mean_states <- states %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat)) %>%
  inner_join(df, by = join_by(region))

highest <- mean_states %>%
  slice_max(urban) %>%
lowest <- mean_states %>%
  slice_min(urban)

states %>%
  left_join(df, by = join_by(region)) %>%
  ggplot() +
  coord_map("albers", 25, 50) +
  geom_polygon(aes(x = long, y = lat, fill = urban, group = group), color = "white", linewidth = 0.3) +
  scale_fill_binned(palette = c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#0c2c84'),
                    labels = percent_format()) +
  theme_void() +
  geom_label_repel(data = highest, aes(x = long, y = lat, label = state), box.padding = 1) +
  geom_label_repel(data = lowest, aes(x = long, y = lat, label = state), box.padding = 1) +
  labs(title = "Urbanization in the United States",
       subtitle = "by state",
       fill = "Urbanization\nfactor") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))
