library(ggplot2)
library(dplyr)
library(maps)


gdp_data <- read.csv("us_gdp.csv") %>%
  rename(state = GeoName, gdp = X2025.Q1) %>%
  select(state, gdp)

gdp_stats <- gdp_data %>%
  summarise(max_state = state[which.max(gdp)],
            min_state = state[which.min(gdp)],
            max_gdp = max(gdp),
            min_gdp = min(gdp))

map_data_final <- map_data("state") %>%
  mutate(state = tools::toTitleCase(region)) %>%
  left_join(gdp_data, by = "state") %>%
  mutate(category = case_when(
    state == gdp_stats$max_state ~ "Najwyższe",
    state == gdp_stats$min_state ~ "Najniższe",
    TRUE ~ "Pozostałe"
  ))

label_positions <- map_data_final %>%
  filter(state %in% c(gdp_stats$max_state, gdp_stats$min_state)) %>%
  group_by(state) %>%
  summarise(long = mean(range(long)), lat = mean(range(lat)), .groups = "drop") %>%
  mutate(long_label = long + c(-4.5, 1), lat_label = lat + c(1.5, 2))

mapa_zarobki <- ggplot(map_data_final, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = gdp), color = "white", linewidth = 0.5) +
  geom_point(data = label_positions, aes(x = long, y = lat,group = NULL)) +
  geom_label(data = label_positions, aes(x = long_label, y = lat_label, label = state,,group = NULL),
             size = 3, fontface = "bold", fill = "white", color = "black") +
  geom_segment(data = label_positions,
               aes(x = long_label + (-.5), y = lat_label - 0.5, xend = long, yend = lat,group = NULL)) +
  scale_fill_gradientn(colors = c("blue", "white", "red"), name = "PKB [$]",
                       labels = scales::comma) +
  coord_fixed(1.3) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 8),
    legend.text = element_text(size = 6),
    plot.title = element_text(face = "bold", size = 12),
    plot.caption = element_text(size = 10)
  ) +
  labs(
    title = "Mapa stanów zjednoczonych z wyróżnionymi stanami z najwyższym i najniższym PKB, dane na kwartał",
    caption = paste0("Max PKB: ", scales::comma(gdp_stats$max_gdp),
                     ", Min PKB: ", scales::comma(gdp_stats$min_gdp))
  )

mapa_zarobki

