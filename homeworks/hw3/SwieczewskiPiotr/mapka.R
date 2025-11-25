library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(ggrepel)


happiness <- read.csv("C:/Users/Piotr/Desktop/twd_/Human-Progress-World-Happiness-Report.csv")

head(happiness)

happiness <- happiness %>%
  mutate(country_name = case_when(
    country_name == "United States" ~ "USA",
    country_name == "United Kingdom" ~ "UK",
    TRUE ~ country_name
  ))

world <- map_data("world") %>%
  filter(region != "Antarctica") %>%
  left_join(happiness, by = c("region" = "country_name")) %>%
  mutate(normalized2024 = (X2024 - min(X2024, na.rm = TRUE)) /
           (max(X2024, na.rm = TRUE) - min(X2024, na.rm = TRUE)))

happiness <- happiness[,c("country_name","X2014","X2024")]
happiness$X2014 <- as.numeric(happiness$X2014)

median_normalized <- median(world$normalized2024, na.rm = TRUE)
min_region <- world %>% filter(X2024 == min(X2024, na.rm = TRUE)) %>% pull(region) %>% unique()
max_region <- world %>% filter(X2024 == max(X2024, na.rm = TRUE)) %>% pull(region) %>% unique()

min_point <- world %>%
  filter(region %in% min_region) %>%
  summarize(
    region = unique(region),
    lon = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    X2024 = mean(X2024, na.rm = TRUE)
  )

max_point <- world %>%
  filter(region %in% max_region) %>%
  summarize(
    region = unique(region),
    lon = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    X2024 = mean(X2024, na.rm = TRUE)
  )

worldplot <- ggplot(data = world, aes(x = long, y = lat, group = group, fill = X2024)) +
  geom_polygon(color = "gray40", size = 0.2) +
  coord_fixed(1.3, expand = FALSE) +
  scale_fill_gradientn(
    colors = c("darkred","yellow","darkgreen"),
    values = c(0, median_normalized, 1),
    na.value = "grey70"
  ) +
  geom_point(data = min_point, aes(x = lon, y = lat), color = "red", size = 3, inherit.aes = FALSE) +
  geom_point(data = max_point, aes(x = lon, y = lat), color = "green", size = 3, inherit.aes = FALSE) +
  geom_label_repel(
    data = min_point,
    aes(x = lon, y = lat, label = paste(region, "\n", X2024)),
    color = "red",
    fill = "black",
    box.padding = 0.5,
    segment.color = "red",
    segment.size = 0.5,
    nudge_x = 40, nudge_y = 10,
    inherit.aes = FALSE
  ) +
  geom_label_repel(
    data = max_point,
    aes(x = lon, y = lat, label = paste(region, "\n", X2024)),
    color = "green",
    fill = "black",
    box.padding = 0.5,
    segment.color = "green",
    segment.size = 0.5,
    nudge_x = -40, nudge_y = 10,
    inherit.aes = FALSE
  ) +
  theme(
    panel.background = element_rect(fill="black"),
    plot.background = element_rect(fill="black"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    legend.background = element_rect(fill="black"),
    legend.text = element_text(color="white"),
    legend.title = element_text(color="white", hjust=0.5),
    plot.title = element_text(color="white", hjust=0.5)
  ) +
  labs(
    fill = "Index",
    title = "Państwa wg. Hapinness index w 2024"
  )

worldplot

