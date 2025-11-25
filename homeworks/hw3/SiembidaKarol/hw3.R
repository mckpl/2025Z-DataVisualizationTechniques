library(ggplot2)
library(dplyr)
library(maps)
library(mapdata)
library(tidyr)

world <- map_data("world") %>% 
  filter(long <= 180,
         region != "Antarctica")

# źródło: https://en.wikipedia.org/wiki/List_of_countries_by_Internet_connection_speeds#cite_note-ookla-2025-01-1

internetspeed <- read.csv("internetspeeddata.csv", sep = ";", header = TRUE, encoding = "UTF-8")
wrongnames <- setdiff(internetspeed$country, unique(world$region))

internetspeed <- internetspeed %>% 
  mutate(country = recode(country,
                          "United States" = "USA",
                          "United Kingdom" = "UK",
                          "DR Congo" = "Democratic Republic of the Congo",
  ))

trinidad_tobago <- tibble(
  country = c("Trinidad", "Tobago"),
  speed = internetspeed$speed[internetspeed$country == "Trinidad and Tobago"]
)
antigua_barbuda <- tibble(
  country = c("Antigua", "Barbuda"),
  speed   = internetspeed$speed[internetspeed$country == "Antigua and Barbuda"]
)

internetspeed <- bind_rows(
  internetspeed,
  trinidad_tobago,
  antigua_barbuda
)

world <- world %>%
  left_join(internetspeed, by = c("region" = "country"))

max_speed <- internetspeed %>% 
  filter(speed == max(speed))
min_speed <- internetspeed %>% 
  filter(speed == min(speed))

max_speed <- world %>% 
  filter(region == max_speed$country) %>% 
  summarise(long = mean(long), lat = mean(lat)) %>%
  mutate(info = paste0("Fastest:", "\n", max_speed$country, ", ", max_speed$speed, " Mbit/s"))
min_speed <- world %>% 
  filter(region == min_speed$country) %>% 
  summarise(long = mean(long), lat = mean(lat)) %>%
  mutate(info = paste0("Slowest:", "\n",min_speed$country, ", ", min_speed$speed, " Mbit/s"))


hw3 <- ggplot(data = world) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = speed), color = "gray80", linewidth = 0.25 ) +
  scale_fill_gradient(
    low  = "#ff001c",
    high = "#0045ff",
    na.value = "grey50",
    name = "Speed (Mbit/s)",
  ) +
  geom_text(data = max_speed,aes(x = long, y = lat, label = info), fontface = "bold", size = 4) +
  geom_text(data = min_speed,aes(x = long, y = lat, label = info), fontface = "bold", size = 4) +
  coord_map("mercator") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.title = element_text(vjust = 0.85))+
  labs(title = "Median download speed (Mbit/s) by country",
       subtitle = "Data for January 2025, ranked by Speedtest.net")+
  annotate("rect", xmin = -2.3, xmax = 5, ymin = -80,  ymax = -75.5, fill = "grey50") +
  annotate("text", x = -24, y = -76.3, label = "No Data", hjust = 0,size = 4)

ggsave("hw3.png", plot = hw3, width = 16, height = 9, dpi = 300)



