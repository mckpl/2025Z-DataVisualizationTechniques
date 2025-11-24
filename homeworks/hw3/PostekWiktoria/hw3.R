library(jsonlite)
library(dplyr)
library(ggplot2)
library(maps)
library(ggrepel)
library(scales)

df <- read.csv("https://ourworldindata.org/grapher/international-tourist-trips.csv?v=1&csvType=full&useColumnShortNames=true")
metadata <- fromJSON("https://ourworldindata.org/grapher/international-tourist-trips.metadata.json?v=1&csvType=full&useColumnShortNames=true")
# dane ze strony: https://ourworldindata.org/grapher/international-tourist-trips
head(metadata)
head(df)

df_tourism <- df %>% 
  rename(country = Entity, year = Year, trips_count = in_tour_arrivals_ovn_vis_tourists) %>% 
  filter(year == 2019)
head(df_tourism)

world <- map_data("world")

europe_tourism <- world %>% 
  left_join(df_tourism, by = c("region" = "country")) %>% 
  filter(long > -25, long < 40, lat > 35, lat< 71)  # ograniczam dane do tych znajdujących się w Europie

# czyszczenie danych----
europe_countries <- unique(europe_tourism$region)

df_europe_tourism <- df_tourism %>% 
  filter(country %in% europe_countries)

# kraje z mapy world, których nie ma w df_tourism
setdiff(europe_tourism$region, df_tourism$country)

# kraje w df_tourism, których nie ma w mapie world
setdiff(df_tourism$country, europe_tourism$region)

# zauważyłam, że braki w danych nie wynikały jedynie z braków w ramce danych df_tourism, ale również z różnego nazewnictwa krajów w dwóch ramkach

df_tourism2 <- df_tourism %>% 
  mutate(country = dplyr::recode(country,
                                 "United Kingdom" = "UK",
                                 "Czechia" = "Czech Republic"))
europe_tourism2 <- world %>% 
  left_join(df_tourism2, by = c("region" = "country")) %>% 
  filter(long > -25, long < 43, lat > 35, lat< 71)
# ----

id <- df_tourism2 %>% 
  filter(country %in% europe_countries) %>% 
  summarise(min = min(trips_count),
            max = max(trips_count))

min_max_points <- df_tourism2 %>% 
  filter(trips_count %in% c(id$min, id$max))

label_points <- europe_tourism2 %>% 
  filter(region %in% min_max_points$country) %>% 
  group_by(region) %>% 
  summarise(long = mean(long),
            lat = mean(lat)) %>% 
  left_join(min_max_points, by = c("region" = "country"))
  
map <- ggplot(data = europe_tourism2) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = trips_count),  color = "black")+
  coord_fixed(1.5)+
  scale_fill_gradient(low = "lightblue", high= "darkblue", labels = label_number(big.mark = " ", accuracy = 1e6), limits = c(0, id$max))+
  theme_minimal()+
  labs(title = "Most frequently visited European countries by tourists in 2019", 
       subtitle = "Number of international tourist arrivals, source: Our World in Data",
       fill = "Number of tourist arrivals",
       x = "Longitude",
       y = "Latitude")
map

map_final <- map+
  geom_point(data = label_points, aes(x = long, y = lat), color = "black", size = 2)+
  geom_label_repel(data = label_points %>% 
                     filter(region == "France"),
                  aes(x = long, y= lat, label = paste0(region, "\n", scales::comma(trips_count, big.mark = " "), " tourists")),
                  nudge_x = -20,
                  nudge_y = 0,
                  size = 3, 
                  fontface = "bold", 
                  min.segment.length = 0, 
                  label.size = 0.2)+
  geom_label_repel(data = label_points %>% 
                     filter(region == "Liechtenstein"),
                   aes(x = long, y= lat, label = paste0(region, "\n", scales::comma(trips_count, big.mark = " ", accuracy= 1), " tourists")),
                   nudge_x = -30,
                   nudge_y = 4,
                   size = 3, 
                   fontface = "bold", 
                   min.segment.length = 0, 
                   label.size = 0.2)+
  theme_void()+
  theme(plot.title = element_text(face = "bold", size = 15))
map_final

#szare tło krajów wynika z braków danych w ramce df_tourism 

