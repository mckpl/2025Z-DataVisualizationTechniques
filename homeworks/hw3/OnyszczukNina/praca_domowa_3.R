## Pakiety --------------------------------------------------------------------
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

## Wczytywanie danych ---------------------------------------------------------
world_map <- map_data('world') %>% filter(long <= 180)

# Dane ze strony: 
# https://www.kaggle.com/datasets/mvieira101/global-cost-of-living
cost_of_coffee <- read.csv('cost-of-living_v2.csv')

# Wybór potrzebnych kolumn oraz obliczenie średniej ceny cappuccino w poszczególnych krajach
# (niektóre kraje mają podane ceny dla kilku miast) --------------------------

cost_of_coffee <- cost_of_coffee %>% 
  select(city, country, x6) %>% #x6 - Cena cappuccino (USD)
  group_by(country) %>% 
  summarize(usd_price_mean = round(mean(x6, na.rm = TRUE), 2), .groups = 'drop')

# Dopsowanie nazw państw ----------------------------------------------------
world_map$region <- tolower(world_map$region) 
cost_of_coffee$country <- tolower(cost_of_coffee$country)

cost_of_coffee$country <- cost_of_coffee$country %>% recode("british virgin islands" = "virgin islands",
                          "congo" = "republic of congo",
                          "kosovo (disputed territory)" = "kosovo",
                          "united kingdom" = "uk",
                          "united states" = "usa",
                          "vatican city" = "vatican"
                          )

# Łączenie ramek -------------------------------------------------------------
world_coffee <- world_map %>% left_join(cost_of_coffee, by = c('region' = 'country'))

## Bazowa mapa świata --------------------------------------------------------
world_base <- ggplot(data = world_coffee, aes(x = long, y = lat, group = group))+
  geom_polygon()+
  coord_fixed(ratio = 1.3)

world_base

## Wykresik ------------------------------------------------------------------
# Nałożenie wartości na wykres bazowy ----------------------------------------
capp_map <- world_base +
  geom_polygon(data = world_coffee, aes(fill = usd_price_mean, color = ''))+
  theme_void()

capp_map

# Zabiegi kosmetyczne --------------------------------------------------------
capp_map <- capp_map +
  labs(title = 'Cappuccino prices around the world',
       subtitle = 'Data from 2022',
       fill = 'Cappuccino price [USD]')+
  scale_fill_steps(low = '#d1dbff', high = '#78022a', na.value = '#ede4df')+
  scale_color_manual(values = '#e2d3cb')+ # zmiana koloru obrysu państw
  guides(fill = guide_legend(order = 1),
         color = guide_legend('No data', override.aes = list(fill = '#ede4df'), order = 2))+ # dodanie pola w legendzie mówiącego o braku danych
  theme(title = element_text(face = 'bold'),
        plot.margin = margin(l = 25,
                             r = 25)
        )

capp_map

## Szukanie skrajnych elementów -----------------------------------------------
min_country <- world_coffee %>% arrange(usd_price_mean) %>% head(1) %>% pull(region) 
max_country <- world_coffee %>% arrange(desc(usd_price_mean)) %>% head(1) %>% pull(region)

# Sprawdzenie czy to jedyne skrajne elementy ---------------------------------
# unique(world_coffee[world_coffee$usd_price_mean == head(world_coffee[world_coffee$region == min_country, 'usd_price_mean'], 1), 'region'])
# unique(world_coffee[world_coffee$usd_price_mean == head(world_coffee[world_coffee$region == max_country, 'usd_price_mean'], 1), 'region'])

# 'Środek' każdego państwa --------------------------------------------------- 
country_middle <- world_coffee %>% 
  group_by(region, usd_price_mean) %>% 
  summarise(mean_lat = mean(lat),
            mean_long = mean(long))

# 'Środki' szukanych państw -------------------------------------------------
min_max_capp <- country_middle %>% 
  filter(region %in% c(min_country, max_country)) %>% 
  mutate(region = stringr::str_to_title(region))

## Nałożenie punktów na mapę -------------------------------------------------
capp_map <- capp_map +
  geom_point(data = min_max_capp, aes(x = mean_long, y = mean_lat), inherit.aes = FALSE, color = 'black')+
  ggrepel::geom_text_repel(data = min_max_capp, aes(x = mean_long, y = mean_lat, label = paste0(region, ': ', usd_price_mean, '$')), inherit.aes = FALSE, 
                           color = 'black', fontface = 'bold',
                           segment.color = 'black', segment.size = 0.5, nudge_y = 15)

capp_map                                  

ggsave("swiatowe_ceny_cappuccino.pdf", plot = capp_map, bg = "transparent")
