library(wbstats)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggthemes)
library(ggtext)

# mapa przedstawiająca jaki odsetek ludności danego kraju ma dostęp do internetu
# źródło danych: World Bank
# https://data.worldbank.org/indicator/IT.NET.USER.ZS

data_internet <- wb_data(
  indicator = "IT.NET.USER.ZS",    
  start_date = 2000,
  end_date = as.numeric(format(Sys.Date(), "%Y")),
  return_wide = FALSE
)


internet_latest <- data_internet %>%
  filter(!is.na(value)) %>%
  group_by(iso3c, country) %>%
  slice_max(date, n = 1) %>%
  ungroup()


min_val <- min(internet_latest$value, na.rm = TRUE)
max_val <- max(internet_latest$value, na.rm = TRUE)

countries_min <- internet_latest %>% filter(value == min_val)
countries_max <- internet_latest %>% filter(value == max_val)


world <- ne_countries(scale = "medium", returnclass = "sf")

world_map <- world %>%
  left_join(internet_latest, by = c("iso_a3" = "iso3c"))


plot <- ggplot(world_map) +
  
  geom_sf(aes(fill = value), color = "gray40", size = 0.2) +
  
  scale_fill_gradientn(
    colours = c("#053061", "#2166ac", "#67a9cf", "#d1e5f0",
                "#fddbc7", "#ef8a62", "#b2182b", "#67001f"),
    name = "% populacji z dostępem\ndo internetu",
    na.value = "lightgray"
  ) +
  
  labs(
    title     = "Dostęp do internetu na świecie",
    subtitle  = "Udział użytkowników Internetu (% populacji) – najnowszy dostępny rok",
    caption   = paste0(
      "Źródło danych: World Bank (WDI)\n",
      "Min: ", countries_min$country, " – ", round(min_val, 1), "%   |   ",
      "Max: ", countries_max$country, " – ", round(max_val, 1), "%"
    )
  ) +
  
  theme_minimal(base_size = 11) +
  theme(

    plot.title = element_text(size = 10, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 9, hjust = 0),
    plot.caption = element_text(size = 7, hjust = 0),
    
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 8),
    
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks   = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    panel.grid = element_blank(),
    
    plot.margin = margin(5, 5, 5, 5)
  ) +
  
  coord_sf(expand = FALSE)


print(plot)


ggsave("mapa_dostep_internet_duza.png", plot, width = 14, height = 8)

