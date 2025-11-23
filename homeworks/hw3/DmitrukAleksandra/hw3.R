library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(SmarterPoland)
library(maps)
library(mapdata)
library(mapproj)
library(sf)
library(rnaturalearth)
library(extrafont)
library(ggrepel)

font_import(pattern = "cambria")
loadfonts()


df <- read.csv("pay_gap_Europe.csv")

df <- df %>% 
  filter(Year == 2021) %>% 
  select(1:6)


world_map <- ne_countries(scale = 50, returnclass = 'sf', continent = "europe")
europe_box <- st_bbox(c(xmin = -25, xmax = 45, ymax = 70, ymin = 29), crs = st_crs(4326))

world_map_cropped <- world_map %>% 
  st_crop(europe_box) %>% 
  rename(Country = name_long)

world_map_cropped <- world_map_cropped %>% left_join(df, by = "Country")

min_max_countries <- world_map_cropped %>%
  filter(!is.na(Business)) %>%
  filter(Business == min(Business) | Business == max(Business)) %>%
  mutate(label = paste0(Country, "\n", Business, "%"))

min_max_coords <- min_max_countries %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(lon = X, lat = Y) 

min_max_countries <- bind_cols(min_max_countries, min_max_coords)

p <- ggplot(world_map_cropped) + 
  geom_sf(aes(fill = Business, color = "--"))+
  geom_label_repel(data = min_max_countries,
                   aes(x = lon, y = lat, label = label),
                   alpha = 0.7,
                   size = 3,
                   family = "Cambria",
                   segment.color = 'black', 
                   min.segment.length = 0, 
                   nudge_x = c(5, -5),  
                   nudge_y = c(1, -1),   
                   force = 5,
                   box.padding = unit(0.5, "lines"),
                   label.padding = unit(0.2, "lines")) +
  theme_void()+
  labs(title = "Gender Pay Gap in Europe in Business in 2021",
       fill = "Pay gap in Business economy (%)",
       caption = "Source: https://www.kaggle.com/datasets/gianinamariapetrascu/gender-pay-gap-europe-2010-2021",
       subtitle = "The unadjusted gender pay gap (GPG) represents the difference between average gross hourly\nearnings of male paid employees and of female paid employees as a percentage of average gross\nhourly earnings of male paid employees. ")+
  scale_fill_gradient(low = "lightpink",
                      high = "steelblue",
                      na.value = "grey")+
  scale_colour_manual(name = NULL,
                      values = "#dde0db",
                      labels = "No Data Available") +
  theme(text = element_text(size = 12, family = "Cambria"),
        plot.subtitle = element_text(size = 7,
                                     color = "#4e4d47",
                                     margin = margin(b = -0.1, t = 0.4, l = 0.5,
                                                     unit = "cm")),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        plot.title = element_text(size = 17,
                                  hjust = 0.01,
                                  margin = margin(b = -0.3, t = 0.4, l = 0.5,
                                                  unit = "cm")),
        legend.position = c(0.50,0.04),
        legend.title = element_text(size = 9,
                                    margin = margin(b = 0.55, r = 0.4,
                                                    unit = "cm")),
        legend.text = element_text(size = 9),
        legend.key.height = unit(0.3, "cm"),
        legend.box = "horizontal",
        plot.caption = element_text(size = 6.5,
                                    color = "#4e4d47",
                                    hjust = 0.01))+
  guides(fill = guide_colorbar(direction = "horizontal",
                               order = 1),
         color = guide_legend(direction = "horizontal",
                              order = 2, 
                              keywidth = 0.5, 
                              keyheight = 0.5,
                              override.aes = list(fill = "grey", size = 0.5)))

p

ggsave(p, filename = "map.png", device = "png",
       height = 8, width = 6, dpi = 300, bg = "transparent")