library(sf)
library(ggplot2)
library(dplyr)
library(ggrepel) 

raw_data <- read_delim("~/Desktop/TWD/HW3/Stakeholder_Traffic.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

df <- raw_data %>%
  select(
    Country = Y2D_CTRY_NAME, 
    Value_Str = Y2D_DIF_2019_PERC
  ) %>%
  filter(!is.na(Country)) %>% 
  mutate(
    Value = as.numeric(trimws(gsub("%", "", Value_Str)))
  ) %>% 
  mutate(Country = case_when(
    Country == "Türkiye" ~ "Turkey",
    Country == "UK" ~ "United Kingdom",
    Country == "Czech Republic" ~ "Czechia",
    TRUE ~ Country
  ))

top5 <- df %>%
  arrange(desc(Value)) %>% 
  head(5)

df_final <- df %>%
  mutate(Category = case_when(
    Country %in% top5$Country ~ "TOP 5",
    TRUE ~ "Rest of Europe" 
  ))

labels_data <- top5 %>%
  mutate(Label_Text = paste0(
    Country, "\n", 
    scales::number_format(accuracy = 0.1, prefix = "+")(Value), "%"
  ))

world_map <- st_read("~/Desktop/TWD/HW3/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp" , quiet = TRUE)

europe_map <- world_map %>%
  filter(CONTINENT == "Europe" | ADMIN %in% c("Turkey", "Cyprus", "Georgia", "Armenia", "Azerbaijan"))

map_data <- europe_map %>%
  left_join(df_final, by = c("ADMIN" = "Country")) %>%
  mutate(Category = ifelse(is.na(Category), "No data", Category))

labels_geo <- map_data %>%
  filter(ADMIN %in% labels_data$Country) %>%
  select(ADMIN, geometry) %>%
  inner_join(labels_data, by = c("ADMIN" = "Country"))

labels_geo <- cbind(labels_geo, st_coordinates(st_centroid(labels_geo)))

labels_geo <- labels_geo %>%
  mutate(
    nudge_x = case_when(
      ADMIN == "Albania" ~ -10,  
      ADMIN == "Armenia" ~ 12,   
      ADMIN == "Bosnia and Herzegovina" ~ -22,
      ADMIN == "Georgia" ~ 10,  
      ADMIN == "Moldova" ~ 7,   
      TRUE ~ 0
    ),
    nudge_y = case_when(
      ADMIN == "Albania" ~ -2,   
      ADMIN == "Armenia" ~ -5,   
      ADMIN == "Georgia" ~ 6,    
      ADMIN == "Moldova" ~ 4,    
      TRUE ~ 0
    )
  )

map_data %>%  
  ggplot() +
  geom_sf(aes(fill = Category), color = "black", linewidth = 0.08) +
  geom_label_repel(
    data = labels_geo,
    aes(x = X, y = Y, label = Label_Text),
    nudge_x = labels_geo$nudge_x, 
    nudge_y = labels_geo$nudge_y,
    color = "black",
    fontface = "bold",
    size = 2.5, 
    min.segment.length = 0,  
    point.padding = 0, 
    segment.color = "black",
    segment.size = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "TOP 5" = "#51a354",         
      "Rest of Europe" = "#deebf7",
      "No data" = "darkgrey" 
    ),
    name = "Category"
  ) +
  coord_sf(xlim = c(-25, 47), ylim = c(34, 72), expand = FALSE) + 
  labs(
    title = "Countries with the Biggest Increase\n in Air Traffic per day vs. Pre-Pandemic",
    subtitle = "Top 5 countries by percentage increase in daily flights",
    caption = "Source: https://ansperformance.eu/traffic/ EUROCONTROL"
  ) +
  theme_void() + 
  theme(
    legend.position = "bottom", 
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_text(face = "bold"),
  )

