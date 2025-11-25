library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(ggrepel)
library(patchwork)

unemployment <- read.csv("/Users/maciek/Desktop/twd hw3/global_unemployment_data.csv")

adults_uemp_change <- unemployment %>% 
  filter(age_categories == "Adults") %>% 
  mutate(change = round((X2024 - X2014)*100/(X2014)),
         country_name = case_when(
           country_name == "Russian Federation" ~ "Russia",
           country_name == "United States" ~ "USA",
           country_name == "Venezuela, Bolivarian Republic of" ~ "Venezuela",
           country_name == "Congo, Democratic Republic of the" ~ "Democratic Republic of the Congo",
           country_name == "Tanzania, United Republic of" ~ "Tanzania",
           country_name == "Moldova, Republic of" ~ "Moldova",
           country_name == "Syrian Arab Republic" ~ "Syria",
           country_name == "Iran, Islamic Republic of" ~ "Iran",
           country_name == "Viet Nam" ~ "Vietnam",
           country_name == "Lao People's Democratic Republic" ~ "Laos",
           TRUE ~ country_name
         )) 

world_data <- map_data("world") %>% 
  filter(region != "Antarctica")

####################################### WYKRES 1 #######################################

male <- adults_uemp_change %>% 
  filter(sex == "Male")

male_map_data <- world_data %>% 
  left_join(male, by = c("region" = "country_name")) %>% 
  mutate(region = ifelse(region == "Cambodia", "Kambodża", region))  # po tym jak zobaczyłem jaki będzie
                                                                    # kraj z najmniejszą wartością zmieniłem
                                                                    # nazwę na polską do spójności z wykresem

male_min_max_change <- male_map_data %>% 
  filter(change %in% c(max(change, na.rm = TRUE), min(change, na.rm = TRUE))) %>% 
  group_by(region) %>% 
  summarise(mean_lat = mean(lat), mean_long = mean(long), change = mean(change)) 
  

male_map <- ggplot(data= male_map_data, aes(x=long, y=lat, fill = change))+
  geom_polygon(color="black", size=0.1, aes(group = group))+
  coord_quickmap() +
  scale_fill_gradient2(high="darkred", low="darkgreen", mid="white", midpoint=0, breaks = c(-80, 0, 80, 160, 240),   
                       labels = c("-80%", "0%", "80%", "160%", "240%")) +
  labs(title = "Procentowy wzrost stopy bezrobocia u mężczyzn po 25 roku życia (2014-2024)", fill = "%",
       caption = paste0("Największy procentowy wzrost - ", male_min_max_change[2,1], ": ",  male_min_max_change[2,4], "%" , 
                        "\n", "Najmniejszy procentowy wzrost - ", male_min_max_change[1,1], ": ",  male_min_max_change[1,4], "%" )) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.caption = element_text(hjust = 0.05, size = 16)) +
  geom_point(data = male_min_max_change, aes(x = mean_long, y = mean_lat)) +
  geom_label_repel(
    data = male_min_max_change,
    aes(x = mean_long, y = mean_lat, label = paste0(change, "%")),
    fill = "white",
    color = "black",
    size = 4,
    min.segment.length = 0
  )

male_map

####################################### WYKRES 2 #######################################

female <- adults_uemp_change %>% 
  filter(sex == "Female")

female_map_data <- world_data %>% 
  left_join(female, by = c("region" = "country_name")) %>% 
  mutate(region = ifelse(region == "Cambodia", "Kambodża", region))  # po tym jak zobaczyłem jaki będzie
                                                                    # kraj z najmniejszą wartością zmieniłem
                                                                    # nazwę na polską do spójności z wykresem

female_min_max_change <- female_map_data %>% 
  filter(change %in% c(max(change, na.rm = TRUE), min(change, na.rm = TRUE))) %>% 
  group_by(region) %>% 
  summarise(mean_lat = mean(lat), mean_long = mean(long), change = mean(change))

female_map <- ggplot(data= female_map_data, aes(x=long, y=lat, fill = change))+
  geom_polygon(color="black", size=0.1, aes(group = group)) +
  coord_quickmap() +
  scale_fill_gradient2(high="darkred", low="darkgreen", mid="white", midpoint=0, breaks = c(-80, 0, 80, 160, 240, 320, 400),   
                       labels = c("-80%", "0%", "80%", "160%", "240%", "320%", "400%")) +
  labs(title = "Procentowy wzrost stopy bezrobocia u kobiet po 25 roku życia (2014-2024)", fill = "%",
       caption = paste0("Największy procentowy wzrost - ", female_min_max_change[2,1], ": ",  female_min_max_change[2,4], "%" , 
                        "\n", "Najmniejszy procentowy wzrost - ", female_min_max_change[1,1], ": ",  female_min_max_change[1,4], "%" )) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.caption = element_text(hjust = 0.05, size = 16)) +
  geom_point(data = female_min_max_change, aes(x = mean_long, y = mean_lat)) +
  geom_label_repel(
    data = female_min_max_change,
    aes(x = mean_long, y = mean_lat, label = paste0(change, "%")),
    fill = "white",
    color = "black",
    size = 4,
    min.segment.length = 0
  )

female_map

####################################### WYKRES KOŃCOWY #######################################

plot <- female_map / male_map
plot

# U kobiet musiałem zastosować inną skalę niż u mężczyzn, ponieważ gdybym zastosował taką samą skalę
# to wykres dla mężczyzn by był jeszcze bardziej biały 

# Wielkości tytułów i adnotacji powiększyłem aż na tyle, aby były widoczne na zapisanym pliku png