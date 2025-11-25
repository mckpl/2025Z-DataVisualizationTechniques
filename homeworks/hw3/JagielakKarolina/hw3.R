library(ggplot2)
library(sf)
library(dplyr)
library(ggnewscale)
library(rnaturalearth)
library(rnaturalearthdata)

#Przygotowanie danych 

world <- ne_countries(scale = "medium", returnclass = "sf")

dane <- read.csv("dane.csv")
dane <- dane%>%
  select(geo, OBS_VALUE)
dane <- dane %>% filter(geo != "Germany including former GDR")
dane$geo <- recode(dane$geo,
                      "Türkiye" = "Turkey",
                      "Kosovo*" = "Kosovo",
                      "Bosnia and Herzegovina" = "Bosnia and Herz.")
kraje_eurostat <- dane$geo

mapa <- world %>%
  left_join(dane, by = c("name" = "geo")) %>%
  mutate(
    data_status = case_when(
      name %in% kraje_eurostat & !is.na(OBS_VALUE) ~ "OK",
      name %in% kraje_eurostat &  is.na(OBS_VALUE) ~ "brak_danych",
      TRUE ~ "poza"
    )
  )

#Znalezienie wartości najmniejszej i największej
max_val <- max(dane$OBS_VALUE, na.rm = TRUE)
min_val <- min(dane$OBS_VALUE, na.rm = TRUE) 

kraje_min_val <- dane%>% 
                filter(OBS_VALUE == min_val)%>%
                pull(geo)
kraje_max_val <- dane%>%
                filter(OBS_VALUE == max_val)%>%
                pull(geo)
#Punkty na mapie
labels_df <- mapa %>%
  filter(name %in% c(kraje_min_val, kraje_max_val)) %>%
  st_point_on_surface() %>% #pobieramy jeden punkt dla kraju
  st_transform(3035) %>% #układ dla analiz na poziomie Europy
  mutate(#wyciągnięcie danych x i y z st_point_on_surface
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2],
    label = paste(name, OBS_VALUE, sep = "\n"),
    # pozycja końca strzałki dla krajów
    lx = x,   
    ly = y - 450000,
    typ = case_when(
      name %in% kraje_min_val ~ "Minimum",
      name %in% kraje_max_val ~ "Maximum"
    )
  ) %>%
  st_set_geometry(NULL)

#Rysowanie mapy

map <- ggplot()+
  
  geom_sf(data = mapa %>% filter(data_status == "poza"),
               fill = "gray80",
               color = "black", size = 0.2) +
  
  geom_sf(data = mapa %>% filter(data_status == "brak_danych"),
               aes(fill = data_status),
               color = "black", size = 0.2)+
  scale_fill_manual(
    name = "Data status",
    values = c("brak_danych" = "gray40"),
    labels = c("brak_danych" = "No data (Eurostat)")
  )+
  new_scale_fill()+ 
  geom_sf(data = mapa %>% filter(data_status =="OK"), 
               aes(fill = OBS_VALUE),
               color = "black", size = 0.2) +
  scale_fill_continuous(
    name = "Crude birth rate",
    type = "gradient",
    low = "white",
    high = "blue"
  ) +
  new_scale_fill()+ 
  geom_segment(
    data= labels_df,
    aes(x = x, y = y, xend =lx, yend = ly),
    size = 0.5,
    color = "black"
  )+
  geom_label(data =labels_df,
             aes( x=lx, y =ly, label = label, fill = typ ),
            color = "black")+
  scale_fill_manual(name = "Value type", values = c("Minimum" = "red","Maximum" = "green")) +
  coord_sf(
    crs = st_crs(3035),
    xlim = c(2500000, 7000000),
    ylim = c(1500000, 6500000),
    expand = FALSE) +
  theme_minimal() +
  labs(title = "Crude birth rate per thousand person in 2024",
       caption = "Source: Eurostat",
       x = NULL, y = NULL)
map
ggsave("hw3_map.png", plot = map,width = 10, height = 8)



