library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)
data_df <- read.csv("dane.csv")


swiat <- st_read("https://r2.datahub.io/clvyjaryy0000la0cxieg4o8o/main/raw/data/countries.geojson")
swiat$ISO3166.1.Alpha.3[swiat$name == "Norway"] <- "NOR"
swiat$ISO3166.1.Alpha.3[swiat$name == "France"] <- "FRA"
swiat$ISO3166.1.Alpha.3[swiat$name == "Syria"] <- "SYR"
swiat$ISO3166.1.Alpha.3[swiat$name == "Somaliland"] <- "SOM"
swiat$ISO3166.1.Alpha.3[swiat$name == "North Korea"] <- "PKR"
swiat$ISO3166.1.Alpha.3[swiat$name == "Taiwan"] <- "TWN"
swiat$ISO3166.1.Alpha.3[swiat$name == "Greenland"] <- "GRL"
swiat$ISO3166.1.Alpha.3[swiat$name == "Kosovo"] <- "XKO"



swiat$ISO3166.1.Alpha.3 <- as.character(swiat$ISO3166.1.Alpha.3)
data_df$ISO_Code <- as.character(data_df$ISO_Code)
swiat$ISO3166.1.Alpha.3 <- trimws(swiat$ISO3166.1.Alpha.3)
data_df$ISO_Code <- trimws(data_df$ISO_Code)




dane <- swiat %>%
  left_join(data_df, by = c("ISO3166.1.Alpha.3" = "ISO_Code"))



min_row <- dane %>% filter(X2023 == min(X2023, na.rm = TRUE))
max_row <- dane %>% filter(X2023 == max(X2023, na.rm = TRUE))


graniczne <- rbind(
  st_coordinates(st_centroid(min_row$geometry)) %>% as.data.frame() %>% mutate(Country = "Kosowo, najniższy"),
  st_coordinates(st_centroid(max_row$geometry)) %>% as.data.frame() %>% mutate(Country = "Niemcy, najwyższy")
)
colnames(graniczne)[1:2] <- c("lon","lat")




q <- colorNumeric(palette = "YlGnBu", domain = dane$X2023)



leaflet(dane) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~q(X2023),
    color = "#444444",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    label = ~paste(name, "Wartość:", X2023)
  ) %>%

  addLabelOnlyMarkers(
    data = graniczne,
    lng = ~lon,
    lat = ~lat,
    label = ~Country,
    labelOptions = labelOptions(
      noHide = TRUE, 
      direction = "top", 
      textsize = "15px", 
      textOnly = TRUE,
      style = list(
        "background-color" = "rgba(255, 255, 255, 0.8)",  
        "border" = "1px solid #333333",
        "border-radius" = "4px",
        "padding" = "3px"

  ))) %>%
  addLegend(
    pal = q,
    values = ~X2023,
    title = "Srednie lata nauki",
    position = "bottomright"
  )

