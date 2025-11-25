library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(ggrepel)


dane<- read.csv("rolictwo.csv")
dane_rolnicze <- data.frame(country = dane[["geo"]],
                            ilosc_wyprodukowanego_zboza = dane[["OBS_VALUE"]])


do_mapki <- dane_rolnicze %>% left_join(map_data("world"),by = c("country"="region"))

do_mapki <- do_mapki %>% mutate(najwysza_najmniejsza =ifelse(do_mapki$ilosc_wyprodukowanego_zboza %in% c(min(do_mapki$ilosc_wyprodukowanego_zboza), max(do_mapki$ilosc_wyprodukowanego_zboza)), country, ""))

two_points <- do_mapki %>%
  group_by(country) %>%
  summarise(
    long = mean(long),
    lat = mean(lat),
    ilosc = unique(ilosc_wyprodukowanego_zboza)
  ) %>%
  filter(
    ilosc %in% c(min(ilosc), max(ilosc))
  )

c(min(do_mapki$ilosc_wyprodukowanego_zboza), median(do_mapki$ilosc_wyprodukowanego_zboza)
  , max(do_mapki$ilosc_wyprodukowanego_zboza))


do_mapki %>% ggplot()+
  coord_map("albers", 25, 50) +
  geom_polygon(aes(x = long, y=lat,group = group,fill = ilosc_wyprodukowanego_zboza),color="black")+
  scale_fill_gradient(low= "yellow",high="brown4", trans="log10",  
  breaks = c(min(do_mapki$ilosc_wyprodukowanego_zboza), median(do_mapki$ilosc_wyprodukowanego_zboza)
  , max(do_mapki$ilosc_wyprodukowanego_zboza)),
  labels = c("low (7.60 kt)", "medium (3406 kt)", "high(64997 kt)"))+
  geom_point(
    data = two_points,
    aes(x= long,y= lat),
    size = 3,
    color="grey"
  ) +
  geom_text_repel(data = two_points,
  aes(long, lat, label = country),min.segment.length = 0)+
  theme_bw() +
  labs(title = "Ilość wyprodukowanych zbóż w ciągu roku",
       subtitle = "Rok 2023",
       fill = "",x = "długość geograficzna", y = "szerokość geograficzna")




