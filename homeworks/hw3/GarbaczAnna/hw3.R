library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(mapdata)
library(RColorBrewer)
df<-read.csv("sdg_15_11$defaultview_linear_2_0.csv")

forest<-df %>% 
  mutate(country=Geopolitical.entity..reporting.,
         year=TIME_PERIOD, 
         percentage= OBS_VALUE) %>%
  filter(year==2022) %>% 
  select(country,percentage)

forest$country[forest$country=="Czechia"]<-"Czech Republic"

europe<-map_data("world") %>% 
  filter(region %in% forest$country)

data<-data.frame(left_join(europe,forest,join_by("region"=="country")))

maks<-data %>% 
  filter(percentage==max(percentage)) %>% 
  summarise(region=first(region),
            median_long=median(long)+3,
            median_lat=median(lat),
            label_long=median_long+12,
            label_lat=median_lat+3,
            percentage=first(percentage))


minimum<-data %>% 
  filter(percentage==min(percentage)) %>% 
  summarise(region=first(region),
            median_long=median(long),
            median_lat=median(lat),
            label_long=median_long-5,
            label_lat=median_lat-2,
            percentage=first(percentage))

max_min<-bind_rows(maks,minimum) %>% 
  mutate(
    label=paste0(region,"\n", percentage,"%")
  )
max_min$col<-c("white","black")

ggplot(data,mapping = aes(x = long, y = lat, group = group,fill=percentage))+
  geom_polygon(color="black")+
  scale_fill_gradientn(colors = brewer.pal(9,"Greens"))+
  labs(title = "Forestation of european countries",
       subtitle = "year 2022",
       fill="Percentage (%) ")+
  theme_void() +
  geom_segment(data=max_min,mapping=aes(x=median_long,y=median_lat,xend =label_long,yend=label_lat),
               inherit.aes = FALSE,
               size=0.5,
               color="black",
               arrow=arrow(length=unit(0.2,"cm"))
  )+
  geom_label(data=max_min,mapping = aes(x = label_long, y =label_lat, label = label),
             inherit.aes = FALSE,
             fill = "white",
             color = "black",
             size = 3.5,
             label.padding = unit(0.2, "lines"),
             label.r = unit(0.1, "lines")
  )+
  scale_color_identity() +
  coord_map("albers", 25, 50)