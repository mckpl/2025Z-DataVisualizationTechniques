library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

w <- map_data("world")

## źródło :   https://ec.europa.eu/eurostat/databrowser/view/crim_off_cat__custom_18957932/default/table
kradzieże<-read.csv("kradzieże_2023.csv")
kradzieże<-kradzieże %>% 
  select(geo,OBS_VALUE)

kradzieże$geo[kradzieże$geo == "Türkiye"] <- "Turkey"
kradzieże$geo[kradzieże$geo == "United Kingdom"] <- "UK"
kradzieże$geo[kradzieże$geo == "Czechia"] <- "Czech Republic"
państwa_europa<-c(
  "Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina",
  "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia",
  "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
  "Ireland", "Italy", "Kosovo", "Latvia", "Liechtenstein",
  "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro",
  "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal",
  "Romania", "San Marino", "Serbia", "Slovakia", "Slovenia",
  "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine",
  "UK", "Vatican City"
)

państwa_kradzieże <- kradzieże$geo

brakujące_państwa <- państwa_europa[!(państwa_europa %in% państwa_kradzieże)]
pomocnicza <- data.frame(
  geo = brakujące_państwa,
  OBS_VALUE = NA
)

kradzieże<-rbind(kradzieże,pomocnicza)
ramka_końcowa<-left_join(kradzieże,w,by=c("geo"="region"))

najwięcej_kradzieży <- kradzieże %>% 
  filter(OBS_VALUE==max(kradzieże$OBS_VALUE,na.rm=TRUE))

najmniej_kradzieży <- kradzieże %>% 
  filter(OBS_VALUE==min(kradzieże$OBS_VALUE,na.rm=TRUE))

linia<- linia <- data.frame(long = c(-5, 6),lat = c(47, 49.75))

linia2 <- data.frame(long = c(18.25, 19.26),lat = c(36, 42.44))

ggplot(ramka_końcowa,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=OBS_VALUE),color="black")+
  geom_text(aes(x=30,y=50,label="NA"),size=4,color="darkgrey")+
  geom_text(aes(x=-1,y=52,label="NA"),size=3,color="darkgrey")+
  geom_text(aes(x=28,y=53,label="NA"),size=3,color="darkgrey")+
  geom_line(data=linia,aes(x=long,y=lat),inherit.aes = FALSE,linewidth =0.65,color="black")+
  geom_text(aes(x = -6, y = 47, label = "Luksemburg"),hjust=1,size = 3)+
  geom_line(data=linia2,aes(x=long,y=lat),inherit.aes = FALSE,linewidth =0.65,color="black")+
  geom_text(aes(x = 19, y = 35, label = "Czarnogóra"),hjust=1,size = 3)+
  coord_map("mercator",ylim=c(30,70))+
  labs(title="Kradzieże w Europie",subtitle = "2023")+
  scale_fill_gradient(
    high  = "#0E2438",   
    low   = "#6BB4F5",
    name  = "     ilość na 100000 mieszkańców",
  )+
  theme_minimal()+
  theme(
    axis.title = element_blank(),     
    axis.text  = element_blank(),    
    axis.ticks = element_blank(),      
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      size = 22,        
      face = "bold.italic",     
      hjust = 1,        
      vjust = -2,          
      color = "black"  
    ),
    plot.subtitle = element_text(
      size=10,
      hjust = 0.5,
      vjust=-4,
      color="darkgrey"
    )
  )

