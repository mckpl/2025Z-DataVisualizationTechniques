library(dplyr)
library(ggplot2)
library(mapdata)
library(maps)


#Populacja w osobach
#Zalesienie w tys hektarów, 1000 ha = 10 km2

obszarZalesiony <- read.csv("obszarZalesiony.csv",sep=";",fileEncoding = "Windows-1250")
populacja <- read.csv("populacja_kraje_swiata.csv",sep=";", fileEncoding = "Windows-1250")

# Drzewa na październik 2025 a populacja grudzień 2024, dane blisko siebie czasowo

populacja <- populacja %>% select(Country.Name, X2024) %>% 
  mutate(Country.Name = case_when(
    Country.Name=="Slovak Republic" ~ "Slovakia",
    .default = Country.Name
  ))


full <- obszarZalesiony %>% select(GEO..Labels., X2025) %>% 
  left_join(populacja, by=c("GEO..Labels."="Country.Name")) 

colnames(full) <- c("kraj","obszar_drzew","populacja")


full <- full %>% mutate(obszar_drzew = gsub(" ","",obszar_drzew)) %>% 
  mutate(obszar_drzew = as.numeric(gsub(",",".",obszar_drzew))) %>% 
  mutate(kraj = case_when(
    kraj=="Czechia"~"Czech Republic",
    kraj=="United Kingdom" ~"UK",
    .default = kraj
  )) %>% mutate(obszar_drzew=obszar_drzew*10) %>%
  mutate(wsk = 1000*obszar_drzew/populacja)


kraje <- c(full$kraj, "Kosovo","Belarus","Russia","Andorra","Turkey")
skip_subregion <- c("Svalbard")

w1 <- map_data("world")
w1 <- w1 %>% filter(region %in% kraje) %>% 
  left_join(full, by=c("region"="kraj")) %>% filter(!(subregion %in% skip_subregion)) %>% 
  filter(long<42)


breaks=c(0,1,3,5,10,20,30,40)
labels=c("0 - 1","1 - 3","3 - 5","5 - 10","10 - 20","20 -30","30 - 40","Brak danych")
gradient=colorRampPalette(c("#c6ffb3","darkgreen"))
kolory=(gradient(7))


min <- w1 %>% filter(wsk==min(wsk,na.rm=TRUE))
max <- w1 %>% filter(wsk==max(wsk,na.rm=TRUE))

map_plot <-w1 %>% ggplot(aes(x=long, y=lat, group=group, 
                  fill=cut(wsk,breaks=breaks,include.lowest=TRUE))) +
                geom_polygon(color="black")+
  coord_fixed(1.5) +
  scale_fill_manual(values=kolory, 
                    name=expression(frac("Powierzchnia lasów w km^2","1000 mieszkańców")),
                    labels=labels)+
  labs(title ="Powierzchnia lasów w km^2 przypadająca na 1000 mieszkańców w krajach Europy.") +
  
  theme(plot.background = element_rect(fill="#F5EEDF",color=NA),
        panel.background = element_rect(fill="transparent",color=NA),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill="transparent"),
        legend.text = element_text(colour="black",size=10),
        legend.ticks = element_line(colour = "black"),
        legend.title = element_text(colour = "black", size=10),
        plot.margin = margin(5,5,5,5),
        axis.title = element_blank(),
        title = element_text(size=12, hjust=0.5, vjust=0),
        plot.title.position = "plot",
        plot.title = element_text(hjust=0.5)) +

  geom_path(data=max,
            aes(long,lat,group=group, color="max - Finlandia"),
            linewidth=1) +
  
  geom_path(data=min, aes(long,lat,group = group, color="min - Malta"),
            linewidth=1) + 
  
  scale_color_manual(values=c("max - Finlandia"="blue","min - Malta"="red"),
                     name="")+
  
  guides(fill = guide_legend(position="right",reverse = TRUE),
         color = guide_legend(position="bottom",
                              label.position="right", title.position="left"))

map_plot

ggsave("wykres_drzewa.png",map_plot,bg = "transparent", width = 7.5, height = 6, dpi = 600)







  
  




