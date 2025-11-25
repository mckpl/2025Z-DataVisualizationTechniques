library(readxl)
library(sf)
library(ggplot2)
library(dplyr)
mapadane <- read_excel("mapka.xlsx")

wojewodztwa <- st_read("PRG_jednostki_administracyjne_2024/A01_Granice_wojewodztw.shp", quiet = TRUE)

mapadane <- mapadane %>% 
  mutate(JPT_NAZWA_ = tolower(Województwo))

dane<-left_join(wojewodztwa,mapadane,by='JPT_NAZWA_')

wykres<-ggplot(data = dane) + 
  geom_sf(aes(fill=Ilość), color = "whitesmoke",linewidth = 0.3)+ 
  theme_void()+
  labs(title = 'Ilość małżenśtw zawartych w roku 2023 \nw różnych województwach', 
       fill = "Ilość małżeństw \nzawartych\nw roku 2023")+
  scale_fill_gradient(low='#F3C677', high = '#B33F62')+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "whitesmoke"),
        legend.title = element_text(face = "bold", color = "whitesmoke"),
        legend.text = element_text(size = 11, color = "whitesmoke"),)
wykres

ggsave("mapamalz.png", plot = wykres, bg = "transparent")

