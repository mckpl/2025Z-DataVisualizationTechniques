library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(sf)
library(stringi)
library(tidyverse)
library(patchwork)

Wskaznik  <- read.csv("../BardziejPoprawioneDane/OgolnoPolskiWskaznikWykrywalnosci2.csv")
Przestepczosc <- read.csv("../BardziejPoprawioneDane/OgolnoPolskaPrzestepczoscNa1000Mieszkancow2.csv")


#Wskaznik %>% View()
#Przestepczosc %>% View()


w <- Wskaznik %>% pivot_longer(!Rodzaj_Przestepstwa,names_to = "Rok",values_to = "Wskaznik")
p <- Przestepczosc %>% pivot_longer(!Rodzaj_Przestepstwa,names_to = "Rok",values_to = "LiczbaNa1000")

# Lacze wskaznik i liczbe przestepstw na 1000 w formacie long. usuwam 'X' z daty
df <- left_join(w,p) %>% mutate(Rok = as.numeric(str_remove(Rok,'X')))

df <- df %>% filter(!Rodzaj_Przestepstwa %in% 
           c("przeciwko rodzinie i opiece","przestępstwa drogowe"))


plot1 <- df %>%
  ggplot(aes(x=Rok,y=Wskaznik,color=Rodzaj_Przestepstwa)) +
  geom_line(size=1) + 
  geom_point() +
  labs(
    #title="Wskaznik wykrywalnosci\nwedlug rodzaju przestępstwa\nna przestrzeni lat w Polsce",
    title = "Wskaźnik wykrywalności (%)",
   # subtitle = "Średni wskaźnik wykrywalności przestępstw w \nlatach 2002-2024 według rodzaju przestępstwa",
       x = "", y = "", color="Rodzaj przestępstwa") +
  scale_y_continuous(labels =function(x) paste0(x,"%"),limits = c(0,100) )+
  theme_minimal() +
  #theme_dark()+
  #scale_color_brewer(palette = 'Dark2')+
  scale_color_manual(values = c("#17A68C", "#F2CB05", "#e6261d", "#F2636F"))+
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),   
    #plot.subtitle = element_text(size = 12)            
  )

plot2 <- df %>% 
  ggplot(aes(x=Rok,y=LiczbaNa1000,color=Rodzaj_Przestepstwa))+
    geom_line(size=1) + geom_point() +
    labs(title="Liczba przestępstw na 1000 mieszkańców",
         x = "", y ="", color="Rodzaj przestępstwa") +
    theme_minimal() + 
    #scale_color_brewer(palette = 'Dark2') +
  scale_color_manual(values = c("#17A68C", "#F2CB05", "#e6261d", "#F2636F"))+
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))


w <- wrap_plots(plot2 , guides = "collect") +   
  plot_annotation(
  #title = "Wykrywalność i liczba przestępstw w Polsce",
  #subtitle = "Średni wskaźnik wykrywalności przestępstw oraz \nliczba przestępstw na 1000 mieszkańców w \nPolsce w latach 2002–2024",
  theme = theme(
    plot.title = element_text(size=16, face="bold",hjust = 0.5),
    plot.subtitle = element_text(size=12,hjust = 0.5, lineheight = 1.2)
    )
  )&theme(
    legend.title = element_text(size=10, face="bold", hjust=0),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.box = "horizontal",
#    legend.key.width = unit(1, "cm")
    #panel.background = element_rect(fill = '#1A1945', colour = 'yellow')
  ) &
  guides(color = guide_legend(nrow = 2))

w

ggsave('liniowe.png',width = 4, height = 6, dpi = 300,
       plot = w& theme(
         panel.background = element_rect(fill='transparent', color=NA), 
         plot.background = element_rect(fill='transparent', color=NA), 
         panel.grid.major = element_line(color = "#7E88BF"), s
         panel.grid.minor = element_line(color="#7E88BF"), 
         legend.background = element_rect(fill='transparent', color=NA), 
         legend.box.background = element_rect(fill='transparent', color=NA), 
         legend.title = element_text(color = "white"),
         plot.subtitle = element_text(color="white"),
         legend.text = element_text(size = 8,color="white"),
         axis.text =  element_text(size = 8,color="white"),
         plot.title = element_text(color="white")))




#########
#########

library(ggrepel)

w2 <- df %>% 
  mutate(Label = ifelse(df$Rok %in% c(round(seq(2002, 2024, by=3)),2024),df$Rok,NA)) %>% 
  arrange(Rodzaj_Przestepstwa, Rok) %>% 
  ggplot(aes(x=Wskaznik,y=LiczbaNa1000,color=Rodzaj_Przestepstwa,label=Label))+
  geom_point(size=2) + 
#  geom_line() +
   theme_bw() +
  scale_x_continuous(labels =function(x) paste0(x,"%"),limits = c(32,100))+
  labs(
    x = "Wskaźnik wykrywalności",
    y = "Liczba przestępstw na 1000 mieszkańców",
    title = "Zależność liczby przestępstw od wskaźnika wykrywalności",
    color = "Rodzaj przestępstwa"
  ) +
  #scale_color_brewer(palette = 'Dark2') +
  scale_color_manual(values = c("#17A68C", "#F2CB05", "#e6261d", "#F2636F"))+
  geom_label_repel(max.overlaps = 24,size=2,
                   show.legend = FALSE,max.iter = 30,fill='#3e23a3', color='white'
                   ) +
  theme(
    plot.title = element_text(size = 14,face = 'bold'),
    
    legend.position = c(x=1,y=1),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill="white", color="#344459"),
    legend.title = element_text(size=10, face="bold", hjust=0),
    #legend.key.size = unit(1.5, "cm"),
    legend.key.height = unit(1.1, "cm"),
    legend.text =element_text(size=9)
    #legend.key.width = unit(1.5, "cm"),
  )

w2

ggsave('scatterPW.png',width = 8, height = 6, dpi = 300,
       plot = w2& theme(
         panel.background = element_rect(fill='transparent', color=NA), 
         plot.background = element_rect(fill='transparent', color=NA), 
         panel.grid.major = element_line(color = "#7E88BF"), 
         panel.grid.minor = element_line(color="#7E88BF"),
         legend.background = element_rect(fill='#1e0056', color='#3e23a3'), 
         legend.box.background = element_rect(fill='transparent', color=NA), 
         legend.title = element_text(color = 'white'),
         plot.subtitle = element_text(color="white"),
         legend.text = element_text(size = 10,color="white"),
         plot.title = element_text(color="white"),
         axis.title = element_text(color="white"),
         axis.text =  element_text(size = 10,color="white"),
         panel.border = element_rect(color = "#7E88BF"),
         axis.ticks = element_line(color = '#7E88BF')
         ))




  #################

library(tidyverse)
#library(janitor)
library(readxl)
library(ggstream)
library(showtext)
library(ggtext)
df %>%  arrange(Rok, Rodzaj_Przestepstwa) %>% 
  ggplot(aes(Rok,LiczbaNa1000,
             fill = Rodzaj_Przestepstwa,
             Label=Rodzaj_Przestepstwa,
             group = Rodzaj_Przestepstwa)) +
  geom_area(position= position_stack())

df %>%
  ggplot(aes(x = factor(Rok), y = LiczbaNa1000, fill = Rodzaj_Przestepstwa)) +
  geom_col(position = "stack", width = 0.7) +
  labs(
    x = "Rok",
    y = "Liczba przestępstw na 1000 mieszkańców",
    fill = "Rodzaj przestępstwa",
    title = "Przestępstwa w Polsce według rodzaju w kolejnych latach"
  ) 



