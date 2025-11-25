
if (!requireNamespace("extrafont", quietly = TRUE)) {
  install.packages("extrafont")
}
library(extrafont)

if (!requireNamespace("showtext", quietly = TRUE)) {
  install.packages("showtext")}
library(showtext)
font_add("LoveloBlack", "Kody/Lovelo-Black.otf") 
font.families()
showtext_auto(enable = TRUE)



# library(nazwa_pakietu)

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

### Wczytuję dane
# kopalne <- read.csv2("Dane/Kopalne.csv",sep=";",fileEncoding = "Windows-1250")
# oze <- read.csv2("Dane/OZE.csv",sep=";", fileEncoding = "Windows-1250")
kopalneOze <- read.csv("Dane/KopalneIOZE.csv",sep=";",fileEncoding = "Windows-1250")


# View(oze)
# View(kopalne)
# 
# View(kopalneOze)

str(kopalneOze)

###
### Eksploracja Danych
###

kopalneOze <- kopalneOze %>% mutate(Wartość = as.numeric(gsub(",",".",Wartość))) %>% 
  mutate(Wartość = ifelse(is.na(Wartość),0, Wartość))


###
### Do stworzenia kresek poziomów
###
kreski<-kopalneOze %>% select(Rok,Wartość,Source) %>% filter(Rok>2009, Source !="All", 
                                                             Source %in% c("fotowoltaika","woda","wiatr","biogaz i biomasa")) %>%
  select(Rok, Wartość) %>% group_by(Rok) %>% summarise(Suma=sum(Wartość))

###
### Do fct_relevel by energia była dobrze ułożona
###
poziomy=c("Pozostałe"="pozostałe","Gaz ziemny"="gaz_ziemny", "Węgiel brunatny"="węgiel_brunatny",
          "Węgiel kamienny"="węgiel_kamienny","Energia Wodna"="woda","Biopaliwa"="biogaz i biomasa",
          "Energia wiatrowa"="wiatr","Energia słoneczna"="fotowoltaika")

###
###Kod wykresu
###
breaks1 <- unique(kopalneOze$Rok)
breaks2 <- seq(2010,2023,2)

kolumnowy<-kopalneOze %>% select(Rok, Wartość, Source) %>% filter(Source != "All") %>%
  filter(Rok>2009) %>% mutate(Source=fct_relevel(Source,poziomy)) %>% 
  ggplot(aes(x=Rok,y=Wartość,fill=Source)) + geom_col() +
  scale_fill_discrete(palette = c("grey20","grey44","#b24b19","darkgrey",
                                  "#4A90E2","#88cb46","lightblue","#fdf31c"),name="Źródło Energii",labels=
                        c("Pozostałe","Gaz ziemny","Węgiel brunatny","Węgiel kamienny","Energia Wodna","Biopaliwa","Energia wiatrowa","Energia słoneczna "))+
  labs(x="Rok",y="Ilość energii w GWh") +
  theme(panel.background = element_blank(),
        axis.title = element_text(family="LoveloBlack",colour = "white",size=85),
        plot.background = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(colour="white", hjust=0.5),
        axis.ticks.y = element_line(colour ="white"),
        axis.ticks.x=element_blank(),
        legend.background = element_blank(),
        legend.ticks = element_line(colour = "black"),
        legend.text = element_text(family="LoveloBlack",colour="white",face="bold",size=75),
        legend.title = element_text(family="LoveloBlack",colour = "white",face="bold",size=100),
        axis.title.y = element_text(margin = margin(t = 0, r = 9, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 11, r = 0, b = 0, l = 0)),
        axis.text = element_text(family="LoveloBlack",color = "white",size=75),
        axis.text.x=element_text(angle=90,vjust=0.5,hjust = 1,
                                 margin=margin(t = -13)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_segment(data=kreski,aes(x=Rok-0.5, y=Suma, xend=Rok+0.5, yend=Suma),
               color="white",linewidth = 0.8, inherit.aes = FALSE)+
  scale_x_continuous(breaks=breaks2)
# ?scale_x_discrete
# View(kopalneOze)
# ?theme

ggsave("Wykresy/wykres2.png", kolumnowy, bg = "transparent", width = 7.5, height = 6, dpi = 600)
###
# ### Wykrey waflowe - nie używane
# ###
# rok <- 2005
# 
# kwadraty<-kopalneOze %>% filter(Rok==rok, Source!="All") %>% mutate(procent=round(Wartość/sum(Wartość)*100)) %>% 
#   uncount(procent) %>% mutate(
#     x = rep(1:10, length.out = n()),
#     y= rep(1:10, each=10,length.out = n())
#   )
# 
# kwadraty %>% ggplot(aes(x,y,fill=Source)) +geom_tile(color="black") +
#   coord_equal() + scale_fill_brewer(palette = "Set3")+theme_void()+
#   labs(title=paste("Rozkład produkcji energii elektrycznej względem\n źródeł w roku ",as.character(rok)))
# 
# ###
# ### Śmieci
# ###
# 
# kopalneOze%>% select(Rok, Wartość, Source) %>% pivot_wider(names_from=Source, values_from = Wartość)
#colors()






