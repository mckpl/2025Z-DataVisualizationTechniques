
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)



if (!requireNamespace("extrafont", quietly = TRUE)) {
  install.packages("extrafont")
}
library(extrafont)

if (!requireNamespace("showtext", quietly = TRUE)) {
  install.packages("showtext")}
library(showtext)
if (!requireNamespace("ggtext", quietly = TRUE)) {
  install.packages("ggtext")}
library(ggtext)
font_add("LoveloBlack", "Kody/Lovelo-Black.otf") 
font.families()
showtext_auto(enable = TRUE)


wojewodztwa<- st_read("Dane/A01_Granice_wojewodztw.shp",quiet=TRUE)
# View(wojewodztwa)
dane <- read.csv("Dane/Energia_elektryczna_wg_woj_2015-2023.csv", sep=";",
                 fileEncoding = "Windows-1250")
ludzie <- read.csv("Dane/Ludność_według_płci_i_województw_-_stan_w_dniu_31.12.2023_roku.csv",
                   sep=";",fileEncoding = "Windows-1250")
ludzie <- ludzie %>% select(Województwo, Ogółem) %>% mutate(Województwo=tolower(Województwo))

filtr <- c("Produkcja energii elektrycznej: OZE (w tym elektrownie wodne z członami pompowymi; łącznie ze współspalaniem) (GWh)",
           "Produkcja energii elektrycznej: ogółem (GWh)")

# View(ludzie)

###
### Wskaźnik = oze*1000/liczba ludzi
### Docenić województwa, które produkują dużo, ale tak ogólnie
### Tutaj dodaąc ile człowiek zużywa GWH
### Człowiek ok 1600 kWh (sprawdzić) czyli 1000 osób 1600MWh = 1,6 GWh
### Oczyiwście to energia w gospodarstwie domowym, a nie ogólnie potrzebna
### no i jest to sama energia elektryczna
###

df<-dane %>% mutate(Zmienna=case_when(
  Zmienna==filtr[1]~"OZE",
  Zmienna==filtr[2]~"Calosc",
  .default =""), Województwo=tolower(Województwo)) %>% 
  filter(Rok==2023,Zmienna %in% c("OZE","Calosc"),Województwo!="ogółem  kraj") %>%
  select(!Rok) %>% pivot_wider(values_from = Wartość, names_from = Zmienna) %>% 
  left_join(ludzie,by="Województwo") %>% 
  mutate(OZE=as.numeric(gsub(",",".",OZE)), Calosc=as.numeric(gsub(",",".",Calosc))) %>% 
  mutate(wsk=OZE*1000/Ogółem)


mapa<-wojewodztwa %>% left_join(df,by=c("JPT_NAZWA_"="Województwo")) %>% ggplot() +
  geom_sf(aes(fill=wsk),color="black",name="GWh z OZE na 1000 mieszkańców") +
  scale_fill_gradient2(low="#de5e20",high="#88cb46", mid="#fdf31c",midpoint=1.6,
                       name=paste0(
                         "<span style='font-family:LoveloBlack; font-size:75pt; color:white; margin-right:50px;'>━━━━━━━━GWh</span><br>",
                         "<span style='font-family:arial; font-size:50pt; color:white'>━━━━━━━━━━━━━━━</span><br>",
                         "<span style='font-family:LoveloBlack; font-size:50pt; color:white'>1000 mieszkańców</span>"
                       )) +
  theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background  = element_rect(fill = "transparent", color = NA),
        legend.background  = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(family="LoveloBlack",colour="white",size=50),
        legend.ticks = element_line(colour = "black"),
        legend.title = element_markdown())
mapa
ggsave("WYkresy/wykres3outdated.png",mapa,bg = "transparent", width = 7.5, height = 6, dpi = 600)

###
### Generowanie mapy 2 - inne kolor, skale, podpis min max
###

breaks=c(0,0.75,1.5,2.25,3,4.5)

gradient=colorRampPalette(c("#c6ffb3","darkgreen"))
kolory=(gradient(5))
breaks_labels=(c("0 - 0.75","0.75 - 1.5","1.5 - 2.25","2.25 - 3","3 - 4.5"))

pom<-wojewodztwa %>% left_join(df,by=c("JPT_NAZWA_"="Województwo")) %>% 
  filter(wsk== min(wsk) | wsk == max(wsk))
extreme <- st_centroid(pom)


mapa2<-wojewodztwa %>% left_join(df,by=c("JPT_NAZWA_"="Województwo")) %>% ggplot() +
  geom_sf(aes(fill=(cut(wsk, breaks=breaks, include.lowest=TRUE,labels=breaks_labels))),
          color="black",name="GWh z OZE na 1000 mieszkańców") +
  scale_fill_manual(values = kolory,
                    name =paste0(
                      "<span style='font-family:LoveloBlack; font-size:75pt; color:white; margin-right:50px;'>━━━━━━━━GWh</span><br>",
                      "<span style='font-family:arial; font-size:50pt; color:white'>━━━━━━━━━━━━━━━━</span><br>",
                      "<span style='font-family:LoveloBlack; font-size:50pt; color:white'>1000 mieszkańców</span>" )) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background  = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(family="LoveloBlack",colour="white",size=65),
    legend.ticks = element_line(colour = "black"),
    legend.position = c(1.12, 0.65),
    legend.title = element_markdown()) +
  geom_sf_text(data=extreme, 
               aes(label=round(wsk,2)),color="black",size=30)+
  guides(fill= guide_legend(reverse = TRUE,
                            keywidth = 2,
                            keyheight = 2))
scale_fill_manual

ggsave("Wykresy/wykres3w2.png",mapa2,bg = "transparent", width = 10, height = 6, dpi = 600)
