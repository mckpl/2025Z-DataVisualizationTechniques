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


library(dplyr)
library(tidyr)
library(ggplot2)

if (!requireNamespace("eurostat", quietly = TRUE)) {
  install.packages("eurostat")
}
library(eurostat)

#Współczynniki emisyjnosci z raportów Kobize w latahc 2014-2023
emisje_wspk <- c(823,810,806,814,792,758,745,762,788,733)
emisje_wspk <-c(rep(823,24),emisje_wspk)
# Definiujemy filtry
filtry <- list(
  geo = "PL",
  siec = "RA000",
  unit = "GWH",
  nrg_bal = "GEP" # Gross electricity production
)

# Pobieranie danych
dane_eurostat <- get_eurostat(id = "nrg_bal_c", 
                              filters = filtry, 
                              time_format = "num") # "num" daje lata jako liczby

Lata <- select(dane_eurostat,time)
Moc_Oze <-select(dane_eurostat,values)
Moc_Oze<-Moc_Oze[[1]]

filtry <- list(
  geo = "PL",
  siec = "TOTAL",
  unit = "GWH",
  nrg_bal = "GEP" # Gross electricity production
)

dane_eurostat <- get_eurostat(id = "nrg_bal_c", 
                              filters = filtry, 
                              time_format = "num") # "num" daje lata jako liczby
dane_eurostat
Moc_Total<- select(dane_eurostat,values)
Moc_Total<-Moc_Total[[1]]



filtry <- list(
  geo = "PL",
  airpol = "CO2",
  unit = "THS_T",
  src_crf = "CRF1A1a"
)

dane_eurostat <- get_eurostat(id = "env_air_gge", 
                              filters = filtry, 
                              time_format = "num") 


CO2_kt_Elektrownie<-select(dane_eurostat,values)

emisje_wspk<-emisje_wspk*1000
Zysk<-emisje_wspk*Moc_Oze
Zysk<-Zysk/1000000

CO2_kt_Elektrownie<-rev(CO2_kt_Elektrownie[[1]])
Suma<-CO2_kt_Elektrownie+Zysk
etykieta<-c(rep("Stan obecny",34),rep("Gdyby nie Oze",34))


data<-data.frame(rep(Lata,2),c(CO2_kt_Elektrownie,Suma),etykieta)
data
data<-rename(data,value=c.CO2_kt_Elektrownie..Suma.)

if (!requireNamespace("viridis", quietly = TRUE)) {
  install.packages("viridis")
}
library(viridis)
# Plot
p <- data %>% 
  ggplot(aes(x=Lata, y=, fill=etykieta, text=etykieta)) +
  geom_area( ) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme(legend.position="none")




library(ggplot2)
library(dplyr)

if (!requireNamespace("tibble", quietly = TRUE)) {
  install.packages("tibble")
}
library(tibble)

if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
library(tidyr)



spalanie<-c(823,810,806,814,792,758,745,762,788,733)


Lata2<-Lata[-(1:24),]


filtry <- list(
  geo = "PL",
  siec = "TOTAL",
  unit = "GWH",
  nrg_bal = "GEP" 
)

dane_total_electr <- get_eurostat(id = "nrg_bal_c", 
                              filters = filtry, 
                              time_format = "num") %>% 
  select(values)


filtry <- list(
  geo = "PL",
  siec = "RA000",
  unit = "GWH",
  nrg_bal = "GEP"
)

dane_oze_electr <- get_eurostat(id = "nrg_bal_c", 
                                  filters = filtry,
                                  time_format = "num") %>% 
  select(values)




dane_oze_electr2<-c(dane_oze_electr[-(1:24),])
dane_total_electr2<-dane_total_electr[-(1:24),]

dane_coal_electr2<-(dane_total_electr2-dane_oze_electr2)
spalanieX<-spalanie*1000

dane_oze_electr2<-rev(dane_oze_electr2[[1]])
dane_total_electr2<-rev(dane_total_electr2[[1]])
Lata2<-rev(Lata2[[1]])
dane_coal_electr2<-rev(dane_coal_electr2[[1]])


etykieta<-c(rep("Stan obecny",10),rep("Wzrost Bez Oze",10))
data<-data.frame(
  val=c(dane_coal_electr2*spalanie/1000,dane_total_electr2*spalanie/1000),
  labelek=etykieta,
  lata=rep(Lata2,2)
  
)
data
p2 <- ggplot(data, aes(x = lata, y = val, color = labelek, fill = labelek)) +
  
  geom_area(position = "identity", alpha = 0.4, linewidth = 0) + 

  geom_line(linewidth = 1.2) + 
  geom_point(size = 2.5) +
  
  
  scale_color_manual(values = c("Stan obecny" = "green", "Wzrost Bez Oze" = "black")) +
  scale_fill_manual(values = c("Stan obecny" = "green", "Wzrost Bez Oze" = "darkgrey")) +
  
  # Używam theme_minimal() jako zamiennika, jeśli nie masz hrbrthemes
  theme_minimal() + 
  
  labs(
    
    y = "Emisje CO2 (w tys. ton)",
    x = "Rok",
    color = "Scenariusz", # Tytuł legendy
    fill = "Scenariusz"  # Tytuł legendy (dla wypełnienia)
  ) +
  theme(legend.position = "bottom")+
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background  = element_rect(fill = "transparent", color = NA),
        legend.background  = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(colour="white", hjust=0.4,face="bold",size=15),
        axis.ticks = element_line(colour ="white"),
        legend.text = element_text(family="LoveloBlack",colour="white",face="bold",size=85),
        axis.text = element_text(family="LoveloBlack",color = "white",size=95),
        axis.title = element_text(family="LoveloBlack",colour = "white",size=98),
        panel.border = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)),
        legend.title = element_blank())
  

# Wyświetl wykres
options(scipen = 999)
print(p2)
ggsave("Wykresy/wykres4.png",p2,bg = "transparent", width = 7.5, height = 6, dpi = 600)

