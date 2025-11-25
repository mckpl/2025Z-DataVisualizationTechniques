library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(sf)
library(stringi)
library(tidyverse)
library(patchwork)
##########
# Ladowanie danych
df <- read.csv("../BardziejPoprawioneDane/OczyszczonePrzestepstwa2.csv")
wojewodztwa <- st_read("../GraniceDoMapek/A01_Granice_wojewodztw.shp", quiet = TRUE)

##### Funkcje do tworzenia map
mapa_roku <- function(rok,Generator,min_val,max_val,rodzaj){
  ggplot(data = Generator) + 
    geom_sf(aes(fill = !!sym(rok)), color="black") + 
    theme_void() +
    labs(
      #title = paste0(rok,"r."),
      title = rodzaj,
         fill="Liczba przestępstw \nna 1000 mieszkancow") +
    # Mozliwe palety:
    #scale_fill_gradient(low = "Yellow",high="Red",limits = c(min_val, max_val))
    #scale_fill_distiller(palette="Spectral",limits = c(min_val, max_val))
    scale_fill_distiller(#palette="RdYlGn",
                          palette='Oranges'
                         ,limits = c(min_val, max_val),
                         #limits= c(0,20),
                         direction = 1
                         )
}
stworz_mapy <- function(lata,rodzaj) {
  df2 <- df %>% filter(Rodzaj_Przestepstwa == rodzaj) %>% 
    pivot_wider(names_from = Rok,values_from = Na_1000_mieszkancow,id_cols = Wojewodstwo) %>% 
    mutate(Wojewodstwo = stri_trans_tolower(Wojewodstwo))
  # Wspolne granice, zeby kolory mialy taki sam zakres
  min_val <- df2 %>% select(all_of(lata)) %>% min()
  #min_val <- 0
  max_val <- df2 %>% select(all_of(lata)) %>% max()
  Generator <- left_join(x = wojewodztwa,  y = df2, by = c("JPT_NAZWA_"= "Wojewodstwo"))
  lapply(lata, mapa_roku,Generator,min_val,max_val,rodzaj)
}
stworz_panel <- function(lata,rodzaj){
  wrap_plots(stworz_mapy(lata,rodzaj),guides = "collect") +
    plot_annotation(title = paste("Przestępczość ", rodzaj," w Polsce")) &
                    theme(
                    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
                    legend.position = "bottom",
                    legend.title = element_text(size = 12, face = "bold"),
                    legend.title.position = "top",
                    legend.direction ="horizontal",
                    #legend.text = element_text(size = 10),
                    legend.key.height = unit(0.75, "cm"),
                    legend.key.width = unit(1.5, "cm"),
                    plot.margin = margin(t=10,r = 30, l = 30)
                    )
                    
}
###############
(rodzaje <-  df$Rodzaj_Przestepstwa %>% unique() %>% sort())
###############
lata <- as.character(c(2005,2024)) # reczny wybor
lata <- as.character(round(seq(2002, 2024, length.out = 1))) # wybierasz ile chcesz lat
#############
# Ponizej wykresy dla danych rodzajow przestepczosci


stworz_panel(lata,rodzaje[1]) # Gospodarcze

############

stworz_panel(lata,rodzaje[2]) # Kryminalne

############

stworz_panel(lata,rodzaje[3]) # przeciwko mieniu

############

stworz_panel(lata,rodzaje[4]) # rodzinie i opiece

############

stworz_panel(lata,rodzaje[5]) # zyciu i zdrowiu

############

stworz_panel(lata,rodzaje[6]) # drogowe



############
library(svglite)

ggsave("mapaTest.png",
       plot =stworz_panel(lata,rodzaje[6])& theme(
         panel.background = element_rect(fill='transparent'),
         plot.background = element_rect(fill='transparent', color=NA), 
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         legend.background = element_rect(fill='transparent'), 
         legend.box.background = element_rect(fill='transparent'), 
         legend.title = element_text(color = "white"),
         plot.title = element_text(color="white"),
         legend.text = element_text(size = 10,color="white")
       ),
       width = 7, height = 6, dpi = 300,
       bg = "transparent")


###################
###################
###################

rok <- '2024'
rodzaje2 <- rodzaje[-c(4,6)]

wykresy <- lapply(rodzaje2, function(r) stworz_mapy(lata = rok, rodzaj = r))
wykresy_flat <- unlist(wykresy, recursive = FALSE)



w <- wrap_plots(wykresy_flat,nrow=4)+
  plot_annotation() &
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5,family = "sans"),
    legend.position = "right",
    legend.title = element_text(size = 6, face = "bold"),
    legend.title.position = "top",
    #legend.direction ="horizontal",
    #legend.text = element_text(size = 10),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(0.4, "cm"),
    plot.margin = margin(t=0,r = 15, l = 15)
  )

w

ggsave("mapaTest2.png",
       plot =w& theme(
         panel.background = element_rect(fill='transparent', color = NA), 
         plot.background = element_rect(fill='transparent', color=NA),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         legend.background = element_rect(fill='transparent', color = NA), 
         legend.box.background = element_rect(fill='transparent', color = NA), 
         legend.title = element_text(color = "white"),
         plot.title = element_text(color="white"),
         legend.text = element_text(size = 6,color="white")
       ),
       width = 7, height = 6, dpi = 300,
       bg = "transparent")














