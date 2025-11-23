library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(countrycode)
library(mapdata)
library(maps)

world_map <- map_data("world")

dane <- read.csv("Crimes_UN_data.csv")
#View(dane)

eur <- codelist %>%
  filter(continent == "Europe") #państwa europejskie do łatwiejszego rysowania mapy

daneeuropa <- dane %>%
  filter(X.1 == "Intentional homicide rates per 100,000") %>%
  filter(X == 2015) %>%
  filter(Intentional.homicides.and.other.crimes %in% eur$country.name.en) %>%
  select(Intentional.homicides.and.other.crimes, X.2)

colnames(daneeuropa) <- c("region","wskaznik")

daneeuropa <- daneeuropa %>%
  mutate(region = recode(region, "Czechia" = "Czech Republic")) %>%
  mutate(region = recode(region, "United Kingdom" = "UK")) %>%
  add_row(region = "Bosnia and Herzegovina", wskaznik = NA) %>%
  add_row(region = "North Macedonia", wskaznik = NA) %>%
  add_row(region = "Belarus", wskaznik = NA) %>%
  add_row(region = "Ukraine", wskaznik = NA) %>%
  add_row(region = "Moldova", wskaznik = NA) %>%
  add_row(region = "Kosovo", wskaznik = NA) %>%
  add_row(region = "Russia", wskaznik = NA) %>%
  add_row(region = "Turkey", wskaznik = NA) %>%
  add_row(region="Luxembourg", wskaznik = NA) %>%
  add_row(region="Andorra", wskaznik = NA)
############# Dla lepszej wizualizacji dodaje kraje, dla których brakuje danych aby umieścić je na mapę

mapa_z_danymi <- world_map %>%
  filter(region %in% daneeuropa$region) %>%
  left_join(daneeuropa, by="region") %>%
  filter(long >= -25) %>%
  filter(long <=41) %>%
  filter(lat <= 75) %>%
  mutate(wskaznik = as.numeric(wskaznik))

wskaznik_max <- max(mapa_z_danymi$wskaznik,na.rm=TRUE)
wskaznik_min <- min(mapa_z_danymi$wskaznik, na.rm=TRUE)
wskaznik_min_bez0 <- mapa_z_danymi %>% #Dla ciekawości dodam jeszcze te kraje, dla których ten wskaznik to nie jest 0
  select(-subregion) %>% #pokazanie że minimalny wskaznik jest dla Monako i Liechtensteinu gdzie mieszka mało ludzi nie jest zbyt interesujące
  drop_na() %>%
  filter(wskaznik > 0)

wskaznik_min_bez0 <- min(wskaznik_min_bez0$wskaznik)

srodki <- mapa_z_danymi %>%
  group_by(region) %>%
  summarise(long = mean(long), lat=mean(lat), wskaznik = mean(wskaznik)) %>%
  drop_na() %>% #usuwam te bez wartosci, bo one i tak nie beda rozpatrywane
  filter(wskaznik %in% c(wskaznik_max,wskaznik_min, wskaznik_min_bez0)) %>%
  mutate(long = ifelse(region=="Norway",9.739843, long)) %>%
  mutate(lat = ifelse(region=="Norway", 60.87032, lat)) %>% #Ustawiam srodek dla Norwegii recznie, bo środek wychodzi na granicy ze szwecją, słabo widoczny
  mutate(tekst = paste0(region, " - ", wskaznik))



ggplot(data = mapa_z_danymi, aes(x = long, y = lat)) + 
  geom_polygon(aes(fill=wskaznik, group=group), color="black", size=0.1) +
  geom_point(aes(color = "No data"), alpha = 0) +
  scale_fill_gradient2(high = "darkred",
                       limits=c(-0.3,6.4), breaks=c(0,1,2,3,4,5,6.0), na.value = "#9ca19c", name = "Rate") +
  scale_color_manual(name = "", values = "#9ca19c") +
  coord_fixed(1.45) +
  labs(title="Intentional homicide rates in Europe in 2015", subtitle="Rates per 100,000 citizens") +
  theme_void() +
  scale_x_continuous(expand=c(0,0))+ 
  theme(plot.background =element_rect(fill="white") ,legend.text = element_text(face="bold"),legend.title = element_text(face="bold", size = rel(1.5)), plot.title=element_text(hjust=0.5,size=rel(2.4) , face="bold"),
        plot.subtitle=element_text(hjust=0.5, face="italic", size=rel(1.5))) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust=0.5, barheight = unit(10, "cm"), barwidth = unit(0.7,"cm"),
                               ticks.colour = "black", frame.colour="#9ca19c", frame.linewidth = 0.5, order=1),
         color = guide_legend(override.aes = list(alpha = 1, shape = 15, size = 8.9), order = 2)) +
  geom_label_repel(data= srodki, aes(x=long,y=lat,label=tekst), box.padding=0.85, label.r=0.4, segment.size=1, fontface="bold")
   

