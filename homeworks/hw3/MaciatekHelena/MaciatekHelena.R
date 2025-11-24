library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(RColorBrewer)

#link do danych: https://www.kaggle.com/datasets/hammadfarooq470/global-gender-equality-rankings-2023

swiat <- map_data('world')
nierownosci <- read.csv("combined_global_gender_data_20251123.csv")
nierownosci$Gender.Inequality.Index..GII...Gender.Inequality.Index.
View(dane)

countries_europe <- c(
  "Albania",
  "Andorra",
  "Armenia",
  "Austria",
  "Belarus",
  "Belgium",
  "Bosnia and Herzegovina",
  "Bulgaria",
  "Croatia",
  "Cyprus",
  "Czechia",
  "Denmark",
  "Estonia",
  "Finland",
  "France",
  "Germany",
  "Greece",
  "Hungary",
  "Iceland",
  "Ireland",
  "Italy",
  "Kosovo",
  "Latvia",
  "Liechtenstein",
  "Lithuania",
  "Luxembourg",
  "Malta",
  "Moldova",
  "Monaco",
  "Montenegro",
  "Netherlands",
  "North Macedonia",
  "Norway",
  "Poland",
  "Portugal",
  "Romania",
  "Russia",
  "San Marino",
  "Serbia",
  "Slovakia",
  "Slovenia",
  "Spain",
  "Sweden",
  "Switzerland",
  "Turkey",
  "Ukraine",
  "United Kingdom",
  "Vatican City"
)

#wybiorę państwa z listy, którą wkleiłam wyżej, ponieważ przy filtrowaniu przez współrzędne, dane z Afryki,
#które bardzo różnią się od tych w Europie, zaburzały skalę kolorów w Europie, a to na niej chciałam się
#skupić (zostawiam pod koniec pliku kod do wykres2, ktory pokazuje tą znaczącą różnice)


dane <- swiat %>%
  filter(region %in% countries_europe) %>% 
  left_join(nierownosci, by=join_by('region'=='Country')) %>% 
  mutate(inequality_index = Gender.Inequality.Index..GII...Gender.Inequality.Index.) %>% 
  filter(long<=60,lat<=72, !is.na(inequality_index))

#wyznaczę "środek" każdego z państw by móc potem wypisać tam nazwę (dla max i min inequality index)
srodki <- dane %>% 
  group_by(region) %>% 
  summarise(meanlong = mean(long, na.rm=TRUE),
            meanlat = mean(lat, na.rm=TRUE),
            inequality = unique(inequality_index))

min_wartosc <- min(srodki$inequality, na.rm=TRUE)
max_wartosc <- max(srodki$inequality, na.rm=TRUE)

min_kraje <- srodki %>% 
  filter(inequality==min_wartosc)
max_kraje <- srodki %>% 
  filter(inequality==max_wartosc)

wykres <- ggplot() +
  geom_polygon(data = dane, aes(x = long, y = lat, group=group, fill=inequality_index), color = "whitesmoke")+ 
  coord_fixed(1.3)+
  scale_fill_gradient(low = 'pink', high = 'red')+
  labs(title = "Gender Inequality in Europe in 2023",
       fill = "Inequality Index\nLower = more equal")+
  geom_point(data = min_kraje, aes(x = meanlong, y = meanlat), color = "black", size = 2)+
  geom_label(data = min_kraje, aes(x = meanlong-2.85, y = meanlat, label = paste0(region, "\n", inequality)), 
             color = "black", fill=alpha("white", 0.5), size=4, label.size = 0.5)+
  geom_point(data = max_kraje, aes(x = meanlong, y = meanlat), color = "black", size = 2)+
  geom_label(data = max_kraje, aes(x = meanlong+2.5, y = meanlat, label = paste0(region, "\n", inequality)), 
             color = "black", fill=alpha("white", 0.5), size=4, label.size = 0.5)+
  theme_bw()+
  theme(panel.border = element_blank(),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        legend.title = element_text(size=12,face = "bold"),
        legend.text = element_text(size = 10),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())
wykres

ggsave("MaciatekHelena.png", plot = wykres,width = 16, height = 9, dpi = 300)


# dane2 <- swiat %>% 
#   filter(lat<=72, lat>=35, long <=65, long>=-25) %>% 
#   left_join(nierownosci, by=join_by('region'=='Country')) %>% 
#   mutate(inequality_index = Gender.Inequality.Index..GII...Gender.Inequality.Index.) %>% 
#   filter(!is.na(inequality_index))
# 
# wykres2 <- ggplot() +
#   geom_polygon(data = dane2, aes(x = long, y = lat, group=group, fill=inequality_index), color = "whitesmoke")+ 
#   coord_fixed(1.3)+
#   theme_void()
# wykres2



