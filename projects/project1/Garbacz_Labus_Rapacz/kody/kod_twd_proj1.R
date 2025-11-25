#biblioteki
library(ggplot2)
library(patchwork)
library(tidyr)
library(dplyr)
library(showtext)

#Bank Danych Polska
dane_hist_44_98 <- read.csv("resources/dane_hist_siec_komunikacyjna.csv", sep=";")
dane_hist_44_98_long <- pivot_longer(dane_hist_44_98,cols=matches("^X[0-9]{4}$"), names_to = "rok", values_to = "wartosc", names_prefix = "X")
dane_hist_44_98_long <- dane_hist_44_98_long %>%
  mutate(rok = as.integer(rok)) %>% 
  mutate(wartosc =as.numeric(gsub("\u00A0", "", dane_hist_44_98_long$wartosc)))

#Bank Danych Lokalnych
wask_99 <- read.csv("resources/waskotor_od99.csv",sep=";")
wask_99$X <- NULL
wask_99$Kod <- NULL
colnames(wask_99)[2:ncol(wask_99)] <- 1999:2024
wask_99 <- pivot_longer(wask_99,cols=colnames(wask_99)[2:ncol(wask_99)], names_to = "rok", values_to = "wartosc")
wask_99 <- wask_99 %>%
  mutate(rok = as.integer(rok))

nor_99 <- read.csv("resources/link_normlan_od99.csv",sep=";")
nor_99$X <- NULL
nor_99$Kod <- NULL
colnames(nor_99)[2:ncol(nor_99)] <- 1999:2024
nor_99 <- pivot_longer(nor_99, cols=colnames(nor_99)[2:ncol(nor_99)], names_to = "rok", values_to = "wartosc")
nor_99 <- nor_99 %>% 
  mutate(rok = as.integer(rok))

#dane dla całego kraju
wask99_pol <- wask_99 %>% 
  filter(Nazwa=="POLSKA") %>% 
  select(rok,wartosc)
nor99_pol <-  nor_99 %>% 
  filter(Nazwa=="POLSKA") %>% 
  select(rok,wartosc)

#dane z odpowiednich kategorii
wask_hist <- dane_hist_44_98_long %>% 
  filter(X.Wskaźnik=="wąskotorowe") %>% 
  select(rok,wartosc)

nor_hist <- dane_hist_44_98_long %>% 
  filter(X.Wskaźnik=="normalnotorowe") %>% 
  select(rok,wartosc)

#sklejamy dane (oba źródła to GUS ale z różnych stron)
sklejka_wask <- rbind(wask_hist,wask99_pol)
sklejka_nor <- rbind(nor_hist,nor99_pol)

#wykres
font_add_google("League Spartan", family="spartan")
showtext_auto()

waskie <- sklejka_wask %>% 
  ggplot(aes(x=rok,y=wartosc, group = 1))+
  geom_line() +
  geom_point()+
  labs(y="Linie wąskotorowe (km)", x="Rok")+
  theme_light(base_size = 16)+
  scale_x_continuous(expand=c(0.01,0.01))+
  scale_y_continuous(expand=c(0.03,0.03))+
  ylim(0,5000)+
  theme(base_family="spartan")

normalne <- sklejka_nor %>% 
  ggplot(aes(x=rok,y=wartosc, group = 1))+
  geom_line() +
  geom_point()+
  theme_light(base_size = 16)+
  labs(title="Linie normalnotorowe i wąskotorowe na przestrzeni lat 1946-2024", y="Linie normalnotorowe (km)")+
  scale_x_continuous(expand=c(0.01,0.01))+
  scale_y_continuous(expand=c(0.03,0.03))+
  theme(base_family="spartan",axis.title.x = element_blank(),axis.text.x  = element_blank(),axis.ticks.x = element_blank(), plot.title=element_text(hjust=0, face="bold"))

komentarz <- ggplot() +
  annotate("text",
           x = -0.1, y = 0.5,
           label = "------------------------------------------------------------------------------------------------------------------------------------------------przeskok skali o 14000 km------------------------------------------------------------------------------------------------------------------------------------------------",
           hjust = 0.5,
           size = 4.5) +
  theme_void()
normalne+komentarz+waskie+plot_layout(ncol=1, heights = c(0.48,0.04, 0.48))