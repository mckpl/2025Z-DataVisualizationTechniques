library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
dane2<-read.csv("dane2_projekt1_twd.csv") # trzeba wczytać
dane2



dane2 %>% 
  mutate(age=factor(age,levels = c("From 15 to 64 years","From 20 to 24 years","From 25 to 34 years"),labels=c("15-64","20-24","25-34"))) %>% 
  mutate(OBS_VALUE=OBS_VALUE/100) %>% 
  mutate(isced11=factor(isced11,levels=c("Less than primary, primary and lower secondary education (levels 0-2)","Upper secondary and post-secondary non-tertiary education (levels 3 and 4)","Tertiary education (levels 5-8)"),
                        labels =c("podstawowe ","średnie","wyższe"))) %>% 
  filter(age=="15-64") %>% 
  filter(isced11=="wyższe") %>% 
  ggplot(aes(x=TIME_PERIOD,y=OBS_VALUE))+
  geom_col(fill="#d0e1f9")+
  labs(title="Udział osób z wykształceniem wyższym w wieku 15-64 lat ",x="Rok",y="Udział (%)",caption = "Źródło: Eurostat")+
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5,size = 15),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    aspect.ratio = 0.5,
    axis.text.x = element_text(size = 5)
  )+
  scale_x_continuous(
    breaks = seq(min(dane2$TIME_PERIOD),
                 max(dane2$TIME_PERIOD),
                 by = 3)   # show every 2 years (adjust if needed)
  )+
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.5)
  )




