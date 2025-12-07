library(dplyr)
library(ggplot2)

# link do danych: https://github.com/rfordatascience/tidytuesday/blob/main/data/2023/2023-08-08/sauces.csv
#link do wykresu z X: https://x.com/JonMinton/status/1689246298019368960?s=20

# Dwie rzeczy warte poprawy to legenda nie wiadamo czym są pionowe linie oraz duże kropki na wykresie,
#oraz tytuł

sosy <- read.csv("sauce.csv")


sosy <- sosy %>%  mutate(procenty= ifelse(sauce_number!=1,(scoville/lag(scoville))*100,0)) %>% 
  mutate(napisy = ifelse(sauce_number!=1, paste(lag(sauce_number),"to",sauce_number),NA))
sosy <- na.omit(sosy)

ggplot(data=sosy ,aes(x= napisy,y=procenty))+
  geom_jitter(alpha = 0.8, size= 0.8,aes(color = napisy),show.legend = FALSE)+
  stat_summary(
    aes(linetype = "Full Range"),
    fun.min = min, 
    fun.max = max, 
    geom = "linerange", 
    color = "gray40", 
    size = 0.8
  ) +
  
  stat_summary(
    aes(shape = "Mean Change"),
    fun = mean, 
    geom = "point", 
    color = "gray23", 
    size = 4
  ) +
  scale_linetype_manual(name = "", values = c("Full Range" = "solid")) +
  scale_shape_manual(name = "", values = c("Mean Change" = 19)) + 
  scale_y_continuous(limits=c(0,1000))+
  labs(
    title = "How much hotter was that !!!",
    subtitle = "Change in sauce hotness from previous sauce in each season of \"Hot Ones.\"",
    y = "Change in sauce hotness from last sauce (%)", 
    x = "Sauce Number Change"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.92, 1), 
    
    legend.text = element_text(size = 8), 
    legend.key.size = unit(0.4, "cm"),      
    legend.spacing.y = unit(-0.7, "cm")
  )


# Obecny wykres jest znacznie czytelniejszy, odbiorca wie o jakich sosach mowa , wie dzięki legendzie czym
# są pionowe linie oraz duże kropki , dzięki zmianie kolorów pomiedzy kolejnymi kolumnami trudniej
# pomylić poszczególne punkty , które mogły wylądować obok siebie przez geom_jitter.