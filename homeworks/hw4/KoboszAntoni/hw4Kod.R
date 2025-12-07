library(ggplot2)
library(dplyr)

# Oryginalny post:
# https://bsky.app/profile/a2aysha.bsky.social/post/3m7du7k7xve27

# dane
# https://github.com/rfordatascience/tidytuesday/tree/main/data/2025/2025-12-02

# W oryginalnym wykresie brakuje, kolorów i tytulu. Autor napisal w poscie ze
# uwzglednia srednia temperature, czego nie zrobil, wiec dodalem ja jako kolor slupkow.
# Postanowilem tez usunac wpis z 1923 roku, bo nie pasuje do reszty i jest dluga przerwa 
# miedzy kolejnymi danymi.
# Wyrownalem slupki do osi za pomoca expand()



tuesdata <- tuesdata <- tidytuesdayR::tt_load(2025, week = 48)

df <- tuesdata$sechselaeuten


#df %>% View()

df <- df |>
  filter(!is.na(duration)) %>% 
  filter(year>1950)

plot <- ggplot(df, aes(x = year, y = duration,fill=tre200m0)) +
  theme_bw()+
  scale_fill_gradientn(colours=c("purple","violet","#F7E26B","#F18D31"), breaks = seq(14,24,2),)+
  geom_col(width=1) +
  labs(title = "How did the duration of the burning and temeperature change over time",
       fill="Average\ntemperature[°C]",
       y = 'duration of the burning [min]') +
  scale_x_continuous(breaks = seq(1950,2025,10),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,60,10),expand = expansion(mult = c(0, 0.05)))

ggsave("hw4MyPlot.png",plot,width=7,height = 4)

  











