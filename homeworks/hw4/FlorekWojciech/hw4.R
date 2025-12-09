library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sysfonts)
library(showtext)

install.packages("tidytuesdayR")

#link do posta:
#https://x.com/AdityaDahiyaIAS/status/1961037166383128708
#link do danych:
#https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-08-26/billboard.csv
#błędy:
#1. z dokładniejszej analizy kodu można wyczytać, że długość piosenki to suma to co na lewo i to co na prawo (były inne przypuszczenia co do interpretacji wykresu co jest dużym błędem)
#2. wykres jest bardzo trudny do odczytu i ciężko znaleść jakiekolwiek zależności
#poprawki:
#Stworzymy wykres punktowy, aby lepiej zobaczyć zależności między danymi

font_add_google("Saira",family = "title_font") 
font_add_google("Saira Condensed",family = "body_font") 
font_add_google("Saira Extra Condensed",family = "caption_font")
showtext_auto()

billboard <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-26/billboard.csv')
tibble <- billboard %>% #dla ułatwienia
  select(fade_out,length_sec,weeks_at_number_one,date)

tibble <- tibble %>% #z jego oryginalnego kodu widać że autor przyciął do 400
  filter(length_sec <= 400)

tibble %>% 
  ggplot(aes(x=date,y=length_sec,color=factor(fade_out),size=weeks_at_number_one))+
  geom_point(alpha=0.5)+
  theme_minimal()+
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     labels = c("0" = "No", "1" = "Yes"))+
  scale_y_continuous(limits = c(min(tibble$length_sec), max(tibble$length_sec)),
                     breaks = seq(100, 400, by = 50))+
  labs(    x="Year",
           y="Duration of the song (in seconds)",
           colour="Does the song fade out?",
           size="Total weeks spent as No.1",
           title="Shorter Songs, Cleaner Endings",
           caption="Data: TidyTuesday 2025-08-26")+
  theme(
    plot.background=element_rect(fill='#fff0d5'),
    axis.text=element_text(size=15,family="body_font"),
    axis.title=element_text(size=20,family="title_font"),
    legend.title=element_text(size=20,family="title_font"),
    legend.text=element_text(size=15,family="body_font"),
    plot.title=element_text(size=25,family="caption_font"),
    plot.caption=element_text(size=15,family="caption_font")
  )

#Uzasadnienie:
#Wykres lepiej wygląda, ponieważ można zauważyć więcej zależności związanej z weeks_at_number_one
#(oprócz dominacji fade in po 2010 można jeszcze zauważyć np początek sławy fade in po 1990 czy dominację krótszych utworów po 2020 [można odczytac dzięki czytelniejszemu weeks_at_number_one])