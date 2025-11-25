
library(dplyr)      #potrzebne biblioteki
library(ggplot2) 

  
  
df3 <- read.csv("ysv_f3.csv")  #wczytywanie danych



df3r <- df3 %>% 
  mutate(Ratio = round(1000*Deaths/Visitors,2))   # dodajemy wskaźnik śmiertelności
View(df3r)




ggplot(df3r, aes(x = Year)) +     # rysujemy wykres
  geom_col(aes(y = Ratio), color = "navy", fill = "#4c63a6", alpha = 0.7) + 
  
  geom_text(aes(label = paste(Deaths,"\nofiar"), y = Ratio + max(Ratio)*0.05), 
            size = 5, color = "darkred", fontface = "bold") +    
  
  labs(
    title = "Wskaźnik śmiertelności w Tatrach (2000-2024)",
    subtitle = "Wysokość słupka: śmiertelność na 1 mln turystów | Liczby nad słupkami: całkowita liczba ofiar",   # opis żeby liczby nad słupkami nie wprowadzały w błąd
    x = "Rok", 
    y = "Liczba ofiar na 1 milion turystów"
  ) + 
  scale_x_continuous(
    breaks = seq(2000, 2024, 1),   # aby każdy rok był widoczny i jasny w odczycie
    limits = c(1999.5, 2024.5)   # trochę marginesu po prawej i lewej stronie
  ) +
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),   #przezroczyste tło żeby ładnie wyglądało na plakacie
    panel.background = element_rect(fill = "transparent"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),     # pochylone, aby lata na siebie nie nachodziły 
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 20),  
    plot.subtitle = element_text(size = 11, color = "gray40"),  
    axis.title = element_text(size = 12),
    text = element_text(size = 14), 
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent")
  )



ggsave("wykres_smiertelnosc_tatry6.png",    
       bg = "transparent",    #  zapis też z przezroczystym tłem 
       width = 16, 
       height = 9,
       dpi = 300)



