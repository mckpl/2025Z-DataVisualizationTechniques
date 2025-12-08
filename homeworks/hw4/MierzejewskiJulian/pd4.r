
# link do oryginału https://x.com/EvaMaeRey/status/1800222775321514148?s=20
# link do danych https://github.com/rfordatascience/tidytuesday/blob/main/data/2024/2024-05-28/harvest_2021.csv

#W oryginale trudno odczytać nazwy i wartości mniejszych kół
#koła które mają znacząco inne wartości są wizualnie rozmiarowo bardzo podobne
#więc trudno porównywać nieskrajne wartosci bez dokładniejszej analizy


# w poprawioenj wersji bardzo łatwo i czytelnie można porównać wartości dla zbliżonych warzyw
# wszystkie warzywa są czytelne a nie jak w oryginale z mniejszymu kołami


library(ggplot2)
library(dplyr)

harvest_2021 <- read.csv("harvest_2021.csv")

harvest_2021 <- harvest_2021 %>%
  group_by(vegetable) %>%                       
  summarise(
    total_weight = sum(weight, na.rm = TRUE),   
    .groups = "drop"                            
  ) %>% 
  arrange((total_weight)) %>% 
  mutate(vegetable = factor(vegetable, levels=vegetable))


harvest_2021 %>% 
ggplot(aes(x = total_weight/1000, y = vegetable)) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(expand = expansion(c(0, .1))) +
  labs(
    title    = "Plony Lisy 2021",
    x        = "Zebrane owoce i warzywa (kg)",
    y        = "Warzywa"
  ) +
  theme_minimal(base_size = 14)+
  geom_text(
    aes(label = paste0(round(as.numeric(total_weight/1000),2), " kg")),
    hjust = -0.2, 
    size  = 4
  ) 

