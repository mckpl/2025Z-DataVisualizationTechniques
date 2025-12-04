
#   link do wykresu: https://www.instagram.com/p/ChOOO6VAppk/?utm_source=ig_web_copy_link&igsh=NTc4MTIwNjQ2YQ==
#   link do repozytorium TidyTuesday:https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-08-09

#   Mimo, że wykres na pierwszy rzut oka przedstawia fajny pomysł (wizualizacja w kształcie diabelskiego koła dotycząca właśnie tych kół), to jest on bardzo nieintuicyjny, ciężki do czytania i wnoszący za mało informacji.

#   Co zostało zrobione dobrze - warto zachować:
#   -> każdy kraj ma inny kolor, kolory są łatwo odróżnialne.

#   Co zostało zrobione źle:
#   -> nie ma jasnej skali, ani sensownej osi radialnej
#   -> prawie nieodróżnialne różnice w wysokości 
#   -> różnice w wielkości kółek są słabo widoczne - oko słabo widzi proporcje powierzchni
#   -> nie jesteśmy w stanie odczytać zależności między średnicą i wysokością
#   -> ciężko odczytać poobracane nazwy, ale nie jest to taki rażący błąd jak pozostałe


library(dplyr)
library(ggplot2)
library(tidyr)

wheels <- read.csv(file = "~/Desktop/nie_spacja/wizualizacja/hw4/wheels.csv",
                  encoding = "UTF-8")

View(wheels)
wheels_clean <- wheels %>% 
  filter(status == "Operating") %>% 
  filter(!is.na(height)) %>% 
  arrange(desc(height)) %>% 
  slice_head(n = 10) %>% 
  mutate(
    name = forcats::fct_reorder(name, height),
    country = as.factor(country),
    height_m =height *0.3048,
    diameter_m = diameter*0.3048 
  )

wheels_clean %>% 
  ggplot(aes(x = name)) +
  geom_col(aes(y = height_m, fill = country), alpha = 0.9) +
  geom_line(aes(y = diameter_m, group = 1),
            color = "#F28E2B", size = 1.2) +
  geom_point(aes(y = diameter_m),
             color = "#F28E2B", size = 3) +
  geom_text(aes(y = height_m, 
                label = paste0(signif(height_m, 4), " m")),
            hjust = -0.2, size = 3) +
  geom_text(aes(y = diameter_m, 
                label = paste0(signif(diameter_m, 4), " m")),
            hjust = 1.3, size = 3) +
  coord_flip() +
  scale_fill_manual(
    values = scales::gradient_n_pal(
      c("#F2E5FF", "#CFA7FF", "#A66BFF", "#7C31FF", "#4B007D")
    )(seq(0, 1, length.out = n_distinct(wheels_clean$country)))
  ) +
  labs(
    title = "The world's highest opperating Ferris wheels",
    x = "Ferris wheel's name",
    y = "Heights (columns) and Diameters (dots and line) ",
    fill = "Country",
    caption = "Source: #TidyTuesday 2022-08-09"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(hjust = 1),
    legend.position = "bottom"
  )


#Drugi wykres jest lepszy, ponieważ wykorzystuje oś liniową, która umożliwia szybkie i precyzyjne porównanie wysokości największych diabelskich młynów. 
#W przeciwieństwie do wykresu radialnego, gdzie długości promieni trudno ocenić dla ludzkiego oka, słupki pokazują różnice w sposób prosty i intuicyjny. 
#Dodana linia średnic dodatkowo prezentuje drugi wymiar danych, nie wprowadzając chaosu. Całość jest bardziej czytelna i uporządkowana niż oryginalny wykres mimo ciekawego pomysłu autora.


