library(readr)
library(ggplot2)
library(dplyr)
library(forcats)

fide_ratings_august <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_august.csv')
fide_ratings_september <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_september.csv')
fide_ratings <- rbind(fide_ratings_august, fide_ratings_september)

fide_ratings %>% mutate("age" = 2025-bday) %>% 
  mutate("age_group" = case_when(
    age <18 ~ "<18",
    age <=29 ~ "18-29",
    age <=49 ~ "30-49",
    age <=64 ~ "50-64",
    TRUE ~ "65+")) %>% 
  mutate(age_group = fct_rev(age_group)) %>% 
  group_by(sex,age_group) %>% 
  summarise("cnt" = n()) %>% 
  ggplot(aes(fill = age_group, x = sex, y = cnt))+
  geom_bar(position = position_fill(), stat = "identity", color = "black")+
  theme_bw()+
  scale_y_continuous(labels = scales::percent, expand = c(0.01,0.01))+
  scale_fill_manual(name = "Age group", values = c("#da0017", "#843692", "#2c69a9", "#40a33a", "#fc6908"))+
  scale_x_discrete(expand = c(0.5,0.0))+
  labs(title = "FIDE Chess Players by Age Group")+
  xlab("Sex")+
  ylab("Proportion (%)")


