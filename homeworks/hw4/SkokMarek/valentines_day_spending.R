
#link do błędnej wizualizacji: https://x.com/GDataScience1/status/1759400123770257627
#link do zestawu danych: https://github.com/rfordatascience/tidytuesday/blob/main/data/2024/2024-02-13/readme.md

#Błędy oryginału:
#Oryginał niepotrzebnie rozdziela kategorie przedmiotów na 8 wykresów, przez co ciężko
#jest je porównać. Dodatkowo różne wykresy mają różne skalowanie osi OY co może wprowadzać w błąd.

#Wyjaśnienie:
#Moja wizualizacja łączy wszystkie kategorie w jeden wykres, co pozwala na łatwe porównanie.
#Wprowadza kilka kolorów, przez co wykres jest potencjalnie estetyczniejszy. Dodatkowo zaznaczony jest okres 
#pandemii Covid-19 czego brakowało w oryginale, a jest to ciekawa informacja tłumacząca zauważalne
#zmiany wykresu.

library(dplyr)
library(ggplot2)
library(tidyr)

historical_spending <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-02-13/historical_spending.csv')

historical_spending <- historical_spending %>% 
  mutate(Other = PerPerson - (Candy + Flowers + Jewelry + GreetingCards + EveningOut + Clothing + GiftCards))
  
historical_spending_long <- historical_spending %>%
  select(Year, Candy, Flowers, Jewelry, GreetingCards, EveningOut, Clothing, GiftCards, Other) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Category",
    values_to = "Spending"
  )

valentine_palette <- c(
  "Jewelry"       = "#FF1493",  # mocny czerwony malinowy
  "EveningOut"    = "#FF6F61",  # łosoś / koralowy
  "Clothing"      = "#FF69B4",  # intensywny róż
  "Flowers"       = "#C71585",  # fuksja (ciemny róż-fiolet)
  "GiftCards"     = "#B3003C",  # głęboki róż
  "Candy"         = "#FFB6C1",  # bardzo jasny róż
  "GreetingCards" = "#DB7093",  # lekko przygaszony róż
  "Other"         = "#7B1FA2"  # Ciemny fiolet
)

historical_spending_long %>% 
  ggplot(aes(x = Year, y = Spending, color = Category)) +
  geom_rect(
    aes(xmin = 2020, xmax = 2021, ymin = -Inf, ymax = Inf),
    fill = "#F9DDE2",
    alpha = 0.07,
    inherit.aes = FALSE
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  labs(
    title = "Valentine's Day Spending",
    subtitle = "From 2010-2022",
    x = "Year",
    y = "Average amount spent ($)",
    color = "Category",
    caption = "Source: National Retail Federation"
  ) +
  annotate(
    "text", x = 2020.5, y = Inf, 
    label = "COVID-19", 
    vjust = 1.3, hjust = 0.5,
    color = "#D8114C", fontface = "bold", size = 2.2,
    lineheight = 0.8
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  scale_color_manual(values = valentine_palette) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#580D22"),
    plot.subtitle = element_text(face = "bold", size = 14, color = "#D8114C"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#FFF8FA", color = NA),
    panel.background = element_rect(fill = "#FFF8FA", color = NA)
  )
