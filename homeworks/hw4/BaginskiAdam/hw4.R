library(tidyverse)
library(tidytuesdayR)
library(tidyr)

# Link do danych: https://github.com/rfordatascience/tidytuesday/blob/main/data/2024/2024-02-06/heritage.csv
# Link do posta: https://x.com/jayatisharmaa/status/1755961046916067544

# Orginalny wykres nie jest znormalizowany - tzn. na wykresie nie jest pokazany stosunek
# liczby posiadanych obiektów przez dany kraj do łącznej liczby obiektów wszystkich 3 państw
# przez co, aby odczytać informacje musimy sami policzyć ilość odpowiednich ikonek. Uważam, że
# jest to po prostu zły typ wykresu na tę wizualizację. Dodatkowo - powtarzająca się legenda.

# Poprawiona wizualizacja zarówno ilustruje stosunek liczby obiektów w danym państwie do sumy 
# obiektów oraz informuje odbiorcę o ilości tychże obiektów w danym kraju.

tuesdata <- tidytuesdayR::tt_load('2024-02-06')
heritage_data <- as.data.frame(tuesdata$heritage)

long_2004 <- heritage_data %>% 
  select(c("country", "2004")) %>% 
  pivot_longer(
    cols = c(`2004`),
    names_to = "year",
    values_to = "count")

long_2022 <- heritage_data %>% 
  select(c("country", "2022")) %>% 
  pivot_longer(
    cols = c(`2022`),
    names_to = "year",
    values_to = "count")

combined_data <- bind_rows(long_2004, long_2022) %>% 
  mutate(country = factor(country, levels = c("Norway", "Denmark", "Sweden")))

colors <- c("Norway" = "#00205B", "Denmark" = "#C8102E", "Sweden" = "#FECC02")

p <- combined_data %>% 
  ggplot(aes(x = factor(year), y = count, fill = country)) +
  geom_col(color = "black", linewidth = 0.5) +
  geom_text(aes(label = count),
            position = position_stack(vjust = 0.5),
            color = "white", 
            fontface = "bold",
            size = 8) +
  scale_fill_manual(values = colors) +
  labs(
    title = "UNESCO Heritage Sites in Selected Countries",
    x = "Year",
    y = "Total Number of Objects",
    caption = "Source: UNESCO World Heritage List"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 30),
    plot.caption = element_text(face = "italic", size = 9),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    panel.grid.major.x = element_blank() #usunięcie pionowych linii siatki
  )

ggsave("poprawiony.png", plot = p, width = 10, height = 8, bg = "white")

