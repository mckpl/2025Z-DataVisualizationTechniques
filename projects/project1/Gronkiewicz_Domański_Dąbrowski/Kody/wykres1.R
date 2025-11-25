library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)


ceny_mieszkan <- read.csv("ceny_najmu_top6.csv", sep=';')
inflacja <- read.csv("inflacja.csv", sep=';', col.names = c("rok", "wskaznik"), header = FALSE )

colnames(ceny_mieszkan)[c(-1,-2)] <- substr(colnames(ceny_mieszkan)[c(-1,-2)], 2, 5)
ceny_mieszkan <- ceny_mieszkan[c(-2)]

srednie_ceny_mieszkan <- ceny_mieszkan %>% 
  group_by(Miasto) %>% 
  summarize(across('2014':'2024', function(x) {mean(x, na.rm = TRUE)}))

srednie_ceny_mieszkan <- data.frame(srednie_ceny_mieszkan)

rownames(srednie_ceny_mieszkan) <- srednie_ceny_mieszkan$Miasto

colnames(srednie_ceny_mieszkan)[-1] <- substr(colnames(srednie_ceny_mieszkan)[-1], 2, 5)

ceny_mieszkan_long <- srednie_ceny_mieszkan %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "rok",
    values_to = "srednia_cena"
  ) %>%
  mutate(
    rok = as.integer(rok),
    srednia_cena = as.numeric(srednia_cena)
  )


inflacja_skumulowana <- inflacja %>% 
  mutate(wskaznik = round(cumprod(wskaznik/100),3))


cena_inflacja <- left_join(ceny_mieszkan_long, inflacja_skumulowana, by = "rok")

labels_df <- cena_inflacja %>% 
  group_by(Miasto) %>% 
  filter(rok == max(rok))

p1 <- ggplot(cena_inflacja, aes(x = rok, color = Miasto, y = srednia_cena)) +
  geom_line(linewidth  = 1) + 
  geom_point(size = 2) +
  labs(title = "Średni koszt wynajmu mieszkania na przestrzeni lat",
       x = "Rok",
       y = "Średnia cena (zł)") +
  theme_minimal() + 
  theme(
    legend.position = "bottom" 
  ) 


p2 <- ggplot(cena_inflacja, aes(x = rok, color = Miasto, y = srednia_cena/wskaznik)) +
  geom_line(linewidth  = 1) + 
  geom_point(size = 2) +
  labs(x = "",
       y = "Średnia cena/wskaźnik inflacji [PLN]",
       color = "") +
  theme_minimal() + 
  theme(
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)), 
    legend.position = "bottom",
    plot.background = element_rect(fill = "#f2f1ec", color = NA),
    legend.direction = "horizontal",
    text = element_text(family = "sans"),
    legend.text = element_text(size = 25),
    legend.key.size = unit(1, "cm"),
    legend.margin = margin(t = 10, b = 10),
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
  ) +
  guides(color = guide_legend(
    nrow = 1,
    override.aes = list(
      size = 5,
      linewidth = 2
  ))) 


p1  
p2


