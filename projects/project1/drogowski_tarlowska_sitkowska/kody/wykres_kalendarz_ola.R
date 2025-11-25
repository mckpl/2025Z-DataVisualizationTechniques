library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

df <- read_excel("/Users/aleksandrasitkowska/Downloads/raport_police.xlsx")

df <- df %>% select(-Strona)

df$Data <- as.Date(df$Data)

df <- df %>%
  mutate(Dzien = day(Data),
         Miesiac = month(Data),
         Rok = year(Data))

# Grupowanie po dniu i miesiącu 
df_grouped <- df %>%
  group_by(Dzien, Miesiac) %>%
  summarise(wypadki = mean(`Wypadki drogowe`), .groups="drop")


# Zakładam, że wszystkie dane są z np. 2024, zeby móc stworzyć kalendarz
df_grouped$rok <- 2024

df_grouped$data <- as.Date(with(df_grouped, paste(rok, Miesiac, Dzien, sep="-")), format="%Y-%m-%d")

#week_of month zeby kazdy miesiac zaczynal sie na tej samej wysokosci na y
df_grouped <- df_grouped %>%
  mutate(
    day_of_week = wday(data, label = TRUE, abbr = TRUE, week_start = 1),  # poniedziałek = 1
    week_of_month = ceiling((day(data) + wday(floor_date(data, "month"), week_start = 1) - 1) / 7))


# granice miesięcy
month_bounds <- df_grouped %>%
  group_by(Miesiac) %>%
  summarise(start = min(Dzien), end = max(Dzien))



#######################################################
#######################################################
######################################################



gradient_fun <- colorRampPalette(c("lightyellow", "maroon"))

my_colors <- gradient_fun(5) 

my_breaks <- c(45, 60, 70, 85) 

s <- ggplot(df_grouped, aes(x = day_of_week, y = -week_of_month, fill = wypadki)) +
  geom_tile(color = "black") +
  scale_fill_stepsn(
    colors = my_colors,     
    breaks = my_breaks,      
    name = "Number of\naccidents",
    labels = c("45", "60", "70", "85")) + 
  facet_wrap(
    ~Miesiac,
    nrow = 1,
    scales = "free_x",
    strip.position = "top",
    labeller = labeller(Miesiac = function(x) month.name[as.numeric(x)]) ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", color = "lightyellow", size = 10),
    panel.spacing = unit(0, "lines"),
    aspect.ratio = 0.8,
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.title = element_text(color = "lightyellow", face = "bold"),
    legend.text = element_text(color = "lightyellow"))


ggsave("final_wykres1.png", s, bg = "transparent", width = 12, height = 4, dpi = 300)







