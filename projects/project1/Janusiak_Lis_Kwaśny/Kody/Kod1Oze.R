oze <- data.frame(
  Rok = c(2023,2022,2021,2020,2019,2018,2017,2016,2015,2014,2013,2012,2011),
  Solar = c(11107.09, 8309.67, 3934.45, 1957.92, 710.67, 300.49, 165.46, 123.88, 56.64, 6.89, 1.48, 1.14, 0.18),
  Wind = c(24176.36, 19779.54, 16233.55,15800.05, 15106.76, 12798.79, 14909.04, 12587.59, 10858.37, 7675.63, 6003.81, 4746.59, 3204.55),
  Bio = c(6374.22, 5934.10, 6890.05, 6932.76, 6441.15, 5333.22, 5308.56, 6913.00, 9026.00, 9161.00, 7932.00, 9529.00, 7149.00), 
  Hydro= c(2409.51, 1968.19, 2339.18, 2118.34, 1958.42, 1970.00, 2559.58, 2139.45, 1832.20, 2182.45, 2439.12, 2036.75, 2331.38 )
)

library(ggplot2)
library(tidyr)
library(tidyverse)
library(GGally)


df_long <- pivot_longer(oze, cols = c(Solar, Wind, Bio, Hydro), 
                        names_to = "Kategoria", 
                        values_to = "Wartość")
df_long$Rok <- factor(df_long$Rok)



if (!requireNamespace("GGally", quietly = TRUE)) {
  install.packages("GGally")
}
library(GGally)


if (!requireNamespace("extrafont", quietly = TRUE)) {
  install.packages("extrafont")
}
library(extrafont)

if (!requireNamespace("showtext", quietly = TRUE)) {
  install.packages("showtext")}
library(showtext)
font_add("LoveloBlack", "Kody/Lovelo-Black.otf")
font.families()
showtext_auto(enable = TRUE)
oze <- data.frame(
  Rok = c(2023,2022,2021,2020,2019,2018,2017,2016,2015,2014,2013,2012,2011),
  Solar = c(11107.09, 8309.67, 3934.45, 1957.92, 710.67, 300.49, 165.46, 123.88, 56.64, 6.89, 1.48, 1.14, 0.18),
  Wind = c(24176.36, 19779.54, 16233.55,15800.05, 15106.76, 12798.79, 14909.04, 12587.59, 10858.37, 7675.63, 6003.81, 4746.59, 3204.55),
  Bio = c(6374.22, 5934.10, 6890.05, 6932.76, 6441.15, 5333.22, 5308.56, 6913.00, 9026.00, 9161.00, 7932.00, 9529.00, 7149.00), 
  Hydro= c(2409.51, 1968.19, 2339.18, 2118.34, 1958.42, 1970.00, 2559.58, 2139.45, 1832.20, 2182.45, 2439.12, 2036.75, 2331.38 )
)


library(ggplot2)
library(tidyr)






df_long <- df_long %>%
  mutate(
    Kategoria = recode(Kategoria,
                       "Bio"   = "Biopaliwa",
                       "Hydro" = "Energia wodna",
                       "Solar" = "Energia słoneczna",
                       "Wind"  = "Energia wiatrowa")
  )


min_val <- min(df_long$Wartość, na.rm = TRUE)
max_val <- max(df_long$Wartość, na.rm = TRUE)
df_long <- df_long %>%
  mutate(Wartość_norm = (Wartość - min_val) / (max_val - min_val))


my_colors <- c(
  "Biopaliwa"            = "#88cb46",
  "Energia wodna"        = "#4A90E2",
  "Energia słoneczna"    = "#fdf31c",
  "Energia wiatrowa"     = "lightblue"
)


p <- ggplot(df_long, aes(x = Rok, y = Wartość, group = Kategoria, color = Kategoria)) +
  geom_line(size = 1.8, alpha = 0.95) +  
  geom_point(size = 3) +
  scale_color_manual(name = "Źródło energii", values = my_colors, p <- ggplot(df_long, aes(x = Rok, y = Wartość, group = Kategoria, color = Kategoria)) +
  geom_line(size = 1.8, alpha = 0.95) +  
  geom_point(size = 3) +
  scale_color_manual(name = "Źródło energii", values = my_colors,   labels = c("Biopaliwa", "Energia wodna", "Energia słoneczna","Energia wiatrowa")) +
  labs(
    x = "Rok",
    y = "Ilość energii w GWh",
    
    title = NULL
  ) +
  theme_minimal() +
  theme(

    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = NA, color = NA),


    axis.title = element_text(family = "LoveloBlack", color = "white", size = 75),
    axis.text  = element_text(family = "LoveloBlack", color = "white", size = 100),
    
    legend.title = element_text(family = "LoveloBlack", color = "white", size = 75),
    legend.text  = element_text(family = "LoveloBlack", color = "white", size = 55),
    

    legend.position = "bottom",
    legend.justification = "left",
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = -47),
    
    panel.grid.major = element_line(color = "gray30"),
    panel.grid.minor  = element_blank(),

    panel.border = element_blank()
  )
) +
  labs(
    x = "Rok",
    y = "Ilość energii w GWh",
    
    title = NULL
  ) +
  theme_minimal() +
  theme(

    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = NA, color = NA),


    axis.title = element_text(family = "LoveloBlack", color = "white", size = 75),
    axis.text  = element_text(family = "LoveloBlack", color = "white", size = 67),
    
    legend.title = element_text(family = "LoveloBlack", color = "white", size = 75),
    legend.text  = element_text(family = "LoveloBlack", color = "white", size = 55),
    

    legend.position = "bottom",
    legend.justification = "left",
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = -47),
    
    panel.grid.major = element_line(color = "gray30"),
    panel.grid.minor  = element_blank(),

    panel.border = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 16, r = 0, b = 0, l = 0))
  )


print(p)
ggsave("Wykresy//wykres1.png", p, bg = "transparent", width = 7.5, height = 4, dpi = 600)
rgb(t(col2rgb("lightblue")), maxColorValue = 255)

