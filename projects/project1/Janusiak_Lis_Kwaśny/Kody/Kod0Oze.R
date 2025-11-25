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
  Rok = c(2023,2020,2017,2014,2011),
  Solar = c(11107.09, 1957.92, 165.46, 99.45, 88.07),
  Wind = c(24176.36, 15800.05, 14909.04, 7675.63, 3204.55),
  Bio = c(7892, 8202, 6440, 10065, 7757), 
  Hydro= c(2409.51, 2118.34, 2559.58, 2182.45, 2331.38 )
)
# Solar = c(11107.09, 1957.92, 165.46, 6.89, 0.18)
library(ggplot2)
library(tidyr)


df_long <- pivot_longer(oze, cols = c(Solar, Wind, Bio, Hydro), 
                        names_to = "Kategoria", 
                        values_to = "Wartość")
df_long$Rok <- factor(df_long$Rok)

p<-ggplot(df_long, aes(x = Rok, y = Wartość, fill = Kategoria)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    name = "Źródło energii",
    labels = c("Biopaliwa", "Energia wodna", "Energia słoneczna","Energia wiatrowa"),
    values = c("#88cb46","blue",   "#fdf31c", "lightblue")) +
  labs(
       x = "Rok", y = "Ilość energii w GWh") +

  
  theme_minimal()+
  theme(
    # panel.background = element_rect(fill = "darkblue", color = NA),
    # plot.background = element_rect(fill = "darkblue", color = NA),
    # panel.border = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_blank(),
    # plot.title = element_text(color = "white", size = 16, face = "bold"),
    axis.title = element_text(family="LoveloBlack",color = "white",size=75),
    axis.text = element_text(family="LoveloBlack",color = "white",size=60),
    legend.title = element_text(family="LoveloBlack",color = "white",size=75),
    legend.text = element_text(family="LoveloBlack",color = "white",size=55),
    legend.position="bottom",
    legend.justification = "left",
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = -50)

  )
p
ggsave("Wykresy/wykres0outdated.png", p, bg = "transparent", width = 7, height = 4, dpi = 600)
rgb(t(col2rgb("lightblue")), maxColorValue = 255)
