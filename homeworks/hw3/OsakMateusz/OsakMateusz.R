library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(maps)
library(mapdata)
library(ggrepel)

#https://www.imf.org/external/datamapper/GGXWDG_NGDP@WEO/OEMDC/ADVEC/WEOWORLD
data <- read_excel('/Users/mateuszosak/Documents/OsakMateusz/debt_prc.xls')
data <- data[,c(1,dim(data)[2]-6)] 
colnames(data)[2] <- 'rate'
data[[1]][158] <- 'Slovakia'
data[[1]][data[[1]] == 'United Kingdom'] <- 'UK'
data[[1]][182] <- 'Turkey'
data[[1]][data[[1]] == "Russian Federation"] <- "Russia"


world_map <- map("world", plot = FALSE, fill = TRUE) %>% 
  st_as_sf()


europe_countries <- c(
  "Albania","Andorra","Austria","Belarus","Belgium","Bosnia and Herzegovina",
  "Bulgaria","Croatia","Czech Republic","Denmark","Estonia","Finland","France",
  "Germany","Greece","Hungary","Iceland","Ireland","Italy","Kosovo","Latvia",
  "Liechtenstein","Lithuania","Luxembourg","Malta","Moldova","Monaco","Montenegro",
  "Netherlands","North Macedonia","Norway","Poland","Portugal","Romania",
  "Russia","San Marino","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Ukraine","UK",'Turkey',
  'Malta')

europe_map <- world_map %>% 
  filter(ID %in% europe_countries) %>% 
  left_join(data, by = c('ID' = colnames(data)[1])) %>% 
  mutate(rate = as.numeric(rate))

sf_use_s2(FALSE) 

labels <- st_centroid(europe_map)
coords <- st_coordinates(labels)
labels$X <- coords[, 1]
labels$Y <- coords[, 2]

labels$X[labels$ID == "Russia"] <- 37
labels$Y[labels$ID == "Russia"] <- 55.5

labels <- labels %>% filter(!is.na(rate))
labels_extremes <- labels %>% 
  filter(rate == min(labels$rate) | rate == max(labels$rate)) %>% 
  mutate(label_text = paste0(ID, "\n", round(rate, 1), "%"))

europe_map %>% 
  ggplot() +
  geom_sf(aes(fill = rate), color = 'white', size = 0.1) +
coord_sf(xlim = c(-10, 42), ylim = c(34, 71), expand = FALSE) + 
theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    text = element_text(family = "serif"), 
    plot.title = element_text(hjust = 0, face = "bold", size = 18, margin = margin(t=15, l=10)),
    plot.subtitle = element_text(hjust = 0, size = 11, color = "grey20", margin = margin(l=10, b=15)),
    plot.caption = element_text(hjust = 1, color = "grey40", size = 9, margin = margin(t=15, r=10, b=5)),
    legend.position = c(0.12, 0.85),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "white", color = NA)
  ) + 
  scale_fill_gradientn(
    colors = c("#228b22", "#ffd700", "#cd5c5c"),
    name = "Public Debt\n(% of GDP)"
  ) + 
  labs(
    title = "General government gross debt (% of GDP, 2024)",
    subtitle = "The gross debt to GDP ratio measures the total gross government debt as a percentage of the\ncountry's GDP. It is a key indicator of a country's financial health and its ability to pay back debts.",
    caption = "Source: IMF World Economic Outlook (October 2024)"
  ) +
  geom_label_repel(
    data = labels_extremes, 
    aes(X, Y, label = label_text),
    family = "serif",
    size = 4,
    fontface = "italic",
    color = "black",
    fill = alpha("white", 0.8), 
    label.size = 0.35,          
    box.padding = 0.5,
    min.segment.length = 0,
    segment.size = 0.5,
    segment.color = "black"
  )