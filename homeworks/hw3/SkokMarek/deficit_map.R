library(ggplot2)
library(maps)
library(mapdata)
library(sf)
library(dplyr)
library(rnaturalearth)

countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
  "Norway", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain",
  "Sweden", "Switzerland", "United Kingdom","Turkey","Belarus","Ukraine","Russia",
  "Albania","Bosnia and Herz.","Bulgaria","Iraq","Croatia","Kosovo","Montenegro",
  "North Macedonia","Serbia","Algeria","Greece","Slovenia",
  "Georgia", "Armenia", "Syria", "Moldova","Morocco","Tunisia"
)
europe <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name %in% countries)
europe <- europe %>%
  mutate(iso_a2 = case_when(
    name == "France" ~ "FR",
    name == "Cyprus" ~ "CY",
    TRUE ~ iso_a2
  ))
deficyt <- read.csv("tec00127_page_linear_2_0.csv")

def <- deficyt %>% 
  select(geo, Geopolitical.entity..reporting., OBS_VALUE)
colnames(def) <- c("geo", "region", "OBS_VALUE")
def <- def %>%
  mutate(geo = case_when(
    geo == "EL" ~ "GR",
    TRUE ~ geo
  ))
map_data <- europe %>%
  left_join(def, by = c("iso_a2" = "geo"))

max_def <- map_data %>% filter(OBS_VALUE == max(OBS_VALUE, na.rm = TRUE))
min_def <- map_data %>% filter(OBS_VALUE == min(OBS_VALUE, na.rm = TRUE))

max_label <- paste(max_def$name, collapse = ", ")
min_label <- paste(min_def$name, collapse = ", ")

ggplot(map_data) +
  geom_sf(aes(fill = OBS_VALUE), color = "gray30", size = 0.2) +
  scale_fill_gradient2(
    low = "red", mid = "lightblue", high = "darkblue", midpoint = 0,
    name = "Budget balance\n(mln €)"
  ) +
  labs(
    title = "Government deficit/surplus of EU countries in 2024",
    subtitle = paste("Highest deficit:", min_label, 
                     "| Highest surplus:", max_label),
    caption = "Source: https://ec.europa.eu/eurostat/databrowser/view/\ngov_10dd_edpt1__custom_19009086/default/table"
  ) +
  coord_sf(xlim = c(-14, 40), ylim = c(36, 70)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()  
  )

