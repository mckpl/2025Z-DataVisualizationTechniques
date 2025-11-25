
# Źródło danych:
# Zurlo, Gina A., ' Religions in Europe: A Statistical Summary', in Grace Davie, and Lucian N. Leustean (eds), 
# The Oxford Handbook of Religion and Europe, Oxford Handbooks (2021; online edn, Oxford Academic, 8 Dec. 2021), 
# https://doi.org/10.1093/oxfordhb/9780198834267.005.0001, accessed 17 Nov. 2025. 

library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(scales)
library(gridExtra)
library(patchwork)
library(tibble)

data <- read.csv("homeworks/hw3/AdrjanJerzy/data.csv")

shp <- st_read("https://gisco-services.ec.europa.eu/distribution/v2/countries/gpkg/CNTR_RG_60M_2024_3857.gpkg")

iceland <- shp %>% filter(NAME_ENGL == "Iceland")
rest <- shp %>% filter(NAME_ENGL != "Iceland")
st_geometry(iceland) <- st_geometry(iceland) + c(1100000, 100000)
st_crs(iceland) <- st_crs(rest)
shp <- rbind(rest, iceland)

shp <- st_crop(shp, st_transform(st_as_sfc(st_bbox(c(xmin = -22, ymin = 34, xmax = 50, ymax = 70), crs = st_crs(4326))), 3857))

data_processed <- data %>% 
  mutate(None=100-(Christian+Jewish+Muslim+Eastern.religions)) %>%
  select(Country.Region, Catholic, Orthodox, Protestant, Jewish, Muslim, Eastern.religions, None) %>% 
  pivot_longer(cols=!Country.Region, names_to="Religion") %>% 
  filter(value > 0.0) %>%
  group_by(Country.Region) %>%
  slice_max(value, n=1) %>%
  right_join(shp, join_by(Country.Region == NAME_ENGL)) %>%
  mutate(value = value/100, border = is.na(Religion))

process_df <- function(df, fun) {
  df %>%
    select(Country.Region, Religion, value) %>%
    filter(!is.na(value)) %>%
    group_by(Religion) %>%
    fun(value, n=1) %>%
    select(Country.Region, Religion)
}

most <- inner_join(process_df(data_processed, slice_min), process_df(data_processed, slice_max), by=join_by(Religion)) %>%
  rename(Highest=Country.Region.x, Lowest=Country.Region.y) %>%
  select(Religion, Highest, Lowest)
  
largest <- data_processed %>% group_by(Country.Region, Religion) %>% summarise(max=max(value)) %>% arrange(desc(max)) %>% head(1)
smallest <- data_processed %>% group_by(Country.Region, Religion) %>% summarise(max=max(value)) %>% arrange(max) %>% head(1)

colours <- c("Catholic" = "#aa0", "Orthodox" = "#c39", "Protestant" = "#44e", "Muslim" = "#191", "No religion" = "#555")

enf.colours <- enframe(colours, name = "religion", value = "colour")
map <- data_processed %>%
  mutate(Religion=ifelse(Religion=="None", "No religion", Religion)) %>%
  ggplot(mapping=aes(fill=factor(Religion, levels=c("Catholic", "Orthodox", "Protestant", "Muslim", "No religion", NA)), geometry=geom, alpha=value, colour=border, label=Country.Region)) +
  geom_sf(show.legend = FALSE) +
  theme_minimal() +
  labs(fill="Religion", title="Largest religion by country") +
  scale_fill_manual(
    values = colours,
    breaks = ~.x[!is.na(.x)],
    na.value = "#ffff"
  ) +
  scale_alpha_binned(
    n.breaks=5, labels=scales::percent_format(), range=c(0.2, 1)
  ) +
  scale_colour_manual(
    values=c("#000f", "#fff0"),
    guide="none"
  ) +
  geom_sf_text(
    data = ~dplyr::filter(.x, Country.Region %in% c(largest$Country.Region, smallest$Country.Region)),
    alpha=0.8,
    size=3,
    angle=0
  ) +
  theme(
    panel.grid = element_blank(),  
    axis.title = element_blank(),  
    axis.text = element_blank(),   
    axis.ticks = element_blank(),
    plot.title = element_text(hjust=1.33, vjust=-1, size=28)
  ) +
  annotate("text", x=2000000, y=3800000, label=paste0("Largest plurality: ", largest$Country.Region, " (", largest$Religion, ", ", largest$max*100, "%)\n", paste0("Smallest plurality: ", smallest$Country.Region, " (", smallest$Religion, ", ", smallest$max*100, "%)")))

alpha_legend <- expand_grid(
  religion = enf.colours$religion,
  alpha    = c(0.4, 0.6, 0.8)
) %>%
  left_join(enf.colours, by = join_by(religion)) %>%
  ggplot(aes(x=factor(alpha, labels=c("40%", "60%", "80%")), y=religion)) +
  geom_tile(aes(fill=colour, alpha=alpha), colour="white", width=0.9, height=0.9) +
  scale_fill_identity() +
  scale_alpha(range=c(0.4, 0.8), guide="none") +
  labs(x=NULL, y=NULL) +
  theme_minimal(base_size = 10) +
  theme(
  axis.text.x = element_text(size=12),
  axis.text.y = element_text(size=14, hjust=0),
  panel.grid = element_blank()
  ) + 
  scale_x_discrete(position = "top") +
  scale_y_discrete(position = "right")  +
  ggtitle("Percentage share of religion")

layout <- "
AAAAC 
AAAAB
AAAAB
AAAAD 
"

# Best seen at ~900:700 resolutions
# (maps do not scale well 
# and should be seen at a specific aspect ratio and resolution to look as intended)
map + alpha_legend + NULL + NULL + plot_layout(design=layout)


