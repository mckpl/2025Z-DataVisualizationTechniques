library(sf)
library(dplyr)
library(ggplot2)
library(giscoR)
library(extrafont)
loadfonts(device="win")
#dane
#########################
df <- read.csv("29.csv")
df <- df %>% 
  mutate(id = substr(TERYT, 1, nchar(TERYT)))
df <- df %>% 
  select(Gmina, Powiat, OGÓŁEM, id)
########################################

#mapa gmin
###################################################
gminy <- gisco_get_lau(country = "Poland", epsg = 4326, cache = TRUE)
gminy <- gminy %>% rename(Gmina = LAU_NAME)
gminy <- gminy %>% 
  mutate(id = paste0(substr(LAU_ID, 5, 6), substr(LAU_ID, nchar(LAU_ID)-3, nchar(LAU_ID)))) %>% 
  mutate(id = sub("^0+", "", id))
gminy <- gminy %>% 
  select(Gmina, id)
##################################################
#łączenie danych do mapy
#####################################
df_gminy <- left_join(gminy, df, by = "id")
df_gminy <- unique(df_gminy)
df_gminy <- df_gminy %>% 
  select(Powiat, OGÓŁEM) %>% 
  group_by(Powiat) %>%
  summarise(
    OGÓŁEM = sum(OGÓŁEM, na.rm = TRUE),
    .groups = "drop",
    do_union = TRUE
  ) %>% 
  st_make_valid()
####################################

##wykres
breaks = c(0, 25, 50, 75, 200)
 

p <- ggplot(df_gminy) +
  geom_sf(aes(fill = OGÓŁEM), color = "#b69287", linewidth = 0.1) +
  scale_fill_steps(
    breaks = breaks,
    n.breaks = length(breaks),
    high = "#520519",
    low = "#decab3",
    name = "Liczba pożarów"
  )+
  theme_void()+
  theme(
    legend.position = "right",
    legend.title = element_text(family = "Times New Roman", face = "bold", size = 22, margin = margin(b = 5)),
    legend.text = element_text(family = "Times New Roman", size = 22),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )
p
ggsave("wykres_pozarow_powaity.pdf", plot = p, width = 9, height = 9, device = cairo_pdf, bg = "transparent")

