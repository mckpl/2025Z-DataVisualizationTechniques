library(dplyr)
library(sf)
library(rnaturalearth)
library(ggplot2)


life_raw <- read.csv("Data.csv", check.names = FALSE)


life_2022 <- life_raw %>%
  rename(
    series_name =  `Series Name`,
    series_code = `Series Code`,
    country  = `Country Name`,
    iso3   = `Country Code`,
    life_exp = `2022 [YR2022]`
  ) %>%
  select(country, iso3, life_exp) %>%
  mutate(
    life_exp = as.numeric(life_exp)
  )


world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::mutate(
    iso3_fix = dplyr::case_when(
      name == "France" ~ "FRA",
      name == "Norway" ~ "NOR",
      TRUE ~ iso_a3
    )
  )


world_life <- world %>%
  dplyr::left_join(life_2022, by =  c("iso3_fix" = "iso3"))


life_stats_df<- world_life %>%
  st_drop_geometry() %>%
  filter(!is.na(life_exp))
     
min_val <-min(life_stats_df$life_exp, na.rm = TRUE)
max_val <- max(life_stats_df$life_exp, na.rm = TRUE)  

kraje_min <- life_stats_df %>%
  filter(life_exp  == min_val) %>%
  pull(name) %>%
  unique()

kraje_max <- life_stats_df %>%
  filter(life_exp == max_val) %>% 
  pull(name) %>%
  unique() 



label_min <- paste(kraje_min,  collapse = ", ")

label_max <- paste(kraje_max, collapse = ", ")


 
cat("Najmniejsza wartość:",round(min_val, 1), "lat –", label_min, "\n")
cat("Największa wartość :", round(max_val, 1), "lat –", label_max, "\n")




p <- ggplot(world_life) +
  geom_sf(aes(fill = life_exp), color = "grey30", size = 0.1) +
  scale_fill_distiller(
    palette = "YlGnBu",   
    na.value = "grey90",
    name = "Lata"
  ) +
  labs(
    title ="Oczekiwana  długość życia przy urodzeniu ",
    subtitle = "Świat, 2022      źródło: World Bank",
    caption = paste0(
      "Najmniejsza wartość: ", round(min_val, 1), " lat (", label_min, ").\n",
      "Największa wartość: ", round(max_val, 1), " lat (", label_max, ")."
    )
  ) +
  theme_minimal() +
  theme(
    plot.title= element_text(size = 16,  face = "bold"),
    plot.subtitle  = element_text(size = 11), 
    plot.caption  = element_text(size = 9),  
    legend.position = "right",
    axis.text = element_blank(), 
    axis.title  = element_blank(),
    panel.grid = element_blank()
  )


print(p)
