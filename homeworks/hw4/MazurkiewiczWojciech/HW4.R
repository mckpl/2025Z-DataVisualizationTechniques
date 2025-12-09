library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(patchwork)

#oryginalny post:
#https://www.instagram.com/p/DQ7roaWCRsj/?img_index=1 
#problem z oryginalnym wykresem jawi się następująco:
#autorzy chcą przedstawić "nierówny rozkład śmiertelności" oraz
#"jak ważne jest monitorowanie tych danych", jednak ich wykres nie pozwala na żadne z wymienionych
#poprzez wielkość populacji indii dane dla innych państw tracą wartość
#dlatego więc trzeba zamienić sposób przedstawiania danych na przedstawiający
#liczbę zgonów na 100 000 mieszkańców uzyskując tym samym wykres lepiej ukazujący nasz problem
#dodatkowo nie pokazujemy w żaden sposób trendu, czyli naszego "monitorowania"
#Również skala kolorystyczna użyta na wykresie prezentuje przed nami problem- braki danych mogą
#nie kontrastować z wartościami pośrednimi. 
#dlatego:
#1) trzeba zmienić sposób przedstawienia danych (zgony/100k)
#2) zmienic gamę kolorystyczną
#3) aby uzyskać zamysł autorów dodać wykres trendu 
#lepiej zeksplorują one dane którymi chcemy się zająć.





who_tb_data <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-11/who_tb_data.csv"
)

mort_num_col <- names(who_tb_data) %>%
  str_subset("^e_mort.*num$") %>%
  .[1]



pop_col <- names(who_tb_data) %>%
  str_subset("^e_pop.*num$") %>%
  .[1]





tb_map_cum100k <- who_tb_data %>%
  filter(year >= 2000, year <= 2023) %>%
  group_by(iso3, country) %>%
  summarise(
    mort_sum = sum(.data[[mort_num_col]], na.rm = TRUE),
    pop_mean = mean(.data[[pop_col]], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mort_cum_100k = ifelse(pop_mean > 0, mort_sum / pop_mean * 100000, NA_real_)
  )

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

world_tb_cum <- world %>%
  left_join(tb_map_cum100k, by = c("iso_a3" = "iso3"))

p_cum100k <- ggplot(world_tb_cum) +
  geom_sf(aes(fill = mort_cum_100k), color = NA) +  # <- brak granic
  scale_fill_viridis_c(
    option = "C",
    na.value = "grey92",
    name = "Suma zgonów\nna 100 tys.\n(2000–2023)"
  ) +
  labs(
    title = "Gruźlica na świecie",
    subtitle = "Skumulowana liczba zgonów przeliczona na 100 tys. mieszkańców, 2000–2023"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )



tb_trend_global <- who_tb_data %>%
  filter(year >= 2000, year <= 2023) %>%
  group_by(year) %>%
  summarise(
    mort_sum = sum(.data[[mort_num_col]], na.rm = TRUE),
    pop_sum  = sum(.data[[pop_col]], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mort_100k = ifelse(pop_sum > 0, mort_sum / pop_sum * 100000, NA_real_)
  )

p_trend_global <- ggplot(tb_trend_global, aes(x = year, y = mort_100k)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Trend zgonów na gruźlicę na świecie",
    subtitle = "Zgony na 100 tys. mieszkańców, 2000–2023",
    x = NULL,
    y = "Zgony na 100 tys."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

(p_cum100k / p_trend_global) +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title = "Gruźlica: mapa skumulowanych zgonów i trend globalny",
    caption = "Źródło: WHO TB Burden Data | #TidyTuesday 2025-11-11"
  )
