library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(patchwork)

minmax <- function(d, name_col, val_col, naglowek){
  d <- d %>% filter(!is.na(.data[[val_col]]))
  mx <- max(d[[val_col]]); mn <- min(d[[val_col]])
  mxn <- d %>% filter(.data[[val_col]] == mx) %>% pull(.data[[name_col]]) %>% unique() %>% sort()
  mnn <- d %>% filter(.data[[val_col]] == mn) %>% pull(.data[[name_col]]) %>% unique() %>% sort()
  paste0(naglowek,
         "\nmax=", mx, "%: ", paste(mxn, collapse = ", "),
         "\nmin=", mn, "%: ", paste(mnn, collapse = ", "))
}
##################
obesity_us_raw <- read.csv("2023-Obesity-by-state.csv")

obesity_us <- obesity_us_raw %>%
  transmute(
    state_name  = State,
    state       = tolower(trimws(State)),
    obesity_pct = as.numeric(gsub(",", ".", Prevalence)),
    ci_95       = X95..CI
  )

usa_map <- map_data("state")

usa_obesity <- usa_map %>%
  left_join(obesity_us, by = c("region" = "state"))

usa_vals <- obesity_us %>%
  semi_join(tibble(state = unique(usa_map$region)), by = "state")

###################
obesity_eur_raw <- read.csv("https://ourworldindata.org/grapher/share-of-adults-defined-as-obese.csv?v=1&csvType=filtered&useColumnShortNames=true&time=2022..latest&overlay=download-data")

value_cols <- setdiff(names(obesity_eur_raw), c("Entity", "Code", "Year", "time"))
value_col  <- value_cols[1]

obesity_eur <- obesity_eur_raw %>%
  transmute(
    country_name = Entity,
    country_key  = tolower(trimws(Entity)),
    obesity_pct  = as.numeric(.data[[value_col]])
  ) %>%
  mutate(
    country_key = recode(
      country_key,
      "czechia"            = "czech republic",
      "north macedonia"    = "macedonia",
      "russian federation" = "russia"
    )
  )

world_map <- map_data("world") %>%
  mutate(region_key = tolower(trimws(region)))

world_obesity <- world_map %>%
  left_join(obesity_eur, by = c("region_key" = "country_key"))

europe_obesity <- world_obesity %>%
  filter(long > -25, long < 45, lat > 34, lat < 72)

mapped_regions <- tibble(country_key = unique(europe_obesity$region_key))
eur_vals <- obesity_eur %>%
  semi_join(mapped_regions, by = "country_key")
###################################
txt_eur <- minmax(eur_vals, "country_name", "obesity_pct", "Europe")
txt_usa <- minmax(usa_vals, "state_name", "obesity_pct", "USA")

global_min <- min(c(usa_vals$obesity_pct, eur_vals$obesity_pct), na.rm = TRUE)
global_max <- max(c(usa_vals$obesity_pct, eur_vals$obesity_pct), na.rm = TRUE)

common_fill <- scale_fill_gradient(
  low = "#e0f0ff", high = "#103060",
  limits = c(global_min, global_max),
  name = "Otyłość (%)",
  na.value = "grey"
)

p_usa <- ggplot(usa_obesity,
                aes(x = long, y = lat, group = group, fill = obesity_pct)) +
  geom_polygon(color = "white") +
  common_fill +
  labs(x = NULL, y = NULL, caption = txt_usa) +
  theme_minimal() +
  theme(
    axis.text  = element_blank(),
    axis.ticks = element_blank()
  )

p_eur <- ggplot(europe_obesity,
                aes(x = long, y = lat, group = group, fill = obesity_pct)) +
  geom_polygon(color = "white") +
  common_fill +
  labs(x = NULL, y = NULL, caption = txt_eur) +
  theme_minimal() +
  theme(
    axis.text  = element_blank(),
    axis.ticks = element_blank()
  )

(p_usa + p_eur) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Porównanie otyłości w Europie z otyłością w Stanach Zjednoczonych wśród osób dorosłych") &
  theme(legend.position = "bottom")
