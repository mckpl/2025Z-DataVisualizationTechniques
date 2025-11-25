library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)


tab_data <- read.csv("data\\2021\\Tab1_Etno-Tabela-1.csv",
                     sep = ";", header = TRUE, fileEncoding = "UTF-8")


tab_clean <- tab_data %>%
  select(Mniejszości, Identyfikacja.pierwsza, Identyfikacja.druga) %>%
  rename(
    identyfikacja = Mniejszości,
    jako_pierwsza = Identyfikacja.pierwsza,
    jako_druga = Identyfikacja.druga
  ) %>%
  filter(
    identyfikacja != "Ogółem",
    identyfikacja != "Nieustalona",
    identyfikacja != "inne",
    identyfikacja != "Inna niż polska"
  ) %>%
  mutate(
    jako_pierwsza = str_replace_all(jako_pierwsza, "\\s", ""),
    jako_druga = str_replace_all(jako_druga, "\\s", "")
  )


tab_clean[tab_clean == "-"] <- 0


tab_clean <- tab_clean %>%
  mutate(
    jako_pierwsza = as.numeric(jako_pierwsza),
    jako_druga = as.numeric(jako_druga)
  ) %>%
  mutate(
    identyfikacja = case_when(
      identyfikacja == "Wyłącznie ponadnarodowa (europejska, światowa itp.)" ~ "Ponadnarodowa",
      identyfikacja == "Bez przynależności narodowo-etnicznej" ~ "Nieustalona",
      TRUE ~ identyfikacja
    )
  ) %>%
  mutate(identyfikacja = str_to_sentence(identyfikacja))


top_10_nazwy <- tab_clean %>%
  filter(identyfikacja != "Polska") %>%
  mutate(lacznie = jako_pierwsza + jako_druga) %>%
  arrange(desc(lacznie)) %>%
  slice_head(n = 20) %>%
  pull(identyfikacja)


tab_top10_long <- tab_clean %>%
  filter(identyfikacja %in% top_10_nazwy) %>%
  pivot_longer(
    cols = c(jako_pierwsza, jako_druga),
    names_to = "typ_identyfikacji",
    values_to = "liczba"
  ) %>%
  filter(liczba > 0) %>%
  mutate(
    typ_identyfikacji = recode(typ_identyfikacji,
                               "jako_pierwsza" = "Deklaracja Pierwsza",
                               "jako_druga" = "Deklaracja Druga")
  )


plot_top10 <- ggplot(tab_top10_long,
                      aes(x = typ_identyfikacji,
                          y = liczba,
                          group = identyfikacja,
                          color = identyfikacja)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = comma) +
  scale_y_log10() + 
  labs(title = "Top 20 Mniejszości Narodowo-Etnicznych w Polsce (2021)",
       subtitle = "Porównanie deklaracji pierwszej i drugiej",
       x = "Typ deklaracji",
       y = "Liczba osób",
       color = "Mniejszość") +
  theme(legend.position = "right",
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background  = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),

        panel.grid.major = element_line(color = "#ffffff"),
        panel.grid.minor = element_line(color = "#ffffff"))


plot_top10


ggsave(plot_top10,
       filename = "plots/Basia/plot_02.png",
       device = "png",
       height = 12,
       width = 10,
       dpi = 300,
       bg = "transparent")