library(wordcloud2)
library(RSQLite)
library(dplyr)
library(ggwordcloud)
library(showtext)
library(purrr)

font_add("Drogowskaz", "./data/Drogowskaz/Drogowskaz.otf")
showtext_auto()

con <- dbConnect(RSQLite::SQLite(), dbname = "./data.db")

df <- dbGetQuery(con, "
           SELECT klasyfikacje.nazwa, klasyfikacje.kategoria, COUNT(*) as count
           FROM klasyfikacje INNER JOIN ulic ON klasyfikacje.nazwa = ulic.nazwa
           GROUP BY klasyfikacje.nazwa
           ORDER BY kategoria, count DESC
           ;")

wordcloud2(data = df %>% filter(kategoria == "Przyrodnicze") %>% select(nazwa, count))

wordcloud2(data = df %>% filter(kategoria == "Geograficzne") %>% select(nazwa, count), size = 0.7)
# size must be smaller because otherwise "Warszawska" is too big and isn't displayed lol

wordcloud2(data = df %>% filter(kategoria == "Patroni") %>% select(nazwa, count), size = 0.4)

wordcloud2(data = df %>% filter(kategoria == "Techniczne") %>% select(nazwa, count))

wordcloud2(data = df %>% filter(kategoria == "Neutralne") %>% select(nazwa, count))

wordcloud2(data = df %>% filter(kategoria == "Inne") %>% select(nazwa, count), size = 3)

wordcloud2(data = df %>% filter(count > 50) %>% select(nazwa, count))

df %>%
  filter(kategoria == "Patroni") %>%
  mutate(nazwa = popraw(nazwa)) %>%
  select(nazwa, count) %>%
  group_by(nazwa) %>%
  summarise(count=sum(count)) %>%
  arrange(desc(count)) %>%
  wordcloud2(size = 0.85, color = rep(c("#fb9", "#f98", "#f89", "#ea8", "#e88"), 1000), backgroundColor = "#444")

df %>%
  mutate(nazwa = sub("^(ul\\. |al\\. |pl\\. )+", "", nazwa)) %>%
  filter(kategoria == "Geograficzne") %>%
  select(nazwa, count) %>%
  wordcloud2(size = 0.78, color = rep(c("#ddf", "#98f", "#abe", "#8ae", "#bcf"), 1000), backgroundColor = "#222")

df %>%
  mutate(nazwa = sub("^(ul\\. |al\\. |pl\\. )+", "", nazwa)) %>%
  filter(kategoria == "Przyrodnicze") %>%
  select(nazwa, count) %>%
  wordcloud2(size = 0.95, color = rep(c("#dfd", "#9f8", "#bea", "#8ea", "#cfb"), 1000), backgroundColor = "#222")

df %>%
  mutate(nazwa = sub("^(ul\\. |al\\. |pl\\. )+", "", nazwa)) %>%
  filter(kategoria == "Techniczne") %>%
  select(nazwa, count) %>%
  wordcloud2(size = 1.0878, color = rep(c("#fc7", "#eb4", "#fb5", "#fd4", "#fd6"), 1000), backgroundColor = "#222")

df %>%
  mutate(nazwa = sub("^(ul\\. |al\\. |pl\\. )+", "", nazwa)) %>%
  filter(kategoria == "Historyczne") %>%
  select(nazwa, count) %>%
  wordcloud2(size = 1.25, color = rep(c("#c8f", "#c9f", "#d9f", "#d8f", "#e8f"), 1000), backgroundColor = "#222")

df %>%
  mutate(nazwa = sub("^(ul\\. |al\\. |pl\\. )+", "", nazwa)) %>%
  filter(kategoria == "Neutralne") %>%
  select(nazwa, count) %>%
  wordcloud2(size = 1.21, color = rep(c("#fc7", "#eb4", "#fb5", "#fd4", "#fd6"), 1000), backgroundColor = "#222")



categories <- c("Neutralne" = "Opisowe",
                "Techniczne" = "Funkcjonalne",
                "Geograficzne" = "Geograficzne",
                "Przyrodnicze" = "Przyrodnicze",
                "Historyczne" = "Historyczne",
                "Patroni" = "Patroni",
                "Inne" = "Inne")

colors <- c("Opisowe" = "#dddd00",
            "Funkcjonalne" = "#ff7f00",
            "Geograficzne" = "#589fe9",
            "Przyrodnicze" = "#4daf4a",
            "Historyczne" = "#b96fc4",
            "Patroni" = "#f55b5d",
            "Inne" = "#888")

colors <- c("Opisowe" = "#ee6",
            "Funkcjonalne" = "#f84",
            "Geograficzne" = "#6af",
            "Przyrodnicze" = "#6d6",
            "Historyczne" = "#e8e",
            "Patroni" = "#f66",
            "Inne" = "#888")

# adjacent square wordcloud facets for different categories
(p <- df %>%
  mutate(kategoria = categories[kategoria]) %>%
  filter(!(kategoria %in% c("Inne"))) %>%
  # remove prefixes from nazwa
  mutate(nazwa = sub("^(ul\\. |al\\. |pl\\. )+", "", nazwa)) %>%
  group_by(kategoria) %>%
  mutate(freq = count / max(count)) %>%
  slice_max(freq, n = 100) %>%
  ungroup() %>%
  ggplot(aes(label = nazwa, size = freq, color = kategoria)) +
  geom_text_wordcloud(area_corr = TRUE,
                      rm_outside = TRUE,
                      family = "Drogowskaz",
                      grid_margin = 0,
                      grid_size = 1) +
  scale_size_area(max_size = 16) +
  scale_color_manual(values = colors) +
  coord_fixed() +
  theme_minimal() +
  facet_wrap(~kategoria, nrow = 2) +
  theme(
    strip.text = element_text(size = 14, family = "Drogowskaz", face = "bold", color = "white"),
    panel.grid = element_blank(),
    plot.background  = element_rect(fill = "#444", color = NA),
    panel.background = element_rect(fill = "#444", color = NA)
    # plot.background  = element_rect(fill = "transparent", color = NA),
    # panel.background = element_rect(fill = "transparent", color = NA)
  ))

ggsave("data/plots/wordcloud.png",
       p,
       width = 12, height = 2.4,    # inches
       dpi = 600,
       # bg = "transparent",
       device = "png")


# przykładowe dane
ulice <- c(
  "Mickiewicza",
  "Adama Mickiewicza",
  "Słowackiego",
  "Juliusza Słowackiego",
  "św. Jana Pawła II",
  "gen. Władysława Sikorskiego"
)


popraw <- function(ulice) {
  # 1. Podział na jedno- i wielowyrazowe
  ulice <- sub("^((ul|al|pl|os)\\. |plac |park |droga |aleja |aleje |rynek |skwer |osiedle |kolonia |bulwary |wzgórze )+", "", ulice, ignore.case = TRUE)
  ulice <- sub("[A-Z][a-ząćęłńóśżź]?\\. ", "", ulice)
  ulice <- sub("[A-Z][a-ząćęłńóśżź]?\\. ", "", ulice)
  ulice <- sub(" [A-Z][a-ząćęłńóśżź]?\\.", "", ulice)
  ulice <- sub(" boczna$", "", ulice, ignore.case=TRUE)
  jedno <- ulice[lengths(strsplit(ulice, " ")) == 1]
  wiele <- ulice[lengths(strsplit(ulice, " ")) > 1]
  
  # 2. Funkcja: dopasuj najkrótszą nazwę, która zawiera dane nazwisko
  dopasuj <- function(nazwisko) {
    kandydaci <- wiele[grepl(nazwisko, wiele)]
    if (length(kandydaci) == 0) return(nazwisko)
    
    # 3. wybór najkrótszej pełnej nazwy
    kandydaci[which.min(nchar(kandydaci))]
  }
  
  # 4. zamiana
  slownik <- setNames(map_chr(jedno, dopasuj), jedno)
  
  ulice_poprawione <- recode(ulice, !!!slownik)
  
  return(ulice_poprawione)
}
