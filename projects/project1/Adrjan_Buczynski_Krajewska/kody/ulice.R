library(RSQLite)
library(dplyr)
library(ggplot2)
library(forcats)
library(sf)

setwd("dev/pw/twd/project1")

con <- dbConnect(RSQLite::SQLite(), dbname = "./data.db")

dbListTables(con)
dbListFields(con, "ulic")
dbListFields(con, "budynki")

df <- dbGetQuery(con, "
  -- EXPLAIN QUERY PLAN
  SELECT ulic.nazwa, klasyfikacje.kategoria, COUNT(*) AS n_ulice, SUM(ulic.n_budynki) AS n_budynki
  FROM (
    SELECT ulic.nazwa, COUNT(*) AS n_budynki
    FROM ulic LEFT JOIN budynki ON ulic.SIMC = budynki.SIMC AND ulic.ULIC = budynki.ULIC
    GROUP BY ulic.SIMC, ulic.ULIC
  ) AS ulic
  INNER JOIN klasyfikacje ON klasyfikacje.nazwa = ulic.nazwa
  GROUP BY klasyfikacje.nazwa
  ORDER BY n_budynki DESC
;")

df %>%
  filter(n_budynki < quantile(n_budynki, 0.995)) %>%
  filter(n_ulice < quantile(n_ulice, 0.999)) %>%
  ggplot(aes(x = jitter(n_ulice), y = jitter(n_budynki), color = kategoria)) +
  # ggplot(aes(x = n_ulice, y = n_budynki, color = kategoria)) +
  geom_point(size = 0.5) +
  geom_smooth() +
  labs(x = "Liczba ulic o takiej samej nazwie",
       y = "Suma liczby budynków przy ulicach o tej nazwie")
  # scale_x_log10() +
  # scale_y_log10()

df %>%
  mutate(budynki_na_ulice = n_budynki/n_ulice,
         kategoria = fct_reorder(kategoria, budynki_na_ulice)) %>%
  ggplot(aes(x = kategoria, y = budynki_na_ulice)) +
  geom_violin(fill = "red", color = "red", alpha = 0.2) +
  geom_boxplot(fill = "transparent") +
  scale_y_log10() +
  coord_flip()

df <- dbGetQuery(con, "
  -- EXPLAIN QUERY PLAN
  SELECT ulic.nazwa, klasyfikacje.kategoria, ulic.n_budynki
  FROM (
    SELECT ulic.nazwa, COUNT(budynki.ULIC) AS n_budynki
    FROM ulic LEFT JOIN budynki ON ulic.SIMC = budynki.SIMC AND ulic.ULIC = budynki.ULIC
    GROUP BY ulic.SIMC, ulic.ULIC
  ) AS ulic
  INNER JOIN klasyfikacje ON klasyfikacje.nazwa = ulic.nazwa
  ORDER BY n_budynki DESC
;")

df %>%
  #filter(n_budynki > 1) %>%
  mutate(kategoria = fct_reorder(kategoria, n_budynki)) %>%
  ggplot(aes(x = kategoria, y = n_budynki)) +
  geom_violin(fill = "red", color = "red", alpha = 0.2) +
  geom_boxplot(fill = "transparent") +
  scale_y_log10() +
  coord_flip()

df %>%
  group_by(n_budynki) %>%
  summarise(n = n())

df <- dbGetQuery(con, "
  -- EXPLAIN QUERY PLAN
  SELECT ulic.nazwa, klasyfikacje.kategoria, ulic.n_budynki,
    gminy.GMI, gminy.NAZWA as gmina, rodzaje.NAZWA_DOD as rodzaj
  FROM (
    SELECT ulic.nazwa, COUNT(budynki.ULIC) AS n_budynki, ulic.SIMC
    FROM ulic LEFT JOIN budynki ON ulic.SIMC = budynki.SIMC AND ulic.ULIC = budynki.ULIC
    GROUP BY ulic.SIMC, ulic.ULIC
  ) AS ulic
  LEFT JOIN klasyfikacje ON klasyfikacje.nazwa = ulic.nazwa
  INNER JOIN simc ON ulic.SIMC = simc.SIMC
  LEFT JOIN gminy ON simc.TERC = gminy.GMI
  LEFT JOIN rodzaje ON gminy.RODZ = rodzaje.RODZ
;")

df %>%
  group_by(kategoria, rodzaj) %>%
  summarise(n_budynki = mean(n_budynki)) %>%
  mutate(rodzaj = fct_reorder(rodzaj, -n_budynki)) %>%
  ggplot(aes(x = rodzaj, fill = kategoria, y = n_budynki)) +
  geom_col()

df %>%
  filter(n_budynki > 0) %>%
  filter(!is.na(kategoria)) %>%
  filter(!(rodzaj %in% c("delegatura", "dzielnica"))) %>%
  mutate(kategoria = fct_reorder(kategoria, -n_budynki)) %>%
  ggplot(aes(x = kategoria, fill = kategoria, y = n_budynki)) +
  facet_wrap(vars(rodzaj)) +
  #geom_violin() +
  geom_boxplot(varwidth = TRUE, width = 1.3, outlier.shape = ".") +
  #scale_y_log10() +
  coord_flip(ylim = c(0, 50)) +
  theme(legend.position = "none")


df %>%
  filter(n_budynki > 0) %>%
  filter(n_budynki < quantile(n_budynki, 0.8)) %>%
  filter(!is.na(kategoria)) %>%
  filter(!(rodzaj %in% c("delegatura", "dzielnica"))) %>%
  mutate(kategoria = fct_reorder(kategoria, -n_budynki)) %>%
  ggplot(aes(x = kategoria, fill = kategoria, y = n_budynki)) +
  facet_wrap(vars(rodzaj)) +
  geom_boxplot(varwidth = TRUE, width = 1.3, outlier.shape = ".") +
  coord_flip(ylim = c(0, 20)) +
  theme(legend.position = "none")

df %>%
  filter(n_budynki > quantile(n_budynki, 0.8)) %>%
  filter(!is.na(kategoria)) %>%
  filter(!(rodzaj %in% c("delegatura", "dzielnica"))) %>%
  mutate(kategoria = fct_reorder(kategoria, -n_budynki)) %>%
  ggplot(aes(x = kategoria, fill = kategoria, y = n_budynki)) +
  facet_wrap(vars(rodzaj)) +
  geom_boxplot(varwidth = TRUE, width = 1.3, outlier.shape = ".") +
  coord_flip(ylim = c(0, 100)) +
  theme(legend.position = "none")

df %>%
  filter(!is.na(kategoria)) %>%
  # filter(n_budynki > 0) %>%
  mutate(q = cut(n_budynki,
                 quantile(n_budynki, seq(0, 1, length.out = 7)),
                 include.lowest = TRUE)) %>%
  group_by(q, kategoria) %>%
  summarise(n = n()) %>%
  group_by(q) %>%
  mutate(kategoria = fct_reorder(kategoria, n)) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(x = q, y = proportion, fill = kategoria)) +
  geom_col() +
  labs(x = "ilość budynków przy ulicy",
       y = "część wszystkich ulic")

df %>%
  filter(!is.na(kategoria)) %>%
  mutate(q = cut(n_budynki,
                 quantile(n_budynki, seq(0, 1, length.out = 7)),
                 include.lowest = TRUE)) %>%
  group_by(kategoria, q) %>%
  summarise(n = n()) %>%
  group_by(kategoria) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(x = kategoria, y = proportion, fill = q)) +
  geom_col() +
  scale_y_reverse()

# best plot I guess
df %>%
  # filter(!is.na(kategoria)) %>%
  mutate(q = cut(n_budynki,
                 # 10 ^ seq(0, 3, length.out = 10),
                 c(0, 1, 2, 5, 10, 20, 50, 100, 200, +Inf),
                 right = FALSE)) %>%
  group_by(q, kategoria) %>%
  summarise(n = n()) %>%
  group_by(q) %>%
  mutate(kategoria = fct_reorder(kategoria, n)) %>%
  mutate(proportion = n / sum(n), number = sum(n)) %>%
  ggplot(aes(x = q, y = proportion, fill = kategoria)) +
  geom_col() +
  labs(x = "ilość budynków przy ulicy",
       y = "część wszystkich ulic")

df %>% filter(n_budynki >= 500)

gminy <- st_read("./data/granice/A03_Granice_gmin.shp", quiet=TRUE)

gminy %>%
  inner_join(df %>%
               group_by(GMI) %>%
               summarize(mean_budynki = mean(n_budynki)),
             by = join_by(JPT_KJ_I_1 == GMI)) %>%
  ggplot(aes(fill = mean_budynki)) +
  geom_sf()

setdiff(gminy$JPT_KOD_JE, df$GMI) %>% sort() %>% top_n(20)
setdiff(df$GMI, gminy$JPT_KOD_JE)

unique(df$GMI) %>% length()
symdiff(df$GMI, gminy$JPT_KOD_JE) %>% length()
symdiff(df$GMI, gminy$JPT_KJ_I_1) %>% length()
symdiff(df$GMI, gminy$JPT_ID) %>% length()

setdiff(gminy$JPT_KOD_JE, df$GMI) %>% length()
setdiff(gminy$JPT_KJ_I_1, df$GMI) %>% length()
setdiff(gminy$JPT_ID, df$GMI) %>% length()

setdiff(df$GMI, gminy$JPT_KOD_JE) %>% length()
setdiff(df$GMI, gminy$JPT_KJ_I_1) %>% length()
setdiff(df$GMI, gminy$JPT_ID) %>% length()

df %>%
  group_by(GMI) %>%
  summarize(mean_budynki = mean(n_budynki)) %>%
  count(mean_budynki)
  slice_min(mean_budynki, n = 10)

df %>%
  filter(rodzaj %in% c("gmina miejska", "miasto")) %>%
  slice_max(n_budynki, n = 10)

df %>%
  filter(rodzaj %in% c("gmina wiejska", "obszar wiejski")) %>%
  slice_max(n_budynki, n = 10)

df %>%
  select(rodzaj) %>%
  table()

df %>%
  filter(rodzaj == "delegatura") %>%
  head(10)

dbDisconnect(con)
