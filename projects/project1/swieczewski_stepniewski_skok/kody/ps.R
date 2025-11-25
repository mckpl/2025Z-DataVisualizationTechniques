library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(patchwork)

european_countries <- c(
  "Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark",
  "Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland",
  "Italy","Latvia","Lithuania","Luxembourg","Malta","Moldova","Netherlands","Norway",
  "Poland","Portugal","Romania","Russia","Serbia","Slovakia","Slovenia","Spain",
  "Sweden","Switzerland","Ukraine","United Kingdom"
)

country_pl <- c(
  "Austria" = "Austria", "Belgium" = "Belgia", "Bulgaria" = "Bułgaria",
  "Croatia" = "Chorwacja", "Cyprus" = "Cypr", "Czech Republic" = "Czechy",
  "Denmark" = "Dania", "Estonia" = "Estonia", "Finland" = "Finlandia",
  "France" = "Francja", "Germany" = "Niemcy", "Greece" = "Grecja",
  "Hungary" = "Węgry", "Iceland" = "Islandia", "Ireland" = "Irlandia",
  "Italy" = "Włochy", "Latvia" = "Łotwa", "Lithuania" = "Litwa",
  "Luxembourg" = "Luksemburg", "Malta" = "Malta", "Moldova" = "Mołdawia",
  "Netherlands" = "Holandia", "Norway" = "Norwegia", "Poland" = "Polska",
  "Portugal" = "Portugalia", "Romania" = "Rumunia", "Russia" = "Rosja",
  "Serbia" = "Serbia", "Slovakia" = "Słowacja", "Slovenia" = "Słowenia",
  "Spain" = "Hiszpania", "Sweden" = "Szwecja", "Switzerland" = "Szwajcaria",
  "Ukraine" = "Ukraina", "United Kingdom" = "Wielka Brytania"
)

df <- read.csv("C:/Users/Piotr/Desktop/twd2/olympics_1896_2024.csv") %>%
  filter(!is.na(medal), team %in% european_countries) %>%
  mutate(team = country_pl[team])

medals_year <- df %>%
  group_by(season, year, team, sex) %>%
  summarise(Total = n(), .groups = "drop")

# bierzemy ostatnie 10 rywalizacji 
summer_years <- medals_year %>% 
  filter(season == "Summer") %>%
  distinct(year) %>%
  arrange(desc(year)) %>%
  head(10) %>%
  pull(year) %>%
  sort()

winter_years <- medals_year %>% 
  filter(season == "Winter") %>%
  distinct(year) %>%
  arrange(desc(year)) %>%
  head(10) %>%
  pull(year) %>%
  sort()

"
Gdyby wziąć top 10 każdego roku to wykres byłby z lekka nieczytelny bo na przestrzeni ostatnich
10 rywalizacji wystarczy że państwo będzie tylko raz w top 10 i na legendzie pojawia się 20 państw...
Dlatego narzuciłem stały zbiór państw - te z najwiekszą historią olimpijską (top 10 najwięcej medali od 18XX. do 2024)
W obrębie kolumn naturalna kolejność: z tych państw kolejno od najlepszego do najgorszego (z podziałem na płcie) po liczbie
medali w danym roku, w razie braku rywalizacji // 0 zachowujemy relatywne pozycje 
"

winter_top1120 <- medals_year %>%
  filter(season == "Winter") %>%
  group_by(team) %>%
  summarise(TotalMedals = sum(Total)) %>%
  arrange(desc(TotalMedals)) %>%
  slice(11:20) %>%
  pull(team)

winter_male_data <- medals_year %>%
  filter(season == "Winter", sex == "M", team %in% winter_top1120) %>%
  complete(year = winter_years, team = winter_top1120, fill = list(Total = 0)) %>%
  filter(year %in% winter_years) %>%
  group_by(year) %>%
  arrange(desc(Total)) %>%
  mutate(Rank = 11:20) %>%
  ungroup()

winter_female_data <- medals_year %>%
  filter(season == "Winter", sex == "F", team %in% winter_top1120) %>%
  complete(year = winter_years, team = winter_top1120, fill = list(Total = 0)) %>%
  filter(year %in% winter_years) %>%
  group_by(year) %>%
  arrange(desc(Total)) %>%
  mutate(Rank = 11:20) %>%
  ungroup()

summer_top10 <- medals_year %>%
  filter(season == "Summer") %>%
  group_by(team) %>%
  summarise(TotalMedals = sum(Total)) %>%
  arrange(desc(TotalMedals)) %>%
  head(10) %>%
  pull(team)

summer_male_data <- medals_year %>%
  filter(season == "Summer", sex == "M", team %in% summer_top10) %>%
  complete(year = summer_years, team = summer_top10, fill = list(Total = 0)) %>%
  filter(year %in% summer_years) %>%
  group_by(year) %>%
  arrange(desc(Total)) %>%
  mutate(Rank = 1:10) %>%
  ungroup()

summer_female_data <- medals_year %>%
  filter(season == "Summer", sex == "F", team %in% summer_top10) %>%
  complete(year = summer_years, team = summer_top10, fill = list(Total = 0)) %>%
  filter(year %in% summer_years) %>%
  group_by(year) %>%
  arrange(desc(Total)) %>%
  mutate(Rank = 1:10) %>%
  ungroup()

col_summer <- setNames(colorRampPalette(brewer.pal(8, "Dark2"))(length(summer_top10)), summer_top10)
col_winter <- setNames(colorRampPalette(brewer.pal(8, "Dark2"))(length(winter_top1120)), winter_top1120)

p_winter_male <- ggplot(winter_male_data, aes(x = factor(year), y = Rank, color = team, group = team)) +
  geom_line(data = winter_male_data %>% filter(team == "Polska"), size = 1.5) +
  geom_point(size = 4) +
  scale_y_reverse(limits = c(20, 11), breaks = 11:20) +
  scale_x_discrete(labels = winter_years) +
  scale_color_manual(values = col_winter, name = "") +
  labs(subtitle = "Mężczyzn") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_blank(),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

p_winter_female <- ggplot(winter_female_data, aes(x = factor(year), y = Rank, color = team, group = team)) +
  geom_line(data = winter_female_data %>% filter(team == "Polska"), size = 1.5) +
  geom_point(size = 4) +
  scale_y_reverse(limits = c(20, 11), breaks = 11:20) +
  scale_x_discrete(labels = winter_years) +
  scale_color_manual(values = col_winter, guide = "none") +
  labs(subtitle = "Kobiet") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

p_winter <- (p_winter_male + p_winter_female) + 
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Zimowe Igrzyska Olimpijskie",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  )

p_summer_male <- ggplot(summer_male_data, aes(x = factor(year), y = Rank, color = team, group = team)) +
  geom_line(data = summer_male_data %>% filter(team == "Polska"), size = 1.5) +
  geom_point(size = 4) +
  scale_y_reverse(limits = c(10, 1), breaks = 1:10) +
  scale_x_discrete(labels = summer_years) +
  scale_color_manual(values = col_summer, name = "") +
  labs(subtitle = "Mężczyzn") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_blank(),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

p_summer_female <- ggplot(summer_female_data, aes(x = factor(year), y = Rank, color = team, group = team)) +
  geom_line(data = summer_female_data %>% filter(team == "Polska"), size = 1.5) +
  geom_point(size = 4) +
  scale_y_reverse(limits = c(10, 1), breaks = 1:10) +
  scale_x_discrete(labels = summer_years) +
  scale_color_manual(values = col_summer, guide = "none") +
  labs(subtitle = "Kobiet") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

p_summer <- (p_summer_male + p_summer_female) + 
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Letnie Igrzyska Olimpijskie",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  )

p_winter
p_summer






