library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(ggnewscale)

male <- read.csv("C:/IAD/Semestr_3/Techniki_Wizualizacji_Danych/Homework/hw3/Male.csv")
female <- read.csv("C:/IAD/Semestr_3/Techniki_Wizualizacji_Danych/Homework/hw3/Female.csv")
total <- read.csv("C:/IAD/Semestr_3/Techniki_Wizualizacji_Danych/Homework/hw3/Total.csv")

df <- left_join(total, female, by = c("Economy", "Year", "Economy.Code"))
df <- left_join(df, male, by = c("Economy", "Year", "Economy.Code"))

df <- df %>% 
  mutate(Male.percent = round(100*Population..male/Population..total, digits = 2)) %>%
  mutate(Female.percent = round(100*Population..female/Population..total, digits = 2))

# Naprawiamy niektóre nazwy w tej ramce danych bo nie pokrywają się z tabelką z biblioteki maps
names <- list(
  "United States" = "USA",
  "Russian Federation" = "Russia",
  "Czechia" = "Czech Republic",
  "Slovak Republic" = "Slovakia",
  "Venezuela, RB" = "Venezuela",
  "Egypt, Arab Rep." = "Egypt",
  "Congo, Dem. Rep." = "Democratic Republic of the Congo",
  "Congo, Rep." = "Republic of Congo",
  "Cote d'Ivoire" = "Ivory Coast",
  "Yemen, Rep." = "Yemen",
  "Kyrgyz Republic" = "Kyrgyzstan",
  "Syrian Arab Republic" = "Syria",
  "Turkiye" = "Turkey",
  "Iran, Islamic Rep." = "Iran",
  "Lao PDR" = "Laos",
  "Korea, Dem. People's Rep." = "North Korea",
  "Korea, Rep." = "South Korea",
  "United Kingdom" = "UK"
)
names <- unlist(names)

df$Economy <- ifelse(
  df$Economy %in% names(names),
  names[df$Economy],
  df$Economy
)

w <- map_data("world")

df1 <- left_join(w, df, by=c("region"="Economy"))

df1$region[which.max(df1$Male.percent)] # Katar 71,28%
df1$region[which.max(df1$Female.percent)] # Mołdawia 53,99%

df_na <- df1 %>%
  filter(is.na("Population..total"))
df_female <- df1 %>%
  filter(!is.na("Population..total"), Female.percent>=Male.percent)
df_male <- df1 %>%
  filter(!is.na("Population..total"), Male.percent>Female.percent)

# W generowaniu wykresu używam pakietu ggnewscale żeby na niebiesko pokolorować te kraje, w których jest więcej mężczyzn i na różowo w przeciwnym przypadku i nałożyć różne skale

ggplot()+
  geom_polygon(
    data = df_na,
    aes(long, lat, group = group),
    fill = "grey",
    color = "white",
    linewidth = 0.1,
  )+
  geom_polygon(
    data = df_female,
    aes(long, lat, group = group, fill = Female.percent),
    color = "white",
    linewidth = 0.1
  )+
  scale_fill_gradient(
    low = "lightpink",
    high = "red",
    name = "Udział kobiet (%)",
    limits = c(50, 75),
    breaks = c(50, 55, 60, 65, 70, 75)
  )+
  new_scale_fill()+
  geom_polygon(
    data = df_male,
    aes(long, lat, group = group, fill = Male.percent),
    color = "white",
    linewidth = 0.1
  )+
  scale_fill_gradient(
    low = "lightblue",
    high = "blue",
    name = "Udział mężczyzn (%)",
    limits = c(50, 75),
    breaks = c(50, 55, 60, 65, 70, 75)
  )+
  coord_fixed(1.3)+
  theme_void()+
  labs(
    title = "Udział kobiet i mężczyzn w liczbie ludności krajów świata",
    caption = "Największy udział kobiet: Mołdawia - 53,99%\nNajwiększy udział mężczyzn: Katar - 71,28%",
  )+
  theme(
    plot.title = element_text(hjust = 0.5)
  )

