# Imię i Nazwisko: Aleksandra Sitkowska
# Link do oryginalnej wizualizacji: https://aditya-dahiya.github.io/projects_presentations/data_vizs/tidy_usa_gas_prices.html
# Link do danych: https://github.com/rfordatascience/tidytuesday/tree/main/data/2025/2025-07-01

# Co w oryginalnej wizualizacji wymagało poprawy:
# a) Oryginalna wizualizacjia miała pokazywać co w danym tygodniu było droższe benzyna czy diesel, jednak
# wykres nie był przejrzysty i w niektórych miejscach linia jest na tyoe cienka, że ciężko zobaczyć kolor.
# b) Zarówno tytuł jak i legenda są na siatce wykresu przez co ciężej się patrzy na wartości na pionowej osi,
# a wykres jest mniej przejrzysty.

# W czym poprawiony wykres jest lepszy?
# Na wykresie dalej widać kiedy ceny diesla, a kiedy benzyny były wyższe, ale oprócz tego możemy odczytać ceny
# tego drugiego paliwa(tego w danym okresie z niższą cena, którego ceny nie widać na oryginalnej wizualizacji). Dodatkowo,
# na nowej wizualizacji możemy odczytać średnie roczne ceny dla obydwu paliw co jescze bardziej pokazuje nam trend.
# Zarówno legenda, jak i tytuł zostały umieszczone w takim miejscu, że nie przeszkadzaja w analizie i odczytywaniu wartości
# z wykresu. Poza tym nowa wizualizacjia jest bardziej estetyczna i przyjemna dla oka.


library(tidyverse)
library(lubridate)
library(scales)


url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-01/weekly_gas_prices.csv"
df <- read_csv(url)

# wybieram all & all zeby nie dublowac danych
df2 <- df %>%
  mutate(date = ymd(date)) %>%
  filter(
    (fuel == "gasoline" & grade == "all" & formulation == "all") |
      fuel == "diesel")

df_yearly <- df2 %>%
  mutate(year = year(date)) %>%
  group_by(year, fuel) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")


p <- ggplot() +
  # weekly
  geom_line(
    data = df2,
    aes(x = date, y = price, group = fuel, color = fuel, alpha = fuel),
    linewidth = 0.8, alpha = 0.3) +
  # yearly avg
  geom_line(
    data = df_yearly,
    aes(x = as.Date(paste0(year, "-06-30")), y = avg_price, color = fuel),
    linewidth = 1.5) +
  geom_point(
    data = df_yearly,
    aes(x = as.Date(paste0(year, "-06-30")), y = avg_price, color = fuel),
    size = 2) +
  labs(
    title = "Fuel Prices in the US 1995 - 2025: Weekly Data with Annual Average",
    subtitle = "Weekly prices (semi-transparent) and yearly averages",
    x = "Date",
    y = "Price (USD per gallon)",
    color = "Fuel Type",
    caption = "Data: TidyTuesday 01.07.2025, weekly gas prices") +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.position = "right",
    panel.grid.minor = element_blank())

p
ggsave("nowa_wizualizacja1.png", plot = p)
