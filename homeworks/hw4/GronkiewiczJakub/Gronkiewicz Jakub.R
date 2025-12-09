library(tidyverse)
library(scales)


# link do posta: https://x.com/JonMinton/status/1689246298019368960?s=20
# link do danych: https://github.com/rfordatascience/tidytuesday/tree/main/data/2023/2023-08-08

# Oryginalny wykres pokazuje zmianę procentową ostrości względem poprzedniego sosu,
# co jest mylące, ponieważ przy skali ostrości rosnącej wykładniczo, procenty ukrywają rzeczywistą różnicę
# w odczuwalnym bólu. Dodatkowo wykres zawiera elementy statystyczne (kropki i linie), które nie są opisane
# w legendzie, przez co są niezrozumiałe dla odbiorcy.



sauces <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/sauces.csv')
df <- sauces %>%
  filter(sauce_number <= 10, !is.na(scoville))

ggplot(df, aes(x = factor(sauce_number), y = scoville)) +
  geom_boxplot(fill = "orange", alpha = 0.6, outlier.shape = NA) +
  geom_point(alpha = 0.4, position = "jitter") +
  scale_y_log10(labels = comma) +
  labs(
    title = "Rzeczywista ostrość sosów w Hot Ones",
    subtitle = "Wartości w skali Scoville'a (logarytmicznie)",
    x = "Numer sosu",
    y = "Ostrość (SHU)",
  )

# Zamiast abstrakcyjnych procentów, wykres pokazuje rzeczywiste wartości ostrości (SHU) na skali logarytmicznej,
# co lepiej oddaje naturę tych danych i wrażenia z jedzenia (gwałtowny wzrost "mocy" przy ostatnich sosach).
# Użycie standardowego wykresu boxplot jest też bardziej czytelne i powszechnie zrozumiałe niż
# niestandardowe kropki z oryginału.

