library(ggplot2)
library(dplyr)

#  link do posta z wykresem: https://x.com/berndatte_john/status/1369589313248313351
# link do zbioru danych: https://github.com/rfordatascience/tidytuesday/blob/main/data/2021/2021-01-05/transit_cost.csv
#
#  Co jest nie tak z tym wykresem: 
#  1. kolory - skoro miasta są podpisane na osi X, nie ma potrzeby kolorować każdego z nich na inny kolor,
#  tym bardziej jeśli mamy tu niekonsekwencje co do doboru kolorów, np. na zielono na pierwszym wykresie 
#  zaznaczone jest San Jose a na drugim Nowy Jork. Jest to nieczytelne i wprowadza w błąd, lepiej użyć po jednym kolorze
#  na każdym z wykresów
#  2. Nazwy kolumn na osiach Y (czyli Total_length i Total_cost), to zapewne nazwy kolumn z kodu, z użyciem "_",
#  co wygląda nieprofesjonalnie, lepiej zamienić labels na takie bez "_" ("Całkowita długość" / "Total length" 
#  i "Całkowity koszt" / "Total cost")
#  3. Mylące jednostki na osi Y na drugim wykresie - widnieją tam wartości $10,000 $20,000 $30,000, jednak 
#  te liczby w dostępnej ramce danych to liczby MILIONÓW $ a nie pojedynczych $, zatem $10.000 to tak naprawde 10 miliardów $, 
#  jest to poważny bład, ponieważ wprowadza oglądających w ogromny błąd i dezorientacje
#  4. Dodatkowo dla lepszego porównania lepiej posortować słupki odpowiadające miastom wg wartości (a nie alfabetycznie)



# wczytanie danych z których będziemy korzystać
transit_cost <- read.csv("transit_cost.csv")


# przygotowanie danych do wykresu
data_do_wykresu <- transit_cost %>% 
  filter(country == "US") %>% 
  mutate(real_cost = as.numeric(real_cost)) %>% 
  mutate(city = str_replace(city, "New York", "Nowy Jork")) %>% 
  group_by(city) %>%
  summarise(
    Total_length = sum(length, na.rm = TRUE),
    Total_cost = sum(real_cost, na.rm = TRUE)/1000 # żeby było bardziej czytelne w mld $
  ) %>%
  arrange(Total_cost) %>% # sortujemy wg kosztów
  mutate(city = factor(city, levels = city)) %>% 
  pivot_longer(cols = c(Total_length, Total_cost), 
               names_to = "Metric", 
               values_to = "Value")


# etykiety - czytelne
metric_labels <- c(
  "Total_cost" = "Całkowity koszt (mld USD)",  
  "Total_length" = "Całkowita długość (km)"
)

# rysujemy poprawiony wykres
ggplot(data_do_wykresu, aes(x = city, y = Value)) +
  geom_col(aes(fill = Metric), show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free", labeller = as_labeller(metric_labels)) +
coord_flip() + # bardziej czytelne - w jednym rzędzie jedno miasto 
  scale_fill_manual(values = c("darkgreen", "magenta4")) + # po jednym kolorze na każdy z 2 wykresów
  labs(
    title = "Infrastruktura tranzytowa w miastach USA",
    subtitle = "Zestawienie jej kosztów i długości",
    x = NULL,
    y = NULL,
    caption = "Źródło: TidyTuesday 2021-01-05"
  ) +
  theme_minimal() 

# Poprawiony wykres jest lepszy od poprzedniego ponieważ:
# 1.kolory - ograniczyliśmy się do 2 kolorów, co poprawia czytelnośc wykresu 
# i nie wprowadza oglądającego w błąd i dezorientacje
# 2. Ładne i "profesjonalne" etykiety, bez niepotrzebnych znaków, bardziej czytelne
# 3. Pozbycie się problemu z jednostkami, jasno opisane osie (wartości w mld USD i km)
# 4. Miasta posortowane wg całkowitego kosztu, co ułatwia porównanie miast wg tej wartości 
# (np. Seattle i San Francisco - słupki są podobne ale dzięki posortowaniu wiemy że to w Seattle 
# wydano więcej na infrastrukturę tranzytową)
