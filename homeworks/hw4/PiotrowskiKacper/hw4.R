library(dplyr)
library(ggplot2)
library(tidyr)

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-05/trashwheel.csv')

# Link do posta z oryginalnym wykresem: https://x.com/simisani10/status/1767540926304415942?s=20
# Link do repozytorium autora: https://github.com/sndaba/2024TidyTuesdayWithRstats/blob/main/week10/week10TidyTuesday.R
# Link do repozytorium z oryginalnymi danymi: https://github.com/rfordatascience/tidytuesday/tree/main/data/2024/2024-03-05

# Co jest nie tak z oryginalnym wykresem
# Wykres jest w zasadzie jedynie grafiką z obrazkami i liczbami.
# Siatka w tle sugeruje, że powinna pojawić się tu skala, jednak reprezentacja śmieci jako obrazki uniemożliwia wyczytanie tego z wykresu.
# Błędem jest również wykorzystanie obrazków zamiast teksu. Na pierwszy rzut oka trudno określić różnicę między szklanymi a plastikowymi butelkami.
# Kategoria HomesPowered reprezentowana domem również jest dezorientująca.

df <- df %>% 
  filter(Year == 2023 | Year == 2022 | Year == 2021 | Year == 2020) %>% 
  select(-all_of(c("ID", "Name", "Dumpster", "Month", "Date", "Weight", "Volume"))) %>% 
  group_by(Year) %>% 
  summarise(across(everything(), sum, na.rm = TRUE)) %>% 
   pivot_longer(
     cols = -Year,
     names_to = "Trash",
     values_to = "Count"
  )

df <- df %>% 
  group_by(Year) %>% 
  mutate(Percentage = round(Count / sum(Count) * 100, 2)) %>% 
  ungroup()

fig <- df %>% 
  ggplot(aes(x = Percentage, y = factor(Year), fill=Trash))+
  geom_col()+
  labs(
    x = "Trash percentage",
    y = "Year",
    fill = "Trash type",
    title = "Trash disposed by the Trashwheel in years 2020-2023"
  )+
  scale_fill_manual(
    values = c(
      CigaretteButts="#CBABD1",
      GlassBottles="#9656A2",
      HomesPowered="#369ACC",
      PlasticBags="#95CF92",
      PlasticBottles="#F8E16F",
      Polystyrene="#F4895F",
      SportsBalls="#FF024c",
      Wrappers="#6F1926"
  ))+
  scale_x_continuous(breaks = seq(0, 100, by=10))+
  theme_minimal()

fig

# Przygotowany wykres jest lepszy od oryginału, bo skala na osi x ułatwia porównanie wartości z samego patrzenia na wykres.
# Zastosowanie kolorów zamiast grafik jest bardziej eleganckie i dokładnie wiadomo, który kolor co reprezentuje
# Pozostałe lata dodałem, by móc porównać zmianę na przestrzeni lat i żeby wizualizacja była pełniejsza.