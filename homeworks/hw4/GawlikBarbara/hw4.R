
# 6.1 --------------------------------------------------------------------------


# Link do posta z wykresem:
# https://x.com/simisani10/status/1774780110538690799/photo/1

# Link do zestawu danych z repozytorium TidyTuesday:
# https://github.com/rfordatascience/tidytuesday/tree/main/data/2024/2024-03-26

# Link do repozytorium GitHub autorki wykresu:
# https://github.com/sndaba/2024TidyTuesdayWithRstats/tree/main/week13 



# 6.2 --------------------------------------------------------------------------


# Co zostało zrobione źle, i co trzeba w związku z tym poprawić:

# 1) Zła forma wykresu - wykres kołowy sprawia, że osoba czytająca wykres nie 
#    jest w stanie porównać wartości pomiędzy sobą, tym bardziej, że bardzo
#    niewiele się one od siebie różnią.
# 2) Nie ma legendy i jednostek - nie wiemy, co tak naprawdę przedstawia wykres.
# 3) Tytuł jest niejasny i nieadekwatny - nie wiemy, co autorka rozumie jako 
#    "progression" i nie wiemy też, co oznaczają "different rounds". Ponadto,
#    zaglądając w oryginalny kod autorki, ustaliłam że wykres ten nie pokazuje 
#    żadnego "progression", żadnych zmian odestków typowania w ciągu turnieju.
# 4) Brak opisu/podtytułu - nie wiemy, co oznaczają białe przerwy na wykresie.
# 5) Złe przedstawienie tak dużej ilości danych - wykres staje się zupełnie 
#    nieczytelny dla drużyn o małej wartości "progression".



# 6.3 --------------------------------------------------------------------------


library("dplyr")
library("ggplot2")
library("readr")
library("tidyr")
library("tidytuesdayR")

data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-26/public-picks.csv")

df <- data %>%
  filter(YEAR == 2024) %>%
  select(TEAM, R64, R32, S16, E8, F4, FINALS) %>%
  pivot_longer(
    cols = c(R64, R32, S16, E8, F4, FINALS),
    names_to = "Round",
    values_to = "Percent"
  ) %>%
  mutate(
    Percent = parse_number(Percent),
    Round = factor(Round, levels = c("R64", "R32", "S16", "E8", "F4", "FINALS"))
  )

top_teams <- df %>%
  filter(Round == "FINALS") %>%
  arrange(desc(Percent)) %>%
  slice(1:8) %>%
  pull(TEAM)

df <- df %>%
  mutate(Color_Group = ifelse(TEAM %in% top_teams, TEAM, "Other"))

wykres <- ggplot(df, aes(x = Round, y = Percent, group = TEAM)) +
  
  geom_line(data = df %>%
              filter(Color_Group == "Other"),
            color = "grey85", size = 0.6) +
  
  geom_line(data = df %>% 
              filter(Color_Group != "Other"),
            aes(color = TEAM), size = 1.1) +
  
  geom_point(data = df %>% 
               filter(Color_Group != "Other"),
             aes(color = TEAM), size = 2) +
  
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) +
  scale_x_discrete(expand = expansion(mult = 0.05)) +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728",
                                "#9467BD", "#8C564B", "#64B5F6", "#BCBD22")) +
  
  labs(
    title = "Zmiany typowań drużyn NCAA w kolejnych rundach sezonu 2024",
    subtitle = "Wykres przedstawia odsetek uczestników, którzy typowali zwycięstwo danej drużyny w każdej z faz rozgrywek.",
    x = "Etap turnieju",
    y = "Odsetek typujących",
    color = "Drużyna",
    caption = "Wyróżniono 8 najczęściej typowanych drużyn, pozostałe są zaznaczone na szaro. \nŹródło danych: TidyTuesday 2024, tydz. 12."
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 17),
    plot.margin = margin(10, 10, 10, 10),
    plot.caption = element_text(hjust = 0.5)
  )

wykres

ggsave("hw4_plot.png", wykres, width = 10, height = 6, dpi = 300, bg = "white")



# 6.4 --------------------------------------------------------------------------


# Przygotowany wykres jest lepszy od oryginału, ponieważ:

# 1) Zmieniliśmy formę wykresu - teraz jasno pokazuje on ewolucję typowania
#    poszczególnych drużyn na przestrzeni czasu, i dzięki czytelnej osi OY od 
#    razu widać różnice między poziomami typowań.
# 2) Wykres jest opisany adekwatnie i jednoznacznie poprzez tytuł, podtytuł,
#    opis, jednostki na osiach i legendę.
# 3) Mimo dużej ilości danych, dzięki zastosowaniu różnych kolorów i różnych
#    grubości linii, wykres jest czytelny.
