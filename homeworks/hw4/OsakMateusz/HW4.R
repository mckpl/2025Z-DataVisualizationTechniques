library(tidyverse)

# Link do posta: https://x.com/simisani10/status/1774290101717094552
# Link do zestawu danych: https://github.com/rfordatascience/tidytuesday/tree/main/data/2024/2024-03-19





# BŁĘDY WYKRESU
#-----------------
# - użycie wykresów kołowych, co utrudnia porównywanie danych dla różnych postaci.
# - Poucinane etykiety
# - nie ma określoneg kąta, od którego autor rozpoczyna rysowanie wykresu, co potęguje chaotyczność
# wykresu. To tak jakby rozpoczynać rysowanie funkcji od różnych punktów.
# - brak skali
# PRZEWAGI MOJEGO WYKRESU:
# -  dodałem podział na dekady, dzięki czemu widzimy ewolucję komiksu, wykres uzyskuje większą głębie, 
# jest bardziej informatywny. 
# - Bohaterowie są posortowani ze względu na sumę wystąpień, uświadamiając czytelnika kto grał jaką 
# rolę w serii komiksów.
# - mój wykres jest dużo bardziej czytelny, łatwiej jest odczytać konkretną liczbę wystąpień każdego
# z bohaterów komiksu.





tuesdata <- tidytuesdayR::tt_load(2024, week = 12)
mutant_moneyball <- tuesdata$mutant_moneyball

p1 <- mutant_moneyball %>%
  select(Member, TotalIssues90s, TotalIssues70s, TotalIssues80s, TotalIssues60s) %>%
  pivot_longer(
    cols = starts_with("TotalIssues"),
    names_to = "Decade",
    values_to = "Issues"
  ) %>%
  mutate(
    Decade = str_remove(Decade, "TotalIssues"),
    Decade = factor(Decade, levels = c("90s", "80s", "70s", "60s")),
    Member_Clean = str_replace_all(Member, "([a-z])([A-Z])", "\\1 \\2"),
    Member_Clean = str_to_title(Member_Clean)
  ) %>%
  group_by(Member_Clean) %>%
  mutate(Total_All = sum(Issues)) %>%
  ungroup()
wykres_1 <-ggplot(p1, aes(x = Issues, y = reorder(Member_Clean, Total_All), fill = Decade)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = c("60s" = "#FFD166", "70s" = "#EF476F", "80s" = "#118AB2", "90s" = "#073B4C"),
    name = "Decade:",
    breaks = c("60s", "70s", "80s", "90s")
  ) +
  
  scale_x_continuous(breaks = seq(0, 200, by = 10),expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "X-Men: member issue appearance",
    subtitle = "number of appearances in each decade (1963-1992)",
    x = "number of appearances",
    y = '',
    caption = "Data: TidyTuesday 2024 week 12 || Mutant Moneyball",
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold", size = 25),
    plot.subtitle = element_text(size = 14),
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 12, face = 'bold'),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14)
  )
ggsave('poprawiony_wykres.png', plot = wykres_1)

