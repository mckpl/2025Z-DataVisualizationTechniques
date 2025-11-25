#%%
library(dplyr)
library(tidyr)
library(stringr)

#%%
tab02 <- read.csv("data\\2011\\tab02_2011.csv", sep=";", header=TRUE, fileEncoding="UTF-8")

#%%
tab02 <- tab02 %>%
  mutate(
    Liczba = str_replace_all(Liczba, "\\s", ""),
    Liczba = str_replace(Liczba, ",", "."),
    Liczba = as.numeric(Liczba)
  )

#%%
tab02 <- tab02 %>%
  mutate(Identyfikacja = case_when(
    Identyfikacja_narodowo.etniczna == "Polska" ~ "Polska",
    Identyfikacja_narodowo.etniczna == "Nieustalona lub bez identyfikacji" ~ "Nieustalona lub bez identyfikacji",
    Identyfikacja_narodowo.etniczna == "Ogółem" ~ "Ogółem",
    TRUE ~ "Niepolska"
  )) %>%
  select(-c('Kod_terytorialny', 'Identyfikacja_narodowo.etniczna', 'Powiat', 'Procent')) %>%
  filter(Liczba != 0)

#%%
tab02 <- tab02 %>%
  group_by(Województwo, Identyfikacja) %>%
  summarise(suma = sum(Liczba), .groups = "drop") %>%
  pivot_wider(names_from = Identyfikacja, values_from = suma, values_fill = 0) %>%
  rowwise() %>%
  mutate(
    total = sum(c(Polska, Niepolska, `Nieustalona lub bez identyfikacji`)),
    Polska = Polska / total * 100,
    Niepolska = Niepolska / total * 100,
    `Nieustalona lub bez identyfikacji` = `Nieustalona lub bez identyfikacji` / total * 100
  ) %>%
  select(-c('total', 'Ogółem')) %>%
  ungroup() %>%
  arrange(desc(Niepolska))

#%%
View(tab02)

#%%
tab02_long <- tab02 %>%
  pivot_longer(cols = c("Niepolska", "Nieustalona lub bez identyfikacji", "Polska"), 
               names_to = "Grupa", values_to = "Procent")

#%%
View(tab02_long)

#%%
plot_03 <- ggplot(tab02_long, aes(x = Województwo, y = Procent, fill = Grupa)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  labs(x = "Województwo", y = "Procent ludności", fill = "Grupa") +
  scale_fill_manual(values = c("Polska" = "red", 
                               "Niepolska" = "navy", 
                               "Nieustalona lub bez identyfikacji" = "orange")) +
  theme_minimal() +
  ggtitle("Udział Polaków, Niepolaków i Nieustalonych wg województw")

#%%
plot_03

#%%
ggsave(plot_03, filename = "plots/Basia/plot_03.jpg", device = "jpeg", height = 8, width = 16, dpi = 300)