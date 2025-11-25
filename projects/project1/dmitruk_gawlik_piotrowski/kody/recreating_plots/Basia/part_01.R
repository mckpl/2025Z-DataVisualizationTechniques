
#%%
install.packages("extrafont")

#%%
library(dplyr)
library(tidyr)
library(ggplot2)
library(extrafont)

#%%
tab01 <- read.csv("kody\\data\\2011\\tab01_2011.csv", sep=";", header=TRUE, fileEncoding="UTF-8")

#%%
View(tab01)

#%%
tab01 <- tab01 %>%
    select(-c('ogolem', 'wiek_prod_razem')) %>%
    rename(
        identyfikacja = identyfikacja_narodowo_etniczna,
        wiek_0_17 = wiek_przedpr_0_17,
        wiek_18_44 = wiek_mob_18_44,
        wiek_45_59 = wiek_niemobilny_45_59.64,
        wiek_60_plus = wiek_popr_60
    )

#%%
View(tab01)

#%%
temp <- tab01 %>%
  select('identyfikacja', 'wiek_0_17', 'wiek_18_44', 'wiek_45_59', 'wiek_60_plus') %>%
  mutate(identyfikacja = case_when(
    identyfikacja == "ogolem" ~ "Ogółem",
    identyfikacja == "polska" ~ "Polska",
    identyfikacja == "inna_niz_polska" ~ "Inna niż Polska",
    identyfikacja == "slaska" ~ "Śląska",
    identyfikacja == "kaszubska" ~ "Kaszubska",
    identyfikacja == "niemiecka" ~ "Niemiecka",
    identyfikacja == "ukrainska" ~ "Ukraińska",
    identyfikacja == "bialoruska" ~ "Białoruska",
    identyfikacja == "romska" ~ "Romska",
    identyfikacja == "rosyjska" ~ "Rosyjska",
    identyfikacja == "amerykanska" ~ "Amerykańska",
    identyfikacja == "lemkowska" ~ "Łemkowska",
    identyfikacja == "angielska" ~ "Angielska",
    identyfikacja == "wloska" ~ "Włoska",
    identyfikacja == "francuska" ~ "Francuska",
    identyfikacja == "litewska" ~ "Litewska",
    identyfikacja == "zydowska" ~ "Żydowska",
    identyfikacja == "wietnamska" ~ "Wietnamska",
    identyfikacja == "hiszpanska" ~ "Hiszpańska",
    identyfikacja == "holenderska" ~ "Holenderska",
    identyfikacja == "ormianska" ~ "Ormiańska",
    identyfikacja == "grecka" ~ "Grecka",
    identyfikacja == "czeska" ~ "Czeska",
    identyfikacja == "slowacka" ~ "Słowacka",
    identyfikacja == "kociewska" ~ "Kociewska",
    identyfikacja == "kanadyjska" ~ "Kanadyjska",
    identyfikacja == "goralska" ~ "Góralska",
    identyfikacja == "bulgarska" ~ "Bułgarska",
    identyfikacja == "irlandzka" ~ "Irlandzka",
    identyfikacja == "tatarska" ~ "Tatarska",
    identyfikacja == "szwedzka" ~ "Szwedzka",
    identyfikacja == "wegierska" ~ "Węgierska",
    identyfikacja == "austriacka" ~ "Austriacka",
    identyfikacja == "australijska" ~ "Australijska",
    identyfikacja == "chinska" ~ "Chińska",
    identyfikacja == "wielkopolska" ~ "Wielkopolska",
    identyfikacja == "norweska" ~ "Norweska",
    identyfikacja == "japonska" ~ "Japońska",
    identyfikacja == "mazurska" ~ "Mazurska",
    identyfikacja == "hinduska" ~ "Hinduska",
    identyfikacja == "arabska" ~ "Arabska",
    identyfikacja == "belgijska" ~ "Belgijska",
    identyfikacja == "turecka" ~ "Turecka",
    TRUE ~ "Nieustalona"
    )) %>%
    mutate(suma = rowSums(across(wiek_0_17:wiek_60_plus)), 
           across(wiek_0_17:wiek_60_plus, ~ .x / suma * 100)) %>%
    select(-suma) %>%
    pivot_longer(cols = wiek_0_17:wiek_60_plus,
                 names_to = "grupa_wiekowa",
                 values_to = "procent"
    )

#%%
View(temp)

#%%
plot_01 <- ggplot(temp, aes(x = identyfikacja, y = procent, fill = grupa_wiekowa)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
  labs(
    title = "Struktura wiekowa według identyfikacji narodowo-etnicznej (2011)",
    subtitle = "Procentowy udział grup wiekowych w ramach każdej kategorii identyfikacji",
    x = "Identyfikacja narodowo-etniczna",
    y = "Udział procentowy (%)",
    fill = "Grupa wiekowa",
    caption = "Każdy słupek pokazuje rozkład grup wiekowych dla danej identyfikacji; wysokości części odpowiadają procentowemu udziałowi."
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("#4A2600", "#C10020", "#ffc300", "navy"),
                    labels = c("0-17", "18-44", "45-59", "60+")) +
  theme(
    axis.text.x = element_text(angle = 45, size = 10, hjust = 1, color = "black", family ="Cambria"),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(t = 10, r = 15, b = 40, l = 15),
    plot.caption = element_text(hjust = 0.5),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    legend.title = element_text(size = 12, family = "Cambria"),
    legend.text  = element_text(family = "Cambria", size = 12),
    text = element_text(family = "Cambria"),
    axis.text.y = element_text(color = "black",size = 12, family ="Cambria" ),
    axis.title.y = element_blank()
  )

#%%
plot_01

#%%
ggsave(plot_01, filename = "kody/plots/Basia/plot_01.png", device = "png", height = 8, width = 12, dpi = 300, bg = "transparent")
