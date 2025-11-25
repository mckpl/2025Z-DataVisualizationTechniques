library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
install.packages("waffle")
library(waffle)
library(ggwaffle)
library(forcats)
library(emojifont)
library(showtext)
library(extrafont)

font_add_google("Merriweather", "Cambria")
showtext_auto()

load.fontawesome()

data21 <- read.csv('kody\\data\\2021\\Tab5_Wyz-Tabela-1.csv', sep=';')

data11 <- read.csv('kody\\data\\2011\\tab07_2011.csv', sep=';')

df1 <- data21 %>% 
  mutate(procent_lud = as.numeric(gsub(",", ".", as.character(ogółu.ludności)))) %>% 
  filter(procent_lud < 1 & !is.na(Ogółem)) %>% 
  arrange(desc(procent_lud)) %>% 
  rename(wyznanie = Przynależność.do.wyznania.religijnego) %>% 
  mutate(wyznanie_skr = case_when(
    str_detect(wyznanie, "bizantyjsko-ukraiński") ~ 'Kościół Katolicki (biz.-ukr.)',
    str_detect(wyznanie, "chrześcijaństwo") ~ 'Chrześcijaństwo',
    TRUE ~ wyznanie
  ))

df_21 <- data21 %>% 
  filter(!(Przynależność.do.wyznania.religijnego %in% c('Ogółem',
                                                        'należący do wyznania',
                                                        'Odmawiający odpowiedzi na pytanie o wyznanie',
                                                        'Udzielający odpowiedzi na pytanie o wyznanie'))) %>% 
  select(Przynależność.do.wyznania.religijnego, Ogółem) %>% 
  rename(wyznanie = Przynależność.do.wyznania.religijnego) %>% 
  mutate(wyznanie = case_when(
    str_detect(wyznanie, "bizantyjsko-ukraiński") ~ 'Kościół Katolicki (biz.-ukr.)',
    str_detect(wyznanie, "chrześcijaństwo") ~ 'Chrześcijaństwo',
    str_detect(wyznanie, "obrządek łaciński") ~ 'Kościół katolicki - obrządek łaciński',
    TRUE ~ wyznanie
  )) %>% 
  mutate(procent.populacji = (Ogółem*100)/(sum(Ogółem))) %>% 
  mutate(wyznanie = if_else(procent.populacji < 2.0, 
                            "Inne",
                            wyznanie)) %>% 
  group_by(wyznanie) %>% 
  summarise(procent.populacji = sum(procent.populacji),
            Ogółem = sum(Ogółem)) %>% 
  
  arrange(desc(procent.populacji)) %>% 
  mutate(procenty_int = c(90, 8, 2)) %>% 
  mutate(wyznanie = fct_reorder(wyznanie, procenty_int, .desc = TRUE)) %>% 
  mutate(wyznanie = case_when(
    wyznanie == "nienależący do żadnego wyznania" ~ "Nienależący do żadnego wyznania",
    TRUE ~ wyznanie
  ))

df_11 <- data11 %>% 
  filter(!(Przynależność.do.wyznania.religijnego %in% c('Ogółem',
                                                        'należący do wyznania',
                                                        'Odmowiający odpowiedzi na pytanie o wyznanie',
                                                        'Udzielajacy odpowiedzi na pytanie o wyznanie'))) %>% 
  select(Przynależność.do.wyznania.religijnego, Ogółem.w.tys.) %>% 
  rename(wyznanie = Przynależność.do.wyznania.religijnego) %>% 
  mutate(wyznanie = case_when(
    str_detect(str_to_lower(wyznanie), "obrządek łaciński") ~ 'Kościół katolicki - obrządek łaciński',
    str_detect(str_to_lower(wyznanie), "bizantyjsko-ukraiński") ~ 'Kościół Katolicki (biz.-ukr.)',
    TRUE ~ wyznanie
  )) %>% 
  
  mutate(procent.populacji = (Ogółem.w.tys.*100)/(sum(Ogółem.w.tys.))) %>% 
  
  mutate(wyznanie = if_else(procent.populacji < 2.0, 
                            "Inne",
                            wyznanie)) %>% 
  
  group_by(wyznanie) %>% 
  summarise(procent.populacji = sum(procent.populacji),
            Ogółem.w.tys. = sum(Ogółem.w.tys.)) %>% 
  arrange(desc(procent.populacji)) %>% 
  mutate(procenty_int = c(94, 3, 3)) %>% 
  mutate(wyznanie = fct_reorder(wyznanie, procenty_int, .desc = TRUE)) %>% 
  mutate(wyznanie = case_when(
    wyznanie == "nienależący do żadnego wyznania" ~ "Nienależący do żadnego wyznania",
    TRUE ~ wyznanie
  ))

colours <- c(
  "Kościół katolicki - obrządek łaciński" = '#C10020',
  "Nienależący do żadnego wyznania" = "#ffc300",
  "Inne" = "navy"
)

df_11_expanded <- df_11[rep(1:nrow(df_11), df_11$procenty_int), ]

df_11_expanded$wyznanie <- fct_reorder(df_11_expanded$wyznanie,
                                       nchar(as.character(df_11_expanded$wyznanie)), .desc = TRUE)

waffle_data_11 <- waffle_iron(df_11_expanded, aes_d(group = wyznanie), rows = 10)
waffle_data_11$label <- fontawesome('fa-user')

waffle_plot_11 <- ggplot(data = waffle_data_11, aes(x, y, colour = group)) +
  geom_text(aes(label=label), family = 'fontawesome-webfont', size = 50, show.legend = FALSE) + 
  coord_equal() + 
  geom_point(alpha = 0) + 
  scale_colour_manual(values = colours,
                      # labels = function(x) stringr::str_wrap(x, width = 20),
                      breaks = c(
                        "Kościół katolicki - obrządek łaciński",
                        "Nienależący do żadnego wyznania",
                        "Inne"
                      )) + 
  theme_waffle() +
  labs(x = NULL, y = NULL, 
    title = "Struktura wyznań",
    subtitle = "Rok 2011",
    caption = "Każda figurka reprezentuje ~1% populacji; legenda pokazuje kategorie wyznań."
  ) +
  theme(
    text = element_text(size = 40, family = "Cambria"),
    plot.title = element_text(size = 60),
    plot.subtitle = element_text(size = 40),
    plot.caption = element_text(size = 40),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(t = 10, r = 15, b = 40, l = 15),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    legend.title= element_blank(),
    legend.text = element_text(size = 40, family = "Cambria"),
    legend.key.size = unit(1.2, "line"),
    legend.key.spacing.x = unit(0.25, "cm"),
    legend.justification = "center",
    legend.margin = margin(t = 0 , r = 0, b = 0, l = 0)
  ) + 
  guides(colour = guide_legend(
    override.aes = list(
      shape = 16, size = 6, alpha = 1)
  ))

print(waffle_plot_11)

df_21_expanded <- df_21[rep(1:nrow(df_21), df_21$procenty_int), ]

df_21_expanded$wyznanie <- fct_infreq(df_21_expanded$wyznanie)

waffle_data_21 <- waffle_iron(df_21_expanded, aes_d(group = wyznanie), rows = 10)
waffle_data_21$label <- fontawesome('fa-user')

waffle_plot_21 <- ggplot(data = waffle_data_21, aes(x, y, colour = group)) +
  geom_text(aes(label=label), family = 'fontawesome-webfont', size = 50, show.legend = FALSE) + 
  coord_equal() + 
  geom_point(alpha = 0) + 
  scale_colour_manual(values = colours,
                      # labels = function(x) stringr::str_wrap(x, width = 20),
                      breaks = c(
                        "Kościół katolicki - obrządek łaciński",
                        "Nienależący do żadnego wyznania",
                        "Inne"
                      )) + 
  theme_waffle() +
  labs(x = NULL, y = NULL,
       title = "Struktura wyznań",
       subtitle = "Rok 2021") +
  theme(
    text = element_text(size = 40, family = "Cambria"),
    plot.title = element_text(size = 60),
    plot.subtitle = element_text(size = 40),
    plot.caption = element_text(size = 40),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 40, family = "Cambria"),
    legend.key.size = unit(1.2, "line"),
    legend.key.spacing.x = unit(0.25, "cm"),
    legend.justification = "center",
    legend.margin = margin(t = 0 , r = 0, b = 0, l = 0)
  ) + 
  guides(colour = guide_legend(
    override.aes = list(
      shape = 16, size = 6, alpha = 1)
  ))

print(waffle_plot_21)

ggsave(waffle_plot_11,
       filename = "kody/plots/Kacper/waffle_plot_11.png",
       device = "png", width = 10,
       height = 8, dpi = 300,
       bg = "transparent")

ggsave(waffle_plot_21,
       filename = "kody/plots/Kacper/waffle_plot_21.png",
       device = "png", width = 10,
       height = 8, dpi = 300,
       bg = "transparent")

