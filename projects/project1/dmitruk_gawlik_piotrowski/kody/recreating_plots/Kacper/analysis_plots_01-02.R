library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
library(waffle)
library(forcats)

data21 <- read.csv('../../data/2021/Tab5_Wyz-Tabela-1.csv', sep=';')

data11 <- read.csv('../../data/2011/tab07_2011.csv', sep=';')

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
  mutate(wyznanie = fct_reorder(wyznanie, procenty_int, .desc = TRUE))

plot_01 <- ggplot(df_21, aes(fill = wyznanie, values = procenty_int)) +
  geom_waffle(
    n_rows = 10,
    size = 0.5,
    colour = "white",
    direction = 'row'
  ) +
  
  scale_fill_manual(values = c(
    "Kościół katolicki - obrządek łaciński" = 'red',
    "nienależący do żadnego wyznania" = "#ffc300",
    "Inne" = "navy"
  )) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  ) +
  labs(
    title = "Struktura wyznaniowa (2021)",
    subtitle = "Każdy kwadrat reprezentuje 1% populacji",
    fill = "Wyznanie"
  )

ggsave(plot_01, filename = "../../plots/Kacper/plot_01.png", device = "png", height = 8, width = 12, dpi = 300, bg = "transparent")

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
  mutate(wyznanie = fct_reorder(wyznanie, procenty_int, .desc = TRUE))

plot_02 <- ggplot(df_11, aes(fill = wyznanie, values = procenty_int)) +
  geom_waffle(
    n_rows = 10,
    size = 0.5,
    colour = "white",
    direction = 'row'
  ) +
  
  scale_fill_manual(values = c(
    "Kościół katolicki - obrządek łaciński" = 'red',
    "nienależący do żadnego wyznania" = "#ffc300",
    "Inne" = "navy"
  )) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  ) +
  labs(
    title = "Struktura wyznaniowa (2011)",
    subtitle = "Każdy kwadrat reprezentuje 1% populacji",
    fill = "Wyznanie"
  )

ggsave(plot_02, filename = "../../plots/Kacper/plot_02.png", device = "png", height = 8, width = 12, dpi = 300, bg = "transparent")




  
    
