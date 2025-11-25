library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(extrafont)
loadfonts()

tab04 <- read.csv("kody\\data\\2021\\Tab4_JDom_woj-Tabela-1.csv", sep = ";", header = TRUE)

tab04 <- tab04 %>%
  rename(`Kujawsko-pomorskie`= `Kujawsko.pomorskie`,
         `Warmińsko-mazurskie` = `Warmińsko.mazurskie`) %>% 
  pivot_longer(cols = -1,       
               names_to = "wojewodztwo",
               values_to = "liczba")


tab04 <- tab04 %>%
  pivot_wider(names_from = 1,  
              values_from = liczba) 

num_cols <- names(tab04)[-1] 

tab04[num_cols] <- tab04[num_cols] %>% 
  mutate(across(where(is.character), ~ as.numeric(str_remove_all(.x, "\\s"))))

tab04 <- tab04 %>% 
  mutate(`Inny niż polski` = `Inny niż polski` - angielski,
         across(-all_of(c("Ogółem", "wojewodztwo")), ~ (.x / .data[["Inny niż polski"]])*100)) %>% 
  select(-c("Ogółem", "Polski", "angielski", "Inny niż polski", "inne", "Nieustalony")) %>% 
  rename(`gwary podlaskie`= `gwary podlaskie (polsko-białorusko-ukraińskie)`)%>% 
  slice(-1) %>% 
  pivot_longer(cols = -1, 
               names_to = "jezyk",
               values_to = "liczba") %>%
  mutate(bin = cut(liczba,
    breaks = seq(0, 100, by = 10),
    include.lowest = TRUE,
    right = FALSE))

plot_06 <- tab04 %>% 
  ggplot(aes(x = jezyk, y = wojewodztwo, fill = bin))+
  geom_tile() +
  scale_fill_manual(values = c("#C10020","orange","yellow", "#b9c764", "#dad83d","blue","darkblue","#6e3784","#53178f","#2f0a55"), 
  na.value = "darkgray") +
  theme(axis.text.x = element_text(angle = 50,
                                   size = 14,
                                    hjust = 1,
                                    color = "black",
                                    family ="Cambria" ),
        legend.justification = "left",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 10, family = "Cambria"),
        legend.text  = element_text(size = 10, family = "Cambria"),
        text = element_text(family = "Cambria"),
        axis.text.y = element_text(size = 14, color = "black", family ="Cambria" ),
        legend.position = "bottom",
        plot.margin = margin(t = 10, r = 15, b = 40, l = 15),
        xlab = NA,
        plot.caption = element_text(hjust = 0.5),
        legend.key.height = unit(0.3, "cm"),
        legend.box = "horizontal",
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA))+
  labs(fill = "Procent (%) jaki stanowi język\nw całości języków innych niż pol. i ang.",
    title = "Procentowy udział języków innych niż polski według województw (2021)",
    subtitle = "Procent wyrażony względem wszystkich języków innych niż polski (bez angielskiego)",
    x = "Język (inne niż polski)",
    y = "Województwo",
    caption = "Kolor kafelka pokazuje, jaki procent wszystkich 'języków innych niż polski' w danym województwie stanowi dany język.",
  )
    
plot_06
#usuwamy polski, angielski i patrzymy tylko na inne jezyki
ggsave(plot_06, filename = "kody//plots//Ola//plot_06.png", device = "png", height = 9, 
width = 9, dpi = 300, bg = "transparent")