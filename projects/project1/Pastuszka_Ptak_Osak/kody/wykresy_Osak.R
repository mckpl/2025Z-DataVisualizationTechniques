library(dplyr)
library(stringr)
library(readxl)
library(ggplot2)
library(tidyr)
library(patchwork)
monetki <- read_excel('/Users/mateuszosak/Documents/ProjektTWD1/struktura.xlsx')


rok <- monetki %>% 
  transmute(rok = ifelse(
    grepl("^(sty|lut|mar|kwi|maj|cze|lip|sie|wrz|paź|lis|gru) \\d{2}$", `1. Struktura obiegu banknotów i monet *)`),
    as.integer(paste0("20", str_extract(`1. Struktura obiegu banknotów i monet *)`, "\\d{2}"))),
    NA_integer_
  ))

monetki$rok <- rok
monetki$rok <- as.numeric(unlist(monetki$rok))
monetki[227, "rok"] <- 2025
monetki<- as.data.frame(lapply(monetki, function(x) if(is.character(x)) as.numeric(x) else x))
monetki <- monetki[monetki$rok != 2025, ]

wykres1 <- monetki[ 4:231 , ]
colnames(wykres1) <- wykres1[1, ]
wykres1 <- wykres1[-c(1:2,231 ), ]
wykres1 <- wykres1[, -1]
row.names(wykres1) <- NULL
colnames(wykres1)[22] <- 'rok' 
wykres1$rok[128] <- '2017'
wykres1$rok[164] <- '2020'


suma_roczna <- wykres1 %>%
  group_by(rok) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))
suma_roczna <- suma_roczna[-20,]
suma_roczna[c(8:9,20:22)] <- NULL
names(suma_roczna) <- c('rok','500zl','200zl','100zl','50zl','20zl','10zl','5zl','2zl','2zlNG','1zl','50gr','20gr','10gr','5gr','2gr','1gr')
suma_roczna[,9] <- suma_roczna[,9] + suma_roczna[,10]
suma_roczna[,10] <- NULL
suma_roczna <- suma_roczna[ -19, ]


wykres1_dane <- suma_roczna %>%
  select(rok, `500zl`, `200zl`, `100zl`, `50zl`, `20zl`, `10zl`) %>%
  mutate(rok = as.factor(rok))

wykres1_long <- wykres1_dane %>%
  pivot_longer(cols = c('500zl', '200zl', '100zl', '50zl', '20zl', '10zl'),
               names_to = "nominał",
               values_to = "liczba_banknotów")

kolory <- c(
  '5zl'  = "#FFFACD",  
  '2zl'  = "#FFD700",  
  '1zl'  = "#FFC300",  
  '50gr' = "#FF8C00",  
  '20gr' = "#D2691E",  
  '10gr' = "#A0522D",  
  '5gr'  = "#8B4513",  
  '2gr'  = "#6F4E37",  
  '1gr'  = "#4A3728"   
)
kolory1 <- c(
  "500zl" = "#FFD700",  
  "200zl" = "#FFB703",  
  "100zl" = "#C99A2E", 
  "50zl"  = "#8B4513",  
  "20zl"  = "#6F4E37",  
  "10zl"  = "#4A3728"   
)

wykres1_long$nominał <- factor(wykres1_long$nominał, levels = c("500zl", "200zl", "100zl", "50zl", "20zl", "10zl"))

plot1 <- ggplot(wykres1_long, aes(x = rok, y = liczba_banknotów, fill = nominał)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Liczba banknotów w obiegu według nominału",
    x = "Rok",
    y = "Liczba banknotów (w mln sztuk)",
    fill = "Nominał"
  ) +
  scale_fill_manual(values = kolory1, breaks = c("500zl", "200zl", "100zl", "50zl", "20zl", "10zl")) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", color = "white"),
    axis.title = element_text(color = "white", face = 'bold'),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "white", size = 14, face = 'bold'),
    axis.text.y = element_text(color = "white", size = 14, face = 'bold'),
    legend.position = "bottom",
    legend.text = element_text(color = "white"), 
    legend.title = element_text(color = "white")
  )

wykres2_dane <- suma_roczna %>%
  select(rok, `5zl`, `2zl`, `1zl`, `50gr`, `20gr`, `10gr`, `5gr`, `2gr`, `1gr`) %>%
  mutate(rok = as.factor(rok)) %>%
  mutate(across(where(is.numeric), ~ . / 100))

wykres2_long <- wykres2_dane %>%
  pivot_longer(cols = c(`5zl`, `2zl`, `1zl`, `50gr`, `20gr`, `10gr`,'5gr', '2gr','1gr'),
               names_to = "nominał",
               values_to = "liczba_monet")
wykres2_long$nominał <- factor(wykres2_long$nominał, 
                               levels = c("5zl", "2zl", "1zl", "50gr", "20gr", "10gr", "5gr", "2gr", "1gr"))

plot2 <- ggplot(wykres2_long, aes(x = rok, y = liczba_monet, fill = nominał)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Liczba monet w obiegu według nominału",
    x = "Rok",
    y = "Liczba monet (w 100mln sztuk)",
    fill = "Nominał"
  ) +
  scale_fill_manual(values = kolory, 
                    breaks = c("5zl", "2zl", "1zl", "50gr", "20gr", "10gr", "5gr", "2gr", "1gr")) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", color = "white"),
    axis.title = element_text(color = "white", face = 'bold'),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "white", size = 14, face = 'bold'),
    axis.text.y = element_text(color = "white", size = 14, face = 'bold'),
    legend.position = "bottom",
    legend.text = element_text(color = "white"), 
    legend.title = element_text(color = "white")
  )
wykres1_wartosc <- suma_roczna %>%
  select(rok, `500zl`, `200zl`, `100zl`, `50zl`, `20zl`, `10zl`) %>%
  mutate(rok = as.factor(rok)) %>%
  mutate(
    `500zl` = `500zl` * 500 / 1000,
    `200zl` = `200zl` * 200 / 1000,
    `100zl` = `100zl` * 100 / 1000,
    `50zl` = `50zl` * 50 / 1000,
    `20zl` = `20zl` * 20 / 1000,
    `10zl` = `10zl` * 10 / 1000
  )
wykres1_long_wartosc <- wykres1_wartosc %>%
  pivot_longer(cols = c('500zl', '200zl', '100zl', '50zl', '20zl', '10zl'),
               names_to = "nominał",
               values_to = "wartość")
ggplot(wykres1_long_wartosc, aes(x = rok, y = wartość, fill = nominał)) +
  geom_bar(stat = "identity") +
  labs(
    title = "wartość banknotów według nominału",
    x = "Rok",
    y = "wartość banknotów w miliardach złotych",
    fill = "Nominał"
  ) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


wykres2_wartosc <- suma_roczna %>%
  select(rok, `5zl`, `2zl`, `1zl`, `50gr`, `20gr`, `10gr`, `5gr`, `2gr`, `1gr`) %>%
  mutate(rok = as.factor(rok)) %>%
  mutate(
    `5zl` = `5zl` * 5 / 1000,
    `2zl` = `2zl` * 2 / 1000,
    `1zl` = `1zl` * 1 / 1000,
    `50gr` = `50gr` * 0.50 / 1000,
    `20gr` = `20gr` * 0.2 / 1000,
    `10gr` = `10gr` * 0.1 / 1000,
    '5gr' = `5gr` * 0.05 / 1000,
    '2gr' = `2gr` * 0.02 / 1000,
    '1gr' = `1gr` * 0.01 / 1000
  )

wykres2_wartosc <- wykres2_wartosc %>%
  pivot_longer(cols = c(`5zl`, `2zl`, `1zl`, `50gr`, `20gr`, `10gr`,'5gr', '2gr','1gr'),
               names_to = "nominał",
               values_to = "wartość")
ggplot(wykres2_wartosc, aes(x = rok, y = wartość, fill = nominał)) +
  geom_bar(stat = "identity",) +
  labs(
    title = "Wartość monet w obiegu wg. nominału",
    x = "Rok",
    y = "wartość monet w mld. złotych",
    fill = "Nominał"
  ) +
  scale_fill_brewer(palette = "PiYG") + 
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = 'bold'),
    axis.text.y = element_text(size = 14, face = 'bold')
  )

wykresy_obok_siebie <- (
  plot1 + 
    theme(
      legend.position = "left", 
      plot.title = element_text(hjust = 0.5, face = "bold", color = "white") 
    )
) | (
  plot2 + 
    theme(
      legend.position = "right", 
      plot.title = element_text(hjust = 0.5, face = "bold", color = "white") 
    )
)

wykresy_obok_siebie <- wykresy_obok_siebie &
theme(plot.background = element_rect(fill = "transparent", color = NA))


ggsave(
  filename = "wykresy_final.png", 
  plot = wykresy_obok_siebie, 
  width = 13,      
  height = 6,    
  units = "in",    
  bg = "transparent" 
)
