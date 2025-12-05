
# Link do posta z wykresem:
# https://x.com/simisani10/status/1771460567858913638/photo/1

# Link do zestawu danych z repozytorium TidyTuesday:
# https://github.com/rfordatascience/tidytuesday/tree/main/data/2024/2024-03-12

# Link do repozytorium orginalnej wizualizacji
# https://github.com/sndaba/2024TidyTuesdayWithRstats/tree/main/week11


# Mankamenty oryginalnej wizualizacji:
# 1) Nieuzasadnione użycie wykresów donutowych
# Porównywanie wartości przez wykres kołowy/donutowy nie sprzyja odzwierciedlaniu proporcji, a przedstawione
# wartości nie stanowią żadnej całości, której przysługiwałoby zamknięcie w okręgu.
# 
# 2) Niezrozumiałe opisy
# Oryginalne tytuły oraz opisy są wyrażone niejasno, a zestawienie najmniejszych wartości zmiennej
# nie ma sensu w tym przypadku. W danych źródłowych znajdziemy wiele wpisów z wartością zmiennej wynoszącą 0,
# natomiast autorka oryginału samodzielnie i ręcznie wybrała rekordy o małych wartościach bez jasnego kryterium.
# 
# 3) Przytłaczająca liczba niewnoszących nic kolorów
# Każdy rekord jest kodowany podwójnie: poprzez etykiety i unikalny kolor.
# W poprawionej wizualizacji zawarłem warstwę kolorystyczną kodującą informację nieobecną w oryginalnej wizualizacji.
# 
# 4) Nakładanie się elementów
# Wizualizacja zawiera etykiety wyraźnie nachodzące na inne elementy graficzne.
# 
# Przygotowany na nowo wykres unika tych wszystkich błędów oraz
# zawiera więcej informacji (Poza sfałszowanymi minimalnymi wartościami zmiennej, których nowa wizualizacja nie zawiera)
#
#

biblioteki <- c("ggplot2","dplyr","showtext","ggtext")

wczytaj_pakiety<-function(biblioteki){
  for (pakiet in biblioteki){
    if (!requireNamespace(pakiet, quietly = TRUE)){
      install.packages(pakiet)
    }
    library(pakiet,character.only = TRUE)
  }}
wczytaj_pakiety(biblioteki)
font_add_google("Roboto Slab", "font")


dane <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-12/fiscal_sponsor_directory.csv')

dane2<- select(dane,name,year_fiscal_sponsor,n_sponsored) %>% 
  arrange(desc(n_sponsored)) %>% 
  top_n(20) %>% 
  mutate(year_fiscal_sponsor=2025-year_fiscal_sponsor)

showtext_auto()

wykres <- ggplot(dane2, aes(
  y = fct_reorder(name, n_sponsored, .desc = FALSE), 
  x = n_sponsored,
  fill = year_fiscal_sponsor
)) +
  geom_col(position = "dodge") + 
  scale_fill_gradient(
    low = "#C6DBEF",
    high = "#08306B",
    name = "Liczba lat funkcjonowania jako sponsor fiskalny"
  ) +
  labs(
    title = "Organizacje o charakterze sponsora fiskalnego, które przekazały\ndarowizny największej liczbie projektów\n ",
    x = "Liczba zasponsorowanych projektów",
    y = "Nazwa organizacji"
  ) +
  theme_minimal()+
  theme(
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=40,family="font",hjust=0,lineheight = 0.40),
        plot.subtitle = element_text(size=25,family="font"),
        axis.title = element_text(size=28,family="font"),
        axis.text = element_text(size=20,family="font"),
        legend.position = c(-0.7, -0.20), 
        plot.margin = margin(l=10,b = 38,r=10,t=10),
        legend.justification = c(0, 0),
        legend.title = element_text(size=25,family="font"),
        legend.text = element_text(size=20,family="font"),
        )+
  guides(
    fill = guide_colorbar(
      title.position = "left",
      title.hjust = 0.5,
      direction = "horizontal", 
      barwidth = 15,
      barheight = 0.5 
    ))


ggsave("LepszaWizualizacja.png",path="homeworks/hw4/JanusiakKamil",plot=wykres,dpi=300,width = 6,height = 6) 

