library(sf)
library(plotly)
library(dplyr)
library(readxl)

# Dane z mapą polski podzielona na powiaty
pow <- st_read("/home/madzia/Dokumenty/twd/poland.counties.json")

# Dane z powierzchnia lasu w powiatach
lasy <-  read_excel("/home/madzia/Dokumenty/twd/zalesienie_powiatów.xlsx", sheet = 2)
lasy$Kod <- substr(lasy$Kod, 1, nchar(lasy$Kod) - 3) #chodzi o to ze bedziely laczyc dane z kolumna kod, aby sie zgadzaly trzeba usunac z kodów ostatnie 3 zera
lasy <- lasy %>% rename(pow_lasu_ha = `lasy ogółem`)

# Dane z powierzchnią powiatów
powiaty_km2 <- read_excel("/home/madzia/Dokumenty/twd/Wykaz_powierzchni_wg_stanu_na_01012025_ha_km2.xlsx")
powiaty_km2$TERYT <- gsub(" ", "", powiaty_km2$TERYT) # by połączyć pozbywamy sie spacji: 02 02 ---> 0202

# łączymy tak aby mieć dany powiat, jego polożenie i powierzchnie lasów
powiaty_dane <- pow %>% left_join(lasy, by = c("terc" = "Kod"))

# teraz dodajemy jeszcze powierzchnię całego powiatu, wybieramy potrzebne kolumny, tworzymy % zalesienia
powiaty_dane <- powiaty_dane %>% left_join(powiaty_km2, by = c("terc" = "TERYT")) 
powiaty_dane <- powiaty_dane %>% select(terc, name, Nazwa, pow_lasu_ha, `Powierzchnia [ha]`, geometry)
powiaty_dane <- powiaty_dane %>%
  mutate(Wartosc_proc = round(as.numeric(pow_lasu_ha) / as.numeric(`Powierzchnia [ha]`) * 100, 1))
View(powiaty_dane)

# do zaznaczenia procentow na mapce:
powiaty_centroids <- st_centroid(powiaty_dane)
powiaty_labels <- st_point_on_surface(powiaty_dane)  # lepsze niż centroidv

p <- ggplot(powiaty_dane) +
  geom_sf(aes(fill = Wartosc_proc), color = "#4b3032", size = 0.1) +
  scale_fill_steps(
    low = "#D1D5C3",
    high = "#4F543B", 
    breaks = seq(0, 70, 10),
    name = "Zalesienie [%]") + 
  theme_void() +
  theme(
    legend.position = "left",
    legend.title = element_text(family = "Times New Roman", face = "bold", size = 15, margin = margin(b = 5)),
    legend.text = element_text(family = "Times New Roman", size = 15),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20))

p
ggsave("wykres_zalesienie_powiatów.pdf", plot = p, units = "cm",
       width = 20, height = 19, dpi = 300, device = cairo_pdf, bg = "transparent") 






