library(RSQLite)
library(dplyr)
library(ggplot2)
library(sf)
library(scales)

con <- dbConnect(RSQLite::SQLite(), dbname = "./data.db")
dbListTables(con)
csv <- dbGetQuery(con,
                  "
                  SELECT POW, kat, COUNT(*) as cnt FROM
                  (SELECT gminy.WOJ, gminy.POW as POW, gminy.GMI, kat FROM 
                  (SELECT simc.TERC, kat FROM 
                  (SELECT klasyfikacje.nazwa, klasyfikacje.kategoria as kat, ulic.SIMC as SIMC 
                  FROM klasyfikacje INNER JOIN ulic ON ulic.nazwa = klasyfikacje.nazwa) AS int
                  INNER JOIN simc ON int.SIMC = simc.SIMC) AS intb
                  INNER JOIN gminy ON intb.TERC = gminy.GMI)
                  GROUP BY POW, kat;
                  ")
head(csv)

powiaty <- st_read("./data/granice/A02_Granice_powiatow.shp", quiet=TRUE)

shade_color <- function(base_color, part) {
  col <- col2rgb(base_color)
  factor <- rescale(part, to = c(1, 0.4))
  rgb(col[1,]*factor, col[2,]*factor, col[3,]*factor, maxColorValue = 255)
}

powiaty %>%
  inner_join(csv %>%
               group_by(POW) %>%
               slice_max(cnt, n = 1) %>%
               ungroup() %>%
               left_join(csv %>%
                           group_by(POW) %>%
                           summarise(sum=sum(cnt)), by=join_by(POW)) %>%
               mutate(part=(cnt/sum)),
            by=(join_by(JPT_KOD_JE == POW))) %>%
  ggplot(mapping=aes(fill=kat, alpha=part)) +
  geom_sf() +
  scale_alpha_binned(
    n.breaks=5,
    range=c(0.2, 1),
    labels=scales::percent_format(),
    guide = guide_legend(
      override.aes = list(fill = "black"),
      title = "Udział procentowy"
    )
  ) +
  scale_fill_manual(
    values=c("Patroni" = "#b00", "Neutralne" = "#0a0", "Przyrodnicze" = "#00b", "Geograficzne" = "#808"),
    name = "Kategoria"
    ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  
    axis.title = element_blank(),  
    axis.text = element_blank(),   
    axis.ticks = element_blank(),
    legend.text = element_text(size = 28, color = "#fefefe"),
    legend.title = element_text(size = 32, color = "#fefefe"),
    legend.key.size = unit(3, "lines"),
    legend.spacing = unit(1.6, "lines")
  )

gminy <- st_read("./data/granice/A03_Granice_gmin.shp", quiet=TRUE)

csv.gminy <- dbGetQuery(con,
                  "
                  SELECT gminy.GMI, gd.SIMC, gd.ULIC, gd.COUNT FROM
                  (SELECT simc.TERC as tc, simc.SIMC as SIMC, ULIC, count FROM
                  (SELECT SIMC, ULIC, COUNT(*) AS count
                  FROM budynki
                  GROUP BY SIMC, ULIC) AS ge
                  LEFT JOIN simc ON ge.SIMC == simc.SIMC) as gd
                  LEFT JOIN gminy ON gminy.GMI == gd.tc;
                  ")

tf.anyroot <- function (n){
  scales::new_transform(paste0(n, "rt"), function(x) x^(1/n), function(x) x^n)
}

t <- gminy %>%
  left_join(csv.gminy %>%
              filter(count >= quantile(count, 0.99)) %>%
              group_by(GMI) %>%
              summarise(n=n()), by=join_by(JPT_KJ_I_1 == GMI)) %>%
  ggplot(mapping=aes(fill=n)) +
  geom_sf(colour=NA) +
  scale_fill_continuous(na.value="#fff", low="#ddf", high="#008", transform="log",  labels=label_number(accuracy = 1)) +
  theme(
    panel.grid = element_blank(),  
    axis.title = element_blank(),  
    axis.text = element_blank(),   
    axis.ticks = element_blank()   
  ) +
  theme(plot.background = element_rect(fill = "transparent"))
  

poland_bbox_ll <- st_bbox(c(xmin = 14.07, ymin = 49.03, xmax = 24.30, ymax = 54.85), crs = 4326)
poland_bbox_3857 <- st_transform(st_as_sfc(poland_bbox_ll), 3857) |> st_bbox() 
year1914 <- st_read("./data/Year_1914.shp")
st_crs(year1914) <- 3857
year1914 <- st_transform(year1914, 3857)
year1914_cropped <- st_crop(year1914, poland_bbox_3857)

t +
  geom_sf(data=year1914_cropped, colour="#000", fill=NA, linewidth=0.687)
  


  

