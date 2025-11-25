library(leaflet)
library(dplyr)
library(htmltools)



punkty_geo <- data.frame(
  miejsce = c(
    "Łysa Polana", "Wierch Poroniec", "Zazadnia", "Cyrhla", "Sucha Woda",
    "Olczyska", "Nosal", "Kuźnice", "Dolina Białego", "Dolina Strążyska",
    "Mała Łąka", "Dolina Kościeliska", "Dolina Chochołowska"
  ),
  bilety = c(
    200192, 62906, 6355, 3165, 22214, 2734, 14142, 157691,
    42496, 101973, 19921, 177696, 112304
  ),
  lat = c(
    49.2557, 49.2844, 49.2817, 49.2815, 49.2893, 49.2837, 49.2787, 
    49.2702, 49.2778, 49.2785, 49.2764, 49.2735, 49.2821
  ),
  long = c(
    20.1052, 20.1128, 20.0879, 20.0181, 20.0323, 19.9988, 19.9818, 
    19.9807, 19.9571, 19.9386, 19.9045, 19.8687, 19.8410
  )
)


paleta <- colorNumeric(
  palette = c("white", "#64b5f6", "#1e88e5", "#0d47a1", "navy"),
  domain = punkty_geo$bilety
)


wybrane_etykiety <- punkty_geo %>% 
  filter(miejsce %in% c("Dolina Kościeliska", "Kuźnice", "Łysa Polana"))


legenda_html <- tags$div(
  style = "background: rgba(255,255,255,0.85); padding: 6px 8px; border-radius: 8px; font-size: 12px; line-height: 1.3;",
  tags$div(
    style = "font-weight:600; margin-bottom:4px; text-align:center;",
    "Liczba biletów (X–III)"
  ),
  tags$div(
    style = "display:flex; align-items:center; gap:8px; justify-content:center;",
    span(format(min(punkty_geo$bilety), big.mark = " ")),
    div(style = sprintf(
      "width:120px; height:12px; background: linear-gradient(to right, %s); border-radius:6px;",
      paste(c("white", "#64b5f6", "#1e88e5", "#0d47a1", "navy"), collapse = ",")
    )),
    span(format(max(punkty_geo$bilety), big.mark = " "))
  )
)


leaflet(punkty_geo, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$OpenTopoMap) %>%
  

  addCircleMarkers(
    lng = ~long, lat = ~lat,
    radius = 9,
    fillColor = ~paleta(bilety),
    color = "black", weight = 1.3,
    fillOpacity = 0.9, stroke = TRUE,
    label = ~paste0(miejsce, ": ", format(bilety, big.mark = " "), " biletów (X–III)")
  ) %>%
  

  addLabelOnlyMarkers(
    data = wybrane_etykiety,
    lng = ~long, lat = ~lat - 0.011, 
    label = ~lapply(paste0(miejsce, "<br>(", format(bilety, big.mark = " "), ")"),HTML),
    labelOptions = labelOptions(
      noHide = TRUE, textsize = "12px", direction = "center", textOnly = TRUE,
      style = list("color" = "black", "font-weight" = "bold", "text-shadow" = "0 0 3px white")
    )
  ) %>%
  

  addControl(html = legenda_html, position = "bottomright") %>%
  

  fitBounds(
    lng1 = min(punkty_geo$long) - 0.005, lat1 = min(punkty_geo$lat) - 0.0025,
    lng2 = max(punkty_geo$long) + 0.005, lat2 = max(punkty_geo$lat) + 0.0025
  )

