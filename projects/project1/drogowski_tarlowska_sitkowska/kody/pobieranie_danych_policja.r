
library(rvest)
library(dplyr)
library(lubridate)
library(stringr)

# Funkcja pobierająca 1 stronę raportu
pobierz_strone <- function(page_num) {
  url <- paste0("https://policja.pl/pol/form/1,dok.html?page=", page_num)
  message(" Pobieram: ", url)
  
  page <- tryCatch(read_html(url),
                   error = function(e) {
                     warning("Błąd w read_html dla strony ", page_num, ": ", conditionMessage(e))
                     return(NULL)
                   })
  if (is.null(page)) return(NULL)
  

  tables <- page %>% html_nodes("table")
  if (length(tables) == 0) {
    warning(" Brak tabeli na stronie ", page_num)
    return(NULL)
  }
  
  tab <- tables[[1]] %>% html_table(fill = TRUE)
  
  tab <- tab %>%
    mutate(Data = as.Date(tab[[1]]),  
           Strona = page_num)
  
  return(tab)
}


lista_df <- list()
stop_loop <- FALSE
max_pages <- 200

for (i in 0:max_pages) {   
  if (stop_loop) break
  
  Sys.sleep(1.5)  
  message(" ", i)
  
  res <- tryCatch({
    dana <- pobierz_strone(i)
    if (is.null(dana)) {
      message(" Nie znaleziono danych (NULL) na stronie ", i, " — kończę pętlę.")
      stop_loop <- TRUE
    } else {
      lista_df[[length(lista_df) + 1]] <- dana
    }
    NULL
  },
  error = function(e) {
    message("Błąd przy stronie ", i, ": ", conditionMessage(e))
    stop_loop <<- TRUE
    NULL
  })
}

message("Zebrano stron: ", length(lista_df))

if (length(lista_df) > 0) {
  dane <- bind_rows(lista_df)
  
  # usuwamy puste kolumny i wiersze bez daty
  dane <- dane %>%
    select(where(~ !all(is.na(.)))) %>%
    filter(!is.na(Data))
  
  write.csv(dane, "raporty_police.xlsx", row.names = FALSE)
  message("Dane zapisane do: raporty_police.xlsx")
  
  print(names(dane))
  print(head(dane, 6))
} else {
  message("️ Nie pobrano danych - plik nie został zapisano")
}
View(dane)
