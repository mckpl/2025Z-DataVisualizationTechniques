library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(plotly)


weekly_gas_prices <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-01/weekly_gas_prices.csv')
weekly_gas_prices$date <- as.Date(weekly_gas_prices$date, format = "%Y-%m-%d")

clean_data <- weekly_gas_prices %>% 
  filter(!is.na(price)) %>% 
  mutate(
    across(c(fuel, grade, formulation), as.factor)  
  ) %>% 
  filter(date > "1995-01-01") %>% 
  filter((fuel == 'gasoline' & grade == 'all' & formulation == 'all') | (fuel == "diesel"))


ui <- fluidPage(
  
  titlePanel("Gasoline and diesel prices in USA"),
  plotlyOutput("distPlot", height = "75vh"),
  
  fluidRow(
    column(12, 
           sliderInput(inputId = "date_range",
                       label = "Date Range",
                       min = min(clean_data$date),
                       max = max(clean_data$date),
                       value = c(min(clean_data$date), max(clean_data$date)),
                       timeFormat = "%Y-%m-%d"),
           
           checkboxInput("gasoline",
                         "Gasoline",
                         value = TRUE), 
           checkboxInput("diesel",
                         "Diesel",
                         value = TRUE))
  )
  )



server <- function(input, output) {
  
  filtered_data <- reactive({
    
    wybrane_paliwa <- c()
    if (input$gasoline){
      wybrane_paliwa <- c(wybrane_paliwa, "gasoline")
    }
    if (input$diesel){
      wybrane_paliwa <- c(wybrane_paliwa, "diesel")
    }

    clean_data %>%
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2],
        fuel %in% wybrane_paliwa
      )
  })
  
  output$distPlot <- renderPlotly({
    
    data_to_plot <- filtered_data()
    
    validate(
      need(nrow(data_to_plot) > 0, "Choose non-zero date range.")
    )
    
    p <- ggplot(data = data_to_plot, 
                aes(x = date, y = price, color = fuel, group = fuel,
                    text = paste("Date:", date, "<br>Price:", round(price, 2), "$", "<br>Fuel:", fuel))) +
      geom_line(linewidth = 0.6) +
      scale_color_manual(values = c("gasoline" = "blue", "diesel" = "red")) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0,0)) +
      theme(axis.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16, face = "bold"),
            legend.position = "top") +
      labs(x = "Date", y = "Price in $") 
    
    gp <- ggplotly(p, tooltip = "text")
    
    for (i in seq_along(gp$x$data)) {
      current_name <- gp$x$data[[i]]$name
      new_name <- sub("^\\((.*),1\\)$", "\\1", current_name)
      gp$x$data[[i]]$name <- tools::toTitleCase(new_name)
    }
    
    gp <- layout(gp, 
      legend = list(orientation = "h", xanchor = "right", x = 1, yanchor = "top", y = -0.2),
      margin = list(b = 50) 
    )
    
    gp
  })
}
  


shinyApp(ui = ui, server = server)
  

