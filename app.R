library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sf)
library(DT)
library(readr)
library(tidyr)
library(webshot2)
library(htmlwidgets)
library(janitor)

# Load and clean data
crime_data <- read_csv("17_Crime_by_place_of_occurrence_2001_2012.csv") %>%
  clean_names() %>%
  mutate(state_ut = toupper(trimws(state_ut)))

# Load and clean shapefile
india_map <- st_read("india_st.shp") %>%
  clean_names() %>%
  mutate(state = toupper(trimws(state)))

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .sidebar { border: 2px solid red; padding: 15px; background-color: #f8f9fa; height: 100%; }
      .title-box { color: red; font-size: 24px; font-weight: bold; margin-bottom: 15px; }
      .red-button { background-color: white; border: 2px solid red; color: red; font-weight: bold; width: 100%; margin-bottom: 10px; }
      .red-button:hover { background-color: red; color: white; }
      .map-container { padding: 10px; background-color: #e6f0ff; border: 1px solid #ccc; }
    "))
  ),
  
  fluidRow(
    column(3,
           div(class = "sidebar",
               div("Crime Explorer", class = "title-box"),
               selectInput("year", "Select Year", 
                           choices = sort(unique(crime_data$year)),
                           selected = min(crime_data$year)),
               selectInput("crime_type", "Select Crime Type", 
                           choices = names(crime_data)[3:ncol(crime_data)],
                           selected = names(crime_data)[3]),
               actionButton("export_map", "Export Map", class = "red-button")
           )
    ),
    column(9,
           div(class = "map-container",
               leafletOutput("crime_map", height = "600px"),
               plotOutput("top_states_plot"),
               plotOutput("yearly_trend_plot"),
               plotOutput("crime_pie_plot")
           )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    crime_data %>%
      filter(year == input$year) %>%
      group_by(state_ut) %>%
      summarise(value = sum(.data[[input$crime_type]], na.rm = TRUE))
  })
  
  joined_data <- reactive({
    india_map %>%
      left_join(filtered_data(), by = c("state" = "state_ut")) %>%
      mutate(value = replace_na(value, 0))
  })
  
  output$crime_map <- renderLeaflet({
    map_data <- joined_data()
    pal <- colorNumeric("YlOrRd", domain = map_data$value)
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(value),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = ~paste(state, "<br>", input$crime_type, ":", value),
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~value, title = input$crime_type)
  })
  
  output$top_states_plot <- renderPlot({
    filtered_data() %>%
      arrange(desc(value)) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(state_ut, value), y = value)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 10 States by Crime Count", x = "State/UT", y = "Crime Count") +
      theme_minimal()
  })
  
  output$yearly_trend_plot <- renderPlot({
    crime_data %>%
      group_by(year) %>%
      summarise(total_crimes = sum(.data[[input$crime_type]], na.rm = TRUE)) %>%
      ggplot(aes(x = year, y = total_crimes)) +
      geom_line(color = "darkred") +
      geom_point(color = "darkred") +
      labs(title = paste("Yearly Trend for", input$crime_type), x = "Year", y = "Total Crimes") +
      theme_minimal()
  })
  
  output$crime_pie_plot <- renderPlot({
    filtered_data() %>%
      arrange(desc(value)) %>%
      slice_head(n = 5) %>%
      ggplot(aes(x = "", y = value, fill = state_ut)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      labs(title = "Top 5 States Crime Distribution")
  })
  
  observeEvent(input$export_map, {
    map_data <- joined_data()
    pal <- colorNumeric("YlOrRd", domain = map_data$value)
    export_map <- leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(value),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = ~paste(state, "<br>", input$crime_type, ":", value),
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~value, title = input$crime_type)
    
    html_file <- tempfile(fileext = ".html")
    png_file <- tempfile(fileext = ".png")
    saveWidget(export_map, file = html_file, selfcontained = TRUE)
    webshot2::webshot(url = html_file, file = png_file, delay = 0.5, vwidth = 1000, vheight = 800)
    showModal(modalDialog(
      title = "Map Exported!",
      paste("Map has been saved as an image on your system."),
      tags$pre(png_file),
      easyClose = TRUE
    ))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

        


