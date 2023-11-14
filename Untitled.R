#library(shiny)
library(tidyverse)
library(plotly)

maize <- read_csv("maize.csv")
soybean <- read_csv("soybean.csv")
sugarcane <- read_csv("sugarcane.csv")

ui <- fluidPage(
  
  sidebarPanel(
    
    selectizeInput(inputId = "datasets",
                   label = "Type of Crop",
                   choices = c("Maize", "Soybean", "Sugar Cane"),
                   multiple = TRUE,
                   options = list('plugins' = list('remove_button'))
    ),
    
    checkboxInput(inputId = "percapita", 
                  label = "Per Capita", 
                  value = TRUE),
    
    width = 2),
  
  mainPanel(
    plotOutput("graph", width = "100%")
  )
)

server <- function(input, output) {
  
  
  selected_datasets <- reactive({
    
    selected_datasets <- character(0)
    
    if ("Maize" %in% input$datasets) {
      selected_datasets <- c(selected_datasets, "maize")}
    if ("Soybean" %in% input$datasets) {
      selected_datasets <- c(selected_datasets, "soybean")}
    if ("Sugar Cane" %in% input$datasets) {
      selected_datasets <- c(selected_datasets, "sugarcane")}
    
    selected_datasets
    
  })
  
  
  output$graph <- renderPlot({
    ggplot() +
      lapply(selected_datasets(), function(dataset) {
        # Plot lines for selected datasets
        df <- get(dataset)
        geom_line(data = df, aes(x = year, y = production_kg, color = dataset), size = 1)
      }) +
      labs(title = "Crop Production Over Years",
           x = "Year", y = "Production") +
      theme_minimal()
  })
}

shinyApp(ui, server)