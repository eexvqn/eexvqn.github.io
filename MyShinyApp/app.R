#library(shiny)
library(tidyverse)
library(plotly)

maize <- read_csv("maize.csv")
soybean <- read_csv("soybean.csv")
sugarcane <- read_csv("sugarcane.csv")

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      h3("Agriculture Breakdown"),
      selectizeInput(inputId = "datasets1",
                     label = "Type of Crop",
                     choices = c("Maize", "Soybean", "Sugar Cane"),
                     multiple = TRUE,
                     options = list('plugins' = list('remove_button'))),
      checkboxInput(inputId = "percapita", 
                    label = "Per Capita", 
                    value = TRUE),
      width = 4
    ),
    
    mainPanel(
      plotOutput("graph1", width = "100%")
    ),
    
    sidebarPanel(
      h3("Proportion of Crops for Animal Feed"),
      selectInput(inputId = "datasets2",
                  label = "Type of Crop",
                  choices = c("Maize", "Soybean", "Sugar Cane")),
      width = 4
    ),
    
    mainPanel(
      plotOutput("graph2", width = "100%")
    )
  )
)

server <- function(input, output) {
  
  selected_datasets1 <- reactive({
    selected_datasets1 <- character(0)
    if ("Maize" %in% input$datasets1) {
      selected_datasets1 <- c(selected_datasets1, "maize")
    }
    if ("Soybean" %in% input$datasets1) {
      selected_datasets1 <- c(selected_datasets1, "soybean")
    }
    if ("Sugar Cane" %in% input$datasets1) {
      selected_datasets1 <- c(selected_datasets1, "sugarcane")
    }
    selected_datasets1
  })
  
  output$graph1 <- renderPlot({
    ggplot() +
      lapply(selected_datasets1(), function(dataset1) {
        df <- get(dataset1)
        production_col <- if (input$percapita) "production_kgpercapita" 
        else "production_kg"
        
        geom_line(data = df, aes(x = year, y = df[[production_col]], color = dataset), size = 1) +
          geom_line(data = df, aes(x = year, y = df$landuse_ha, color = dataset), size = 1, linetype = "dashed")
      }) +
      labs(title = "Crop Production and Land Use Over Years",
           x = "Year", y = "Values") +
      theme_minimal()
  })
  
  output$graph2 <- renderPlot({
    selected_dataset2 <- switch(input$datasets2,
                                "Maize" = maize,
                                "Soybean" = soybean,
                                "Sugar Cane" = sugarcane,
                                NULL)
    
    ggplot(selected_dataset2) +
      geom_line(aes(x = year, y = animalfeed_kg / production_kg)) +
      labs(title = "Percentage of Crop for Animal Feed",
           x = "Year", y = "Percentage") +
      theme_minimal()
  })
}

shinyApp(ui, server)
