#library(shiny)
library(tidyverse)
library(plotly)

maize <- read_csv("maize.csv")
soybean <- read_csv("soybean.csv")
sugarcane <- read_csv("sugarcane.csv")
cattle <- read_csv("cattle.csv")
pigmeat <- read_csv("pigmeat.csv")
poultry <- read_csv("poultry.csv")

ui <- fluidPage(
  titlePanel("Breakdown of Agriculture Production"),
  
  tags$head(
    tags$style(
      HTML(
        ".wellcropslight {
          background-color: #E1E7DF;
        }
        .wellwhite {
          background-color: #FFFFFF;
        }
        .wellmeatlight {
          background-color: #EDDDD4;
        }"
      )
    )
  ),
  
  wellPanel(
    h3("Controls"),
    class = "wellcropslight",
    selectizeInput(inputId = "datasets1", 
                   label = "Type of Crop", 
                   choices = c("Maize", "Soybean", "Sugar Cane"), 
                   multiple = TRUE, options = list('plugins' = list('remove_button')))
  ),

  wellPanel(
    h3("Graph"),
    textOutput("cropgraph"),
    class = "wellcropslight",
    
    tabsetPanel(
      
      tabPanel("Crop Production",
               wellPanel(
                 class = "wellwhite",
                 h4("Crop Production over the Years"),
                 fluidRow(column(width = 10, plotlyOutput("graph1", width = "100%")),
                          column(width = 2, checkboxInput(inputId = "percapita", label = "Per Capita", value = TRUE))
                          ))),
      tabPanel("Land Use", 
               wellPanel(
                 class = "wellwhite",
                 h4("Land Use over the Years"),
                 plotlyOutput("graph2", width = "100%"))),
      tabPanel("Animal Feed", 
               wellPanel(
                 class = "wellwhite",
                 h4("Percentage of Crops Used for Animal Feed over the Years"),
                 plotlyOutput("graph3", width = "100%")))
      )),
  
  titlePanel("Breakdown of Meat Production"),
  
  wellPanel(
    h3("Controls"),
    class = "wellmeatlight",
    selectizeInput(inputId = "datasets2", 
                   label = "Type of Animal", 
                   choices = c("Cattle", "Poultry", "Pigmeat"), 
                   multiple = TRUE, 
                   options = list('plugins' = list('remove_button'))),
    selectInput(inputId = "datatype",
                label = "Choice of Units",
                choices = c("Mass", "Quantity"))
    ),
  
  wellPanel(
    h3("Graph"),
    class = "wellmeatlight",
        conditionalPanel(
          condition = "input.datatype == 'Mass'",
          
          wellPanel(
            class = "wellwhite",
            h4("Meat Production over the Years in Mass"),
            (fluidRow(
              column(width = 10, plotlyOutput("graphA", width = "100%")),
              column(width = 2, checkboxInput(inputId = "percapitaA", 
                                              label = "Per Capita", 
                                              value = TRUE)))))),
          conditionalPanel(
            condition = "input.datatype == 'Quantity'",
            
            wellPanel(
              class = "wellwhite",
              h4("Meat Production over the Years in Quantity"),
              (fluidRow(
                column(width = 10, plotlyOutput("graphB", width = "100%")),
                column(width = 2, checkboxInput(inputId = "percapitaB", 
                                                label = "Per Capita", 
                                                value = TRUE)))))
          )
  )
)

server <- function(input, output) {
  
  output$cropgraph <- renderText({
    "Click on any of the tabs below to navigate between graphs on crop production, land Use by the different crops, or the percentage of the different crops being used as animal feed!"
  })
  
  selected_datasets1 <- reactive({
    selected_datasets1 <- character(0)
    if ("Maize" %in% input$datasets1) {
      selected_datasets1 <- c(selected_datasets1, "maize")}
    if ("Soybean" %in% input$datasets1) {
      selected_datasets1 <- c(selected_datasets1, "soybean")}
    if ("Sugar Cane" %in% input$datasets1) {
      selected_datasets1 <- c(selected_datasets1, "sugarcane")}
    selected_datasets1
  })
  
  output$graph1 <- renderPlotly({
    plot1 <- plot_ly()
    
    if ("maize" %in% selected_datasets1()) {
      plot1 <- add_trace(plot1,
                        x = maize$year,
                        y = if (input$percapita) maize$production_kgpercapita else maize$production_kg,
                        type = "scatter", mode = "lines+markers",
                        name = "Maize")}
    if ("soybean" %in% selected_datasets1()) {
      plot1 <- add_trace(plot1,
                        x = soybean$year,
                        y = if (input$percapita) soybean$production_kgpercapita else soybean$production_kg,
                        type = "scatter", mode = "lines+markers",
                        name = "Soybean")}
    if ("sugarcane" %in% selected_datasets1()) {
      plot1 <- add_trace(plot1,
                        x = sugarcane$year,
                        y = if (input$percapita) sugarcane$production_kgpercapita else sugarcane$production_kg,
                        type = "scatter", mode = "lines+markers",
                        name = "Sugar Cane")}
    
    plot1 <- layout(plot1, 
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Production of Crop (kg)"),
                   showlegend = TRUE)
    plot1})
  
  output$graph2 <- renderPlotly({
    plot2 <- plot_ly()
    
    if ("maize" %in% selected_datasets1()) {
      plot2 <- add_trace(plot2,
                         x = maize$year,
                         y = (maize$landuse_ha),
                         type = "scatter", mode = "lines+markers",
                         name = "Maize")}
    if ("soybean" %in% selected_datasets1()) {
      plot2 <- add_trace(plot2,
                         x = soybean$year,
                         y = (soybean$landuse_ha),
                         type = "scatter", mode = "lines+markers",
                         name = "Soybean")}
    if ("sugarcane" %in% selected_datasets1()) {
      plot2 <- add_trace(plot2,
                         x = sugarcane$year,
                         y = (sugarcane$landuse_ha),
                         type = "scatter", mode = "lines+markers",
                         name = "Sugar Cane")}
    
    plot2 <- layout(plot2,
                    xaxis = list(title = "Year"), yaxis = list(title = "Land Use (ha)"),
                    showlegend = TRUE)
    plot2})
  
  output$graph3 <- renderPlotly({
    plot3 <- plot_ly()
    
    if ("maize" %in% selected_datasets1()) {
      plot3 <- add_trace(plot3,
                         x = maize$year,
                         y = (maize$animalfeed_kg/maize$production_kg)*100,
                         type = "scatter", mode = "lines+markers",
                         name = "Maize")}
    if ("soybean" %in% selected_datasets1()) {
      plot3 <- add_trace(plot3,
                         x = soybean$year,
                         y = (soybean$animalfeed_kg/soybean$production_kg)*100,
                         type = "scatter", mode = "lines+markers",
                         name = "Soybean")}
    if ("sugarcane" %in% selected_datasets1()) {
      plot3 <- add_trace(plot3,
                         x = sugarcane$year,
                         y = (sugarcane$animalfeed_kg/sugarcane$production_kg)*100,
                         type = "scatter", mode = "lines+markers",
                         name = "Sugar Cane")}
    
    plot3 <- layout(plot3,
                    xaxis = list(title = "Year"), yaxis = list(title = "Percentage of Crops Used for Animal Feed (%)"),
                    showlegend = TRUE)
    plot3})
  
  selected_datasets2 <- reactive({
    selected_datasets2 <- character(0)
    if ("Cattle" %in% input$datasets2) {
      selected_datasets2 <- c(selected_datasets2, "cattle")}
    if ("Poultry" %in% input$datasets2) {
      selected_datasets2 <- c(selected_datasets2, "poultry")}
    if ("Pigmeat" %in% input$datasets2) {
      selected_datasets2 <- c(selected_datasets2, "pigmeat")}
    selected_datasets2
  })
 
  output$graphA <- renderPlotly({
    plotA <- plot_ly()
    
    if ("cattle" %in% selected_datasets2()) {
      plotA <- add_trace(plotA,
                        x = cattle$year,
                        y = if (input$percapitaA) cattle$production_kgpercapita else cattle$production_t*1000,
                        type = "scatter", mode = "lines+markers",
                        name = "Cattle")}
    if ("poultry" %in% selected_datasets2()) {
      plotA <- add_trace(plotA,
                        x = poultry$year,
                        y = if (input$percapitaA) poultry$production_kgpercapita else poultry$production_t*1000,
                        type = "scatter", mode = "lines+markers",
                        name = "Poultry")}
    if ("pigmeat" %in% selected_datasets2()) {
      plotA <- add_trace(plotA,
                        x = pigmeat$year,
                        y = if (input$percapitaA) pigmeat$production_kgpercapita else pigmeat$production_t*1000,
                        type = "scatter", mode = "lines+markers",
                        name = "Pigmeat")}
    
    plotA <- layout(plotA,
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Production of Meat(kg)"),
                   showlegend = TRUE)
    plotA})
  
  output$graphB <- renderPlotly({
    plotB <- plot_ly()
    
    if ("cattle" %in% selected_datasets2()) {
      plotB <- add_trace(plotB,
                         x = cattle$year,
                         y = if (input$percapitaB) cattle$producing_or_slaughtered_animals_percapita 
                         else cattle$producing_or_slaughtered_animals,
                         type = "scatter", mode = "lines+markers",
                         name = "Cattle")}
    if ("poultry" %in% selected_datasets2()) {
      plotB <- add_trace(plotB,
                         x = poultry$year,
                         y = if (input$percapitaB) poultry$producing_or_slaughtered_animals_percapita 
                         else poultry$producing_or_slaughtered_animals,
                         type = "scatter", mode = "lines+markers",
                         name = "Poultry")}
    if ("pigmeat" %in% selected_datasets2()) {
      plotB <- add_trace(plotB,
                         x = pigmeat$year,
                         y = if (input$percapitaB) pigmeat$producing_or_slaughtered_animals_percapita
                         else pigmeat$producing_or_slaughtered_animals,
                         type = "scatter", mode = "lines+markers",
                         name = "Pigmeat")}
    
    plotB <- layout(plotB,
                    xaxis = list(title = "Year"),
                    yaxis = list(title = "Quantity"),
                    showlegend = TRUE)
    plotB})

  
}

shinyApp(ui, server)