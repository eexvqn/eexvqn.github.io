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
  
  #to change the colour of the different wells
  tags$head(
    tags$style(
      HTML(
        ".wellcropslight {
          background-color: #E1DECE;
        }
        .wellwhite {
          background-color: #FFFFFF;
        }
        .wellmeatlight {
          background-color: #A1B4A4;
        }"
      )
    )
  ),
  
  #to put the different features/controls into each well
  wellPanel(
    h3("Controls"),
    class = "wellcropslight",
    
    #to select and look at multiple datasets at once
    selectizeInput(inputId = "datasets1", 
                   label = "Type of Crop", 
                   choices = c("Maize", "Soybean", "Sugar Cane"), 
                   multiple = TRUE, options = list('plugins' = list('remove_button')))
  ),
  
  wellPanel(
    h3("Graph"),
    
    #to apply the colours of each well
    class = "wellcropslight",
    
    #to create tabs in the well that allows user to click to see crop production, land use and animal feed tabs - each with different output/graphs generated
    tabsetPanel(
      
      tabPanel("Crop Production",
               wellPanel(
                 class = "wellwhite",
                 h4("Crop Production over the Years"),
                 
                 #changes the width of the graph and the control (the option for per capita)
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
    ),
    #link for references at the bottom
    a("(Ritchie et al., 2023)", href = "https://ourworldindata.org/agricultural-production#explore-data-on-agricultural-production", target="_blank")),
  
  #new title for the meat production portion
  titlePanel("Breakdown of Meat Production"),
  
  wellPanel(
    h3("Controls"),
    class = "wellmeatlight",
    #to select and look at multiple datasets at once
    selectizeInput(inputId = "datasets2", 
                   label = "Type of Animal", 
                   choices = c("Cattle", "Poultry", "Pigmeat"), 
                   multiple = TRUE, 
                   options = list('plugins' = list('remove_button'))),
    
    #to select to see information on the datasets based on either mass or quantity produced
    selectInput(inputId = "datatype",
                label = "Choice of Units",
                choices = c("Mass", "Quantity"))
  ),
  
  wellPanel(
    h3("Graph"),
    class = "wellmeatlight",
    
    #allows for the navigation between mass and quantity such that their respective output is generated when selected
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
    ),
    a("(Ritchie et al., 2023)", href = "https://ourworldindata.org/agricultural-production#explore-data-on-agricultural-production", target="_blank")
  )
)

server <- function(input, output) {
  
  #If an item (i.e., "Maize") is selected from the UI, it will be added to selected_datasets1 (on top of the items that are already in selected_datasets1)
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
    
    #if maize is found in selected_datasets1, there will be a line added to the graph for maize production against year
    if ("maize" %in% selected_datasets1()) {
      plot1 <- add_trace(plot1,
                         x = maize$year,
                         
                         #if percapita is being checked, production per capita (kg) would be used instead of production (kg)
                         y = if (input$percapita) maize$production_kgpercapita 
                         else maize$production_kg,
                         
                         #line design
                         type = "scatter", mode = "lines+markers",
                         
                         #name of line
                         name = "Maize")}
    
    #if soybean is found in selected_datasets1, there will be a line added to the graph for soybean production against year
    if ("soybean" %in% selected_datasets1()) {
      plot1 <- add_trace(plot1,
                         x = soybean$year,
                         
                         #if percapita is being checked, production per capita (kg) would be used instead of production (kg)
                         y = if (input$percapita) soybean$production_kgpercapita 
                         else soybean$production_kg,
                         
                         #line design
                         type = "scatter", mode = "lines+markers",
                         
                         #name of line
                         name = "Soybean")}
    
    #if sugarcane is found in selected_datasets1, there will be a line added to the graph for sugarcane production against year
    if ("sugarcane" %in% selected_datasets1()) {
      plot1 <- add_trace(plot1,
                         x = sugarcane$year,
                         
                         #if percapita is being checked, production per capita (kg) would be used instead of production (kg)
                         y = if (input$percapita) sugarcane$production_kgpercapita 
                         else sugarcane$production_kg,
                         
                         #line design
                         type = "scatter", mode = "lines+markers",
                         
                         #name of line
                         name = "Sugar Cane")}
    
    #the graph plotted 
    plot1 <- layout(plot1, 
                    xaxis = list(title = "Year"),
                    yaxis = list(title = "Production of Crop (kg)"),
                    showlegend = TRUE)
    plot1})
  
  #the format of the code is the same for the items below
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
                         
                         #the formula below calculates the percentage of the crop used as animal feed
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
                    xaxis = list(title = "Year"), 
                    yaxis = list(title = "Percentage of Crops Used for Animal Feed (%)"),
                    showlegend = TRUE)
    plot3})
  
  #this function is the same format as the one for the crops, but with different elements on meat production instead
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
                    yaxis = list(title = "Production of Meat (kg)"),
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