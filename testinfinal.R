library(shiny)
library(tidyverse)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Introduction"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput(inputId = "category", 
                  label = "Category:",
                  choices = c("Education","Co-Curricular Activity","Experience","Family","Interests")
      )),
    
    # Main panel for displaying outputs ----
    mainPanel(
      verbatimTextOutput("content"))
  ))

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  content <- reactive({
    switch(input$category,
           "Education" = "Currently, I study in National University of Singapore",
           "Co-Curricular Activity" = "CCA is Uni-Y",
           "Experience" = "I worked as a marketing intern last summer",
           "Family" = "I have two sisters",
           "Interests" = "I like running")
  })
  
  output$content <- renderText({
    content()
  })
  
  
}

shinyApp(ui, server)
