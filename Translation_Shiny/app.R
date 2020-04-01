#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjqui)
source("Shift_to_R.R")
# Define UI for application that applies the IBM translation models
ui <- fluidPage(
    shinyjs::useShinyjs(),
    # Application title
    titlePanel("French-English Translation Using the IBM Models"),

    # Dropdown to select which model you wish to use for translation 
    sidebarLayout(position = "left",
        sidebarPanel(width=4,
            textInput("input_File", "Input the path to the folder with trained distributions:"),
            selectInput("ModelChoice","Which model to use for translation:",c("IBM 1","IBM 2","IBM 3","IBM 4","IBM 5")),
        ),
        # boxes to input the desired phrase for translation
        # And to view the output
        mainPanel(width=8,
                  textAreaInput("input_Box", "Please input the French sentence for translation:", resize="none"),
                  actionButton("trans_Button", "Press to Translate"),
                  br(),
                  br(),
                  textOutput("output_Box")
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  shinyjs::disable("output_Box")
  observeEvent(input$trans_Button, {
    folder <- input$input_File
    model <- input$ModelChoice
    french <- unlist(strsplit(input$input_Box,split = " "))
    if (model == "IBM 1"){
      translation <- IBM1_Translate(french, 10000)
      output$output_Box <- renderText({"You pressed the button"})
    }
      
    
    
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
