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
# Define UI for application that draws a histogram
ui <- fluidPage(
    shinyjs::useShinyjs(),
    # Application title
    titlePanel(""),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(position = "left",
        sidebarPanel(width=4,
            selectInput("ModelChoice","Which model to use for translation:",c("IBM 1","IBM 2","IBM 3","IBM 4","IBM 5")),
        ),
        # Show a plot of the generated distribution
        mainPanel(width=8,
                  textAreaInput("input_Box", "Please input the French sentence for translation:", resize="none"),
                  actionButton("Trans_button", "Press to Translate", position = "centre"),
                  textAreaInput("output_Box", label ="",resize="none")
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  shinyjs::disable("output_Box")
   
}

# Run the application 
shinyApp(ui = ui, server = server)
