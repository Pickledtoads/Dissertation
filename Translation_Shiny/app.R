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
                  textAreaInput("input_Box", label = "Please input the French sentence for translation:", resize="none"),
                  actionButton("trans_Button", "Press to Translate"),
                  br(),
                  br(),
                  textOutput("output_Box"),
                  br(),
                  textAreaInput(inputId="ref_Box", label = "Please input the reference sentence for determining BLEU:", resize="none"),
                  actionButton("BLEU_Button", "Press to Calculate BLEU"),
                  br(),
                  br(),
                  textOutput("BLEU_Box"),
                  br(),
                  textAreaInput(inputId="ref_A_Box", value = 50,label = "How many sentences do you want to average BLEU across?", resize="none"),
                  actionButton("BLEU_A_Button", "Press to Calculate Average BLEU"),
                  br(),
                  br(),
                  textOutput("BLEU_A_Box")


        )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # prevent user from entering data in the output box
  shinyjs::disable("output_Box")
  rResult <- reactiveValues(translation = "")
  observeEvent(input$trans_Button, {

    # read in the inputs from textboxes
    folder <- input$input_File
    model <- input$ModelChoice
    french <- unlist(strsplit(input$input_Box,split = " "))

    # use the dropdown to select the right model
    # then run the model to translate
    if (model == "IBM 1"){
      translation <- IBM1_Translate(french, 10000, folder)
      output$output_Box <- renderText({paste(translation)})
    }
    else if (model == "IBM 2"){
      translation <- IBM2_Translate(french, 10000,folder)
      output$output_Box <- renderText({paste(translation)})
    }
    else if (model == "IBM 3"){
      translation <- IBM3_Translate(french, 10000,folder)
      output$output_Box <- renderText({paste(translation)})
    }
    else if (model == "IBM 4"){
      translation <- IBM4_Translate(french, 10000,folder)
      output$output_Box <- renderText({paste(translation)})
    }
    else if (model == "IBM 5"){
      translation <- IBM5_Translate(french, 10000,folder)
      output$output_Box <- renderText({paste(translation)})
    }

    # pass the translation result to the reactive
    rResult$translation <- translation

  })

  # button to calculate the BLEU
  observeEvent(input$BLEU_Button, {
    reference <- input$ref_Box
    val <- BLEU(reference,rResult$translation)
    output$BLEU_Box <- renderText({paste(val)})

  })

  observeEvent(input$BLEU_A_Button, {

    source("Corpus_BLEU.R")
    model <- input$ModelChoice


    # choose the correct filepaths for the corpus
    path <- as.character(input$input_File)
    folder <- path
    path <- substr(path,1,(nchar(path)-8))
    Fre_path <- paste(path,"CleanedShortFrench.txt", sep="")
    Eng_path <- paste(path,"CleanedShortEnglish.txt", sep="")
    BLEU <- Corpus_BLEU(Fre_path, Eng_path, model, input$ref_A_Box, folder)
    output$BLEU_A_Box <- renderText({paste(BLEU)})

  })
}

# Run the application
shinyApp(ui = ui, server = server)
