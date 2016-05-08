require(shiny)

shinyUI(fluidPage(
    
    title = "Use me to predict next word!",
    
    fluidRow(
        column(7,
               textInput("text", label = h4("Enter your phrase here:"), 
                         value = "Enter text ..."),
               br(),
               h4("Visualization"),
               tabsetPanel(
                   tabPanel("Word Cloud", plotOutput("wordCloud")), 
                   tabPanel("Word Table", tableOutput("wordTable")),
                   tabPanel("Model Summary", verbatimTextOutput("modelSummary"))
               )
        ),
        column(5, 
               h4("Predicted words are here:"),
               verbatimTextOutput("predictedWords"),
               h4("AI comments:"),
               verbatimTextOutput("AIanswer")
        )
    ),
    
    hr(),
    
    fluidRow(
        column(12, 
               h4("Instructions"),
               verbatimTextOutput("instructions")
        )
    )
))