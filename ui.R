require(shiny)

shinyUI(fluidPage(
    
    h1(strong("TSafer. Use to predict the next word!")),
    
    fluidRow(
        column(6,
               textInput("text", label = h3("Enter your phrase here:"), width = '100%')
        ),
        column(1,
               br(), br(), br(),
               actionButton("goButton", "Predict!")
        ),
        column(5, 
               br(),
               h4("Predicted words:"),
               verbatimTextOutput("predictedWords")
        )
    ),
    
    fluidRow(
        column(12, 
               h4("Instructions"),
               verbatimTextOutput("instructions")
        )
    ),
    
    hr(),
    
    fluidRow(
        column(7,
               h4("Visualization"),
               tabsetPanel(
                   tabPanel("Word Cloud", plotOutput("wordCloud")), 
                   tabPanel("Probability Table", tableOutput("wordTable")),
                   tabPanel("Model Summary", verbatimTextOutput("modelSummary"))
               )
        ),
        column(5,
               br(),br(),
               h4("AI comments:"),
               verbatimTextOutput("AIanswer")
        )
    )
))