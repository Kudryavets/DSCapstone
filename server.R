require(shiny)

# загружаем функции
# загружаем модели
# run tests

shinyServer(function(input, output) {
    
    # input$text
    
    output$predictedWords <- renderText({
        
    })
    
    output$wordCloud <- renderPlot({
        
    })
    
    output$wordTable <- renderTable({
        
    })
    
    output$AIanswer <- renderText({
        
    })
    
    output$modelSummary <- renderTable({
        
    })
    
    output$instructions <- renderText({
        
    })
})