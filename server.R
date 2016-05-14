require(shiny)

# загружаем функции
# загружаем модели
# run tests
# загружаем метрики качества модели
# загружаем инструкции для пользователя

shinyServer(function(input, output) {
    
    # input$text
    # делаем предсказания
    
    output$predictedWords <- renderText({
        # расписываем предсказания
    })
    
    output$wordCloud <- renderPlot({
        # отрисовываем предсказания
    })
    
    output$wordTable <- renderTable({
        # расписываем предсказания с вероятностями
    })
    
    output$AIanswer <- renderText({
        # выводим логи предсказателя, тестов
    })
    
    output$modelSummary <- renderTable({
        # показываем метрики качества модели
    })
    
    output$instructions <- renderText({
        # показываем инструкции для пользователя
    })
})