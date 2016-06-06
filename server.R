require(shiny)
require(wordcloud)

# загружаем функции
source("global.R")

# загружаем профанити vec
profanity.vec <- get.profanity.vec()

# загружаем модели
names <- get.models()

# run tests
status <- get.status()

# загружаем метрики качества модели
summary <- get.summary()
    
# загружаем инструкции для пользователя
instructions <- get.instructions()

shinyServer(function(input, output) {
    
    input.text.parsed <- reactive({
        # обрабатываем текст
        sub(" <EOS>", "", process.text(input$text, profanity.vec))
    })
    
    predict.words <- eventReactive(input$goButton, {
        # делаем предсказания
        isolate({
            withProgress({
                setProgress(message = "Processing...")
                if (length(input.text.parsed()) > 0) {
                    time <- system.time(
                        logs <- capture.output(
                            pr <- meta.model.predict(input.text.parsed(), names, 10, loging=TRUE)))
                    preds <- pr[!is.na(pr)]
                    list(logs = logs, predicted.words = preds, time = time)
                }
            })
        })
    })
    
    output$predictedWords <- renderText({
        # выводим предсказания
        preds <- predict.words()
        pr <- preds$predicted.words[1:5]
        pr <- pr[!is.na(pr)]
        paste(get.restored(names(pr)), collapse = '\n')
    })
    
    output$instructions <- renderText({
        # показываем инструкции для пользователя
        if (nchar(input$text) == 0)
            instructions
        else if (length(input.text.parsed()) == 0)
            print("Please, enter more than two words! Then press Predict.")
        else
            sprintf("Processing query: %s\nPress predict!", input.text.parsed())
    })
    
    wordcloud_rep <- repeatable(wordcloud)
    
    output$wordCloud <- renderPlot({
        # отрисовываем предсказания
        if (nchar(input$text) == 0)
            NULL
        else {
            w <- predict.words()$predicted.words
            wordcloud_rep(words = names(w), freq = w, scale = c(4,0.5),
                          colors=brewer.pal(8, "Dark2"))
        }
    })
    
    output$wordTable <- renderTable({
        # расписываем предсказания с вероятностями 
        if (nchar(input$text) == 0)
            NULL
        else
            data.frame(predicted.words=predict.words()$predicted.words)
    })
    
    output$modelSummary <- renderText({
        # показываем метрики качества модели
        paste(summary, collapse = "\n")
    })
    
    output$AIanswer <- renderText({
        # выводим логи предсказателя, тестов
        if (nchar(input$text) == 0)
            paste(status, collapse = "\n")
        else {
            logs <- unique(unlist(strsplit(predict.words()$logs, "[1]", fixed = TRUE)))
            time <- c()
            for (i in 1:3) {
                time <- c(time, sprintf("time spent (by) %s: %.3f", 
                                        sub(".self", "", names(predict.words()$time[i]), fixed = T), 
                                        unname(predict.words()$time[i])))
            }
            paste(c("Predictor logs:",
                logs[2:length(logs)], 
                time), collapse = '\n')
        }
    })
})