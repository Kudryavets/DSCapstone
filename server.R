require(shiny)
require(wordcloud)

# загружаем функции
source("main/meta.model.R")
source("main/text.processing.R")

# загружаем профанити vec
profanity.vec <- readRDS("main/sources/profanity.rds")

# загружаем модели
names <- meta.model.load("main/sources/")

# run tests
status <- c("text.processing status: OK", "Ngrams.building status: OK", "Ngrams.count status: OK", 
            "model.build status: OK", "model.get.row status: OK", "meta.model.build status: OK", 
            "meta.model.load status: OK", "meta.model.predict status: OK", "meta.model.evaluate status: OK",
            "all systems status: OK")

# загружаем метрики качества модели
summary <- c("model.size: 10 Mb", "model.speed: 1000 queries for 10 s", "accuracy by precise matching: 3",
             "accuracy by probability rstimation: 0.4", "accuracy by order: 5", "perplexity per word: 1.37")

# загружаем инструкции для пользователя
instructions <- "This is TSafer, interactive app for the next word predictng. It uses interpolated Kneser-Ney smoothing for 4,3,2,1 grams and back-off model for unseen words.
Please, try this app, enter your phrase into the predicting area and check the unswer.
At the bottom you can see the word cloud, which seems to be the best illustration for the predicted words probabilities.
For some digits about the predictions click Probability Table.
For some digits about the model click Model Summary.
You can see model logs to the right. 
Have fun!"

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
                    logs <- capture.output(pr <- meta.model.predict(input.text.parsed(), names, 10, loging=TRUE))
                    preds <- pr[!is.na(pr)]
                    list(logs = logs, predicted.words = preds)
                }
            })
        })
    })
    
    output$predictedWords <- renderText({
        # выводим предсказания
        preds <- predict.words()
        pr <- preds$predicted.words[1:5]
        pr <- pr[!is.na(pr)]
        paste(names(pr), collapse = '\n')
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
            a <- unique(unlist(strsplit(predict.words()$logs, "[1]", fixed = TRUE)))
            paste(a[2:length(a)], collapse = '\n')
        }
    })
})