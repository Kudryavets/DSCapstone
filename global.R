source("main/meta.model.R")
source("main/text.processing.R")

get.profanity.vec <- function() readRDS("main/sources/profanity.rds")

get.models <- function() meta.model.load("main/sources/")

get.status <- function() c("text.processing status: OK", "Ngrams.building status: OK", "Ngrams.count status: OK", 
                           "model.build status: OK", "model.get.row status: OK", "meta.model.build status: OK", 
                           "meta.model.load status: OK", "meta.model.predict status: OK", "meta.model.evaluate status: OK",
                           "all systems status: OK")

get.summary <- function() c("model.size: 10 Mb", "model.speed: 1000 queries for 10 s", "accuracy by precise matching: 3",
                            "accuracy by probability rstimation: 0.4", "accuracy by order: 5", "perplexity per word: 1.37")

get.instructions <- function() "This is TSafer, interactive app for the next word predictng. It uses interpolated Kneser-Ney smoothing for 4,3,2,1 grams and back-off model for unseen words.
Please, try this app, enter your phrase into the predicting area and check the unswer.
At the bottom you can see the word cloud, which seems to be the best illustration for the predicted words probabilities.
For some digits about the predictions click Probability Table.
For some digits about the model click Model Summary.
You can see model logs to the right. 
Have fun!"

get.restored <- function(preds.ch.vec) {
    paterns <- list(c("<email>",'<Some email?>'), c("<user>",'<Some @user_name?>'),
                    c('<ordinal>','<Some ordinal number?>'), c('<hashtag>','<Some #hashtag?>'),
                    c('<link>', '<Some ww.link.com?>'), c("<money>",'<Some money $ amount?>'),
                    c('<percent>', '<Some % percent?>'), c('<othdigit>', '<Some othdigits?>'),
                    c('<mr>', 'Mr.'), c('<mrs>', 'Mrs.'), c('<ms>', 'Ms.'), c('<dr>', 'Dr.'),
                    c('<ich>', 'I'), c('<foreign>', '<Some foreign 飛散す words?>'), 
                    c("<EOS>","<End of sentence>!!!"), c("<profanity>","<Some f.cking profanity word?>"))
    
    for (pt in paterns) preds.ch.vec <- gsub(pt[1],pt[2], preds.ch.vec)
    
    return(preds.ch.vec)
}