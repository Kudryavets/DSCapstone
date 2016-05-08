require(RUnit)
source("./project/main/text.processing.R")

text.processing.tests <- function() {
    
    
    sample <- sample(blogs,1)
    sample.processed <- process.text(sample,profanity.vec)
    sample
    sample.processed
    
    test.corpus <- с("short sentence", 
                     "shit always heppens with good people",
                     "last night I saw a nightmare (you know one of this staff you do not wanna live after)",
                     "Write me on my email atrozkiy@wonderfullworld.com", 
                     "call @Justin if you want to dance to the deth", 
                     "1. I'm not as small as a potato. 2. Give me this banana", 
                     "look at www.popcornhub.com where every #beatifull_woman can show her big popcorn for only $27.97",
                     "information 100%, I promise",
                     "some other digits 123 1123  ",
                     "  looking for something?! here is terrible noizy",
                     "some   other punctuation $^$^$^#@!",
                     "Write me on my email atrozkiy@wonderfullworld.com",
                     
                     " Вставь сюда немного арабского и странных символов, приятель "
                     )
    
    # test.corpus <- readRDS('project/test/sources/test.corpus.rds')
    profanity.vec <- readRDS('project/main/sources/profanity.rds')
    processing.result.test <- readRDS('project/test/sources/text.processing.result.test.rds')
    
    processing.result <- process.text(test.corpus,profanity.vec,verbose=T)
    
    checkEquals(processing.result, processing.result.test)
}