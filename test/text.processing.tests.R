require(RUnit)
source("./project/main/text.processing.R")

text.processing.tests <- function() {
    
    # Чистим от плохих слов, заменяем на <profanity>  
    # Вытаскиваем конструкции в () в отдельные документы
    # Ищем special_words и заменяем на <special words>
    
    #     emails                     <email>
    #     social network user        <user> 
    #     порядковое числительное    <ordinal>
    #     hash-tags                  <hashtag>
    #     web-site references        <link>
    #     деньги                     <money>
    #     percents                   <percent>
    #     оставшиеся цифры           <othdigit>
    #     I                          <ich>
    
    # Replace [?!;.]+ with [.]
    # Разбиваем документы по фразам
    # Удаляем всю оставшуюся пунктуацию, кроме '
    # Удаляем дублирующиеся пробелы в начале и в конце предложения' 
    # Удаляем все оставшиеся странные знаки
    # Удаляем неанглийские слова
    # Удаляем лишние пробелы в конце предложения и в начале тоже
    # Приводим к нижнему регистру, оставляем только уникальные фразы
    
    sample <- sample(blogs,1)
    sample.processed <- process.text(sample,profanity.vec)
    sample
    sample.processed
    
    test.corpus <- c("short sentence", )
    
    # test.corpus <- readRDS('project/test/sources/test.corpus.rds')
    profanity.vec <- readRDS('project/main/sources/profanity.rds')
    processing.result.test <- readRDS('project/test/sources/text.processing.result.test.rds')
    
    processing.result <- process.text(test.corpus,profanity.vec,verbose=T)
    
    checkEquals(processing.result, processing.result.test)
}