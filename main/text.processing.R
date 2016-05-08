require(doParallel)

process.text <- function(text, profanity.vec, verbose=F) {
    
    if (verbose) cat("\n", '# Удаляем <>                                       ')
    text <- gsub("[<>]+","",text)
    
    if (verbose) cat("\r", '# Чистим от плохих слов, заменяем на <profanity>   ')
    profanity.vec[1] = paste(' +',profanity.vec[1], collapse = '')
    profanity.vec[length(profanity.vec)] = paste(profanity.vec[length(profanity.vec)],'+')
    profanity.search <- gregexpr(paste(profanity.vec,collapse = ' +| +'), tolower(text))
    regmatches(text, profanity.search) <- '<profanity>'
    text <- gsub("<profanity>[a-zA-Z]+", '<profanity>', text)
    
    ############################################ Удаляем повторяющиеся символы (>3 приводим к 2)
    
    ############################################ Разбираемся с -, '
    
    if (verbose) cat("\r", '# Вытаскиваем конструкции в () в отдельные документы')
    new.phrases.search <- gregexpr("\\(([[:graph:]]+\\s)+[[:graph:]]+\\)", text)
    new.phrases <- unlist(regmatches(text, new.phrases.search))
    text <- gsub("\\(([[:graph:]]+\\s)+[[:graph:]]+\\)","", text)
    text <- c(text, new.phrases)
    
    ############################################# 8. Ищем общеупотребительные названия и имена заключаем их в <>
    
    if (verbose) cat("\r", '# Ищем special_words и заменяем на <special words>   ')
    #     emails                     <email>
    text <- gsub("[[:graph:]]+@[a-z]+\\.[a-z]+",'<email>', text)
    
    #     social network user        <user> 
    text <- gsub("@[[:graph:]]+",' <user>', text)
    
    #     порядковое числительное    <ordinal>
    text <- gsub('[0-9]+(nd|st|rd|th)','<ordinal>', text)
    
    #     hash-tags                  <hashtag>
    text <- gsub('#[A-Za-z]+','<hashtag>', text)
    
    #     web-site references        <link>
    text <- gsub('(http|www)[[:graph:]]+[A-Za-z]+', '<link>', text)
    
    #############################################     телефоны                   <phonenumber>
    
    #############################################     время, даты                <time>, <date>
    
    #############################################     дни недели, месяцы
    
    #############################################     почтовые коды              <postalcode>
    
    #############################################     имена                      <name>
    
    #     деньги                     <money>
    money.pattern <- "(USD|EUR|GBP|[£€$]) *[[:digit:]]+|(USD|EUR|GBP|[£€$]) *[[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+ *(USD|EUR|GBP|cents|[£€$])|[[:digit:]]*\\.[[:digit:]]+ *(USD|EUR|GBP|cents|[£€$])"  
    text <- gsub(money.pattern,'<money>', text)
    
    #     percents                   <percent>
    text <- gsub('[[:digit:]]+\\.?[[:digit:]]+ ?%', '<percent>', text)
    
    #     оставшиеся цифры           <othdigit>
    text <- gsub('[[:digit:]]+\\.?[[:digit:]]+', '<othdigit>', text)
    
    #     I                          <ich>
    text <- gsub('(I)([[:punct:][:blank:]])', '<ich>\\2', text)
    
    # Replace [?!;.]+ with [.]
    text <- gsub("[?!;.]+",".",text)
    
    ############################################## ToDo - классификатор конца преддложения, заменяем на <end>, используем \\1,\\2, уделяем внимание Mr. Dr. и пр
    
    if (verbose) cat("\r", '# Разбиваем документы по фразам                               ')
    text <- unlist(strsplit(text, "\\.\\s+"))
    
    if (verbose) cat("\r", "# Удаляем всю оставшуюся пунктуацию, кроме '                  ")
    text <- gsub("[-»,–—^‘’(:)%/\\|&§¶@+*“”`´„~″˚$#=£®_★☆♥〜∇·･●°¡€…-）]+"," ",text)
    text <- gsub("\\.","",text)
    text <- gsub("[{}]+|\"|\\[+|\\]+\""," ",text)
    
    if (verbose) cat("\r", '# Удаляем дублирующиеся пробелы в начале и в конце предложения')
    text <- gsub("^[ \t\r\n\f]+|[ \t\r\n\f]+$","", text)     
    
    if (verbose) cat("\r", '# Удаляем все оставшиеся странные знаки                       ')
    text <- iconv(text, "latin1", "ASCII", sub="")
    
    if (verbose) cat("\r", '# Удаляем неанглийские слова                                  ')
    text <- gsub('[^[:alpha:][:punct:][:blank:]]', '<foreign>', text)
    text <- gsub('((<foreign>)+ )+', '<foreign> ', text)
    
    if (verbose) cat("\r", '# Удаляем лишние пробелы в конце предложения и в начале тоже   ')
    text <- gsub("[ \t\r\n\f]+"," ", text)
    text <- gsub("^[ \t\r\n\f]+|[ \t\r\n\f]+$","", text)
    
    if (verbose) cat("\r", '# Удаляем все что содержит одно, два слова или ничего          ')
    text <- text[sapply(strsplit(text, "\\s+"), length) > 2]
    
    if (verbose) cat("\r", '# Приводим к нижнему регистру, оставляем только уникальные фразы')
    text <- unique(tolower(text))
    
    if (verbose) cat("\r", '# проставляем начало и конец предложения                         ')
    text <- gsub("^","<BOS> ", text)
    text <- gsub("$"," <EOS>", text)
    
    if (verbose) cat("\r", '# Возвращаем текст                                               ')
    return(text)
}

process.text.par <- function(text, profanity.vec,cores) {
    
    parts = ceiling(length(text)/50000)
    scope = if (parts>cores) parts else cores
    
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    
    text.processed <- foreach (i = 1:scope, .combine=c, .export='process.text') %dopar% {
        start <- floor((i-1)*length(text)/scope) + 1
        end <- floor(i*length(text)/scope)
        process.text(text[start:end], profanity.vec)
    }
    
    stopCluster(cl)
    
    return(text.processed)
}


