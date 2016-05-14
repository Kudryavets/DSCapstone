require(doParallel)

process.text <- function(text, profanity.vec, verbose=F) {
    
    if (verbose) cat('\n', '# Удаляем <>\n', text)
    text <- gsub("[<>]+","",text)
    
    ############################################ Удаляем повторяющиеся символы (>3 приводим к 2)
    
    ############################################ Разбираемся с -, '
    
    if (verbose) cat('\n', '# Вытаскиваем конструкции в () в отдельные документы\n', text)
    new.phrases.search <- gregexpr("\\(([[:graph:]]+\\s)+[[:graph:]]+\\)", text)
    new.phrases <- unlist(regmatches(text, new.phrases.search))
    text <- gsub("\\(([[:graph:]]+\\s)+[[:graph:]]+\\)","", text)
    text <- c(text, new.phrases)
    
    ############################################# 8. Ищем общеупотребительные названия и имена заключаем их в <>
    
    if (verbose) cat('\n', '# Ищем special_words и заменяем на <special words>\n', text)
    #     emails                     <email>
    text <- gsub("[[:graph:]]+@[a-z]+\\.[a-z]+",'<email>', text)
    
    #     social network user        <user> 
    text <- gsub("@[A-Za-z_]+",' <user>', text)
    
    #     порядковое числительное    <ordinal>
    text <- gsub('[0-9]+(nd|st|rd|th)','<ordinal>', text)
    
    #     hash-tags                  <hashtag>
    text <- gsub('#[A-Za-z_]+','<hashtag>', text)
    
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
    
    # Replace [?!;.]+ with [.]
    text <- gsub("[?!;.]+",".",text)
    
    # Replace [Mr.,Ms,Mrs,Dr]+ with <mr>,<mrs>,<ms>,<dr>
    text <- gsub('Mr\\.|mr\\.', '<mr>', text)
    text <- gsub('Mrs\\.|mrs\\.', '<mrs>', text)
    text <- gsub('Ms\\.|ms\\.', '<ms>', text)
    text <- gsub('Dr\\.|dr\\.', '<dr>', text)
    
    #     I                          <ich>
    text <- gsub('(I)([[:punct:][:blank:]])', '<ich>\\2', text)
    
    if (verbose) cat('\n', '# Разбиваем документы по фразам, tolower\n', text)
    text <- unlist(strsplit(tolower(text), "\\.\\s+"))
    
    text <- gsub("'s", ' is', text)
    text <- gsub("'re", ' are', text)
    text <- gsub("'d", ' would', text)
    text <- gsub("'ve", ' have', text)
    text <- gsub("n't", ' not', text)
    text <- gsub("'m", ' am', text)
    
    if (verbose) cat('\n', "# Удаляем всю оставшуюся пунктуацию, кроме '\n", text)
    text <- gsub("[»,–—^‘’(:)%/\\|&§¶@+*“”`´„~″˚$#=£®_★☆♥〜∇·･●°¡€…]+"," ",text)
    text <- gsub("\\.","",text)
    text <- gsub("[{}]+|\"|\\[+|\\]+\""," ",text)
    
    if (verbose) cat('\n', '# Удаляем неанглийские слова\n', text)
    text <- gsub('[—-）]+', '<foreign>', text)
    
    if (verbose) cat('\n', '# Удаляем все оставшиеся странные знаки\n', text)
    text <- iconv(text, "latin1", "ASCII", sub="")
    
    if (verbose) cat('\n', '# Удаляем пробелы в конце и в начале предложения \n', text)
    text <- gsub("[ \t\r\n\f]+"," ", text)
    text <- gsub("^[ \t\r\n\f]+|[ \t\r\n\f]+$","", text)
    
    if (verbose) cat('\n', '# проставляем начало и конец предложения\n', text)
    text <- gsub("^","<BOS> ", text)
    text <- gsub("$"," <EOS>", text)
    
    if (verbose) cat('\n', '# Чистим от плохих слов, заменяем на <profanity>\n', text)
    profanity.vec[1] = paste(' +',profanity.vec[1], collapse = '')
    profanity.vec[length(profanity.vec)] = paste(profanity.vec[length(profanity.vec)],'+')
    profanity.search <- gregexpr(paste(profanity.vec,collapse = ' +| +'), tolower(text))
    regmatches(text, profanity.search) <- ' <profanity> '
    
    if (verbose) cat('\n', '# чистим от повторений\n', text)
    text <- gsub('((<[a-z]+>)+ )\\1+', '\\1', text)
    text <- gsub('(>)[a-z]+|[a-z]+(<)', '\\1', text)
    
    if (verbose) cat('\n', '# Удаляем все что содержит одно, два слова или ничего\n', text)
    text <- text[sapply(strsplit(text, "\\s+"), length) > 4]
    
    if (verbose) cat('\n', '# Оставляем только уникальные фразы\n', text)
    text <- unique(text)
    
    if (verbose) cat('\n', '# Возвращаем текст\n', text)
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


