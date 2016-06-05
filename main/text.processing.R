source("main/multiprocessing.R")


#' Title
#'
#' @param text : text corpus in ch.vector
#' @param profanity.vec : ch.vector of profanity words from google list 
#' @param verbose : logging parametr, if true print function processing progress 
#'
#' @return processed text corpus
#' 
process.text <- function(text, profanity.vec, verbose=F) {
    
    if (verbose) cat('\n# Удаляем <>\n', text)
    text <- gsub("[<>]+","",text)
    
    ############################################ Удаляем повторяющиеся символы (>3 приводим к 2)
    
    ############################################ Разбираемся с -, '
    
    if (verbose) cat('\n# Вытаскиваем конструкции в () в отдельные документы\n', text)
    new.phrases.search <- gregexpr("\\(([[:graph:]]+\\s)+[[:graph:]]+\\)", text)
    new.phrases <- unlist(regmatches(text, new.phrases.search))
    text <- gsub("\\(([[:graph:]]+\\s)+[[:graph:]]+\\)","", text)
    text <- gsub('(I)([[:punct:][:blank:]])', '<ich>\\2', text)
    text <- tolower(c(text, new.phrases))
    
    ############################################# 8. Ищем общеупотребительные названия и имена заключаем их в <>
    
    gsub.multiple.patterns <- function(text, patterns){
        for (pt in patterns) text <- gsub(pt[1],pt[2], text)
        return(text)
    }
    
    if (verbose) cat('\n# Ищем special_words и заменяем на <special words>\n', text)
    money.pattern <- "(usd|eur|gbp|[£€$]) *[[:digit:]]+|(usd|eur|gbp|[£€$]) *[[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+ *(usd|eur|gbp|cents|[£€$])|[[:digit:]]*\\.[[:digit:]]+ *(usd|eur|gbp|cents|[£€$])"  
    patterns.list.part1 <- list(c("[[:graph:]]+@[a-z]+\\.[a-z]+",'<email>'),
                            c("@[a-z_]+",' <user>'), c('[0-9]+(nd|st|rd|th)','<ordinal>'),
                            c('#[a-z_]+','<hashtag>'), c('(http|www)[[:graph:]]+[a-z]+', '<link>'),
                            c(money.pattern,'<money>'), c('[[:digit:]]+\\.?[[:digit:]]+ ?%', '<percent>'),
                            c('[[:digit:]]+\\.?[[:digit:]]+|[[:digit:]]+', '<othdigit>'), c("[?!;.]+","."),
                            c('mr\\.', '<mr>'), c('mrs\\.', '<mrs>'), c('ms\\.', '<ms>'), c('dr\\.', '<dr>'))
    text <- gsub.multiple.patterns(text, patterns.list.part1)
    
    #############################################     телефоны                   <phonenumber>
    #############################################     время, даты                <time>, <date>
    #############################################     дни недели, месяцы
    #############################################     почтовые коды              <postalcode>
    #############################################     имена                      <name>
    
    if (verbose) cat('\n# Разбиваем документы по фразам \n', text)
    text <- unlist(strsplit(text, "\\.\\s+"))
    
    patterns.list.part2 <- list(c("'s", ' is'),c("'re", ' are'), c("'d", ' would'),
                                c("'ve", ' have'), c("n't", ' not'), c("'m", ' am'),
                                c("[»,–—^‘’(:)%-/\\|&§¶@+*“”`´„~″˚$#=£®_★☆♥〜∇·･●°¡€…]+"," "),
                                c("\\.",""), c("[{}]+|\"|\\[+|\\]+\""," "), c('[—-）]+', '<foreign>'))
    text <- gsub.multiple.patterns(text, patterns.list.part2)
    
    if (verbose) cat('\n# Удаляем все оставшиеся странные знаки\n', text)
    text <- iconv(text, "latin1", "ASCII", sub="")
    
    patterns.list.part3 <- list(c("[ \t\r\n\f]+"," "), c("^[ \t\r\n\f]+|[ \t\r\n\f]+$",""),
                                c("^","<BOS> "), c("$"," <EOS>"))
    text <- gsub.multiple.patterns(text, patterns.list.part3)
    
    if (verbose) cat('\n# Чистим от плохих слов, заменяем на <profanity>\n', text)
    profanity.vec[1] = paste(' +',profanity.vec[1], collapse = '')
    profanity.vec[length(profanity.vec)] = paste(profanity.vec[length(profanity.vec)],'+')
    profanity.search <- gregexpr(paste(profanity.vec,collapse = ' +| +'), tolower(text))
    regmatches(text, profanity.search) <- ' <profanity> '
    
    if (verbose) cat('\n# чистим от повторений\n', text)
    text <- gsub('((<[a-z]+>)+ )\\1+', '\\1', text)
    text <- gsub('(>)[a-z]+|[a-z]+(<)', '\\1', text)
    
    if (verbose) cat('\n# Удаляем все что содержит одно, два слова или ничего\n', text)
    text <- text[sapply(strsplit(text, "\\s+"), length) > 4]
    
    if (verbose) cat('\n# Оставляем только уникальные фразы\n', text)
    text <- unique(text)
    
    if (verbose) cat('\n# Возвращаем текст\n', text)
    return(text)
}


#' process.text.par
#'
#' @param cores : core to use for multiprocessing
#' 
#' the same as process.text but work in parallel and
#' @export apply.func.par from multiprocessing.R
#'
process.text.par <- function(text, profanity.vec, cores=8) {
    pt <- apply.func.par(text, 50000, process.text, profanity.vec, 'process.text', cores)
    return(unique(pt))
}


#' compute.rare.vocabulary
#'
#' @param text : text corpus in ch.vector
#' @param rare_tres : treshold indicates which words shold be considered as rare (in counts)
#' @param path : path to store result
#'
#' @return rare words in ch.vector
#'
compute.rare.vocabulary <- function(text, rare_tres, path=NULL) {
    words_t <- table(unlist(strsplit(text, " ")))
    vocabulary_rare <- names(words_t[words_t < rare_tres])
    vocabulary_rare[1] = paste('',vocabulary_rare[1])
    vocabulary_rare[length(vocabulary_rare)] = paste(vocabulary_rare[length(vocabulary_rare)],'')
    if(!is.null(path)) saveRDS(vocabulary_rare, path) 
    else return(vocabulary_rare)
}


#' process.rare
#'
#' @param text : text corpus as ch.vector
#' @param rr.vcb : dictionary of rare words wich should be replaced by <unk>
#'
#' @return text corpus as ch.vector with rare words replaced
#'
process.rare <- function(text, rr.vcb) {
    vocabulary.search <- gregexpr(paste(rr.vcb, collapse = ' | '), text)
    regmatches(text, vocabulary.search) <- ' <unk> '
    return(text)
}


#' process.rare.par
#'
#' analog of process.rare but work in parallel and
#' @param cores : core to use for multiprocessing
#'
#' @export apply.func.par from multiprocessing.R
#'
process.rare.par <- function(text, rr.vcb, cores=8) {
    apply.func.par(text, 50000, process.rare, rr.vcb, 'process.rare', cores)
}
