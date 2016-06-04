require(data.table)


#' model.learn
#'
#' @param Ngrams.count : ngrams count table as named numeric.vector
#' @param model.name : name of the model (str)
#' @param level : level of model for Kneser-ney smoothing, one of ('highest','higher')
#'
#' @return assign @model.name to Data Table 
#'
model.learn <- function(Ngrams.count, model.name, level='higher') {
    
    N = length(unlist(strsplit(names(Ngrams.count[1]), " ")))
    
    model <- data.table(Ngrams.words=names(Ngrams.count),Ngrams.count=Ngrams.count)
    
    split.words = strsplit(model[,Ngrams.words],' ')
    
    if (N == 2) {
        model[, ':=' (
            pre.first = sapply(split.words, "[", 1),
            first = sapply(split.words, "[", 2),
            Ngrams.words = NULL
        )]
        unique.Ngram.count <- length(Ngrams.count)
        model[, pre.first.count := length(unique(pre.first)), by = first]
        model[, ':=' (
            pre.first = NULL,
            Ngrams.count = NULL
        )]
        model <- unique(model, by = 'first')
        model[, ':=' (
            last.term = pre.first.count/unique.Ngram.count,
            pre.first.count = NULL
        )]
        
        setkey(model, first)
        
        assign(model.name, model, envir = .GlobalEnv)
        return()
    }
    
    range = if (level=='highest') -N else c(-1,-N)
                                                
    model[, ':=' (
        first = sapply(
            lapply(split.words, "[", range), 
            function(x) paste(x, collapse = " ")),
        last = sapply(split.words, "[", N),
        Ngrams.words = NULL
    )]
                                                
    if (level == 'higher') {
        model[, pre.first := sapply(split.words, "[", 1)]
        model[, ':=' (
            pre.first.count = length(unique(pre.first)),
            Ngrams.count = sum(Ngrams.count)), 
            by = .(first,last)]
        model[, ':=' (
            last.count = length(unique(last))
        ), by = .(pre.first,first)]
        model[, pre.first := NULL]
        model <- unique(model, by = c('first','last'))
        unique.Ngram.count <- nrow(model)
    }
    
    n.1 <- model[Ngrams.count==1,.N]
    if (n.1 == 0) n.1 <- 1
    n.2 <- model[Ngrams.count==2,.N*2]
    if (n.2 == 0) n.2 <- 1
    D <- n.1/(n.1+2*n.2)
    
    if (level == 'higher') {
        model[, ':=' (
            first.term = sapply(pre.first.count - D, function(x) max(x,0))/unique.Ngram.count,
            second.term = D*last.count/unique.Ngram.count,
            pre.first.count = NULL,
            last.count = NULL
        )]
    }
    
    model[, ':=' (
        first.accurance.count = sum(Ngrams.count),
        last.count = length(unique(last))), 
        by=first]
    
    model[, ':=' (
        highest.first.term = sapply(Ngrams.count - D, function(x) max(x,0))/first.accurance.count,
        highest.second.term = D*last.count/first.accurance.count,
        first.accurance.count = NULL,
        last.count = NULL,
        Ngrams.count = NULL
    )]
    
    setkey(model, first)
    
    assign(model.name, model, envir = .GlobalEnv)
}


#' model.get.row
#'
#' @param model : ngram model in Data table with index "first" 
#' @param NGram : ngram to find (string)
#'
#' @return Data Table with @Ngram as first in each row
#' 
model.get.row <- function(model, NGram) {
    return(model[first==NGram])
}