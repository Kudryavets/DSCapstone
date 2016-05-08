source("project/main/ngrams.R")
source("project/main/model.R")

meta.model.learn <- function(corpus, highest.ngram, path=NULL, cores=8, valid=0) {
    result = list()
    names <- c()
    for (i in 2:(highest.ngram+1)) {
        if (i == highest.ngram+1) {
            level = 'highest'
            i = highest.ngram
            name = sprintf('N%dgram.model',i)
        } else {
            level = 'higher'
            name = sprintf('N%dgram.model',i-1)
        }
        cat("\r", sprintf("building %s", name))
        NGrams <- Ngrams.build.par(corpus,i,cores)
        NGrams.count <- Ngrams.count.valid(NGrams,valid)
        model.learn(NGrams.count, name, level)
        if (!is.null(path)) {
            saveRDS(get(name), sprintf("%s%s.rds", path, name))
        }
        names <- c(names,name)
    }
    return(names)
}

meta.model.load <- function(path) {
    files <- list.files(path, pattern = "N.gram", full.names = T)
    result <- list()
    names <- c()
    for (file in files) {
        name = sprintf("%s.model",regmatches(file, regexpr("N.gram",file)))
        assign(name, readRDS(file), envir= .GlobalEnv)
        names <- c(names,name)
    }
    return(names)
}

meta.model.predict <- function(sentence, names, preds.num) {
    highest.ngram = max(as.numeric(regmatches(names, regexpr("[[:digit:]]",names))))
    
    sorting <- function(short.seq,long.seq) {
        acc1 <- c()
        for (name in short.seq) {
            acc1 <- c(acc1,long.seq[name])
            long.seq <- long.seq[names(long.seq) != name]
        }
        long.seq <- c(acc1,long.seq)
        return(long.seq)
    }
    
    compute.Ngram.prob <- function(ngram.rang, text, highest.ngram=T) {
        split.words = unlist(strsplit(text,' '))
        len <- length(split.words)
        offset = if (ngram.rang == 1) 0 else ngram.rang - 2
        Ngram <- paste(split.words[(len-offset):len], collapse=" ")
        
        model.name <- names[grepl(ngram.rang,names)]
        model.rows <- model.get.row(get(model.name), Ngram)
        
        if (nrow(model.rows)==0) {
            if (ngram.rang == 1) {
                result <- "Here will be the model for unknown words"
            } else {
                result <- compute.Ngram.prob(ngram.rang-1, Ngram)
            }
        } else {
            if (ngram.rang==1) {
                result <- model.rows[,last.term]
                names(result) <- model.rows[,first]
            } else {
                lower.ngram.prob <- 
                    if (ngram.rang == 2) {
                        do.call(c, 
                            lapply(model.rows[,last], 
                                   function (word) compute.Ngram.prob(ngram.rang-1,word,highest.ngram=F)))
                    } else {
                        compute.Ngram.prob(ngram.rang-1,text,highest.ngram=F)
                    }
                
                diff <- length(lower.ngram.prob)-nrow(model.rows)
                lower.ngram.prob <- sorting(model.rows[,last],lower.ngram.prob)
                
                if (highest.ngram) {
                    term.1 <- c(model.rows[,highest.first.term],rep(0,diff))
                    term.2 <- c(model.rows[,highest.second.term],rep(1,diff))
                } else {
                    term.1 <- c(model.rows[,first.term],rep(0,diff))
                    term.2 <- c(model.rows[,second.term],rep(1,diff))
                }
                
                result <- term.1 + term.2*lower.ngram.prob
            }
        }
        return(result)
    }
    
    preds <- compute.Ngram.prob(highest.ngram,sentence)
    prep.preds <- sort(preds, decreasing=T)[1:preds.num]
    return(prep.preds)
}