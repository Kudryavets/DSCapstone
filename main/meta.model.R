source("main/ngrams.R")
source("main/model.R")


#' meta.model.learn
#'
#' @param corpus : processed training text corpus as ch.vector
#' @param highest.ngram : highest level of models (int)
#' @param path : path to store models (string)
#' @param cores : number of cores to use for multiprocessing
#' @param valid : list of params for every model, every param is a treshold for NGrams.count
#'
#' @return names of builded models, assign it to DataTables
#' @export Ngrams.build.par, Ngrams.count.valid from ngrams.R, model.learn from model.R
#'
meta.model.learn <- function(corpus, highest.ngram, path=NULL, valid=list(
                            N4gram.model=0, N3gram.model=0, N2gram.model=0, N1gram.model=0), cores=8) {
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
        
        NGrams <- Ngrams.build.par(corpus, i, cores)
        NGrams.count <- Ngrams.count.valid(NGrams, valid[[name]])
        
        model.learn(NGrams.count, name, level)
        
        if (!is.null(path)) saveRDS(get(name), sprintf("%s%s.rds", path, name))
        names <- c(names,name)
    }
    return(names)
}


#' meta.model.load
#'
#' @param path : path to load models from (string)
#'
#' @return find models and load it
#'
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


#' meta.model.predict
#'
#' @param sentence : query for predicting string
#' @param names : names of models to use for predicting ch.vector
#' @param preds.num : number of predictings to show (int)
#' @param loging : loging parametr, if true prints statement of process
#'
#' @return named numeric vector with preds and its probabilities
#' @export model.get.row from model.R
#'
meta.model.predict <- function(sentence, names, preds.num, loging=FALSE) {
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
    
    compute.Ngram.prob <- function(ngram.rang, text, highest.ngram.bool=T) {
        if (loging) print(sprintf("Using %dgram model", ngram.rang))
        split.words = unlist(strsplit(text,' '))
        len <- length(split.words)
        offset = if (ngram.rang == 1) 0 else ngram.rang - 2
        Ngram <- if (len>1) paste(split.words[(len-offset):len], collapse=" ") else split.words
        
        model.name <- names[grepl(ngram.rang,names)]
        model.rows <- model.get.row(get(model.name), Ngram)
        
        if (nrow(model.rows)==0) {
            if (ngram.rang == 1) {
                if (loging) print("Using models for unknown words")
                s <- unlist(strsplit(sentence, " "))
                s <- paste(c(s[1:(length(s) - 1)], "<unk>"), collapse = " ")
                result <- compute.Ngram.prob(highest.ngram, s)
            } else {
                if (loging) print(sprintf("%dgram did't find, searching for %dgram", ngram.rang, ngram.rang-1))
                result <- compute.Ngram.prob(ngram.rang-1, Ngram)
            }
        } else {
            if (loging) print(sprintf("Using %dgram model", ngram.rang))
            lower.ngram.prob <- 
                if (ngram.rang == 2) {
                    model.name.1 <- names[grepl(ngram.rang-1,names)]
                    model.rows.1 <- model.get.row(get(model.name.1), model.rows[,last], ret.na=T)
                    if (loging) print("Using unigram probabilities")
                    result <- model.rows.1[,last.term]
                    result[is.na(result)] <- 0
                    names(result) <- model.rows.1[,first]
                    result
                } else {
                    compute.Ngram.prob(ngram.rang-1,text,highest.ngram.bool=F)
                }
            
            diff <- length(lower.ngram.prob)-nrow(model.rows)
            lower.ngram.prob <- sorting(model.rows[,last],lower.ngram.prob)
            
            if (highest.ngram.bool) {
                term.1 <- c(model.rows[,highest.first.term],rep(0,diff))
                term.2 <- c(model.rows[,highest.second.term],rep(1,diff))
            } else {
                term.1 <- c(model.rows[,first.term],rep(0,diff))
                term.2 <- c(model.rows[,second.term],rep(1,diff))
            }
            
            result <- term.1 + term.2*lower.ngram.prob
        }
        return(result)
    }
    
    preds <- compute.Ngram.prob(highest.ngram,sentence)
    if (loging) print("Sorting")
    prep.preds <- if (is.finite(preds.num)) sort(preds, decreasing=T)[1:preds.num] 
                    else sort(preds, decreasing=T)
    if (loging) print("Done!")
    return(prep.preds)
}


#' meta.model.evaluate.size
#'
#' @param model_names : models to evaluete (ch.vector)
#' @param path : path to where models stored (string)
#' @param unit : unit of size "MB" and etc (string)
#'
#' @return prints all-in-all size in rows, in memory usage and disk usage for these models
#'
meta.model.evaluate.size <- function(model_names, path, unit="MB") {
    print("# model size")
    files <- list.files(path, pattern = "N.gram", full.names = T)
    size.in.memory = 0
    size.in.nrows = 0
    for (name in model_names) {
        size.in.memory = size.in.memory + object.size(get(name))
        size.in.nrows = size.in.nrows + nrow(get(name))
    }
    size.on.disc = sum(file.info(files)$size)
    
    cat(sprintf("\nSize on disc in MB: %.2f", size.on.disc/1048576))
    cat(sprintf('\nSize in memmory in %s: ', unit), format(size.in.memory, units=unit))
    cat('\nSize in rows: ', size.in.nrows)
}


#' meta.model.evaluate.speed
#'
#' @param model_names : models to evaluate (ch.vector)
#' @param request : request to evaluete speed of @N.requests reqests string
#' @param N.requests : number of reqests to do int
#'
#' @return system.time needed to do @N.requests 
#'
meta.model.evaluate.speed <- function(model_names, request, N.requests=1000) {
    print("# requests time")
    print(system.time( for (i in 1:N.requests) {
        meta.model.predict(request, model_names, 1)
    }))
}


#' meta.model.evaluate.accuracy
#'
#' @param model_names : models to evaluate (ch.vector)
#' @param test.corpus : corpus for testing (processed ch.vector)
#' @param highest.ngram : highest.ngram in models (int)
#' @param N.preds : amount of predictions to consider (int)
#' @param penalty : penalty on accuracy to use when preds doesn't contain right answer (int)
#' @param cores : cores to use for multiprocessing
#'
#' @return prints 
#'              accuracy.precise - averaged number of cases where rigth answer is first prediction,
#'              accuracy.prob - average probability of right answer
#'              accuracy.order - average order number of rigth answer
#'
#' @export Ngrams.build.par from ngrams.R
#'
meta.model.evaluate.accuracy <- function(model_names, test.corpus, highest.ngram=3, 
                                         N.preds=5, penalty=1, cores=8) {
    for (i in 1:highest.ngram+1) {
        ngrams.split <- strsplit(Ngrams.build.par(test.corpus, i, cores),' ')
        queries <- sapply(
            lapply(ngrams.split,'[',-i), 
            function(x) paste(x, collapse=' ')
        )
        preds.verif <- sapply(ngrams.split,'[',i)
        
        accuracy.precise.acc <- 0
        accuracy.prob.acc <- 0
        accuracy.order.acc <- 0
        scope <- length(queries)
        for (j in 1:scope) {
            pred = meta.model.predict(queries[j], model_names, N.preds)
            verif = preds.verif[j]
            accuracy.precise.acc <- accuracy.precise.acc + (names(pred[1]) == verif)
            if (is.na(pred[verif])) {
                accuracy.order.acc <- accuracy.order.acc + N.preds + penalty
            } else {
                accuracy.prob.acc <- accuracy.prob.acc + pred[[verif]]
                accuracy.order.acc <- accuracy.order.acc + which(names(pred) == verif)
            } 
        }
        print(sprintf("%d accuracy.precise: %.6f", i-1, accuracy.precise.acc/scope))
        print(sprintf("%d accuracy.prob:  %.6f", i-1, accuracy.prob.acc/scope))
        print(sprintf("%d accuracy.order:  %.2f", i-1, accuracy.order.acc/scope))
    } 
}


#' meta.model.evaluate.perplexity
#'
#' @param model_names : models to evaluate (ch.vector)
#' @param test.corpus : corpus for testing (processed ch.vector)
#' @param penalty : penalty on accuracy to use when preds doesn't contain right answer (int)
#' @param cores : cores to use for multiprocessing
#'
#' @return prints perplexity per word in test corpus
#'
meta.model.evaluate.perplexity <- function(model_names, test.corpus, penalty = 10^-5, cores=8) {
    perplexity.acc <- 0
    scope <- length(test.corpus)
    
    for (sentence in test.corpus) {
        sent.perplexity.acc <- 1
        sentence.split <- unlist(strsplit(sentence,' '))
        sentence.scope <- length(sentence.split)
        for (i in 2:sentence.scope) {
            preds.verif <- sentence.split[[i]]
            query <- paste(sentence.split[1:(i-1)], sep=" ")
            preds <- meta.model.predict(query, model_names, Inf)
            prob <- if (is.na(preds[preds.verif])) penalty else preds[[preds.verif]]
            sent.perplexity.acc <- sent.perplexity.acc * prob
        }
        perplexity.acc <- perplexity.acc + sent.perplexity.acc^(-1/(sentence.scope-1))
    }
    print(sprintf("perplexity per word %.2f", perplexity.acc/scope))
}
