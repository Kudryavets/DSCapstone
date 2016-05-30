require(RWeka)

source("main/multiprocessing.R")


#' Ngrams.build
#' @param text : text corpus ch.vector 
#' @param N : n in n-gram 
#' @return ch.vector of ngrams
#' 
Ngrams.build <- function(text, N) NGramTokenizer(text, Weka_control(min = N, max = N))


#' Ngrams.build.par
#' 
#' @param cores : core to use for multiprocessing
#' analog of Ngrams.build but work in parallel and
#' 
#' @export apply.func.par from multiprocessing.R
#' 
Ngrams.build.par <- function(text, N, cores=8) {
    apply.func.par(text, 200000, NGramTokenizer, Weka_control(min = N, max = N), c('NGramTokenizer','Weka_control'), cores)
}


#' Ngrams.count.valid
#'
#' @param Ngrams : ch.vector of ngrams 
#' @param sparcity : treshold of sparcity which sholudn't be considered (in counts) 
#'
#' @return counts ngrams table as named numreic.vector
#' 
Ngrams.count.valid <- function(Ngrams, sparcity) {
    Ngrams.count <- table(Ngrams)
    valid.Ngrams.count <- Ngrams.count[(Ngrams.count > sparcity)]
    print(sprintf("Reduced %d from %d", length(Ngrams) - length(valid.Ngrams.count), length(Ngrams)))
    return(valid.Ngrams.count)
}