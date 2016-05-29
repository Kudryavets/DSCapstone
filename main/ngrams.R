require(RWeka)

source("main/multiprocessing.R")

Ngrams.build <- function(text, N) NGramTokenizer(text, Weka_control(min = N, max = N))

Ngrams.build.par <- function(text, N, cores=8) {
    apply.func.par(text, 200000, NGramTokenizer, Weka_control(min = N, max = N), c('NGramTokenizer','Weka_control'), cores)
}

Ngrams.count.valid <- function(Ngrams, sparcity) {
    
    Ngrams.count <- table(Ngrams)
    valid.Ngrams.count <- Ngrams.count[(Ngrams.count > sparcity)]
    print(sprintf("Reduced %d from %d", length(Ngrams) - length(valid.Ngrams.count), length(Ngrams)))
    return(valid.Ngrams.count)
}