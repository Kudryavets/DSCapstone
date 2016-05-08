require(RWeka)
require(doParallel)

Ngrams.build <- function(text, N) NGramTokenizer(text, Weka_control(min = N, max = N))

Ngrams.build.par <- function(text, N, cores) {
    
    parts = ceiling(length(text)/200000)
    scope = if (parts>cores) parts else cores
 
    cl <- makeCluster(cores)
    registerDoParallel(cl)

    tokens <- foreach (i = 1:scope, .combine=c, .export= c('NGramTokenizer','Weka_control')) %dopar% {
        start <- floor((i-1)*length(text)/scope) + 1
        end <- floor(i*length(text)/scope)
        NGramTokenizer(text[start:end], Weka_control(min = N, max = N))
    }

    stopCluster(cl)

    return(tokens)
}

Ngrams.count.valid <- function(Ngrams, sparcity) {
    
    Ngrams.count <- table(Ngrams)
    valid.Ngrams.count <- Ngrams.count[(Ngrams.count > sparcity)]
    print(sprintf("Reduced %d from %d", length(Ngrams) - length(valid.Ngrams.count), length(Ngrams)))
    return(valid.Ngrams.count)
}