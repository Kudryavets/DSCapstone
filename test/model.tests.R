require(RUnit)
source("main/model.R")

model.tests <- function(){
    
    Ngrams.count2 <- c(1,2,2,1)
    names(Ngrams.count2) <- c("foo bar", "pluch bar", "foo pluch", "<BOS> <EOS>")
    
    model.learn(Ngrams.count2, 'Uni.gram')
    Uni.gram.test <- readRDS("test/sources/model.test.N1.gram.rds")
    print(sprintf("UniGram.model status: %s", checkEquals(Uni.gram, Uni.gram.test)))
    
    Ngrams.count3 <- c(1,2,1,1,1,1,1)
    names(Ngrams.count3) <- c("pluch foo bar", "free foo bar", "cement foo bro", "stels foo bro",
                              "submit drunk elephant", "submit drunk vector", "deploy in prod")
    
    model.learn(Ngrams.count3, 'N2.gram', level='higher')
    N2.gram.test <- readRDS("test/sources/model.test.N2.gram.rds")
    print(sprintf("N2.gram.model status: %s", checkEquals(N2.gram, N2.gram.test)))
    
    model.learn(Ngrams.count3, 'N3.gram', level='highest')
    N3.gram.test <- readRDS("test/sources/model.test.N3.gram.rds")
    print(sprintf("N3.gram.model status: %s", checkEquals(N3.gram, N3.gram.test)))
    
    get.row.test.model <- data.table(first=c("cement foo", "free foo", "submit drunk"),
                                     last=c("bro","bar","elephant"),
                                     highest.first.term=c(0.4,0.7,0.2),
                                     highest.second.term=c(0.6,0.3,0.6))
    get.row.test <- model.get.row(get.row.test.model, 'free foo')
    get.row.test.result <- data.table(first=c("free foo"),
                                      last=c("bar"),
                                      highest.first.term=c(0.7),
                                      highest.second.term=c(0.3))
    print(sprintf("Get.row status: %s", checkEquals(get.row.test, get.row.test.result)))
    
    rm(Uni.gram, N2.gram, N3.gram, pos = ".GlobalEnv")
}