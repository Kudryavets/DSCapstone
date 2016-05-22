require(RUnit)
source("main/ngrams.R")

ngrams.tests <- function() {
    
    test.corpus <- c("<BOS> dayton oh august <othdigit> <link> republican john mccain has picked first term alaska <EOS>",
                     "<BOS> pics to come <EOS>")
    
    Ngrams.build.test2 <- Ngrams.build(test.corpus,2)
    Ngrams.build.test2.par <- Ngrams.build.par(test.corpus,2,2)
    Ngrms.build.result2 <- c("<BOS> dayton","dayton oh","oh august","august <othdigit>","<othdigit> <link>",
                       "<link> republican","republican john","john mccain","mccain has","has picked",
                       "picked first","first term","term alaska","alaska <EOS>","<BOS> pics","pics to",
                       "to come","come <EOS>")
    print(sprintf("Ngrams.build - 2 status: %s", checkEquals(Ngrams.build.test2,Ngrms.build.result2)))
    print(sprintf("Ngrams.build.par - 2 status: %s", checkEquals(Ngrams.build.test2.par,Ngrms.build.result2)))
    
    Ngrams.build.test3 <- Ngrams.build(test.corpus,3)
    Ngrams.build.test3.par <- Ngrams.build.par(test.corpus,3,2)
    Ngrms.build.result3 <- c("<BOS> dayton oh","dayton oh august","oh august <othdigit>","august <othdigit> <link>",
                       "<othdigit> <link> republican","<link> republican john","republican john mccain",
                       "john mccain has","mccain has picked","has picked first","picked first term",
                       "first term alaska","term alaska <EOS>","<BOS> pics to","pics to come","to come <EOS>")
    print(sprintf("Ngrams.build - 3 status: %s", checkEquals(Ngrams.build.test3,Ngrms.build.result3)))
    print(sprintf("Ngrams.build.par - 3 status: %s", checkEquals(Ngrams.build.test3.par,Ngrms.build.result3)))
    
    Ngrams.build.test4 <- Ngrams.build(test.corpus,4)
    Ngrams.build.test4.par <- Ngrams.build.par(test.corpus,4,2)
    Ngrms.build.result4 <- c("<BOS> dayton oh august","dayton oh august <othdigit>","oh august <othdigit> <link>",
                       "august <othdigit> <link> republican","<othdigit> <link> republican john",
                       "<link> republican john mccain","republican john mccain has","john mccain has picked",
                       "mccain has picked first","has picked first term","picked first term alaska",
                       "first term alaska <EOS>","<BOS> pics to come","pics to come <EOS>")
    print(sprintf("Ngrams.build - 4 status: %s", checkEquals(Ngrams.build.test4,Ngrms.build.result4)))
    print(sprintf("Ngrams.build.par - 4 status: %s", checkEquals(Ngrams.build.test4.par,Ngrms.build.result4)))
    
    Ngrams.build.test5 <- Ngrams.build(test.corpus,5)
    Ngrams.build.test5.par <- Ngrams.build.par(test.corpus,5,2)
    Ngrms.build.result5 <- c("<BOS> dayton oh august <othdigit>","dayton oh august <othdigit> <link>",
                       "oh august <othdigit> <link> republican","august <othdigit> <link> republican john",
                       "<othdigit> <link> republican john mccain","<link> republican john mccain has",
                       "republican john mccain has picked","john mccain has picked first",
                       "mccain has picked first term","has picked first term alaska",
                       "picked first term alaska <EOS>","<BOS> pics to come <EOS>")
    print(sprintf("Ngrams.build - 5 status: %s", checkEquals(Ngrams.build.test5,Ngrms.build.result5)))
    print(sprintf("Ngrams.build.par - 5 status: %s", checkEquals(Ngrams.build.test5.par,Ngrms.build.result5)))
    
    Ngrams.corpus <- c("pluch pluch", "pluch pluch", "foo foo", "bro bro", "bro bro", "bro bro")
    Ngrams.count.test <- Ngrams.count.valid(Ngrams.corpus, 1)
    Ngrams.count.result <- array(c(3,2))
    names.for.array <- list()
    names.for.array$Ngrams <- c("bro bro","pluch pluch")
    dimnames(Ngrams.count.result) <- names.for.array
    print(sprintf("Ngrams.count status: %s", checkEquals(Ngrams.count.test,Ngrams.count.result)))
}