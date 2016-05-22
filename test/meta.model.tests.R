require(RUnit)
source("main/meta.model.R")

meta.model.tests <- function(){
    
    corpus <- c("the only 4gram occurrence test12", 
                "two different occurrences of 4gram test31",
                "two different occurrencesss of 4gram test32",
                "two simillar occurrences of 4gram test4",
                "two simillar occurrences of 4gram test4",
                "one occurrence of 4gram test51",
                "several occurrence of 4gram test51",
                "several occurrence of 4gram test52",
                "no occurrences of 4gram test6",
                "3gram one occurrence test7",
                "of test8", "steven spielberg test9")
    
    names.learn <- meta.model.learn(corpus, 4,"test/sources/")
    names.load <- meta.model.load("test/sources/")
    names.test <- c("N1gram.model", "N2gram.model", "N3gram.model", "N4gram.model")
    
    print(sprintf("names.learn status: %s", checkEquals(names.learn, names.test)))
    print(sprintf("names.load status: %s", checkEquals(names.load, names.test)))
    
    stat = list()
    stat$N1gram.model.ncol <- 2
    stat$N1gram.model.nrow <- 20
    stat$N2gram.model.ncol <- 6
    stat$N2gram.model.nrow <- 19
    stat$N3gram.model.ncol <- 6
    stat$N3gram.model.nrow <- 15
    stat$N4gram.model.ncol <- 4
    stat$N4gram.model.nrow <- 18
    
    for (name in names.learn){
        print(sprintf("%s ncol status: %s", 
                      name, checkEquals(length(get(name)), stat[[sprintf("%s.ncol", name)]])))
        print(sprintf("%s nrow status: %s", 
                      name, checkEquals(nrow(get(name)), stat[[sprintf("%s.nrow", name)]])))
    }
    
    # Test.1 Given 5gram one occurance of 4gram
    test.1.meta.model.result <- c(0.62069936, 0.08397661, 0.02729045, NA, NA)
    names(test.1.meta.model.result) <- c("test12","of","test7",NA,NA)
    print(sprintf("Test.1 meta.model status: %s", checkEquals(
        meta.model.predict("split only 4gram occurrence", names.learn, 5)
        , test.1.meta.model.result, tolerance = .Machine$double.eps^0.4)))
    
    # Test.2 Given the existing 5gram one occurance of 4gram
    test.2.meta.model.result <- c(0.62069936, 0.08397661, 0.02729045, NA, NA)
    names(test.2.meta.model.result) <- c("test12","of","test7",NA,NA)
    print(sprintf("Test.2 meta.model status: %s", checkEquals(
        meta.model.predict("the only 4gram occurrence", names.learn, 5)
        , test.2.meta.model.result, tolerance = .Machine$double.eps^0.4)))

    # Test.3 One 4gram but 2 3grams
    test.3.meta.model.result <- c(0.62075986, 0.03921318, 0.03921318, 0.03921318, 0.03826334)
    names(test.3.meta.model.result) <- c("test32", "test31", "test4",  "test6",  "test51")
    print(sprintf("Test.3 meta.model status: %s", checkEquals(
        meta.model.predict("occurrencesss of 4gram", names.learn, 5) 
        , test.3.meta.model.result, tolerance = .Machine$double.eps^0.4)))

    # Test.4 Tree different occurances of 4gram
    test.4.meta.model.result <- c(0.41310087, 0.16310087, 0.16310087, 0.03826334, 0.03826334)
    names(test.4.meta.model.result) <- c("test4", "test31", "test6", "test51", "test52")
    print(sprintf("Test.4 meta.model status: %s", checkEquals(
        meta.model.predict("occurrences of 4gram", names.learn, 5) 
        , test.4.meta.model.result, tolerance = .Machine$double.eps^0.4)))

    # Test.5 No ocurrances of 4gram, but several of 3gram
    test.5.meta.model.result <- c(0.20386762, 0.20386762, 0.07886762, 0.07886762, 0.07886762)
    names(test.5.meta.model.result) <- c("test51", "test4",  "test52", "test31", "test6")
    print(sprintf("Test.5 meta.model status: %s", checkEquals(
        meta.model.predict("peace of 4gram", names.learn, 5)
        , test.5.meta.model.result, tolerance = .Machine$double.eps^0.4)))
    
    # Test.6 Given 3gram one occurance
    test.6.meta.model.result <- c(0.55785929, 0.08397661, 0.02627680, NA, NA)
    names(test.6.meta.model.result) <- c("test7", "of", "test12", NA, NA)
    print(sprintf("Test.6 meta.model status: %s", checkEquals(
        meta.model.predict("one occurrence", names.learn, 5) 
        , test.6.meta.model.result, tolerance = .Machine$double.eps^0.4)))
    
    # Test.7 Given 3 gram one occurance of 2gram find
    test.7.meta.model.result <- c(0.4992593, NA, NA, NA, NA)
    names(test.7.meta.model.result) <- c("test9", NA, NA, NA, NA)
    print(sprintf("Test.7 meta.model status: %s", checkEquals(
        meta.model.predict("steven allan spielberg", names.learn, 5) 
        , test.7.meta.model.result, tolerance = .Machine$double.eps^0.4)))
    
    # Test.8 Given Uni gram should work
    test.8.meta.model.result <- c(0.4992593, NA, NA, NA, NA)
    names(test.8.meta.model.result) <- c("test9", NA, NA, NA, NA)
    print(sprintf("Test.8 meta.model status: %s", checkEquals(
        meta.model.predict("spielberg", names.learn, 5) 
        , test.8.meta.model.result, tolerance = .Machine$double.eps^0.4)))
    
    # Test.9 No ocurrances of any Ngram at all finded
    
    cat ("\n\nmeta.model.evaluate.size should print:\n\nSize in mb:  15 Kb\nSize in rows:  72\n\n")
    meta.model.evaluate.size(names.learn, 'KB')
    
    cat ("\n\nmeta.model.evaluate.speed should print:\n\nuser  system elapsed\n4.140   0.000   4.038\n\n")
    meta.model.evaluate.speed(names.learn, "two different")
    
    cat ("\n\nmeta.model.evaluate.accuracy shold print:\n\n\t1 accuracy.precise: 0.666667
        1 accuracy.prob:  0.619472\n\t1 accuracy.order:  2.33
        2 accuracy.precise: 0.500000\n\t2 accuracy.prob:  0.470430
        2 accuracy.order:  3.00\n\t3 accuracy.precise: 0.000000
        3 accuracy.prob:  0.081550\n\t3 accuracy.order:  4.00\n\n")
    meta.model.evaluate.accuracy(names.learn,
                                 c("occurrences of 4gram test31", "occurrences of 4gram testevaluation"),
                                 highest.ngram=3, penalty=1, cores=1)

    cat ("\n\nmeta.model.evaluate.perplexity should print:\n\nperplexity per word 26.02\n\n")
    meta.model.evaluate.perplexity(names.learn,
                                   c("occurrences of 4gram test31", "occurrences of 4gram testevaluation"),
                                   cores=1)
    
    rm(N1gram.model, N2gram.model, N3gram.model, N4gram.model, pos = ".GlobalEnv")
}
