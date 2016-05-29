<<<<<<< HEAD
source("global.R")

global.tests <- function() {
    corpus <- c("write on my <email>", "Ok <profanity> <EOS>")
    restored <- get.restored(corpus)
    restored.valid <- c("write on my <Some email?>", "Ok <Some f.cking profanity word?> <End of sentence>!!!")
    
    print(sprintf("get.restored status: %s", checkEquals(restored, restored.valid)))
=======
require(RUnit)
source("global.R")


global.tests <- function() {
    print(sprintf("get.restored status: %s", checkEquals(get.restored(c("<email>", "<link>")),
                                                         c("<Some email?>", "<Some ww.link.com?>"))))
>>>>>>> files deleted for deployment
}