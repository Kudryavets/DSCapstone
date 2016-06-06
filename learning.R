setwd("./Tasks/DSCapstone/TSafer/")
source("main/text.processing.R")
source("main/meta.model.R")

profanity.vec <- readRDS('main/sources/profanity.rds')

# loading # processing # sampling
sampl.conf = list(blogs = list(sample.size=0.5, seed=15),
                  news = list(sample.size=0.4, seed=26),
                  twitter = list(sample.size=0.6,  seed=30))

for (src in c("blogs", "news", "twitter")){
    print(sprintf("%s: loading and processing", src))
    assign(src, process.text.par(
        readRDS(sprintf("../dataRDS/%s.rds", src)),
        profanity.vec, 8))
    # assign(src, readRDS(sprintf("../dataRDS/%s.processed.rds", src)))
    set.seed(sampl.conf[[src]][["seed"]])
    train.ind <- sample(seq_len(length(get(src))), size = sampl.conf[[src]][["sample.size"]]*length(get(src)))
    print("spliting")
    assign(sprintf("%s.train", src), get(src)[train.ind])
    assign(sprintf("%s.test", src), get(src)[-train.ind])
    print("saving")
    saveRDS(file = sprintf("../dataRDS/%s.processed.rds", src), object = get(src))
}

train_corpus <- c(blogs.train, news.train, twitter.train)
rm(blogs, news, twitter, blogs.train, news.train, twitter.train)
gc()

# compute.rare.vocabulary(train_corpus, 2, "tmp/rr_vcb.rds")
# rr.vcb <- readRDS("tmp/rr_vcb.rds")
# set.seed(20)
# rr.vcb <- sample(rr.vcb,500)
# train_corpus_rr <- process.rare.par(train_corpus, rr.vcb)
# saveRDS(file = "tmp/train_corpus_rr.rds", object = train_corpus_rr)
train_corpus_rr <- readRDS("tmp/train_corpus_rr.rds")

# learning
# config = list(N4gram.model=1, N3gram.model=1, N2gram.model=1, N1gram.model=1)
# names <- meta.model.learn(train_corpus_rr, 4,"main/sources/", config)
names <- meta.model.load("main/sources/")

# preparing test corpus
set.seed(5)
bl.tst.smpl <- sample(blogs.test, 10)
set.seed(7)
nw.tst.smpl <- sample(news.test, 10)
set.seed(10)
tw.tst.smpl <- sample(twitter.test, 10)

test.corpus <- c(bl.tst.smpl, nw.tst.smpl, tw.tst.smpl)

#evaluation
meta.model.evaluate.size(names, "main/sources/", 'MB')
meta.model.evaluate.speed(names, "hi cute little", 10)/10
meta.model.evaluate.accuracy(names, test.corpus, highest.ngram=4, penalty=1, cores=1)
meta.model.evaluate.perplexity(names, test.corpus, cores=1)






