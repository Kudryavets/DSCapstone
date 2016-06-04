setwd("./Tasks/DSCapstone/")
source("main/text.processing.R")
source("main/meta.model.R")

profanity.vec <- readRDS('main/sources/profanity.rds')

# loading # processing # sampling
sampl.conf = list(blogs = list(sample.size=0.5 * length(blogs), seed=15),
                  news = list(sample.size=0.4 * length(news), seed=26),
                  twitter = list(sample.size=0.6 * length(twitter),  seed=30))

for (src in c("blogs", "news", "twitter")){
    assign(src, process.text.par(
        readRDS(sprintf("../dataRDS/%s.rds", src)), 
        profanity.vec, 8))
    set.seed(sampl.conf[src][["sample.size"]])
    train.ind <- sample(seq_len(length(get(src))), size = sampl.conf[src][["seed"]])
    assign(sprintf("%s.train", src), get(src)[train.ind, ])
    assign(sprintf("%s.tesr", src), get(src)[-train.ind, ])
}

train_corpus <- c(blogs.train, news.train, twitter.train)

# learning
config = list(N4gram.model=1, N3gram.model=2, N2gram.model=3, N1gram.model=4)
names <- meta.model.learn(train_corpus, 4,"./TSafer/main/sources/", config)

# preparing test corpus
set.seed(5)
bl.tst.smpl <- sample(bl.test, 1000)
set.seed(7)
nw.tst.smpl <- sample(nw.test, 1000)
set.seed(10)
tw.tst.smpl <- sample(tw.test, 1000)

test.corpus <- c(bl.tst.smpl, nw.tst.smpl, tw.tst.smpl)

#evaluation
meta.model.evaluate.size(names, "train/sources/", 'MB')
meta.model.evaluate.speed(names, "hi cute little")
meta.model.evaluate.accuracy(names, test.corpus, highest.ngram=3, penalty=1, cores=1)
meta.model.evaluate.perplexity(names, test.corpus, cores=1)

