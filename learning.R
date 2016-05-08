setwd("./Tasks/DSCapstone/")
source("project/main/text.processing.R")
source("project/main/meta.model.R")

blogs <- readRDS("./data/raw_hashed/blogs.rds")
profanity.vec <- readRDS('project/main/sources/profanity.rds')

sample <- sample(blogs,1)
sample.processed <- process.text.par(sample,profanity.vec,8)

names <- meta.model.learn(sample.processed,4,"./project/main/sources/")
names <- meta.model.load("./project/")




model.predict(N3gram.model,'make the')

sample.Ngrams.table[0:10]
