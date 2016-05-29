setwd("./Tasks/DSCapstone/")
source("main/text.processing.R")
source("main/meta.model.R")

blogs <- readRDS("../dataRDS/blogs.rds")
profanity.vec <- readRDS('main/sources/profanity.rds')

text<- sample(blogs,2)
sample.processed <- process.text.par(sample,profanity.vec,8)

names <- meta.model.learn(sample.processed, 4,"./TSafer/main/sources/")
names <- meta.model.load("./TSafer/")

model.predict(N3gram.model,'make the')

