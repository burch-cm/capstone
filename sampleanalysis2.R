library(data.table)
library(ngram)
library(magrittr)
library(tm)

wd <- "~/Documents/code/capstone"
if(getwd() != wd) setwd(wd)

doc <- fread("./data/sampledata_10.csv")

doc[, text := tolower(text)]
doc[, text := removePunctuation(text)]
doc[, text := removeNumbers(text)]

n3 <- doc[, ngram_asweka(text, min = 1, max = 3)]
n3 <- data.table(term = n3)
weights <- n3[, .N, by = term]
setnames(weights, old = names(weights), new = c("term", "weight"))
n3[, c('t1', 't2', 't3') := tstrsplit(term, " ")]
n3 <- unique(n3)
setkey(n3, term)
setkey(weights, term)
# merge unique terms and weights
terms <- merge(weights, n3, all.x = TRUE)
# termlist
termlist <- terms[, list(t1, t2, weight)]
setorderv(termlist, c("t1", "weight"), c(1, -1))
rm(terms, n3, weights)