library(data.table)
library(ngram)
library(readr)
library(magrittr)
library(tm)

wd <- "~/Documents/code/capstone"
if(getwd() != wd) setwd(wd)
fn <- c("twit", "blog", "news")
f1 <- function(x) paste("./data/", x, ".csv", sep ="")
files <- sapply(fn, f1)

for(i in 1:length(files)){
    rows <- as.numeric(system(paste("wc -l < ", files[i], sep=""), TRUE))
    chunksize <- 2000 # how many lines to read in at once
    nchunks <- ceiling(rows/chunksize) # how many total iterations
    start <- 0
    pb1 <- txtProgressBar(min = 0, max = nchunks*length(files), style = 3)
    for(j in 1:nchunks){
        mytext <- read_csv(files[i], skip = start, n_max = chunksize)
        mytext <- paste(mytext, collapse = " ") # make into one string
        mytext <- gsub("[^a-zA-z]", " ", mytext) # remove non-alpha
        mytext <- stripWhitespace(mytext)
        mytext <- gsub("^\\s+|\\s+$", "", mytext) # leading and trailing whitespace
        n4 <- ngram_asweka(mytext, min = 4, max = 4)
        rm(mytext) # free up some space
        n4 <- data.table(term = n4)
        weights <- n4[, .N, by = term]
        setnames(weights, old = names(weights), new = c("term", "weight"))
        n4[, c('t1', 't2', 't3', 't4') := tstrsplit(term, " ")]
        setkey(n4, term)
        n4 <- unique(n4)
        setkey(weights, term)
        # merge unique terms and weights
        terms <- merge(weights, n4, all.x = TRUE)
        # termlist
        termlist <- terms[, list(t1, t2, t3, t4, weight)]
        setorderv(termlist, c("t1", "weight"), c(1, -1))
        rm(terms, n4, weights)
        # write termlist to file
        outname <- paste("./data/chunks/", fn[i], j, ".csv", sep ="")
        write.csv(termlist, outname)
        rm(termlist)
        start <- start + chunksize
        setTxtProgressBar(pb1, value = i*j)
    }
    print("Wrote chunks to ./data/chunks")
    close(pb1)
}

