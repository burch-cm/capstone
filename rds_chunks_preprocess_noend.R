library(data.table)
library(ngram)
library(readr)
library(qdap)
library(magrittr)
# library(tm)

wd <- "~/Documents/code/capstone"
if(getwd() != wd) setwd(wd)
files <- dir("./data/chunks")
pb2 <- txtProgressBar(min = 0, max = length(files), style = 3)
for(i in 1:length(files)){
    mytext <- as.data.table(read_rds(paste0("./data/chunks/", files[i])))
    setnames(mytext, old = names(mytext), new = c("text"))
    mytext[, text := tolower(text)]
    mytext[, text := gsub("[[:punct:]]", "", text)]
    mytext[, text := gsub("[0-9]", "", text)]
    mytext[, text := paste(text, collapse = " ")]
    # mytext[, text := gsub("[?.!]+", " END", text)]
    mytext <- mytext[1,]
    setTxtProgressBar(pb2, value = i - .75)
    # mytext[, text := removeWords(text, words = stopwords('english'))]
    setTxtProgressBar(pb2, value = i - .5)
    mytext[, text := Trim(clean(text))]
    setTxtProgressBar(pb2, value = i - .25)
    n4 <- mytext[, ngram_asweka(text, min = 4, max = 4)]
    rm(mytext) # free up some space
    n4 <- data.table(term = n4)
    weights <- n4[, .N, by = term]
    setnames(weights, old = names(weights), new = c("term", "weight"))
    n4[, c('t1', 't2', 't3', 't4') := tstrsplit(term, " ")]
    setkey(n4, term)
    setkey(weights, term)
    n4 <- unique(n4)
    term_counts <- merge(n4, weights)
    setTxtProgressBar(pb2, value = i - .1)
    outfile <- paste("./data/rds_noend/preproc_", i, ".rds", sep = "")
    write_rds(term_counts, outfile)
    rm(n4, weights)
    setTxtProgressBar(pb2, value = i)
}

close(pb2)