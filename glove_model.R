library(data.table)
library(readr)
library(magrittr)
library(tm)
library(text2vec)

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
        ## text preprocessing
        mytext <- read_csv(files[i], skip = start, n_max = chunksize)
        names(mytext) <- c("id", "text")
        mytext$text <- gsub("[^a-zA-z]", " ", mytext$text) # remove non-alpha
        mytext$text <- stripWhitespace(mytext$text)
        mytext$text <- gsub("^\\s+|\\s+$", "", mytext$text) # leading and trailing whitespace
        
        ## tokenize
        tokens <- mytext$text
        it <- 
        
        start <- start + chunksize
        setTxtProgressBar(pb1, value = i*j)
    }
    print("Wrote chunks to ./data/chunks")
    close(pb1)
}
