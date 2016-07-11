library(readr)
suppressMessages(library(data.table))
library(qdap)
library(ngram)

# use ngram counts and proportions
n1 <- read_rds("./data/unigram_count.rds")
n2 <- read_rds("./data/bigram_count.rds")
n3 <- read_rds("./data/trigram_count.rds")
n4 <- read_rds("./data/tetragram_count.rds")

phrase <- "today is going to be a"

get.nw <- function(phrase, n = 1){
    require(data.table)
    get.ng <- function(phrase){
        require(qdap)
        require(ngram)
        phrase <- tolower(phrase)
        phrase <- gsub("[[:punct:]]", "", phrase)
        phrase <- gsub("[0-9]", "", phrase)
        phrase <- Trim(clean(phrase))
        ng <- ngram_asweka(phrase, min = 1, max = 1) # get ngrams
        return(ng)
    }
    ng <- get.ng(phrase)
    k <- length(ng)
    if(k < 1) return("Phrase length of less than one.")
    w.last <- ng[k] # last word in phrase
    w.ntl <- ifelse(k >= 2, ng[k-1], NA) # next-to-last word
    w.2tl <- ifelse(k >= 3, ng[k-2], NA) # second-to-last word
    if(k >= 4){
        return(n4[term1 == w.2tl & term2 == w.ntl & term3 == w.last, term4][1:n])
    } else if(k == 3){
        return(n3[term1 == w.ntl & term3 == w.last, term3][1:n])
    } else if(k == 2){
        return(n2[term1 == w.last, term2][1:n])
    } else {
        return(n1[1:n, term1])
    }
}
