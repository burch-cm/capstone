# prep -----
library(data.table)
wd <- "~/Documents/code/capstone"
if(getwd() != wd) setwd(wd)
# clear all -----
rm(list=ls())
# load data -----
dt1 <- fread("./data/merged_table_n4.csv")

get.ng <- function(phrase){
    require(tm)
    require(ngram)
    phrase <- gsub("[^a-zA-z]", " ", phrase) # remove non-alpha
    phrase <- stripWhitespace(phrase) # remove interword whitespace
    phrase <- gsub("^\\s+|\\s+$", "", phrase) # leading and trailing whitespace
    ng <- ngram_asweka(phrase, min = 1, max = 1) # get ngrams
    return(ng)
}

predict.next <- function(phrase = "", n = 1, dt = dt1){
    require(data.table)
    require(magrittr)
    if(phrase == "") return("Phrase empty.")
    ng <- get.ng(phrase)
    k <- length(ng)
    if(k < 1) return("Phrase length of less than one.")
    w.last <- ng[k] # last word in phrase
    w.ntl <- ifelse(k >= 2, ng[k-1], NA) # next-to-last word
    w.2tl <- ifelse(k >= 3, ng[k-2], NA) # second-to-last word
    setorder(dt, -weight.x1) # make sure dt is in order
    # greedy
    y <- dt[t1 == w.2tl & t2 == w.ntl & t3 == w.last, t4]
    if(length(y[complete.cases(y)]) == 0){
        y <- dt[t1 == w.ntl & t2 == w.last, t3]
    }
    if(length(y[complete.cases(y)]) == 0){
        y <- dt[t1 == w.last, t2]
    }
    if(length(y[complete.cases(y)]) == 0){
        y <- dt[, t1]
    }
    return(unique(y)[1:n])
}