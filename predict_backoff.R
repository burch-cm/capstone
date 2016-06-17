# prep -----
library(data.table)
wd <- "~/Documents/code/capstone"
if(getwd() != wd) setwd(wd)
# clear all -----
rm(list=ls())
# load data -----
dt1 <- fread("./data/merged_table.csv")

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
    w.ntl <- ifelse(k >= 2, ng[k-1], NA) # penultimate, if it exists
    setorder(dt, -weight.x1) # make sure dt is in order
    # if ntl and last, then look for t1 = ntl and t2 = last
    if(w.ntl %in% dt[, t1] & w.last %in% dt[, t2]){
        y <- dt[t1 == w.ntl & t2 == w.last, t3][1:n]
    } else if(w.ntl %in% dt[, t1] & !w.last %in% dt[, t2]){
        y <- dt[t1 == w.ntl, t2][1:n]
    } else if(!w.ntl %in% dt[, t1] & w.last %in% dt[, t2]){
        y <- dt[t1 == w.last, t2][1:n]
    } else {
        y <- dt[, t1][1:n]
    }
    if(all(is.na(y))) y <- c("Result was NA")
    return(y)
}