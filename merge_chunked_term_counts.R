# assemble chunked term counts into one master term count document
# use that document for predicting next words in ngrams
library(readr)
suppressMessages(library(data.table))
library(magrittr)

if(getwd() != "~/Documents/code/capstone/") setwd("~/Documents/code/capstone/")
# for the sake of my ppor laptop, only take the top x terms
max_terms <- 2000000
files <- dir("./data/rds_noend/")
pb3 <- txtProgressBar(min = 0, max = length(files), style = 3)
for(i in 2:10){
    fn <- paste0("./data/rds_noend/", files[i])
    if(exists("merged") == FALSE) merged <- read_rds(paste0("./data/rds_noend/", files[1]))
    to_be_merged <- read_rds(fn)
    merged <- merge(merged[,list(term,weight)], to_be_merged[,list(term,weight)], by = c("term"), all = TRUE)
    merged[is.na(weight.x) == TRUE, weight.x := 0]
    merged[is.na(weight.y) == TRUE, weight.y := 0]
    merged[, weight := (weight.x + weight.y)]
    merged[, c("weight.x", "weight.y") := NULL]
    setorder(merged, -weight, term)
    if(nrow(merged) > max_terms) merged <- merged[1:max_terms, ]
    setTxtProgressBar(pb3, value = i-1)
}
merged <- merged[is.na(term) == FALSE, ]
setnames(merged, old = names(merged), new = c("term", "count"))
merged[, c('t1', 't2', 't3', 't4') := tstrsplit(term, " ")]
setTxtProgressBar(pb3, value = length(files))
write_rds(merged, "./data/merged_term_counts.rds")
close(pb3)
