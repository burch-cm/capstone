library(data.table)
library(igraph)
library(ngram)
library(magrittr)
library(tm)

wd <- "~/Documents/code/capstone"
if(getwd() != wd) setwd(wd)

doc <- fread("./data/sampledata_10.csv")

doc[, text := tolower(text)]
doc[, text := removePunctuation(text)]
doc[, text := removeNumbers(text)]

n2 <- doc[, ngram_asweka(text, min = 2, max = 2)]
n2 <- data.table(term = n2)
weights <- n2[, .N, by = term]
setnames(weights, old = names(weights), new = c("term", "weight"))
n2[, c('t1', 't2') := tstrsplit(term, " ")]
n2 <- unique(n2)
setkey(n2, term)
setkey(weights, term)
# merge unique terms and weights
terms <- merge(weights, n2, all.x = TRUE)
# edgelst
edgelist <- terms[, list(t1, t2, weight)]
rm(terms, n2, weights)
# create graph from weighted edgelist
g <- graph.data.frame(edgelist, directed = TRUE)

plot(g, edge.arrow.size = .1, 
     vertex.color = adjustcolor("SkyBlue2", alpha.f = 0.5),
     vertex.size = 2, vertex.frame.color = "gray", 
     vertex.label.color = "black", vertex.label.cex = 0.7, 
     vertex.label.dist = .1, asp = 0,
     layout = layout_nicely,
     edge.width = (E(g)$weight*.2)+.2)

