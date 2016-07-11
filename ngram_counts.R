# make ngram proportions / probs (count-based)
library(magrittr)
library(readr)
suppressMessages(library(data.table))

start_time <- Sys.time()
dt <- read_rds("./data/merged_term_counts.rds")

# set up txt progress bar
mypb <- txtProgressBar(min = 0, max = 4, style = 3)

# count for each word will be unsmoothed count across tetragrams
words.1 <- dt[,list(t1, count)] %>% setnames(., old = names(.), new = c("word", "count"))
words.1 <- rbind(words.1, (dt[,list(t2, count)] %>% setnames(., old = names(.), new = c("word", "count"))))
words.1 <- rbind(words.1, (dt[,list(t3, count)] %>% setnames(., old = names(.), new = c("word", "count"))))
words.1 <- rbind(words.1, (dt[,list(t4, count)] %>% setnames(., old = names(.), new = c("word", "count"))))
unigram <- words.1[, (.N * sum(as.numeric(count))), by = word]
rm(words.1)
setnames(unigram, old = names(unigram), new = c("term1", "count"))
unigram[, prop := count/sum(as.numeric(count))]
write_rds(unigram, "./data/unigram_count.rds")
rm(unigram)
setTxtProgressBar(mypb, 1)

words.2 <- dt[, list(t1, t2, count)] %>% setnames(., old = names(.), new = c("term1", "term2", "count"))
words.2 <- rbind(words.2, (dt[,list(t2, t3, count)] %>% setnames(., old = names(.), new = c("term1", "term2", "count"))))
words.2 <- rbind(words.2, (dt[,list(t3, t4, count)] %>% setnames(., old = names(.), new = c("term1", "term2", "count"))))
bigram <- words.2[, (.N * sum(as.numeric(count))), by = list(term1, term2)]
rm(words.2)
setnames(bigram, old = names(bigram), new = c("term1", "term2", "count"))
bigram[, prop := count/sum(as.numeric(count))]
write_rds(bigram, "./data/bigram_count.rds")
rm(bigram)
setTxtProgressBar(mypb, 2)

words.3 <- dt[, list(t1, t2, t3, count)] %>% setnames(., old = names(.), new = c("term1", "term2", "term3", "count"))
words.3 <- rbind(words.3, (dt[,list(t2, t3, t4, count)] %>% setnames(., old = names(.), new = c("term1", "term2", "term3", "count"))))
trigram <- words.3[, (.N * sum(as.numeric(count))), by = list(term1, term2, term3)]
rm(words.3)
setnames(trigram, old = names(trigram), new = c("term1", "term2", "term3", "count"))
trigram[, prop := count/sum(as.numeric(count))]
write_rds(trigram, "./data/trigram_count.rds")
rm(trigram)
setTxtProgressBar(mypb, 3)

words.4 <- dt[, list(t1, t2, t3, t4, count)] %>% setnames(., old = names(.), new = c("term1", "term2", "term3", "term4", "count"))
tetragram <- words.4[, (.N * sum(as.numeric(count))), by = list(term1, term2, term3, term4)]
rm(words.4)
setnames(tetragram, old = names(tetragram), new = c("term1", "term2", "term3", "term4", "count"))
tetragram[, prop := count/sum(as.numeric(count))]
write_rds(tetragram, "./data/tetragram_count.rds")
rm(tetragram)
setTxtProgressBar(mypb, 4)

close(mypb)
end_time <- Sys.time()
print(difftime(end_time, start_time))
