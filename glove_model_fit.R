library(text2vec)
library(readr)
library(tm)

text1 <- read_lines("./final/en_US/twitter_sample.txt")
text2 <- read_lines("./final/en_US/blog_sample.txt")
text3 <- read_lines("./final/en_US/news_sample.txt")
textf <- c(text1, text2, text3)
rm(text1, text2, text3)

tokens <- textf %>% 
    tolower() %>%
    removeNumbers() %>%
    removePunctuation() %>%
    stripWhitespace() %>%
    # regexp_tokenizer(pattern = stringr::fixed(" "))
    word_tokenizer()

## glove
vocab.g <- create_vocabulary(itoken(tokens))
vocab.g <- prune_vocabulary(vocab.g, term_count_min = 5L)
it <- itoken(tokens)
vectorizer <- vocab_vectorizer(vocab.g,
                               grow_dtm = FALSE,
                               skip_grams_window = 5L)
tcm <- create_tcm(it, vectorizer)

## remove some stuff
rm(tokens, textf, vocab.g)

## fit the glove
fit <- glove(tcm = tcm,
             word_vectors_size = 100,
             x_max = 10,
             num_iters = 15)

word_vectors <- fit$word_vectors[[1]] + fit$word_vectors[[2]]
rownames(word_vectors) <- rownames(tcm)
word_vectors_norm <- sqrt(rowSums(word_vectors ^ 2))
rm(fit, it, tcm, vectorizer)

write.table(word_vectors, "./data/word_vectors.txt")

library(Rtsne)
rtsne_out <- Rtsne(word_vectors)

plot(rtsne_out$Y, t='n', main="Word Vectors from Sample")
text(rtsne_out$Y, labels=rownames(word_vectors))

df <- data.frame(word = rownames(word_vectors),
                 x = rtsne_out$Y[,1],
                 y = rtsne_out$Y[,2])
library(ggplot2)
g <- ggplot(df, aes(x = x, y = y))
svg("word_sample_vector_plot.svg", width = 20, height = 20)
g + geom_point(size = 1, alpha = .5) +
    geom_text(aes(label = word), size = 2) + coord_equal()
dev.off()

