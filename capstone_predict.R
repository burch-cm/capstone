wd <- "~/Documents/code/capstone"
if(getwd() != wd) setwd(wd)
library(data.table)
# prediction function -----
markov_predict <- function(phrase = "", dt, n = 1, remove_sw = FALSE){
    require(data.table)
    require(tm)
    rsw <- remove_sw
    get.ng <- function(phrase, remove_sw = rsw){
        require(tm)
        require(ngram)
        phrase <- gsub("[^a-zA-z]", " ", phrase) # remove non-alpha
        phrase <- stripWhitespace(phrase) # remove interword whitespace
        phrase <- gsub("^\\s+|\\s+$", "", phrase) # leading and trailing whitespace
        if(remove_sw == TRUE) phrase <- removeWords(phrase, words = stopwords('english'))
        ng <- ngram_asweka(phrase, min = 1, max = 1) # get ngrams
        return(ng)
    }
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

glove_predict <- function(phrase, dt_source, n = 10){
    require(text2vec)
    require(magrittr)
    require(tm)
    cos_dist <- function(m_query, dt_source){
        m_source <- as.matrix(dt_source[, c(2:length(dt_source)), with = FALSE])
        rownames(m_source) <- dt_source[, word]
        m_source_norm = sqrt(rowSums(m_source^2))
        m_query_norm = sqrt(rowSums(m_query^2))
        (m_query %*% t(m_source))/outer(m_query_norm, m_source_norm)
    }
    tokens <- phrase %>% 
        tolower() %>%
        removeNumbers() %>%
        removePunctuation() %>%
        gsub("^\\s+|\\s+$", "", .) %>%
        removeWords(stopwords('english')) %>%
        word_tokenizer() %>%
        unlist
    setnames(dt_source, old = 1, new = c('word'))
    m_query <- sapply(tokens, FUN = function(x) dt_source[word == x,])
    m_query <- m_query[2:nrow(m_query), ]
    if(length(tokens) > 1){
        m_query <- apply(m_query, 1, FUN = function(x) sum(unlist(x))) %>% as.matrix %>% t
    } else {
        m_query <- as.matrix(unlist(m_query)) %>% t
    }
    cos_dist(m_query, dt_source)[1,] %>% sort(decreasing = T) %>% head(n)
}
# load data -----
markov_dt <- fread("./data/merged_table_n4.csv")
glove_dt <- fread("./data/word_vectors.csv")
# -----

phrase <- "I do not like green eggs and"
markov_predict(phrase, markov_dt, n = 10)
glove_predict(phrase, glove_dt, n = 10)
