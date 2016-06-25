glove_predict <- function(phrase, m_source, n = 10){
    require(text2vec)
    require(magrittr)
    require(tm)
    cos_dist <- function(m_query, m_source){
        m_source_norm = sqrt(rowSums(m_source^2))
        m_query_norm = sqrt(rowSums(m_query^2))
        tcrossprod(m_query, m_source)/outer(m_query_norm, m_source_norm)
    }
    tokens <- phrase %>% 
        tolower() %>%
        removeNumbers() %>%
        removePunctuation() %>%
        gsub("^\\s+|\\s+$", "", .) %>%
        removeWords(stopwords('english')) %>%
        word_tokenizer() %>%
        unlist()
    m_query <- sapply(tokens, FUN = function(x) m_source[x,,drop=FALSE]) %>% rowSums
    cos_dist(m_query, wv)[1,] %>% sort(decreasing = T) %>% head(10)
}
