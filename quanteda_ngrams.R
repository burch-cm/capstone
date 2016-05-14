library(quanteda)
library(markovchain)
library(data.table)
library(magrittr)

wd <- "~/Documents/code/capstone/"
if(getwd() != wd) setwd(wd)

savedfm <- function(filename, chunklines = 10000, maxchunks = 0, todir = "./test/"){
    require(quanteda)
    require(data.table)
    filelines <- as.numeric(system(paste("wc -l < ", filename, sep=""), TRUE))
    chunks <- floor(filelines/chunklines)
    if(maxchunks > 0) chunks <- min(maxchunks, chunks)
    file.con <- file(filename, "r")
    pb <- txtProgressBar(0, chunks, style = 3)
    for(i in 1:chunks){
        x <- scan(file.con, what = "", 
                  sep = "\n", nlines = chunklines, 
                  skipNul = TRUE, quiet = TRUE)
        corp <- corpus(x)
        dfm_i <- dfm(corp,
                     ngrams = 1:3,
                     ignoredFeatures = stopwords("english"),
                     toLower = TRUE,
                     removeNumbers = TRUE,
                     removePunct = TRUE, 
                     removeSeparators = TRUE,
                     removeTwitter = TRUE,
                     language = "english",
                     verbose = FALSE)
        dfm_count <- data.table(
            term = names(topfeatures(dfm_i, length(features(dfm_i)))),
            count = topfeatures(dfm_i, length(features(dfm_i)))
            )
        tofile <- paste0(todir, "dfmcount_", i, ".rds")
        saveRDS(dfm_count, tofile)
        setTxtProgressBar(pb, i)
        # print(sprintf("Write file %s of %s to disk.", i, chunks))
    }
    close(file.con)
    close(pb)
}
    

savedfm("./final/en_US/en_US.twitter.txt", chunklines = 10000, maxchunks = 100)

testText <- scan("./final/en_US/en_US.twitter.txt", 
                 what = "",
                 sep = "\n",
                 nlines = 50,
                 skipNul = TRUE,
                 quiet = TRUE)

x <- dfm(testText, ngrams = 1:3,
         ignoredFeatures = stopwords('english'),
        toLower = TRUE,
        removeNumbers = TRUE,
        removePunct = TRUE, 
        removeSeparators = TRUE,
        removeTwitter = TRUE,
        language = "english",
        verbose = TRUE)



# 
# blogs <- corpus(textfile("./corpus/en_US.blogs.txt.txt"))
# news <- corpus(textfile("./corpus/en_US.news.txt.txt"))
# 
# corp <- twitter + blogs + news    rows <- as.numeric(system(paste("wc -l < ", filename, sep=""), TRUE))

# 
# docmatrix <- dfm(corp)
# 
# textvec <- tokenize(testtext, removePunct = TRUE, removeNumbers = TRUE) %>% unlist
# seq <- createSequenceMatrix(textvec)
# prob <- markovchainFit(textvec)
# 
# transprob <- prob$estimate %>% as('data.frame') %>% as.data.table
# setnames(transprob, old = names(transprob), new = c('term1', 'term2', 'prob'))
