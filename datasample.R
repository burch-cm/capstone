wd <- "~/Documents/code/capstone"
if(getwd() != wd) setwd(wd)

# reads in a percentage of a target file
sampleLines <- function(filename, percent = 0.1){
    rows <- as.numeric(system(paste("wc -l < ", filename, sep=""), TRUE))
    file.con <- file(filename)
    sel <- round(percent * rows)
    remaining <- rows
    recs <- sort(sample(1:remaining, sel))
    skip <- diff(c(1, recs)) - 1
    mysel <- vector('character', sel)
    for (i in 1:sel){
        mysel[i] <- scan(file.con, what="", sep="\n", skip=skip[i], n=1, quiet=TRUE)
    }
    close(file.con)
    return(mysel)
}

# specifiy the target files
set.seed(100)
file1 <- "./data/final/en_US/en_US.blogs.txt"
file2 <- "./data/final/en_US/en_US.news.txt"
file3 <- "./data/final/en_US/en_US.twitter.txt"

blog.sample <- sampleLines(file1, percent = .05)
write.csv(as.data.frame(blog.sample), "./data/blogsample.csv")
rm(blog.sample)

news.sample <- sampleLines(file2, percent = .05)
write.csv(as.data.frame(news.sample), "./data/newssample.csv")
rm(news.sample)

twitter.sample <- sampleLines(file3, percent = .05)
write.csv(as.data.frame(twitter.sample), "./data/twittersample.csv")
rm(twitter.sample)

library(data.table)
library(magrittr)
# load in sample data sets

twit <- fread("./data/twittersample.csv", header=TRUE, stringsAsFactors = FALSE,
              col.names = c("line", "text"))
twit[, c("source") := "twitter"]; twit[, line := NULL]
news <- fread("./data/newssample.csv", header=TRUE, stringsAsFactors = FALSE,
              col.names = c("line", "text"))
news[, c("source") := "news"]; news[, line := NULL]
blog <- fread("./data/blogsample.csv", header=TRUE, stringsAsFactors = FALSE,
              col.names = c("line", "text"))
blog[, c("source") := "blogs"]; blog[, line := NULL]

# combine the text documents into one data table for writing to file
samples <- rbind(twit, news, blog)
write.csv(samples, "./data/sampledata.csv")