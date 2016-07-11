# chunk large files into smaller files to iterate over
# save text files into easier to read format (rds)

library(readr)
# set correct dir
if(getwd() != "~/Documents/code/capstone/"){
    setwd("~/Documents/code/capstone/")
}
# the files to read in
files <- c("./final/en_US/en_US.twitter.txt",
           "./final/en_US/en_US.blogs.txt",
           "./final/en_US/en_US.news.txt")
# the chunk suffixes
outfiles <- c("twitter.rds", "blogs.rds", "news.rds")
# number of lines per chunk file
num_lines <- 10000
# set up a txt progress bar
pb1 <- txtProgressBar(min = 0, max = 3)
# for each file we're reading in
for(i in 1:length(files)){
    # set chunk iterator
    start <- 0
    # load the files
    current_file <- read_lines(files[i])
    # how many chunks to process in this file
    num_chunks <- ceiling(length(current_file) / num_lines)
    # for each chunk in the current file
    for(j in 1:num_chunks){
        # how many entries go into this chunk
        end <- start + num_lines
        # read vector from start to end = chunk
        chunk <- current_file[start:end]
        # start next chunk immediately after the previous chunk ends
        start <- end + 1
        # write this chunk to a file (rds)
        outname <- paste("./data/chunks/chunk", j, outfiles[i], sep = "_")
        write_rds(chunk, outname)
        # clear up some space in memory
        rm(chunk, outname)
    }
    # update the progress bar
    setTxtProgressBar(pb1, value = i)
    # clear up some memory
    rm(current_file)
}
close(pb1)
print("Wrote rds files to ./data/chunks/")