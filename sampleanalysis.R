library(data.table)
library(magrittr)
# load in sample data set
samples <- fread("./data/sampledata.csv", col.names = c("line", "text", "source"))
samples[, line := as.numeric(line)]
samples[, c("source") := as.factor(source)]

