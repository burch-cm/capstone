library(data.table)
library(magrittr)
# load in sample data sets
samples <- fread("./data/sampledata.csv", col.names = c("line", "text", "source"))
 