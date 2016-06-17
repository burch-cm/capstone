# smush the constituent csv files into one data frame
library(data.table)
if(getwd() != "~/Documents/code/capstone") setwd("~/Documents/code/capstone")
file_list <- paste0("./data/chunks/", list.files("./data/chunks"))
# 
# dataset <- do.call("rbind",lapply(file_list, FUN=function(files){
#     read.table(files, header=TRUE, sep="\t")
#     }))
control_value = 1500000
merged <- fread(file_list[1])
merged[, V1 := NULL]
setnames(merged, old = names(merged), new = c("t1","t2","t3", "t4","weight.x1"))
setkeyv(merged, c("t1", "t2", "t3", "t4"))
pb <- txtProgressBar(min=1, max=length(file_list), style=3)
for(i in 2:length(file_list)){
    file2 <- fread(file_list[i])
    file2[, V1 := NULL]
    merged <- merge(merged, file2, by = c("t1", "t2", "t3", "t4"), all = TRUE)
    merged[is.na(weight.x1), weight.x1 := 0]
    merged[is.na(weight), weight := 0]
    merged[, weight.x1 := weight.x1 + weight]
    try(merged[, weight := NULL], silent = TRUE)
    rm(file2)
    merged <- unique(merged)
    if(nrow(merged) > control_value){
        merged <- merged[order(-weight.x1)][1:control_value]
    }
    setTxtProgressBar(pb, value = i)
}
write.csv(merged, "./data/merged_table_n4.csv")
print("Wrote merged table to ./data/merged_table_n4.csv")
close(pb)