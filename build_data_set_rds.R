start_time <- Sys.time()

print("Chunking text files...")
source("txt_files_to_rds_chunks.R")

print("Preprocessing chunks...")
source("rds_chunks_preprocess_noend.R")

print("chunked term counts completed.")

end_time <- Sys.time()
print(difftime(end_time, start_time))