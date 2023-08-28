#rm(list = ls(all.names = TRUE))

# libraries
#library(magrittr)

# functions
#source("https://pastebin.com/raw/97NNTTzu")

# read the data file(s) and combine to data frame
df = pavlovia_Merge(
  dir_path = "C:/Users/gcook/Sync/Courses/GCDS/data/stroop-master/data") %>%
  dplyr::select(., -c(1:13)) %>% # select cols
  dplyr::distinct()              # keep distinct rows/dedup

message("read_data.R Done!")
