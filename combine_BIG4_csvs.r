library(tidyverse)

file_paths_features <- c("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/features/ABC_full_features.csv", 
                         "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/features/Guardian_full_features.csv", 
                         "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/features/NYT_full_features.csv", 
                         "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/features/TOI_full_features.csv")

file_paths_syntax <- c("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/syntax/ABC_full_root.csv",
                       "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/syntax/Guardian_full_root.csv", 
                       "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/syntax/NYT_full_root.csv", 
                       "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/syntax/TOI_full_root.csv")

file_paths_sentiment <- c("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/sentiment/ABC_full_flair.csv", 
                          "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/sentiment/Guardian_full_flair.csv",
                          "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/sentiment/NYT_full_flair.csv",
                          "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/sentiment/TOI_full_flair.csv")

outlets <- c("ABC", "Guardian", "NYT", "TOI")


BIG4_features <- map2_dfr(file_paths_features, outlets, function(file, outlet) {
  df <- read_csv(file)
  df$outlet<- outlet
  return(df)
})

BIG4_syntax <- map2_dfr(file_paths_syntax, outlets, function(file, outlet) {
  df <- read_csv(file)
  df$outlet<- outlet
  return(df)
})

BIG4_sentiment <- map2_dfr(file_paths_sentiment, outlets, function(file, outlet) {
  df <- read_csv(file)
  df$outlet<- outlet
  return(df)
})

# conbine the BIG4 corpora, using a new index for the whole (since the all start with row number 0 respectively otherwise)
BIG4_features <- BIG4_features %>% mutate(index = row_number())
BIG4_syntax <- BIG4_syntax %>% mutate(index = row_number())
BIG4_sentiment <- BIG4_sentiment %>% mutate(index = row_number())

BIG4 <- BIG4_features %>%
  left_join(BIG4_sentiment %>% select(index, sentiment, score), by = "index") %>%
  left_join(BIG4_syntax %>% select(index, tree, root), by = "index")

BIG4$source_domain <- BIG4$outlet
saveRDS(BIG4, file = "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/BIG4.rds")
