library(tidyverse)


BIG4 <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/BIG4.rds")
NOW <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW.rds")
upworthy <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/upworthy.rds")
ARXIV <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/ARXIV.rds")
NYT_2000_onwards <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NYT_2000_onwards.rds")

extract_syntax_features <- function(df) {
  df %>% mutate(
    is_Sentence = root == "['S']",
    is_NP = root == "['NP']",
    is_NP_S = root == "['NP', 'S']",
    is_S_S = root == "['S', 'S']",
    is_FRAG = root == "['FRAG']",
    contains_sentence = as.integer(grepl("'S'", root))
  )
}

extract_sentiment_features <- function(df, cutoff) {
  df %>% mutate(
    is_negative = ifelse(sentiment == "NEGATIVE" & score > cutoff, 1, 0),
    is_positive = ifelse(sentiment == "POSITIVE" & score > cutoff, 1, 0),
    is_neutral = ifelse(is_negative == 0 & is_positive == 0, 1, 0)
  )
}
#extract demonstratives, individually and altogether
# for that, check if words_lower contains 'now' or 'here' or here's' or 'that's'

contains_demonstratives <- function(df) {
  df$this <- grepl("'this'", df$words_lower)
  df$that <- grepl("'that'", df$words_lower)
  df$thats <- grepl("\\\"that's\\\"", df$words_lower)
  df$those <- grepl("'those'", df$words_lower)
  df$these <- grepl("'these'", df$words_lower)
  df$here <- grepl("'here'", df$words_lower)
  df$heres <- grepl("\\\"here's\\\"", df$words_lower)
  df$hereis <- grepl("'here', 'is'", df$words_lower)
  df$thisis <- grepl("'this', 'is'", df$words_lower)
  df$thatis <- grepl("'that', 'is'", df$words_lower)
  df$hereswhy <- grepl("\\\"here's\\\", 'why'", df$words_lower)
  return(df)
}

# combine this all in one step
augment_df <- function(df) {
  df <- contains_demonstratives(df)
  df <- extract_sentiment_features(df, 0.9)  # Assuming 0.9 is a threshold or similar parameter
  df <- extract_syntax_features(df)
  return(df)
}
# I should remove duplicates from upworthy or I am making some features appear more
# often than they do 

BIG4_plotting <- augment_df(BIG4)
saveRDS(BIG4_plotting, "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/BIG4_plotting.rds")

NOW_plotting <- augment_df(NOW)
saveRDS(NOW_plotting, "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW_plotting.rds")

upworthy_plotting <- augment_df(upworthy)
saveRDS(upworthy_plotting, "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/upworthy_plotting.rds")

ARXIV_plotting <- augment_df(ARXIV)
saveRDS(ARXIV_plotting, "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/ARXIV_plotting.rds")

NYT_2000_onwards_plotting <- augment_df(NYT_2000_onwards)
saveRDS(NYT_2000_onwards_plotting, "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NYT_2000_onwards_plotting.rds")
