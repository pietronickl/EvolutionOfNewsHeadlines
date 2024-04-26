library(tidyverse)

# the BIG4 is being commpiled from the individual corpora (NYT, ABC, TOI, Guardian) in a 
# different script and processed further into one coherent BIG4 .rds file containing all the columns
# features, sentiment and syntax


# import the relevant annotated NOW corpus and the reference corpora
# import the relevant annotated NOW corpus and the reference corpora
NOW_features <- read.csv("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/features/NOW_full_features.csv")
NOW_sentiment <- read.csv("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/sentiment/NOW_full_sentiment.csv")
NOW_syntax <- read.csv("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/syntax/NOW_full_syntax.csv")

# make sure Unnamed.00 is unique
NOW <- NOW_features %>%
  left_join(NOW_sentiment %>% select(Unnamed..0, sentiment, score), by = "Unnamed..0") %>%
  left_join(NOW_syntax %>% select(Unnamed..0, tree, root), by = "Unnamed..0")

# add the source domain and export as rds to load later 
# annotate with the source_domain
NOW_annotated$source_domain <- domain(NOW_annotated$url)
saveRDS(NOW, file = "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW.rds")


ARXIV_features <- read.csv('/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/features/ARXIV_full_features.csv')
ARXIV_sentiment <- read.csv('/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/sentiment/ARXIV_full_flair.csv')
ARXIV_syntax <- read.csv('/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/syntax/ARXIV_full_root.csv')
ARXIV <- ARXIV_features %>%
  left_join(ARXIV_sentiment %>% select(Unnamed..0, sentiment, score), by = "Unnamed..0") %>%
  left_join(ARXIV_syntax %>% select(Unnamed..0, tree, root), by = "Unnamed..0")
saveRDS(ARXIV, file = "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/ARXIV.rds")

upworthy_features <- read.csv('/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/features/upworthy_full_features.csv')
upworthy_sentiment <- read.csv('/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/sentiment/upworthy_full_flair.csv')
upworthy_syntax <- read.csv('/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/syntax/upworthy_full_root.csv')
upworthy <- upworthy_features %>%
  left_join(upworthy_sentiment %>% select(Unnamed..0, sentiment, score), by = "Unnamed..0") %>%
  left_join(upworthy_syntax %>% select(Unnamed..0, tree, root), by = "Unnamed..0")
saveRDS(upworthy, file = "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/upworthy.rds")

TED_features <- read.csv('/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/features/TED_full_features.csv')
TED_sentiment <- read.csv('/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/sentiment/TED_full_flair.csv')
TED_syntax <- read.csv('/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/syntax/TED_full_root.csv')
TED <- TED_features %>%
  left_join(TED_sentiment %>% select(Unnamed..0, sentiment, score), by = "Unnamed..0") %>%
  left_join(TED_syntax %>% select(Unnamed..0, tree, root), by = "Unnamed..0")
saveRDS(TED, file = "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/TED.rds")


NOW <- readRDS("NOW.rds")
NOW <- readRDS("NOW.rds")
NOW <- readRDS("NOW.rds")
NOW <- readRDS("NOW.rds")
NOW <- readRDS("NOW.rds")



