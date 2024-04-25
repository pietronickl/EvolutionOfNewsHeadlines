# the purpose of this script is to compare the subets of the NOW with different political orientations
library(tidyverse)

# import all the datasets
NOW <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW_plotting.rds")
upworthy_raw <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/upworthy_plotting.rds")
TED <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/TED_plotting.rds")
ARXIV <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/ARXIV_plotting.rds")

# to only use the cleaned NOW corpus
NOW <- NOW %>%
  filter(!grepl("\\.{3}$", raw_text)) %>%
  distinct(raw_text, .keep_all = TRUE)


saveRDS(NOW, "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW_cleaned_plotting.rds")

# upworthy has many duplicates, since this is an A/B testing corpus
# and the same headline was shown with several images, for example
upworthy <- distinct(upworthy_raw, raw_text, .keep_all = TRUE)

binary_vars <- c("double_quote",
                 "single_quote",
                 "number",
                 "uppercase",
                 "question_mark",
                 "exclamation_mark",
                 "dot",
                 "colon",
                 "n_dash",
                 "m_dash",
                 "noun",
                 "pronoun",
                 "verb",
                 "adj",
                 "adv",
                 "superlative",
                 "wh_word",
                 "interjection",
                 "det",
                 "def_article",
                 "indef_article",
                 "I",
                 "you",
                 "he",
                 "she",
                 "it",
                 "they",
                 "is_Sentence",
                 "is_NP",
                 "is_NP_S",
                 "is_FRAG",
                 "contains_sentence",
                 "is_negative",
                 "is_positive",
                 "is_neutral",
                 "this",
                 "that",
                 "thats",
                 "those",
                 "these",
                 "here",
                 "heres",
                 "hereis",
                 "thisis",
                 "thatis",
                 "hereswhy")

#change all these variables to 1/0 in all the datasets 
for (var in binary_vars) {
  upworthy[[var]] <- as.numeric(as.logical(upworthy[[var]]))
  ARXIV[[var]] <- as.numeric(as.logical(ARXIV[[var]]))
  TED[[var]] <- as.numeric(as.logical(TED[[var]]))
}

# subset the NOW_corpus
NOW_subsets <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/descriptive_plots/NOW_subsets.rds")

allsides <- read.csv("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/descriptive_plots/all_sides_chart.csv")

# add the political leanings
# I am grouping together "slight right" and "right"; and "slight left" and "left"
# I am doing this because we also lack the granularity in some cases, e.g. we don't 
# distinguish between New York Times News and New York Times opinion 
#  www.nationalreview.com     2
# www.nytimes.com            2
# www.wsj.com   
# I am still getting the following warning: 
#Detected an unexpected many-to-many relationship between `x` and `y`.
#ℹ Row 297 of `x` matches multiple rows in `y`.
#ℹ Row 15 of `y` matches multiple rows in `x`.
#ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
# there should not be a many-to-many mapping now...

allsides_leanings <- allsides %>% 
  select(source_domain, political_leaning) %>%
  mutate(political_leaning = case_when(
    source_domain == "www.wsj.com" ~ "center",  # Exception for WSJ
    political_leaning %in% c("left", "slight_left") ~ "left-leaning",
    political_leaning %in% c("right", "slight_right") ~ "right-leaning",
    political_leaning == "center" ~ "center",
  ))

left_leaning <- allsides_leanings %>%
  filter(political_leaning == "left-leaning") %>%
  pull(source_domain)

right_leaning <- allsides_leanings %>%
  filter(political_leaning == "right-leaning") %>%
  pull(source_domain)

center <- allsides_leanings %>%
  filter(political_leaning == "center") %>%
  pull(source_domain)


# all the vars
cont_vars <- c("char_n",
               "words_n",
               "avg_word_len",
               "FRE",
               "GFI")

#binary_vars <- c("verb")
#cont_vars <- c("char_n")

vars <- c(binary_vars, cont_vars)

# for the NOW corpus
for (var in vars) {
  
  # create a directory if it doesn't exist yet
  dirname <- paste0("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/final_figures/figure_3/figure_3_a/NOW_cleaned/", var)
  if (!dir.exists(dirname))
  {
    dir.create(dirname, recursive = TRUE)
  }
  
  # subset the NOW corpus
  NOW_right_leaning <- NOW[NOW$source_domain %in% right_leaning, ]
  
  NOW_left_leaning <- NOW[NOW$source_domain %in% left_leaning, ]
  
  NOW_center <- NOW[NOW$source_domain %in% center, ]
  
  reference_corpora_plot <- plot_reference_corpora(var)
  
  NOW_right_plot <- plot_corpus_color(var, NOW_right_leaning, "red")
  
  NOW_left_plot <- plot_corpus_color(var, NOW_left_leaning, "blue")
  
  NOW_center_plot <- plot_corpus_color(var, NOW_center, "black")
  

  combined_plot <- reference_corpora_plot + 
    NOW_right_plot$layers + 
    NOW_left_plot$layers + 
    NOW_center_plot$layers + 
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 12), # Adjust size as needed
          axis.text.y = element_text(size = 12))  +
    labs(#title = paste(var, " across political leanings"),
         #caption = paste0("Mean ", var, " over Years"),
         x = NULL,
         y = NULL) +
    scale_x_continuous(breaks = seq(2010, 2022, by = 2)) +
    coord_cartesian(xlim = c(2010, 2022))
  
  # Conditionally add ylim and caption if var is in binary_vars
  if (var %in% binary_vars) {
    combined_plot <- combined_plot + ylim(0, 1) #+ 
      #labs(caption = paste0("Proportion of headlines featuring ", var, " over Years"))
  }
  if (var == "words_n") {
    combined_plot <- combined_plot + ylim(0, 20) #+ 
    #labs(caption = paste0("Proportion of headlines featuring ", var, " over Years"))
  }
  print(combined_plot)
  
  # save plot 
  filepath <- paste0(dirname, "/political_leaning.jpg")
  ggsave(filepath, plot = combined_plot, width = 7, height = 7)
}


plot_reference_corpora <- function(var) {
  ARXIV_summary <- ARXIV %>%
    group_by(year) %>%
    summarise(
      mean = mean(!!sym(var), na.rm = TRUE))
  
  reference_plot <- ggplot() +
    geom_hline(yintercept = mean(upworthy[[var]]), linetype = "dashed", color = "red") +
    #annotate("text", x = 2022, y = mean(upworthy[[var]]), label = "upworthy", hjust = 1, vjust = 1.5, color = "red") +
    
    # geom_hline(yintercept = mean(TED[[var]]), linetype = "dashed", color = "red") +
    # annotate("text", x = 2022, y = mean(TED[[var]]), label = "TED", hjust = 1, vjust = 1.5, color = "red") +
    
    geom_line(data = ARXIV_summary, aes(x = year, y = mean), linetype = "dashed", color = "black") #+
    #annotate("text", x = 2022, y = mean(ARXIV[[var]]), label = "ARXIV", hjust = 1, vjust = 0, color = "black") 
}

plot_corpus_color <- function(var, corpus, color) {
  corpus_summary <- corpus %>%
    group_by(year) %>%
    summarise(
      mean = mean(!!sym(var), na.rm = TRUE))
  
  outlet_means <- corpus %>%
    group_by(source_domain, year) %>%
    summarise(
      doc_mean = mean(!!sym(var), na.rm = TRUE))
  
  plot <- ggplot() +
    geom_line(data = outlet_means, aes(x = year, y = doc_mean, group = source_domain), color = color, alpha = 0.3) +
    geom_line(data = corpus_summary, aes(x = year, y = mean), color = color, linewidth = 1.2) 
}
