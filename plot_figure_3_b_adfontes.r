# the purpose of this script is to compare, within the NOW corpus, the sources
# of high journalistic quality (more serious, broadsheet-style news, e.g., The Guardian) with those of 
# lower journalistic quality (more tabloid-style, sensationalist etc., e.g., The Mirror)
library(tidyverse)

# import all the datasets

# for NOW raw:
#NOW <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW_plotting.rds")
NOW <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW_cleaned_plotting.rds")

upworthy_raw <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/upworthy_plotting.rds")
ARXIV <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/ARXIV_plotting.rds")

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
}

# subset the NOW_corpus
#NOW_subsets <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/descriptive_plots/NOW_subsets.rds")

# all the vars
cont_vars <- c("char_n",
               "words_n",
               "avg_word_len",
               "FRE",
               "GFI")

#binary_vars <- c("verb")
#cont_vars <- c("words_n")

green <- read_csv("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/final_figures/figure_3/adfontes_green.csv",
                  col_names = FALSE)

yellow <- read_csv("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/final_figures/figure_3/adfontes_yellow.csv",
                   col_names = FALSE)

outlets_green <- green[[1]]  # Extract the first column as a vector
outlets_yellow <- yellow[[1]]  # Extract the first column as a vector

vars <- c(binary_vars, cont_vars)
#vars <- c("words_n", "is_NP") # for testing purposes use a subset 
# for the NOW corpus
for (var in vars) {
  
  # create a directory if it doesn't exist yet
  dirname <- paste0("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/final_figures/figure_3/figure_3_b/NOW_cleaned_adfontes/", var)
  
  if (!dir.exists(dirname))
  {
    dir.create(dirname, recursive = TRUE)
  }
  
  # subset the NOW corpus
  NOW_journalism_low <- NOW[NOW$source_domain %in% outlets_yellow, ]
  
  NOW_journalism_high <- NOW[NOW$source_domain %in% outlets_green, ]
  
  reference_corpora_plot <- plot_reference_corpora(var)
  
  journalism_low_plot <- plot_corpus_color(var, NOW_journalism_low, "red")
  
  journalism_high_plot <- plot_corpus_color(var, NOW_journalism_high, "blue") 
  
  combined_plot <- reference_corpora_plot + 
    journalism_low_plot$layers + 
    journalism_high_plot$layers + 
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 20), # Adjust size as needed
          axis.text.y = element_text(size = 20))  +
    labs(#title = paste(var, " across political leanings"),
      #caption = paste0("Mean ", var, " over Years"),
      x = NULL,
      y = NULL) +
    scale_x_continuous(breaks = seq(2010, 2022, by = 5)) +
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
  filepath <- paste0(dirname, "/journalism_low_vs_high.jpg")
  ggsave(filepath, plot = combined_plot, width = 7, height = 7, bg = "white")
}


plot_reference_corpora <- function(var) {
  ARXIV_summary <- ARXIV %>%
    group_by(year) %>%
    summarise(
      mean = mean(!!sym(var), na.rm = TRUE))
  
  reference_plot <- ggplot() +
    geom_hline(yintercept = mean(upworthy[[var]]), linetype = "dashed", color = "red", linewidth = 1.3) +
    #annotate("text", x = 2022, y = mean(upworthy[[var]]), label = "upworthy", hjust = 1, vjust = 1.5, color = "red") +
    
    geom_line(data = ARXIV_summary, aes(x = year, y = mean), linetype = "dashed", color = "black", linewidth = 1.3) #+
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
    geom_line(data = outlet_means, aes(x = year, y = doc_mean, group = source_domain), color = color, alpha = 0.1) +#0.3) +
    geom_line(data = corpus_summary, aes(x = year, y = mean), color = color, linewidth = 3)#1.2) 
}
