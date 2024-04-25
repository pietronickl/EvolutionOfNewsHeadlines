library(tidyverse)
#library(ggplot2)     
#library(urltools)
#library(ggrepel)

# import all the datasets

BIG4 <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/BIG4_plotting.rds")
NOW_raw <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW_plotting.rds")
NOW_clean <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW_cleaned_noBIG4_plotting.rds")
NOW_consistent <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW_cleaned_consistent500since2010_plotting.rds")
upworthy_raw <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/upworthy_plotting.rds")
TED <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/TED_plotting.rds")
ARXIV <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/ARXIV_plotting.rds")

# upworthy has many duplicates, since this is an A/B testing corpus
# and the same headline was shown with several images, for example
upworthy <- distinct(upworthy_raw, raw_text, .keep_all = TRUE)

plot_reference_corpora <- function(var) {
  ARXIV_summary <- ARXIV %>%
    group_by(words_n) %>%
    summarise(
      mean = mean(!!sym(var), na.rm = TRUE))
  
  upworthy_summary <- upworthy %>%
    group_by(words_n) %>%
    summarise(
      mean = mean(!!sym(var), na.rm = TRUE))
  
  reference_plot <- ggplot() +
    geom_line(data = upworthy_summary, aes(x = words_n, y = mean), linetype = "dashed", color = "red", linewidth = 1.5) +#+
    #annotate("text", x = 15, y = mean(upworthy[[var]]), label = "upworthy", hjust = 1, vjust = 0, color = "red", size = 6, fontface = "bold") + 
  
    geom_line(data = ARXIV_summary, aes(x = words_n, y = mean), linetype = "dashed", color = "black", linewidth = 1.5)# +
    #annotate("text", x = 15, y = mean(ARXIV[[var]]), label = "ARXIV", hjust = 1, vjust = 0, color = "black", size = 6, fontface = "bold") 
}

plot_main_corpus <- function(var, corpus) {
  corpus_name <- deparse(substitute(corpus))
  
  corpus_summary <- corpus %>%
    group_by(words_n) %>%
    summarise(
      mean = mean(!!sym(var), na.rm = TRUE),
      sd = sd(!!sym(var), na.rm = TRUE),
      lower = mean - sd,  # Lower bound for 1 standard deviation
      upper = mean + sd)   # Upper bound for 1 standard deviation
  
  outlet_means <- corpus %>%
    group_by(source_domain, words_n) %>%
    summarise(
      doc_mean = mean(!!sym(var), na.rm = TRUE))
  
  # Determine alpha based on corpus_name
  #line_alpha <- if(corpus_name == "BIG4_corpus") 1 else 0.7
  
  plot <- ggplot() +
    geom_ribbon(data = corpus_summary, aes(x = words_n, ymin = lower, ymax = upper), alpha = 0.3, fill = "gray")# +
    #geom_line(data = corpus_summary, aes(x = words_n, y = mean), color = "black", linewidth = 1.2) 
  
  # add line labels if the corpus is "BIG4_corpus"
  if (corpus_name == "BIG4_corpus") {
    
    #last_points <- outlet_means %>%
    #  group_by(source_domain) %>%
    #  filter(words_n == min(words_n)) %>%
    #  ungroup()
    
    plot <- plot +
      geom_line(data = outlet_means, aes(x = words_n, y = doc_mean, color = source_domain), alpha = 1, linewidth = 6) +
      scale_color_manual(values = c("Guardian" = "blue", 
                                    "NYT" = "grey", 
                                    "TOI" = "red", 
                                    "ABC" = "green")) #+
     # geom_text(data = last_points, aes(x = words_n, y = doc_mean, label = source_domain, color = source_domain), 
    #            vjust = -0.5, hjust = 0, check_overlap = TRUE, size = 5, fontface = "bold")
  }
  else {
    plot <- plot +
      geom_line(data = corpus_summary, aes(x = words_n, y = mean), color = "black", linewidth = 6) 
  }
  return(plot)
}

# 
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
  #TED[[var]] <- as.numeric(as.logical(TED[[var]]))
}

# all the vars
cont_vars <- c("char_n",
               "words_n",
               "avg_word_len",
               "FRE",
               "GFI")

binary_vars <- c("verb", "pronoun", "wh_word", "is_NP", "is_Sentence", "is_negative", "is_positive")
cont_vars <- c("words_n")

vars <- c(binary_vars, cont_vars)
#vars <- c("is_Sentence")


# plot both NOW and BIG4 in one plot
NOW_corpus <- NOW_raw #NOW_clean
NOW_corpus_name <- deparse(substitute(NOW_raw)) #NOW_clean))
BIG4_corpus <- BIG4

for (var in vars) {
  
  # create a directory if it doesn't exist yet
  dirname <- paste0("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/final_figures/supplement/figure_1/BIG4_and_", NOW_corpus_name, "_plots/", var)
  if (!dir.exists(dirname))
  {
    dir.create(dirname, recursive = TRUE)
  }
  
  reference_corpora_plot <- plot_reference_corpora(var)
  
  BIG4_corpus_plot <- plot_main_corpus(var, BIG4_corpus)
  
  NOW_corpus_plot <- plot_main_corpus(var, NOW_corpus)
  
  combined_plot <- BIG4_corpus_plot + NOW_corpus_plot$layers + reference_corpora_plot$layers + 
    coord_cartesian(xlim = c(0, 25)) +
    theme_minimal() +
    theme(legend.position = "none") +
    theme(
      panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.5),  # Customize minor x gridlines
      panel.grid.major.x = element_line(color = "grey80", linewidth = 1),  # Customize major x gridlines
      panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.5),  # Customize minor y gridlines
      panel.grid.major.y = element_line(color = "grey80", linewidth = 1),  # Customize major y gridlines
      axis.text.x = element_text(size = 20),  # Increase x-axis tick label size
      axis.text.y = element_text(size = 20)   # Increase y-axis tick label size
    ) +
    labs(x = NULL, #"number of words",
         y = NULL)#var) 
  
  # Conditionally add ylim and caption if var is in binary_vars
  if (var %in% binary_vars) {
    combined_plot <- combined_plot + ylim(0, 1)
  }
  print(combined_plot)
  
  # save plot 
  filepath <- paste0(dirname, "/BIG4_and_", NOW_corpus_name, ".jpg")
  ggsave(filepath, plot = combined_plot, width = 8, height = 7, bg = "white")
}


###########################


# <- NOW_filtered
# for the NOW corpus
for (var in vars) {
  
  
  # subset the NOW corpus
  for (subset in names(NOW_subsets))
  {
    subset_outlets <- NOW_subsets[[subset]]
    
    # create a directory if it doesn't exist yet
    dirname <- paste0("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/final_figures/supplement/figure_1/NOW_plots/", subset, "/", var)
    if (!dir.exists(dirname))
    {
      dir.create(dirname, recursive = TRUE)
    }
    
    # subset the NOW corpus
    NOW_corpus <- NOW_filtered[NOW_filtered$source_domain %in% subset_outlets, ]

    #NOW_corpus <- NOW_raw[NOW_raw$source_domain %in% subset_outlets, ]
    
    reference_corpora_plot <- plot_reference_corpora(var)
    
    main_corpus_plot <- plot_main_corpus(var, NOW_corpus)
    
    
    combined_plot <- main_corpus_plot + reference_corpora_plot$layers + 
      coord_cartesian(xlim = c(0, 25)) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = "number of words",
           y = var, " in ", subset) 
    
    # Conditionally add ylim and caption if var is in binary_vars
    if (var %in% binary_vars) {
      combined_plot <- combined_plot + ylim(0, 1)
    }
    print(combined_plot)
    
    # save plot 
    #filepath <- paste0(dirname, "/", subset, "_raw.jpg") #_filtered.jpg
    filepath <- paste0(dirname, "/", "filtered.jpg") #
    ggsave(filepath, plot = combined_plot, width = 8, height = 7)
  }
}

##### for the BIG4 corpus
BIG4_corpus <- BIG4

#BIG4_corpus$source_domain <- BIG4_corpus$outlet
vars <- c(cont_vars, binary_vars)

for (var in vars) {
  
  # create a directory if it doesn't exist yet
  dirname <- paste0("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/final_figures/supplement/figure_1/BIG4_plots/", var)
  if (!dir.exists(dirname))
  {
    dir.create(dirname, recursive = TRUE)
  }
  
  reference_corpora_plot <- plot_reference_corpora(var)
  
  main_corpus_plot <- plot_main_corpus(var, BIG4_corpus)
  
  combined_plot <- main_corpus_plot + reference_corpora_plot$layers + 
    coord_cartesian(xlim = c(0, 25)) +
    theme_minimal() +
    labs(x = "number of words",
         y = var) 
  
  # Conditionally add ylim and caption if var is in binary_vars
  if (var %in% binary_vars) {
    combined_plot <- combined_plot + ylim(0, 1)
  }
  print(combined_plot)
  
  # save plot 
  filepath <- paste0(dirname, "/BIG4", ".jpg")
  ggsave(filepath, plot = combined_plot, width = 8, height = 7, bg = "white")
}


# Create a dataset for labels
#label_data <- outlet_means %>%
#  group_by(source_domain) %>%
#  summarise(year = min(year), doc_mean = min(doc_mean))#
# geom_text_repel(data = label_data, aes(x = year, y = doc_mean, label = source_domain)) 
