library(tidyverse)

# import all the datasets

BIG4 <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/BIG4_plotting.rds")
NOW_raw <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW_plotting.rds")
NOW_clean <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW_cleaned_noBIG4_plotting.rds")
NOW_consistent <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW_cleaned_consistent500since2010_plotting.rds")
upworthy_raw <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/upworthy_plotting.rds")
TED <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/TED_plotting.rds")
ARXIV <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/ARXIV_plotting.rds")
NOW_consistent_noTOI <- NOW_consistent %>%
  filter(source_domain != "timesofindia.indiatimes.com")
# upworthy has many duplicates, since this is an A/B testing corpus
# and the same headline was shown with several images, for example
upworthy <- distinct(upworthy_raw, raw_text, .keep_all = TRUE)


plot_reference_corpora <- function(var) {
  ARXIV_summary <- ARXIV %>%
    group_by(year) %>%
    summarise(
      mean = mean(!!sym(var), na.rm = TRUE))
  
  reference_plot <- ggplot() +
    geom_hline(yintercept = mean(upworthy[[var]]), linetype = "dashed", color = "red", linewidth = 1.5) +
    #annotate("text", x = 2021.5, y = mean(upworthy[[var]]), label = "upworthy", hjust = 1, vjust = 1.5, color = "red", size = 6, fontface = "bold") +
    
    geom_line(data = ARXIV_summary, aes(x = year, y = mean), linetype = "dashed", color = "black", linewidth = 1.5) #+
    #annotate("text", x = 2021.5, y = mean(ARXIV[[var]]), label = "ARXIV", hjust = 1, vjust = 0, color = "black", size = 6, fontface = "bold") 
}

plot_main_corpus <- function(var, corpus) {
  corpus_name <- deparse(substitute(corpus))
  
  corpus_summary <- corpus %>%
    group_by(year) %>%
    summarise(
      mean = mean(!!sym(var), na.rm = TRUE),
      sd = sd(!!sym(var), na.rm = TRUE),
      lower = mean - sd,  # Lower bound for 1 standard deviation
      upper = mean + sd)   # Upper bound for 1 standard deviation
  
  outlet_means <- corpus %>%
    group_by(source_domain, year) %>%
    summarise(
      doc_mean = mean(!!sym(var), na.rm = TRUE))
  
  # Determine alpha based on corpus_name
  linewidth <- if(corpus_name == "BIG4_corpus") 2.2 else 6
  
  plot <- ggplot() +
    geom_ribbon(data = corpus_summary, aes(x = year, ymin = lower, ymax = upper), alpha = 0.3, fill = "gray") +
    geom_line(data = corpus_summary, aes(x = year, y = mean), color = "black", linewidth = linewidth)#2.2) #change to 1.2 for BIG4 corpus 
  
  # add line labels if the corpus is "BIG4_corpus"
  if (corpus_name == "BIG4_corpus") {
    
  #  last_points <- outlet_means %>%
  #    group_by(source_domain) %>%
  #    filter(year == min(year)) %>%
  #    ungroup()
    
  plot <- plot +
      geom_line(data = outlet_means, aes(x = year, y = doc_mean, color = source_domain), alpha = 1, linewidth = 6) +
      scale_color_manual(values = c("Guardian" = "blue", 
                                    "NYT" = "grey", 
                                    "TOI" = "red", 
                                    "ABC" = "green"))
      #geom_text(data = last_points, aes(x = year, y = doc_mean, label = source_domain, color = source_domain), 
      #          vjust = -0.5, hjust = 0, check_overlap = TRUE, size = 5, fontface = "bold")
  }
  return(plot)
}


# the 8 variables for Figure 1
binary_vars <- c("verb", "pronoun", "wh_word", "is_NP", "is_Sentence", "is_negative", "is_positive")
cont_vars <- c("words_n")

vars <- c(binary_vars, cont_vars)
#vars <- c("words_n", "verb")
#change all these variables to 1/0 in all the datasets 
for (var in binary_vars) {
  upworthy[[var]] <- as.numeric(as.logical(upworthy[[var]]))
  ARXIV[[var]] <- as.numeric(as.logical(ARXIV[[var]]))
  TED[[var]] <- as.numeric(as.logical(TED[[var]]))
}

# subset the NOW_corpus
#NOW_subsets <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/descriptive_plots/NOW_subsets.rds")
# add a subset that contains all of the source_domains in the NOW corpus.
#all_NOW_source_domains <- unique(NOW_raw$source_domain)
#NOW_subsets$all_NOW_domains <- all_NOW_source_domains
# add a subset that exclued outlets already present in BIG4
#BIG4_outlets <- c("www.nytimes.com", "timesofindia.indiatimes.com", "www.abc.net.au", "www.theguardian.com")

# Create a new list element with the remaining outlets
#NOW_subsets$noBIG4 <- setdiff(all_NOW_source_domains, BIG4_outlets)


# for the NOW corpus

NOW_corpus <- NOW_raw# NOW_clean#NOW_consistent#NOW_consistent_noTOI# NOW_consistent #NOW_consistent 
NOW_corpus_name <- deparse(substitute(NOW_raw)) #NOW_clean))#NOW_consistent))##NOW_consistent_noTOI)) 
for (var in vars) {
  
  # create a directory if it doesn't exist yet
  dirname <- paste0("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/final_figures/figure_1/NOW_plots/", NOW_corpus_name, '/', var)
  
  if (!dir.exists(dirname)){
    dir.create(dirname, recursive = TRUE)
    }
    
  reference_corpora_plot <- plot_reference_corpora(var)
  
  main_corpus_plot <- plot_main_corpus(var, NOW_corpus)
  
  combined_plot <- main_corpus_plot + reference_corpora_plot$layers + 
    coord_cartesian(xlim = c(2010.5, 2021.2)) +
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
    scale_x_continuous(
      breaks = seq(floor(2000), ceiling(2022), by = 5),  # Major breaks every 5 units
      minor_breaks = seq(floor(2000), ceiling(2022), by = 1)  # Minor breaks every 1 unit
    ) + 
    labs(x = NULL,
         y = NULL)

  # Conditionally add ylim and caption if var is in binary_vars
  if (var %in% binary_vars) {
    combined_plot <- combined_plot + ylim(0, 1)
    }
  print(combined_plot)

  # save plot 
  filepath <- paste0(dirname, "/", NOW_corpus_name, "_", var, ".jpg") 
  ggsave(filepath, plot = combined_plot, width = 5, height = 7)
}

##### for the BIG4 corpus
BIG4_corpus <- BIG4
  
#BIG4_corpus$source_domain <- BIG4_corpus$outlet
vars <- c(cont_vars, binary_vars)

for (var in vars) {
  
  # create a directory if it doesn't exist yet
  dirname <- paste0(  "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/final_figures/figure_1/BIG4_plots/", var)
  if (!dir.exists(dirname))
    {
    dir.create(dirname, recursive = TRUE)
    }
  
  reference_corpora_plot <- plot_reference_corpora(var)
  
  main_corpus_plot <- plot_main_corpus(var, BIG4_corpus)
  
  combined_plot <- main_corpus_plot + reference_corpora_plot$layers + 
    coord_cartesian(xlim = c(2000.5, 2021.2)) +
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
    scale_x_continuous(
      breaks = seq(floor(2000), ceiling(2022), by = 5),  # Major breaks every 5 units
      minor_breaks = seq(floor(2000), ceiling(2022), by = 1)  # Minor breaks every 1 unit
    ) + 
    labs(x = NULL,
         y = NULL) 

  # Conditionally add ylim and caption if var is in binary_vars
  if (var %in% binary_vars) {
    combined_plot <- combined_plot + ylim(0, 1)
    }
  print(combined_plot)
  
  # save plot 
  filepath <- paste0(dirname, "/BIG4_", var, ".jpg")
  ggsave(filepath, plot = combined_plot, width = 8, height = 7)
}


# Create a dataset for labels
#label_data <- outlet_means %>%
#  group_by(source_domain) %>%
#  summarise(year = min(year), doc_mean = min(doc_mean))#
# geom_text_repel(data = label_data, aes(x = year, y = doc_mean, label = source_domain)) 



# all binary variables:
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

# all the vars
cont_vars <- c("char_n",
               "words_n",
               "avg_word_len",
               "FRE",
               "GFI")
