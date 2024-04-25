library(tidyverse)
#install.packages("patchwork")
library(patchwork)

# linear variables
cont_vars <- c("char_n",
               "words_n",
               "avg_word_len",
               "FRE",
               "GFI")


rename_lin_vector <- c(
  words_n = "number of words",
  char_n = "number of characters",
  avg_word_len = "average word length",
  GFI = "Gunning Fog Index",
  FRE = "Flesch Reading Ease"
)
rename_lin_vector <- rev(rename_lin_vector)

rename_log_vector <- c(
  double_quote = "\"quote\"",
  single_quote = "'quote'",
  question_mark = "?",
  exclamation_mark = "!",
  dot = ".",
  colon = ":",
  n_dash = "m-dash –",
  m_dash = "n-dash -",
  wh_word = "wh-word",
  interjection = "interjection",
  superlative = "superlative",
  det = "determiner",
  def_article = "definite article: the",
  indef_article = "indefinite article: a",
  number = "number",
  uppercase = "UPPERCASE",
  noun = "noun",
  verb = "verb",
  adj = "ajective",
  adv = "adverb",
  pronoun = "pronoun",
  I = "I",
  you = "you",
  he = "he",
  she = "she",
  it = "it",
  they = "they",
  this = "this",
  that = "that",
  thats = "that's",
  those = "those",
  these = "these",
  here = "here",
  heres = "here's",
  hereis = "here is",
  thisis = "this is",
  thatis = "that is",
  hereswhy = "here's why",
  is_Sentence = "full-sentence headline",
  is_NP = "Noun Phrase headline",
  is_NP_S = "Noun Phrase + Sentence headline",
  is_FRAG = "Fragment headline",
  contains_sentence = "headline contains full sentence",
  is_negative = "negative headline",
  is_positive = "positive headline",
  is_neutral = "neutral headline"
)
rename_log_vector <- rev(rename_log_vector)

# Initialize an empty tibble to store results
extract_lin_coefs <- function(corpus_lin) {
  lin_coefficients_df <- tibble()  # Initialize an empty tibble
  
  for (var in cont_vars) {
    # Access model_2 for the variable
    model <- corpus_lin[[var]]$model_2
    
    # Extract the coefficient for 'year_centered'
    coef_estimate <- coef(summary(model))["year_centered", "Estimate"]
    
    
    conf_int <- confint(model)["year_centered", ]
    
    # Append to the tibble
    lin_coefficients_df <- rbind(lin_coefficients_df, 
                                 tibble(variable = var, 
                                        estimate = coef_estimate, 
                                        lower = conf_int[1], 
                                        upper = conf_int[2]))
  }
  return(lin_coefficients_df)
}

#rename_vars <- function(df, rename_vector) {
#  df <- df %>%
#    mutate(variable = factor(variable, levels = names(rename_vector))) %>%
#    mutate(variable = rename_vector[variable])
#  return(df)
#}

rename_vars <- function(df, rename_vector) {
  # Rename variables
  df$variable <- rename_vector[df$variable]
  
  # Set the factor levels to the order of rename_vector
  df$variable <- factor(df$variable, levels = rename_vector[names(rename_vector)])
  
  return(df)
}

## BIG 4
#corpus_name <- "BIG4"
BIG4_lin <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/statistical_analysis/BIG4_lme4_lin.rds")
BIG4_log <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/statistical_analysis/log_reg_on_centred_year_BIG4.rds")
#corpus_name <- "NOW_major_global_news"
NOW_lin <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/statistical_analysis/NOW_clean_lme4_lin.rds")
NOW_log <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/statistical_analysis/log_reg_on_centred_yearNOW_clean.rds")
#corpus_name <- "ARXIV"
#ARXIV_lin <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/statistical_analysis/ARXIV_lme4_lin.rds")
#ARXIV_log <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/statistical_analysis/ARXIV_2_log_coefficients.rds")

BIG4_lin <- extract_lin_coefs(BIG4_lin)
NOW_lin <- extract_lin_coefs(NOW_lin)
#ARXIV_lin <- extract_lin_coefs(ARXIV_lin)

BIG4_lin <- rename_vars(BIG4_lin, rename_lin_vector)
NOW_lin <- rename_vars(NOW_lin, rename_lin_vector)
#ARXIV_lin <- rename_vars(ARXIV_lin, rename_lin_vector)

BIG4_log <- rename_vars(BIG4_log, rename_log_vector)
NOW_log <- rename_vars(NOW_log, rename_log_vector)
#ARXIV_log <- rename_vars(ARXIV_log, rename_log_vector)

#corpus_log <- corpus_log %>%
#  mutate(variable = factor(variable, levels = names(rename_log_vector))) %>%
#  mutate(variable = rename_log_vector[variable])#

#lin_coefficients_df <- lin_coefficients_df %>%
#  mutate(variable = factor(variable, levels = names(rename_lin_vector))) %>%
#  mutate(variable = rename_lin_vector[variable])

NOW_lin$corpus <- 'NOW'
BIG4_lin$corpus <- 'BIG4'
combined_lin <- rbind(NOW_lin, BIG4_lin)

library(ggplot2)

plot_linear <-ggplot(combined_lin, aes(x = variable, y = estimate, color = corpus)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(title = "Coefficient Estimates for continuous Variables", 
       x = NULL, 
       y = "Estimate") +
  coord_flip() +
  scale_color_manual(values = c('NOW' = 'red', 'BIG4' = 'blue'))


###### logistic regressions
# just the year as predictor (no random slope or)

# filter out m_dash for plotting 

NOW_log$corpus <- 'NOW'
BIG4_log$corpus <- 'BIG4'
combined_log <- rbind(NOW_log, BIG4_log)

plot_logistic <- ggplot(combined_log, aes(x = variable, y = estimate, color = corpus)) +
  geom_point() +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  # Add line on 0
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(title = "Coefficient Estimates for binary Variables", 
       x = NULL, 
       y = "Log Odds Estimate") +
  coord_flip() +
  scale_color_manual(values = c('NOW' = 'red', 'BIG4' = 'blue'))


### do two different plots and stack those

# Combine the plots, stacking linear on top of logistic 
combined_plot <- plot_linear / plot_logistic +
  plot_layout(heights = c(1, 5)) 


# only Fig 1 vars
fig_1_vars <- c("number of words", "verb", "pronoun", "wh-word", "Noun Phrase headline",
                       "full-sentence headline", "negative headline", "positive headline")

# Filter combined_log for only those variables
combined_lin_filtered <- combined_lin %>%
  filter(variable %in% fig_1_vars)

plot_linear_filtered <-ggplot(combined_lin_filtered, aes(x = variable, y = estimate, color = corpus)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(title = "Coefficient Estimates for continuous Variables", 
       x = NULL, 
       y = "Estimate") +
  coord_flip() +
  scale_color_manual(values = c('NOW' = 'red', 'BIG4' = 'blue'))


# Filter combined_log for only those variables
combined_log_filtered <- combined_log %>%
  filter(variable %in% fig_1_vars)

plot_logistic_filtered <- ggplot(combined_log_filtered, aes(x = variable, y = estimate, color = corpus)) +
  geom_point() +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  # Add line on 0
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(title = "Coefficient Estimates for binary Variables", 
       x = NULL, 
       y = "Log Odds Estimate") +
  coord_flip() +
  scale_color_manual(values = c('NOW' = 'red', 'BIG4' = 'blue'))

# Combine the plots, stacking linear on top of logistic 
combined_plot_filtered <- plot_linear_filtered / plot_logistic_filtered +
  plot_layout(heights = c(1, 5)) 


# Print the combined plot
filepath <- paste0("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/final_figures/figure_2/", corpus_name,"_reg_coefs.jpg")
ggsave(filepath, plot = combined_plot, width = 4, height = 9)

####### plot linear and logistic regression coefficients in one go
#######
# Add a column to each data frame to distinguish the type of regression
#coefficients_df <- coefficients_df %>%
#  mutate(regression_type = "Linear")

#log_coefficients_df <- log_coefficients_df %>%
#  mutate(regression_type = "Logistic")

# Combine the data frames
#combined_df <- bind_rows(coefficients_df, log_coefficients_df)

# now combine lin_coefficients and log_coefficients 
#plot_logistic <- ggplot(combined_df, aes(x = variable, y = estimate)) +
#  geom_point() +
#  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
#  theme_minimal() +
#  labs(title = "Coefficient Estimates", 
#       x = "Variable", 
#       y = "Estimate") +
#  coord_flip() +
#  facet_grid(regression_type ~ ., scales = "free", space = "free")  # Create facets

