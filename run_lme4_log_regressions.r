########## LINEAR REGRESSION cont_var IN LOOP 
library(tidyverse)
#install.packages("lme4")
library(lme4)
binary_vars <- c(#"double_quote",
                 #"single_quote",
                 #"number",
                 #"uppercase",
                 #"question_mark",
                 #"exclamation_mark",
                 #"dot",
                 #"colon",
                 #"n_dash",
                 #"m_dash",
                 #"noun",
                 "pronoun",
                 "verb",
                 #"adj",
                 #"adv",
                 #"superlative",
                 "wh_word",
                 #"interjection",
                 #"det",
                 #"def_article",
                 #"indef_article",
                 #"I",
                 #"you",
                 #"he",
                 #"she",
                 #"it",
                 #"they",
                 "is_Sentence",
                 "is_NP",
                 #"is_NP_S",
                 #"is_FRAG",
                 #"contains_sentence",
                 "is_negative",
                 "is_positive")#,
                 #"is_neutral",
                 #"this",
                 #"that",
                 #"thats",
                 #"those",
                 #"these",
                 #"here",
                 #"heres",
                 #"hereis",
                 #"thisis",
                 #"thatis",
                 #"hereswhy")

BIG4 <- readRDS('/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/BIG4_plotting.rds')
NOW_raw <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW_plotting.rds")
NOW_clean <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW_cleaned_noBIG4_plotting.rds")
NOW_consistent <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NOW_cleaned_consistent500since2010_plotting.rds")
ARXIV <- readRDS('/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/ARXIV_plotting.rds')

# center year
BIG4$year_centered = BIG4$year - mean(BIG4$year)
NOW_raw$year_centered = NOW_raw$year - mean(NOW_raw$year)
NOW_clean$year_centered = NOW_clean$year - mean(NOW_clean$year)
NOW_consistent$year_centered = NOW_consistent$year - mean(NOW_consistent$year)
ARXIV$year_centered = ARXIV$year - mean(ARXIV$year)

# standardizing predictors
# standardizing year
#BIG4$year_std <- scale(BIG4$year)
#NOW$year_std <- scale(NOW$year)
#ARXIV$year_std <- scale(ARXIV$year)

######

NOW_LogReg <- list()


corpus <- NOW_clean
corpus_name <- deparse(substitute(NOW_clean))

for (var in binary_vars) {
  
  # Construct the model formulas dynamically
  formula_1 <- as.formula(paste(var, "~ 1"))
  model_1 <- glm(formula_1, data = corpus, family = binomial())
  
  formula_2 <- as.formula(paste(var, "~ 1 + year_centered"))
  model_2 <- glm(formula_2, data = corpus, family = binomial())
  
  formula_3 <- as.formula(paste(var, "~ 1 + year_centered + (1 | source_domain)"))
  model_3 <- glmer(formula_3, data = corpus, family = binomial())
  
  formula_4 <- as.formula(paste(var, "~ 1 + year_centered + (1 + year_centered | source_domain)"))
  model_4 <- glmer(formula_4, data = corpus, family = binomial())
  
  # Store models in the results list
  NOW_LogReg[[var]] <- list(model_1 = model_1, model_2 = model_2, model_3 = model_3, model_4 = model_4)
}

# save model list as rds
filename <- paste0("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/statistical_analysis/", corpus_name, "_lme4_log.rds")
saveRDS(NOW_LogReg, filename)





######

BIG4_LogReg <- list()

corpus = BIG4 
corpus_name <- deparse(substitute(BIG4))

for (var in binary_vars) {
  
  # Construct the model formulas dynamically
  formula_1 <- as.formula(paste(var, "~ 1"))
  model_1 <- glm(formula_1, data = corpus, family = binomial())
  
  formula_2 <- as.formula(paste(var, "~ 1 + year_centered"))
  model_2 <- glm(formula_2, data = corpus, family = binomial())
  
  formula_3 <- as.formula(paste(var, "~ 1 + year_centered + (1 | source_domain)"))
  model_3 <- glmer(formula_3, data = corpus, family = binomial())
  
  formula_4 <- as.formula(paste(var, "~ 1 + year_centered + (1 + year_centered | source_domain)"))
  model_4 <- glmer(formula_4, data = corpus, family = binomial())
  
  # Store models in the results list
  BIG4_LogReg[[var]] <- list(model_1 = model_1, model_2 = model_2, model_3 = model_3, model_4 = model_4)
}

# save model list as rds
filename <- paste0("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/statistical_analysis/", corpus_name, "_lme4_log.rds")
saveRDS(BIG4_LogReg, filename)


######

corpus = BIG4 

log_coefficients_df <- tibble()  # Initialize an empty tibble

for (var in binary_vars) {
  
  # Construct the model formulas dynamically
  formula_1 <- as.formula(paste(var, "~ year_centered"))
  model_1 <- glm(formula_1, data = corpus, family = binomial())
  
  coef_estimate <- coef(summary(model_1))["year_centered", "Estimate"]
  
  
  conf_int <- confint(model_1)["year_centered", ]
  
  # Append to the tibble
  log_coefficients_df <- rbind(log_coefficients_df, 
                               tibble(variable = var, 
                                      estimate = coef_estimate, 
                                      lower = conf_int[1], 
                                      upper = conf_int[2]))
}


# Save the summary information as an RDS file
saveRDS(log_coefficients_df, file="/mnt/home/nickl/Users/NicklPietro/PaperIIwb/statistical_analysis/BIG4_log_coefficients.rds")

  # Store models in the results list
  #LogReg_BIG4[[var]] <- list(model_1 = model_1)
#}

# to access a specific model: 
#LogReg_BIG4$verb$model_1

# save model list as rds
#saveRDS(LogReg_BIG4, file="/mnt/home/nickl/Users/NicklPietro/PaperIIwb/statistical_analysis/BIG4_lme4_log.rds")


#########
######### do the same for NOW

# subset the NOW corpus
NOW_subsets <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/descriptive_plots/NOW_subsets.rds")

corpus = NOW[NOW$source_domain %in% NOW_subsets$major_global_news, ]

log_coefficients_df <- tibble()  # Initialize an empty tibble

for (var in binary_vars) {
  # Construct the model formulas dynamically
  formula_1 <- as.formula(paste(var, "~ year_centered"))
  model_1 <- glm(formula_1, data = corpus, family = binomial())
  
  coef_estimate <- coef(summary(model_1))["year_centered", "Estimate"]
  
  conf_int <- confint(model_1)["year_centered", ]
  
  # Extract AIC and BIC
  aic_value <- AIC(model_1)
  bic_value <- BIC(model_1)
  
  # Append to the tibble
  log_coefficients_df <- rbind(log_coefficients_df, 
                               tibble(variable = var, 
                                      estimate = coef_estimate, 
                                      lower = conf_int[1], 
                                      upper = conf_int[2],
                                      AIC = aic_value,
                                      BIC = bic_value))
}


# Save the summary information as an RDS file
saveRDS(log_coefficients_df, file="/mnt/home/nickl/Users/NicklPietro/PaperIIwb/statistical_analysis/NOW_major_global_log_coefficients.rds")


##### and the same for ARXIV
#####

#change all these variables to 1/0 in all the datasets 
for (var in binary_vars) {
  ARXIV[[var]] <- as.numeric(as.logical(ARXIV[[var]]))
}

corpus = ARXIV


ARXIV_log_coefficients_df <- tibble()  # Initialize an empty tibble

for (var in binary_vars) {
  # Construct the model formulas dynamically
  formula_1 <- as.formula(paste(var, "~ year_centered"))
  model_1 <- glm(formula_1, data = corpus, family = binomial())
  
  coef_estimate <- coef(summary(model_1))["year_centered", "Estimate"]
  
  
  conf_int <- confint(model_1)["year_centered", ]
  
  # Append to the tibble
  ARXIV_log_coefficients_df <- rbind(ARXIV_log_coefficients_df, 
                               tibble(variable = var, 
                                      estimate = coef_estimate, 
                                      lower = conf_int[1], 
                                      upper = conf_int[2]))
}

# Save the summary information as an RDS file
saveRDS(ARXIV_log_coefficients_df, file="/mnt/home/nickl/Users/NicklPietro/PaperIIwb/statistical_analysis/ARXIV_2_log_coefficients.rds")
