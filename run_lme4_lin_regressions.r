########## LINEAR REGRESSION cont_var IN LOOP 
#install.packages("lme4")
library(lme4)
library(tidyverse)

cont_vars <- c("char_n",
               "words_n",
               "avg_word_len",
               "FRE",
               "GFI")

#cont_vars <- c("words_n")

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

######
######
######

LinReg_BIG4 <- list()

corpus = BIG4 

for (var in cont_vars) {
  
  # Construct the model formulas dynamically
  formula_1 <- as.formula(paste(var, "~ 1"))
  model_1 <- lm(formula_1, data = corpus)
  
  formula_2 <- as.formula(paste(var, "~ 1 + year_centered"))
  model_2 <- lm(formula_2, data = corpus)
  
  formula_3 <- as.formula(paste(var, "~ 1 + year_centered + (1 | source_domain)"))
  model_3 <- lmer(formula_3, data = corpus)
  
  formula_4 <- as.formula(paste(var, "~ 1 + year_centered + (1 + year_centered | source_domain)"))
  model_4 <- lmer(formula_4, data = corpus)
  
  # Store models in the results list
  LinReg_BIG4[[var]] <- list(model_1 = model_1, model_2 = model_2, model_3 = model_3, model_4 = model_4)
}

# to access a specific model: 
LinReg_BIG4$words_n$model_4

# save model list as rds
saveRDS(LinReg_BIG4, file="/mnt/home/nickl/Users/NicklPietro/PaperIIwb/statistical_analysis/BIG4_lme4_lin.rds")


#########
######### do the same for NOW

NOW_LinReg <- list()


corpus <- NOW_consistent#NOW_raw # NOW_clean
corpus_name <- deparse(substitute(NOW_consistent))#NOW_raw)) #NOW_clean))

for (var in cont_vars) {
  
  # Construct the model formulas dynamically
  formula_1 <- as.formula(paste(var, "~ 1"))
  model_1 <- lm(formula_1, data = corpus)
  
  formula_2 <- as.formula(paste(var, "~ 1 + year_centered"))
  model_2 <- lm(formula_2, data = corpus)
  
  formula_3 <- as.formula(paste(var, "~ 1 + year_centered + (1 | source_domain)"))
  model_3 <- lmer(formula_3, data = corpus)
  
  formula_4 <- as.formula(paste(var, "~ 1 + year_centered + (1 + year_centered | source_domain)"))
  model_4 <- lmer(formula_4, data = corpus)
  
  # Store models in the results list
  NOW_LinReg[[var]] <- list(model_1 = model_1, model_2 = model_2, model_3 = model_3, model_4 = model_4)
}

# save model list as rds
filename <- paste0("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/statistical_analysis/", corpus_name, "_lme4_lin.rds")
saveRDS(NOW_LinReg, filename)


##### and the same for ARXIV
#####


ARXIV_LinReg <- list()

corpus = ARXIV

for (var in cont_vars) {
  
  # Construct the model formulas dynamically
  formula_1 <- as.formula(paste(var, "~ 1"))
  model_1 <- lm(formula_1, data = corpus)
  
  formula_2 <- as.formula(paste(var, "~ 1 + year_centered"))
  model_2 <- lm(formula_2, data = corpus)
  
  # Store models in the results list
  ARXIV_LinReg[[var]] <- list(model_1 = model_1, model_2 = model_2)
}

# to access a specific model: 
ARXIV_LinReg$words_n$model_2

# save model list as rds
saveRDS(ARXIV_LinReg, file="/mnt/home/nickl/Users/NicklPietro/PaperIIwb/statistical_analysis/ARXIV_lme4_lin.rds")
