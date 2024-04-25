# plot words_n over time with groupings by type of material
# plot words_n over time with groupings by section
library(tidyverse)

# combine NYT_features, NYT_syntax, NYT_sentiment
#NYT_features <- read_csv('/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/features/all_NYT_2000_onwards_features.csv', na = "")
#NYT_syntax <- read_csv('/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/syntax/all_NYT_2000_onwards_root.csv')
#NYT_sentiment <- read_csv('/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/sentiment/all_NYT_2000_onwards_flair.csv', na = "")


#NYT <- NYT_features %>%
#  left_join(NYT_sentiment %>% select("Unnamed: 0.2", sentiment, score), by = "Unnamed: 0.2") %>%
#  left_join(NYT_syntax %>% select("Unnamed: 0.2", tree, root), by = "Unnamed: 0.2")
#saveRDS(NYT, file = "/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NYT_2000_onwards.rds")
NYT <- readRDS("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/NYT_2000_onwards_plotting.rds")

# plot by type of material

type_freq <- table(NYT$type_of_material) # Create a frequency table of type of material

sorted_type_freq <- sort(type_freq, decreasing = TRUE) # Sort the table in decreasing order of frequency

filtered_type_freq <- sorted_type_freq[sorted_type_freq >= 10000] # filter to keep only those with more than 1000 occurrences

types_over_10000 <- names(filtered_type_freq) # Get the names of types that occur more than 10000 times

NYT_type_of_material <- NYT[NYT$type_of_material %in% types_over_10000, ] # Filter the dataframe to only include those types


# plot by section (only the most common ones)
section_freq <- table(NYT$section_name)

# Sort the table in decreasing order of frequency
sorted_section_freq <- sort(section_freq, decreasing = TRUE)

# Filter to keep only those with more than 1000 occurrences
filtered_section_freq <- sorted_section_freq[sorted_section_freq >= 10000]

# Get the names of sections that occur more than 10000 times
sections_over_10000 <- names(filtered_section_freq)

# Filter the dataframe to only include those sections
NYT_sections <- NYT[NYT$section_name %in% sections_over_10000, ]

# how do sections and types of material relate to each other?

NYT_grouped_by_material <- NYT %>%
  group_by(type_of_material) %>%
  summarise(count = n()) %>%
  filter(count >= 10000) %>%
  arrange(desc(count))

NYT_grouped_by_section <- NYT %>%
  group_by(section_name) %>%
  summarise(count = n()) %>%
  filter(count >= 10000) %>%
  arrange(desc(count))

# include word_count in continuous features 
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

cont_vars <- c("char_n",
               "words_n",
               "avg_word_len",
               "FRE",
               "GFI",
               "word_count") #this is just available for the NYT 

vars <- c(binary_vars, cont_vars)

#vars <-c("you", "words_n") # for testing purposes: just try two


corpus <- NYT_type_of_material
corpus_name <- "Type of Material"
corpus_file_name <- "type_of_material"

corpus <- NYT_sections
corpus_name <- "Section"
corpus_file_name <- "section_name"

for (var in vars) {
  
  # create a directory if it doesn't exist yet
  dirname <- paste0("/mnt/home/nickl/Users/NicklPietro/PaperIIwb/plotting/final_figures/figure_4/", corpus_file_name, '/', var)
  if (!dir.exists(dirname))
  {
    dir.create(dirname, recursive = TRUE)
  }
  
  corpus_summary <- corpus %>%
    group_by(year) %>%
    summarise(
      mean = mean(!!sym(var), na.rm = TRUE),
      sd = sd(!!sym(var), na.rm = TRUE),
      lower = mean - sd,  # Lower bound for 1 standard deviation
      upper = mean + sd)   # Upper bound for 1 standard deviation
    
  subgroup_means <- corpus %>%
    group_by(!!sym(corpus_file_name), year) %>%
    summarise(
      doc_mean = mean(!!sym(var), na.rm = TRUE))
  
  plot <- ggplot() + 
    geom_line(data = corpus_summary, aes(x = year, y = mean,  linewidth = 1.2), color = "black") +
    #geom_ribbon(data = corpus_summary, aes(x = year, ymin = lower, ymax = upper), alpha = 0.1) +
    geom_line(data = subgroup_means, 
              aes(x = year, y = doc_mean, color = !!sym(corpus_file_name), linewidth = ifelse(!!sym(corpus_file_name) == "News", 1.1, 1))) +  
    #coord_cartesian(ylim = c(0, 20)) +
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
    labs(#color = corpus_name, 
         y = NULL, #"Words Count in article", 
         x = NULL) #+#, "Year",
         #title = paste(var, " across years")) +
    #guides(size = NULL)  # Disable the size legend
  
  if (var %in% binary_vars) {
    plot <- plot + ylim(0, 1)# + labs(caption = paste0("Proportion of headlines featuring ", var, " over Years"))
  }
  
  print(plot)
  
  # save plot 
  filepath <- paste0(dirname, "/", corpus_file_name, "_", var, ".jpg")
  ggsave(filepath, plot = plot, width = 10, height = 7)
}


corpus_summary <- filtered_df %>%
  group_by(year) %>%
  summarise(
    mean = mean(words_n, na.rm = TRUE),
    sd = sd(words_n, na.rm = TRUE),
    lower = mean - sd,  # Lower bound for 1 standard deviation
    upper = mean + sd   # Upper bound for 1 standard deviation
  )

subgroup_means <- filtered_df %>%
  group_by(section_name, year) %>%
  summarise(
    doc_mean = mean(words_n, na.rm = TRUE)
  )

ggplot() + 
  geom_line(data = subgroup_means, aes(x = year, y = doc_mean, color = section_name)) + #, size = ifelse(section_name %in% c("Opinion", "Sports", "U.S.", "World", "Business Day", "Fashion & Style"), 1, 0.5))) +
  geom_line(data = corpus_summary, aes(x = year, y = mean), color = "black", linewidth = 1.5) +
  geom_ribbon(data = corpus_summary, aes(x = year, ymin = lower, ymax = upper), alpha = 0.1) +
  coord_cartesian(ylim = c(0, 20)) +
  theme_minimal() +
  labs(color = "section name", y = "Words Count", x = "Year",
       title = "Yearly Words Count by section name") #+
#theme(legend.title = element_text(size = 8),  # Smaller legend title
#      legend.text = element_text(size = 6),   # Smaller legend text
#      legend.key.size = unit(0.1, "cm"))        # Smaller legend keys


# Define the subset of section names you're interested in
sections_to_plot <- c("Opinion", "Sports", "U.S.", "World", "Business Day", "Fashion & Style", "Real Estate", "Corrections")

ggplot(data = corpus_summary, aes(x = year, y = mean)) +
  
  geom_line(data = filter(merged_data, section_name %in% sections_to_plot), 
            aes(x = year, y = doc_mean, color = section_name)) +
  geom_line(data = corpus_summary, aes(x = year, y = mean), color = "black", linewidth = 1.5) +
  geom_ribbon(data = corpus_summary, aes(x = year, ymin = lower, ymax = upper), alpha = 0.1) +
  coord_cartesian(ylim = c(0, 20)) +
  theme_minimal() +
  labs(color = "section name", 
       y = "Words Count", 
       x = "Year",
       title = "Yearly Words Count by section name") #+
#theme(legend.title = element_text(size = 8),  # Smaller legend title
#      legend.text = element_text(size = 6),   # Smaller legend text
#      legend.key.size = unit(0.1, "cm")) +       # Smaller legend keys
#guides(size = "none")  # Disable the size legend

# Opinion, Sports, U.S., World, Business Day, Sports, Stoy,e Fashion & Style




unique_combos <- NYT %>% 
  group_by(type_of_material, section_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))  # arranging in descending order based on count

print(unique_combos, n = 20)

# maybe I should only print the news? not order by type
only_news <- NYT %>%
  filter(type_of_material == "News")

type_freq <- table(only_news$section_name)

# Sort the table in decreasing order of frequency
sorted_type_freq <- sort(type_freq, decreasing = TRUE)

# Filter to keep only those with more than 1000 occurrences
filtered_type_freq <- sorted_type_freq[sorted_type_freq > 10000]

# Get the names of types that occur more than 10000 times
types_over_10000 <- names(filtered_type_freq)

# Filter the dataframe to only include those types
filtered_df <- only_news[only_news$section_name %in% types_over_10000, ]


###
corpus_summary <- filtered_df %>%
  group_by(year) %>%
  summarise(
    mean = mean(words_n, na.rm = TRUE),
    sd = sd(words_n, na.rm = TRUE),
    lower = mean - sd,  # Lower bound for 1 standard deviation
    upper = mean + sd   # Upper bound for 1 standard deviation
  )

subgroup_means <- filtered_df %>%
  group_by(section_name, year) %>%
  summarise(
    doc_mean = mean(words_n, na.rm = TRUE)
  )

# real estate is weird
ggplot() + 
  geom_line(data = corpus_summary, aes(x = year, y = mean), color = "black", linewidth = 1.5) +
  geom_ribbon(data = corpus_summary, aes(x = year, ymin = lower, ymax = upper), alpha = 0.1) +
  geom_line(data = subgroup_means, aes(x = year, y = doc_mean, color = section_name, 
                                       linewidth = ifelse(section_name %in% c("Real Estate"), 1.5, 1))) +
  
  #size = ifelse(section_name %in% c("Opinion", "Sports", "U.S.", "World", "Business Day", "Fashion & Style"), 1.5, 1))) +
  coord_cartesian(ylim = c(0, 20)) +
  theme_minimal() +
  labs(color = "section name", y = "Words Count", x = "Year",
       title = "Yearly Words Count by section name") +
  guides(size = "none")  # Disable the size legend

# Real Estate is the outlier here, I'm not sure why, but it is clearly an outlier


# to find out what subcategories the "News" section and other sections have,
# I group by news and then count the different subsections 

library(dplyr)

only_news <- only_news %>%
  mutate(
    has_print_section = ifelse(is.na(print_section), 0, 1),
    has_print_page = ifelse(is.na(print_page), 0, 1)
  )


# Calculate mean of words_n for each year and has_print_section
summary_data <- only_news%>%
  group_by(year, has_print_section) %>%
  summarise(mean_words_n = mean(words_n, na.rm = TRUE)) %>%
  ungroup()

# Plotting the means
ggplot(summary_data, aes(x = year, y = mean_words_n, color = as.factor(has_print_section))) +
  geom_line() +
  scale_color_manual(values = c("blue", "red"), labels = c("No Print Section", "Has Print Section")) +
  coord_cartesian(ylim = c(0, 20)) +
  theme_minimal() +
  labs(color = "Print Section", y = "Average Words Count", x = "Year",
       title = "Yearly Average Words Count by Print Section Presence")


## actually having a print section / page does NOT make the article headline shorter
# at the same time, we don't know if the article appeared with the same headline 

# Calculate mean of words_n for each year and has_print_section
summary_data <- NYT %>%
  group_by(year, has_print_page) %>%
  summarise(mean_words_n = mean(words_n, na.rm = TRUE)) %>%
  ungroup()

# Plotting the means
ggplot(summary_data, aes(x = year, y = mean_words_n, color = as.factor(has_print_page))) +
  geom_line() +
  scale_color_manual(values = c("blue", "red"), labels = c("has_print_page == 0", "has_print_page == 1")) +
  coord_cartesian(ylim = c(0, 20)) +
  theme_minimal() +
  labs(color = "Print Section", y = "Average Words Count", x = "Year",
       title = "Yearly Average Words Count by Print Page Presence")

###
library(dplyr)
library(tidyr)

# Assuming your dataframe is named df
# and "section name" and "has print page" are the exact column names

# Create a contingency table
contingency_table <- NYT %>%
  group_by(section_name, has_print_page) %>%
  summarise(count = n(), .groups = 'drop') %>%
  spread(has_print_page, count, fill = 0)

# View the contingency table
print(contingency_table)


contingency_table <- NYT %>%
  group_by(type_of_material, has_print_page) %>%
  summarise(count = n(), .groups = 'drop') %>%
  spread(has_print_page, count, fill = 0)

# View the contingency table
print(contingency_table)

