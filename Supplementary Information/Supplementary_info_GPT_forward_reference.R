### Install Required Packages
library(httr)
library(tidyverse)

#########################
##### GPT prompting #####
#########################
# This code below is adapted from this paper's supplemental
# material : https://osf.io/preprints/psyarxiv/sekf5

# Note: code we are using was adapted by this blog post: https://rpubs.com/nirmal/setting_chat_gpt_R. 
# We highly recommend you read over that blog post in detail if you are stuck at any of these steps 

# Then, put your API key in the quotes below: 
my_API <- # enter your API key here

#The "hey_chatGPT function will help you access the API and prompt GPT 
hey_chatGPT <- function(answer_my_question) {
  chat_GPT_answer <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_API)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      temperature = 0,
      messages = list(
        list(
          role = "user",
          content = answer_my_question
        )
      )
    ),
    config = httr::config(http_version = 1.1) # Force HTTP/1.1
  )
  return(str_trim(content(chat_GPT_answer)$choices[[1]]$message$content))
}


# Read in dataset
data1 <- read_csv("/Users/nickl/Regressions/upworthy_headlines.csv")

data <- head(data1, 50)


# Create a "gpt" column
data$gpt <- NA

# Run a loop over dataset and prompt ChatGPT 
# the prompt contains the definition of "forward reference" given by Blom & Hansen (2014)
# paper (Click bait: Forward-reference as lure in online news headlines), in the introduction.
# I have removed the references in brackets in this prompt, and added this text in the beginning:
# "Classify the title below as containing a forward reference or not. Here is the definition of forward reference we adopt:"
# and this text in the end:
# "Against this background, does this title make use of forward reference? Answer only with 0 or 1, 0 for no and 1 for yes. Here is the title:"

for (i in 1:nrow(data)) {
  #print(i)
  question <- "Classify the title below as containing a forward reference or not. Here is the definition of forward reference we adopt: Forward-reference in headlines occurs in two forms, discourse deixis and cataphora. Forward-referring discourse deixis as reference to forthcoming (parts of the) discourse relative to the current location in the discourse, e.g. ‘This is the best news story you will ever read.’ Cataphors point forward as well, though not at the discourse level, but to a word or a phrase later in the sentence or text, e.g. ‘When he arrived at the crime scene, the journalist interviewed the victim’s wife.’ Here, the pronoun he refers to the postcedent, the journalist, later in the sentence. Here are three more examples of forward reference: 1: This is an A-minus paper? 2: He loves Beatles, menthol cigs..and longs for muscles like Van Damme , 3: This Amazing Kid Died. What He Left Behind is Wondtacular.
  In (1) the pronoun this refers deictically to a forthcoming discourse segment in the full text that must be read or viewed in order for the reader to understand what is being referred to. Thus, the pronoun can be regarded as a sort of teaser, an information gap that must be filled. Similarly, the pronoun he in (2) points forwards, not at a discourse segment, but at a name, and is thus an instance of cataphora. Finally, (3) induces an information gap by the pronouns this and what.
  Against this background, does this title make use of forward reference? Answer only with 0 or 1, 0 for no and 1 for yes. Here is the title:"
  text <- data[i,2]       
  concat <- paste(question, text)
  result <- hey_chatGPT(concat)
  while(length(result) == 0){
    result <- hey_chatGPT(concat)
    #print(result)
  }
  #print(result)
  data$gpt[i] <- result
}


#Take only the first string from gpt and convert to a numeric 
data$gpt <- substr(data$gpt, 1, 1)  
data$gpt <- as.numeric(data$gpt)


#library(writexl) 

#write_xlsx(subset_data, path = "first50forGPT.xlsx")

# load the data I annotated:
# my annotations are labeled "forward_reference"
library(readxl)
data2 <- read_excel("first50forGPT.xlsx")

# join data frames on the 
combined_data <- inner_join(data, data2, by = "headline")

selected_data <- combined_data %>% select(headline, gpt, forward_reference)
write.csv(selected_data, "forward_reference_interrater_table.csv", row.names = FALSE)

agreement_table <- table(combined_data$forward_reference, combined_data$gpt)
print(agreement_table)

disagreement_data <- combined_data %>% 
  filter(forward_reference != gpt)

agreement_count <- sum(diag(agreement_table))
total_count <- sum(agreement_table)
agreement_rate <- agreement_count / total_count
print(agreement_rate) 
# it's only 0.66% 
# many texts recommend 80% agreement as the minimum acceptable interrater agreement

# To follow this up more thoroughly, we would need to repeat this with more headlines,
# more human raters, and perhaps following clearer annotation instructions, while also giving these to the LLM. 
# Note that the definition of forward reference here implies a stricter/smaller
# phenomenon than what Blom & Hansen ultimately lay out, the present definition
# clearly applies to phenomena 1 & 2 in the list Blom & Hansen provide (forward-referring
# demonstrative pronouns and personal pronouns), but does not extend (in my reading)
# to phenomenon 8. (general nouns). 
# For example, the last headline in this small dataset (#50) was classigied by GPT as containing a FR:
# "A Man Falls Down And Cries For Help Twice. The Second Time, My Jaw Drops."
# I agree that this can arguably be construed as a forward reference more generally,
# but not on the basis of the definition given above 
# Similarly, this was classified by GPT as containing a FR, but not by me:
# "Disney Introduces A Gay Couple On A Kids' Show, Confusing Children Everywhere. Wait, No..."
# This could also be a sophisticated form of FR (Wait...no?), but not in the "classic" way outlined above
# Overall, GPT classified most headlines as containing FR. We would need to check whether this is
# a) due to a a kind of yes-bias by GPT to overclassify text generally according to some criterion or
# b) whether most upworthy headlines genuinely contain *some* form of FR, although then a
# clearer definition needs to be given.
# In retrospect, it is unfortunate that the definition here ends with "Finally, (3) induces an information gap by the pronouns this and what."
# although "this" and "what" are not pronouns, demonstratives and wh-words may trigger FR, but this addition here
# widens the definition, and not all demonstratives and wh-words will be FRs.
# An alternative going forward would be to focus more clearly on a specific subset of FR, e.g.,
# only forward-referring demonstratives (this, that) or personal pronouns (she, he, it)
