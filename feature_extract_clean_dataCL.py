''' 
This script, extract_features_clean_data, takes the clean data as input and 
extracts some linguistic features from it. The script is complementary
to the sentiment_analyse_clean_data and syntax_analyse_clean_data.
The present script focuses on basic whole-headline features (number of characters)
and word-level features (number of words, contains word from a specific word 
class etc.). Therefore, many steps of this script rely on tokenisation of the
headline into words, and focus on these, whereas the syntax and sentiment analysis scripts 
take the whole headline as input. 
'''

#%%
# import the clean_data
# from tqdm import tqdm #for progress_apply
# tqdm.pandas() #for progress_apply
 

import pandas as pd 
import sys
import os 
from datetime import datetime

if len(sys.argv) != 2:
    print("Usage: python3 feature_extract_clean.py <filepath>")
    sys.exit(1)

filepath = sys.argv[1]
df = pd.read_csv(filepath)

# Extract the base name of the file (without path)
base_name = os.path.basename(filepath)
df_name = os.path.splitext(base_name)[0]

#%%
# all the functions for extracting simple features 

# count the number of characters in the text 
def count_chars(text):
    return len(str(text))

from nltk import sent_tokenize

# count sentences
def count_sentences(headline):
    return len(sent_tokenize(headline))

# tokenize the headlines into words and punctuation
from nltk.tokenize import TweetTokenizer

tweet_tokenizer = TweetTokenizer()

def tokenize(headline): 
    if type(headline) == str: #apparently some of them aren't strings...
        tokenized_headline = tweet_tokenizer.tokenize(headline)
        return tokenized_headline


# remove punctuation from tokens and lowercase them, leaving only lower_words
import string 
string.punctuation

def remove_punct_and_lowercase(text): 
    lower = [token.lower() for token in text]
    no_punctuation = [token for token in lower if token not in string.punctuation] 
    return no_punctuation

# get average word length
def get_average_word_length(tokens):
    lengths = [len(token) for token in tokens]
    
    if lengths:
        avg_word_length = sum(lengths) / len(tokens)
        
    else:
        avg_word_length = None
    return avg_word_length

# count stopwords
from nltk.corpus import stopwords
# nltk.download('stopwords') #make sure the stopwords corpus is downloaded

nltk_stopwords = set(stopwords.words('english')) #get the stopwords as a set, makes checking quicker

def count_stopwords(headline): 
    stopwords = [token for token in headline if token in nltk_stopwords]
    return len(stopwords)

def contains_uppercase_word(tokens): # checks for uppercase
    return any(token.isupper() and len(token) > 1 for token in tokens)

def contains_question_mark(tokenized_nltk): # checks for presence of at least one question mark
    return "?" in tokenized_nltk

def contains_exclamation_mark(tokenized_nltk): 
    return "!" in tokenized_nltk

def contains_dot(tokenized_nltk): 
    return "." in tokenized_nltk

def contains_colon(tokenized_nltk): 
    return ":" in tokenized_nltk

def contains_n_dash(tokenized_nltk): 
    return "-" in tokenized_nltk

def contains_m_dash(tokenized_nltk): 
    return "–" in tokenized_nltk

def contains_number(headline):
    return bool(re.search(r'\b\d+\b', headline))

import re

def contains_double_quote(text):
    # Check for paired double quotes at word boundaries or start/end of the string
    pattern = r'(\b"|^").*?("\b|"$)'
    if re.search(pattern, text):
        return True
    else:
        return False

# single quotes are trickier because two apostrophes in a text might be interpreted as a quote
def contains_single_quote(text):
    # Check for paired single quotes at the start and end of words or strings
    pattern = r"(?<!\w)'(.*?)'(?!\w)"
    if re.search(pattern, text):
        return True
    else:
        return False


#%%

# these functions all take the whole string as input
df['clean_text'] = df['clean_text'].apply(str)
df['char_n'] = df['clean_text'].apply(count_chars)
df['sent_n'] = df['clean_text'].apply(count_sentences)
df['tokens'] = df['clean_text'].apply(tokenize)
df['double_quote'] = df['clean_text'].apply(contains_double_quote)
df['single_quote'] = df['clean_text'].apply(contains_single_quote)
df['number'] = df['clean_text'].apply(contains_number)


# these functions all take the tokens as input (which contains punctuation
# and preserves uppercase)
df['words_lower'] = df['tokens'].apply(remove_punct_and_lowercase)
df['uppercase'] = df['tokens'].apply(contains_uppercase_word)
df['question_mark'] = df['tokens'].apply(contains_question_mark)
df['exclamation_mark'] = df['tokens'].apply(contains_exclamation_mark)
df['dot'] = df['tokens'].apply(contains_dot)
df['colon'] = df['tokens'].apply(contains_colon)
df['n_dash'] = df['tokens'].apply(contains_n_dash)
df['m_dash'] = df['tokens'].apply(contains_m_dash)


# these functions take the lowercase words as input
df['words_n'] = df['words_lower'].apply(len)
df['avg_word_len'] = df['words_lower'].apply(get_average_word_length)
df['stopwords_n'] = df['words_lower'].apply(count_stopwords)

print(datetime.now().strftime("%H:%M:%S"), " extracted first_pass features")

#%%
import spacy
import en_core_web_sm
nlp = spacy.load('en_core_web_sm')


def get_pos_tags(sentence):
    doc = nlp(sentence)
    return [(token.text, token.tag_) for token in doc]

def get_nouns(pos_tags):
    nouns = [pos_tag[0] for pos_tag in pos_tags if pos_tag[1].startswith('N')]
    return nouns

def get_pronouns(pos_tags):
    pronouns = [pos_tag[0] for pos_tag in pos_tags if pos_tag[1].startswith('PRP')]
    return pronouns

def get_verbs(pos_tags):
    verbs = [pos_tag[0] for pos_tag in pos_tags if pos_tag[1].startswith('V')]
    return verbs
    
def get_adjs(pos_tags):
    adjs = [pos_tag[0] for pos_tag in pos_tags if pos_tag[1].startswith('J')]
    return adjs 

def get_advs(pos_tags):
    advs = [pos_tag[0] for pos_tag in pos_tags if pos_tag[1].startswith('RB')]
    return advs
      
def get_superlatives(pos_tags):    
    superlatives = [pos_tag[0] for pos_tag in pos_tags if pos_tag[1] == 'JJS'or pos_tag[1] == 'RBS'] # adjective and adverbial superlative 
    return superlatives 

def get_wh_words(pos_tags):
    wh_words = [pos_tag[0] for pos_tag in pos_tags if pos_tag[1].startswith('W')]
    return wh_words

def get_interjections(pos_tags):
    interjections = [pos_tag[0] for pos_tag in pos_tags if pos_tag[1] == 'UH']
    return interjections

def get_dets(pos_tags):
    dets = [pos_tag[0] for pos_tag in pos_tags if pos_tag[1] == 'DT']
    return dets
#%%
def contains_noun(pos_tags):
    return any(pos_tag[1].startswith('N') for pos_tag in pos_tags)

def contains_pronoun(pos_tags):
    return any(pos_tag[1].startswith('PRP') for pos_tag in pos_tags)

def contains_verb(pos_tags):
    return any(pos_tag[1].startswith('V') for pos_tag in pos_tags)
    
def contains_adj(pos_tags):
    return any(pos_tag[1].startswith('J') for pos_tag in pos_tags)

def contains_adv(pos_tags):
    return any(pos_tag[1].startswith('RB') for pos_tag in pos_tags)
      
def contains_superlative(pos_tags):    
    return any(pos_tag[1].startswith('JJS') for pos_tag in pos_tags)

def contains_wh_word(pos_tags):
    return any(pos_tag[1].startswith('W') for pos_tag in pos_tags)

def contains_interjection(pos_tags):
    return any(pos_tag[1].startswith('UH') for pos_tag in pos_tags)

def contains_det(pos_tags):
    return any(pos_tag[1].startswith('DT') for pos_tag in pos_tags)

def contains_def_article(pos_tags):
    for word, tag in pos_tags:
        if tag == 'DT' and word.lower() == 'the':
            return True
    return False

def contains_indef_article(pos_tags):
    for word, tag in pos_tags:
        if tag == 'DT' and word.lower() in ['a', 'an']:
            return True
    return False

#%%

# these functions all take the whole string as input
df['pos_tags'] = df['clean_text'].apply(get_pos_tags)

print(datetime.now().strftime("%H:%M:%S"), " got POS tags")


#%%
# check presence of relevant word classes (adv, noun, pronoun etc.)

df['noun'] = df['pos_tags'].apply(contains_noun)
df['pronoun'] = df['pos_tags'].apply(contains_pronoun)
df['verb'] = df['pos_tags'].apply(contains_verb)
df['adj'] = df['pos_tags'].apply(contains_adj)
df['adv'] = df['pos_tags'].apply(contains_adv)
df['superlative'] = df['pos_tags'].apply(contains_superlative)
df['wh_word'] = df['pos_tags'].apply(contains_wh_word)
df['interjection'] = df['pos_tags'].apply(contains_interjection)
df['det'] = df['pos_tags'].apply(contains_det)
df['def_article'] = df['pos_tags'].apply(contains_def_article)
df['indef_article'] = df['pos_tags'].apply(contains_indef_article)

print(datetime.now().strftime("%H:%M:%S"), " got word types")


# extract relevant word classes (adv, noun, pronoun etc.) for wordclouds (?)

#%%
# also record which specific pronouns occur
pronoun_dict = { 'I': ["I", "me", "my", "mine", "myself"], 
                'you': ["you", "your", "yours", "yourself", "yourselves"], 
                'he': ["he", "him", "his", "himself"],
                'she': ["she", "her", "hers", "herself"],
                'it': ["it", "its", "itself"],
                'they': ["they", "them", "their", "theirs", "themselves", "themself"]
               }

# add new columns to df
for pronoun, forms in pronoun_dict.items():
    df[pronoun] = df['words_lower'].apply(lambda x: any(form in x for form in forms))

print(datetime.now().strftime("%H:%M:%S"), " got pronouns")

#%%

# Readability / Complexity
# Gunning-Fog
# Flesch Reading Ease

# get Flesch Reading Ease
import textstat

def get_FRE(text):
    return textstat.flesch_reading_ease(text)

def get_GFI(text):
    return textstat.gunning_fog(text)


df['FRE'] = df['clean_text'].apply(get_FRE)
df['GFI'] = df['clean_text'].apply(get_GFI)

print("got readability scores")

#%% export as csv
    
df.to_csv(f'/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/features/{df_name}_features.csv')
print("saved")

# %%
#if __name__ == "__main__":
#    main()