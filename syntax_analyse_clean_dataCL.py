'''
syntax_analyse_clean_data
open questions: the constituency parser seems to be doing better work than the spacy POS tagger.
should I use the leaves (terminal nodes) from the constituency parser instead of the spacy POS tagger?
Philipp: get the whole tree and save in the dataframe, then save as a pickle. 

Consituency parsing is one way of capturing syntactic structure. 
Constituents are analytically complex, linguistically nontrivial but very intuitive ways to chunk up sentences
into smaller parts. 
A constituent is any contiguous part of a sentence that you can ask about. To give an example:
The constituent tree may look very arcane at first, but the underlying intuition is simple. 
Constituents work especially well for English, as it is a fixed-word order language,
where word order plays an important grammatical role (e.g. to mark grammatical case).
This means that syntactic rules are rather strict in English. 


this script works in two steps:
1) parse all the headlines with space and benepar, save this as a df / pickle
2) then extract different syntactic constituent structure from it.

This has one clear advantage: parsing using spaCy is the most time-intensive step.
But this step is the first the first step for many other questions we can ask
about constituency. In order not to repeat it, I save the output of the parsing in a
df / pickle so we don't have to run the step again. 

On the downside, pickle is not a portable format. Also, it is less straightforward.

'''

#%%
# import the clean_data

import pandas as pd 
import sys
import os 
from datetime import datetime
from tqdm import tqdm #for progress_apply
tqdm.pandas() #for progress_apply

if len(sys.argv) != 2:
    print("Usage: python3 feature_extract_clean.py <filepath>")
    sys.exit(1)

filepath = sys.argv[1]
df = pd.read_csv(filepath)

# Extract the base name of the file (without path)
base_name = os.path.basename(filepath)
df_name = os.path.splitext(base_name)[0]


import nltk

def ensure_nltk_resources():
    try:
        nltk.data.find('tokenizers/punkt')
        nltk.data.find('corpora/stopwords')
    except LookupError:
        nltk.download('punkt')
        nltk.download('stopwords')

# download resources that cannot be installed via pip
ensure_nltk_resources()


#%%

import benepar, spacy, nltk 
import en_core_web_sm
#benepar.download('benepar_en3')
#nlp = en_core_web_sm.load()
nlp = spacy.load('en_core_web_sm')
nlp.add_pipe('benepar', config={'model': 'benepar_en3'})

#%%

df['clean_text'] = df['clean_text'].apply(str)
#df['parsed_doc'] = df['clean_text'].apply(nlp)
#df.to_pickle(f'/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/syntax/{df_name}_syntax.pkl')

#def extract_tree_and_root(sentence):
#    doc = nlp(sentence)
#    tree_structures = []
#    roots = []
#    for sent in doc.sents:
#        tree_structures.append(sent._.parse_string)
#        roots.append(sent._.labels[0])
#    return tree_structures, roots

# this would be even better: 
'''
def extract_tree_and_root(sentence):
    try:
        # Check for sentence length
        if len(sentence.split()) > 512:  # Adjust this limit as needed
            print(f"Skipping long sentence: '{sentence[:50]}...'")
            return [], []

        doc = nlp(sentence)
        tree_structures = [sent._.parse_string for sent in doc.sents]
        roots = [sent._.labels[0] for sent in doc.sents]
        return tree_structures, roots
    except Exception as e:
        print(f"Error processing sentence: '{sentence}'")
        print(f"Error: {e}")
        return [], []
'''

def extract_tree_and_root(sentence):
    try:
        doc = nlp(sentence)
        tree_structures = [sent._.parse_string for sent in doc.sents]
        roots = [sent._.labels[0] for sent in doc.sents]
        return tree_structures, roots
    except Exception as e:
        print(f"Error processing sentence: '{sentence}'")
        print(f"Error: {e}")
        return [], []

# Applying the function and assigning the results to two columns
df[['tree', 'root']] = df['clean_text'].progress_apply(extract_tree_and_root).apply(pd.Series)

df.to_csv(f'/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/syntax/{df_name}_root.csv')

#%% the functions below take the clean_text as input and output different kinds of 
# readily interpretable constituency information. They all share the first step
#   "doc = nlp(sentence)", which is quite time-consuming. I therefore outsource this
# first step above and process the doc further below
'''
#
def get_root_V2(sentence):
    doc = nlp(sentence)
    root = []
    for sent in doc.sents:
        # Index into the tuple and append the first element
        root.append(sent._.labels[0])
    return root


def get_terminal_labels(tree):
    if isinstance(tree, str):  # Terminal node
        return []
    elif len(tree) == 1 and isinstance(tree[0], str):  # Pre-terminal node
        return [tree.label()]
    else:  # Non-terminal node
        labels = []
        for child in tree:
            labels.extend(get_terminal_labels(child))
        return labels

def list_leaves(sentence):
    doc = nlp(sentence)
    leaves = []
    for sent in doc.sents:
        tree = nltk.Tree.fromstring(sent._.parse_string)
        leaves.append(get_terminal_labels(tree))
    return leaves

'''
'''
#%%
def get_root_from_doc(doc):
    root = []
    for sent in doc.sents:
        # Index into the tuple and append the first element
        root.append(sent._.labels[0])
    return root

def extract_tree_structure(sentence):
    doc = nlp(sentence)
    return [sent._.parse_string for sent in doc.sents]

def get_root_and_tree_from_doc(doc):
    root = []
    for sent in doc.sents:
        # Index into the tuple and append the first element
        root.append(sent._.labels[0])
    return root

for name, df in dfs.items():
    df['root'] = df['parsed_doc'].apply(get_root_from_doc)
    df.to_csv(f'/Volumes/Users/NicklPietro/PaperIIwb/results/syntax/{name}_root.csv')
'''

#%%
'''
more analyses at the constituent level would be appropriate and interesting, 
some of which are listed below.
For the moment, I am only interested in the syntactic representation at the 
highest level: Is the whole headline one sentence, two sentences, only part of a sentence, 
a question? 
In a follow-up, it would be rewarding to look deeper at more granuar syntactic 
structures.


import benepar, spacy, nltk 
import en_core_web_sm
#benepar.download('benepar_en3')
#nlp = en_core_web_sm.load()
nlp = spacy.load('en_core_web_sm')
nlp.add_pipe('benepar', config={'model': 'benepar_en3'})

def get_parsed_sentence(sentence):
    doc = nlp(sentence)
    parsed_sentence = []

    for sent in doc.sents:
        parsed_sentence.append(sent._.parse_string)

    return parsed_sentence

# to understand if it is a question you need to look at S or SQ – the topmost tree
# I could go through things hierarchically 
# first look at them on the highest level (S)
# then on the one below
# and the on the node level 
# just get the lowest node level 

# this is almost too acribic: it distinguishes between proper nouns, noun phrases and singular and plural nouns 
# bit tiresome: it distinguishes between NN and NNS, which stands for plural noun

# there is a question of whether we want to keep punctuation or not-
# I guess we could be conservative and keep it for now and delete it later
def get_constituents_under_S(sentence):
    doc = nlp(sentence)
    constituents_under_S = []
    for sent in doc.sents:
        for i in sent._.children:
            constituents_under_S.append(i._.labels) 
    return constituents_under_S




def get_constituents_under_S_from_doc(sentence):
    constituents_under_S = []
    for sent in doc.sents:
        for i in sent._.children:
            constituents_under_S.append(i._.labels) 
    return constituents_under_S

'''
