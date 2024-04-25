''' 
This script, sentiment_analyse_clean_data, takes the clean data as input and 
gives sentiment analysis scores. 
It uses flair, a state-of-the-art sentiment analysis tool
TextBlob, PySenti, VADER and NRC (?)
'''

#%%
# import the clean_data
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

#%% flair

from flair.nn import Classifier
from flair.data import Sentence

# load the model
tagger = Classifier.load('sentiment')

def get_flair_sentiment(text):
    try:
        sentence = Sentence(text)

        # predict NER tags
        tagger.predict(sentence)

        return (sentence.tag, # POSITIVE or NEGATIVE
        sentence.score) # a value between 0 and 1, this value is ALWAYS positive
    except Exception as e:
        print(f"Error processing text: '{text}'")
        print(f"Error: {e}")
        return None, None


#%%
# all the dataframes with flair  
# extract both the sentiment, and the score. Scores are always positive values between 0 and 1.
# There is no neutral sentiment class, meaning all headlines will be categorised as either positive or negative
# The model seems overconfident. The score will be important later to set a threshold for what we want to count as
# positive or negative 

df['clean_text'] = df['clean_text'].apply(str)
df[['sentiment', 'score']] = df['clean_text'].apply(get_flair_sentiment).apply(pd.Series)

df.to_csv(f'/mnt/home/nickl/Users/NicklPietro/PaperIIwb/results/{df_name}_flair.csv')

