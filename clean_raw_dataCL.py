#%%
'''
1) this script wrangles the different datasets into the same format:
at the end we get a dataframe for each corpus with the columns
'text' string containing the headline (or title in the ARXIV corpus)
'year' as an int
'outlet' string referring to the website, platform or wherever the text is from


2) this script cleans the data so we can continue with NLP
'''
#!pip3 install pandas
import pandas as pd 
from datetime import datetime
from tqdm import tqdm
tqdm.pandas()
#%%

#NYT = pd.read_csv('/Volumes/Users/NicklPietro/PaperIIwb/raw_data/NYT_headlines_2000_2022.csv')
#Guardian = pd.read_csv('/Volumes/Users/NicklPietro/PaperIIwb/raw_data/Guardian_headlines_2000_2022.csv')
#ABC = pd.read_csv('/Volumes/Users/NicklPietro/PaperIIwb/raw_data/abcnews-date-text.csv')
#TOI = pd.read_csv('/Volumes/Users/NicklPietro/PaperIIwb/raw_data/india-news-headlines.csv')
#NOW = pd.read_csv('/Volumes/Users/NicklPietro/PaperIIwb/raw_data/NOW_sources_2010_2023.csv')
NOW = pd.read_csv('u:\\NicklPietro\\PaperIIwb\\NOW_sources_2010_2023.csv')
#upworthy = pd.read_csv('/Volumes/Users/NicklPietro/PaperIIwb/raw_data/upworthy-archive-exploratory-packages-03.12.2020.csv')
#ARXIV = pd.read_csv('/Volumes/Users/NicklPietro/PaperIIwb/raw_data/arXiv_2007_2023.csv')

#%%
# rename the column containing the text (headline/sentence/title ) "text" 
#NYT = NYT.rename(columns = {'0' : 'raw_text'}) 
#Guardian = Guardian.rename(columns = {'0' : 'raw_text'}) 
#ABC = ABC.rename(columns = {'headline_text' : 'raw_text'}) 
#TOI = TOI.rename(columns = {'headline_text' : 'raw_text'}) 
NOW = NOW.rename(columns = {'title' : 'raw_text'}) 
#upworthy = upworthy.rename(columns = {'headline' : 'raw_text'}) 
#ARXIV = ARXIV.rename(columns = {'title' : 'raw_text'}) 

#%%
# extract the year information as int and name the column "year"
# this does not apply to ARXIV and upworthy data
# upworthy does not span a wide enough range 
# we also use ARXIV as a static reference point for "scientific titles"
# but there the corpus spans such a wide range that it is interesting to
# plot the development over time 

# upworthy doesn't get a 'year' col 

# NYT already has 'year' col
# Guardian already has 'year' col
# ARXIV already has 'year' col

#ABC['date'] = ABC['publish_date'].progress_apply(lambda x: pd.to_datetime(x, format='%Y%m%d'))
#ABC['year'] = ABC['date'].progress_apply(lambda x: x.year)

#TOI['date'] = TOI['publish_date'].progress_apply(lambda x: pd.to_datetime(x, format='%Y%m%d'))
#TOI['year'] = TOI['date'].progress_apply(lambda x: x.year)

NOW['date'] = NOW['date'].progress_apply(lambda x: pd.to_datetime(x, format='%y-%m-%d'))
NOW['year'] = NOW['date'].progress_apply(lambda x: x.year)


#%%
# add a column recording the outlet
# for the NOW corpus, which combines several sites, 
# we simply rename the "site" column to "outlet" for consistency

# NYT already has col 'outlet'
#Guardian = Guardian.assign(outlet='Guardian')
#ABC = ABC.assign(outlet='ABC')
#TOI = TOI.assign(outlet='TOI')
NOW = NOW.rename(columns = {'site' : 'outlet'}) 
#upworthy = upworthy.assign(outlet='upworthy')
#ARXIV = ARXIV.assign(outlet='ARXIV')

# %%
# clean the text column
from bs4 import BeautifulSoup #pip install beautifulsoup4
import re
# Function to remove HTML tags
def remove_html_tags_bs(text):
    soup = BeautifulSoup(text, 'html.parser')
    return soup.get_text()

def remove_escape_characters(text):
    for char in ['\n', '\t', '\r']:
        text = text.replace(char, '')
    return text

def remove_double_whitespace(text):
    return re.sub(r'\s+', ' ', text)



# %%
# do this for each corpus

df = NOW
name = "NOW_2010_2023"
# %%

df['raw_text'] = df['raw_text'].astype(str)
# %%

df['clean_text'] = df['raw_text'].progress_apply(remove_html_tags_bs)
# %%

df['clean_text'] = df['clean_text'].progress_apply(remove_escape_characters)
# %%

df['clean_text'] = df['clean_text'].progress_apply(remove_double_whitespace)
# %%

#df.to_csv(f'/Volumes/Users/NicklPietro/PaperIIwb/clean_data/{name}_full.csv')
df.to_csv(f'u:\\NicklPietro\\PaperIIwb\\clean_data\\{name}_full.csv')
# functions to check if they are indeed clean
# %%
for year in df['year'].unique():
    # Filter the DataFrame for the specific year
    df_year = df[df['year'] == year]

    # Define the file path and name
    file_path = f"u:\\NicklPietro\\PaperIIwb\\clean_data\\NOW_full_{year}_.csv"

    # Export to CSV
    df_year.to_csv(file_path, index=False)

    print(f"Exported {year} data to {file_path}")
