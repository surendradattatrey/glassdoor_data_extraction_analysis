# -*- coding: utf-8 -*-
"""
Created on Mon Dec  2 20:13:45 2019

@author: surendra_dattatrey
"""

import pandas as pd
from nltk.corpus import stopwords
from nltk.corpus import wordnet 
from nltk.stem.wordnet import WordNetLemmatizer
import string
from nltk.stem import PorterStemmer
from nltk.tokenize import sent_tokenize, word_tokenize
from nltk.tokenize import RegexpTokenizer
from nltk.tokenize import RegexpTokenizer
import nltk
import pandas as pd
import numpy as np
import re
from nltk.corpus import wordnet 
from fuzzywuzzy import fuzz

matched_entities = []

####main file read#######
main_file=pd.read_excel("C:\\Users\\surendra_dattatrey\\Desktop\\S&P Data\\Glassdoor\\Intermediate files\\S&P reviews.xlsx",encoding = "ISO-8859-1")  #,sheet="Sheet1"
df=main_file.dropna()
df['answer_re'] = df['Pros'].str.replace('\d+', '')
#df['answer_re'] = df['answer_re'].map(lambda x: re.sub(r'\W+', '', x))
df['answer_re_oth'] = df['answer_re'].str.replace('\W', ' ')
df['answer_re_oth'] = df['answer_re_oth'].str.lower()
df_new = df[df['answer_re'].notnull()]

df_new = df_new.replace('\s+', ' ', regex=True)

word_df=pd.read_excel("C:\\Users\\surendra_dattatrey\\Desktop\\S&P Data\\Glassdoor\\keywords\\top ngrams.xlsx")

def word_finder(x):
  df_words = set(x.split(' '))
  extract_words =  word_set.intersection(df_words)
  return ', '.join(extract_words)

#df = pd.read_excel(r'C:\Users\shruti_goel\Desktop\Surender_Work\review.xlsx',encoding = "ISO-8859-1")
#word_df = pd.read_excel(r'C:\Users\shruti_goel\Desktop\Surender_Work\words.xlsx',encoding = "ISO-8859-1")
word_set = set(word_df['words'])
word_set = {str(r) for r in word_set}
df_new['answer_re_n'] = df_new.answer_re.apply(word_finder)

df_new.to_csv("C:\\Users\\surendra_dattatrey\\Desktop\\S&P Data\\Glassdoor\\Intermediate files\\S&P reviews_withwords.csv", sep=',')
