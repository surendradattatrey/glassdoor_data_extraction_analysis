# -*- coding: utf-8 -*-
"""
Created on Sun Sep  1 13:05:33 2019

@author: shruti_goel
"""

import pandas as pd
from textblob import TextBlob 
import re
from nltk.corpus import stopwords
from nltk.probability import FreqDist
from nltk.tokenize import word_tokenize
#from nltk.stem import PorterStemmer
from nltk.stem.wordnet import WordNetLemmatizer
from nltk import pos_tag, sent_tokenize, word_tokenize, BigramAssocMeasures,\
    BigramCollocationFinder, TrigramAssocMeasures, TrigramCollocationFinder
#import glob


#file_names_list = glob.glob("C:\Users\shruti_goel\Desktop\S&P\New folder\August\VIBE 2018\VIBE 2018\*.csv")
#
#for file_ in file_names_list:
#    
#    print("Working on",file_[71:])
#    text_data = pd.read_csv(file_,encoding='ISO-8859-1')

text_data = pd.read_csv('C:\\Users\\shruti_goel\\Desktop\\S&P\\New folder\\August\\VIBE 2018\\VIBE 2018\\Enablement_tech_2018.csv',encoding='ISO-8859-1')

print(len(text_data))

############################### POLARITY ######################################

text_data[['polarity', 'subjectivity']] = text_data['Text Response'].apply(lambda Text: pd.Series(TextBlob(Text).sentiment))

############# MARKING POSITIVE,NEGATIVE AND NEUTRAL SENTIMENTS ################

text_data['sentiment'] = 'neu'    
text_data['sentiment'][text_data['polarity'] > 0] = 'pos'
text_data['sentiment'][text_data['polarity'] < 0] = 'neg'
text_data['sentiment'][text_data['polarity'] == 0] = 'neu'

#text_data.to_csv(r'C:\Users\shruti_goel\Desktop\S&P\work_environment.csv',encoding = 'utf-8')
####################### TOP 10 WORDS FROM EACH CATEGORY #######################

################################ Cleaning Data ################################
        
text_data['Text Response'] = text_data['Text Response'].replace('[^a-zA-Z]', ' ',regex=True)
text_data['Text Response'] = text_data['Text Response'].str.lower()
stop = stopwords.words('english')
stop = map(str,stop)

user_defined_lst = ['p','would','need','also','like','get','good','one',
                    'better','make','ha','day','dr','look','small','other',
                    "allows","negative","truly","across","daily" ,"basis","last","since", "tech","seem",
                    "feel","new","action","taken","able","actually","anything","done","become","give"]
stop.extend(user_defined_lst)  

                  
#for i in range(len(text_data)):
##    print(i)
#    text_data.loc[0,'Text Response'] = word_tokenize(str(text_data.loc[0,'Text Response']))

text_data['Text Response'] = text_data.apply(lambda row: word_tokenize(str(row['Text Response'])),axis = 1)


lemmatizer = WordNetLemmatizer()

text_data['Text Response'] = text_data['Text Response'].apply(lambda x : [lemmatizer.lemmatize(y) for y in x])
   
text_data['Text Response'] = text_data['Text Response'].apply(lambda x: [item for item in x if item not in stop])
 
for i in range(len(text_data)):
     text_data.loc[i,'Text Response'] = str(" ".join(text_data.loc[i,'Text Response']))





lem = WordNetLemmatizer()

corpus_neu = text_data[text_data['sentiment'] == 'neu']['Text Response'].str.cat(sep=' ')
corpus_neg = text_data[text_data['sentiment'] == 'neg']['Text Response'].str.cat(sep=' ')
corpus_pos = text_data[text_data['sentiment'] == 'pos']['Text Response'].str.cat(sep=' ')


tokenized_word_neu = word_tokenize(corpus_neu)
tokenized_word_neg = word_tokenize(corpus_neg)
tokenized_word_pos = word_tokenize(corpus_pos)

lem_words_neu=[]
for w in tokenized_word_neu:
    lem_words_neu.append(lem.lemmatize(w))
lem_words_neu = map(str,lem_words_neu)
fdist_neu = FreqDist(lem_words_neu)
print(fdist_neu.most_common(10))

lem_words_neg=[]
for w in tokenized_word_neg:
    lem_words_neg.append(lem.lemmatize(w))
lem_words_neg = map(str,lem_words_neg)
fdist_neg = FreqDist(lem_words_neg)
print(fdist_neg.most_common(10))


lem_words_pos =[]
for w in tokenized_word_pos:
    lem_words_pos.append(lem.lemmatize(w))
lem_words_pos = map(str,lem_words_pos)
fdist_pos = FreqDist(lem_words_pos)
print(fdist_pos.most_common(10))


#    7409
import matplotlib.pyplot as plt
    
plt.title('plot for fdist_neg')
fdist_neg.plot(30,cumulative=False)
plt.show()

plt.title('plot for fdist_pos')
fdist_pos.plot(30,cumulative=False)
plt.show()

plt.title('plot for fdist_neu')
fdist_neu.plot(30,cumulative=False)
plt.show()


################################# BI-GRAMS  ###################################
bigram_measures = BigramAssocMeasures()

text_neu = " ".join(lem_words_neu) 
finder_bi_neu = BigramCollocationFinder.from_words(text_neu.split(" "))
finder_bi_neu.apply_freq_filter(3)

bigrams_neu = {" ".join(words): "_".join(words)
        for words in finder_bi_neu.above_score(bigram_measures.likelihood_ratio,min_score=0)}
            



text_neg = " ".join(lem_words_neg) 
finder_bi_neg = BigramCollocationFinder.from_words(text_neg.split(" "))
finder_bi_neg.apply_freq_filter(3)

bigrams_neg = {" ".join(words): "_".join(words)
        for words in finder_bi_neg.above_score(bigram_measures.likelihood_ratio,min_score=0)}  
            
            
            
            
text_pos = " ".join(lem_words_pos) 
finder_bi_pos = BigramCollocationFinder.from_words(text_pos.split(" "))
finder_bi_pos.apply_freq_filter(3)

bigrams_pos = {" ".join(words): "_".join(words)
        for words in finder_bi_pos.above_score(bigram_measures.likelihood_ratio,min_score=1)}   
            
len(bigrams_pos)            
##################################### TRI-GRAMS ###############################
trigram_measures = TrigramAssocMeasures()

text_neu = " ".join(lem_words_neu) 
finder_tri_neu = TrigramCollocationFinder.from_words(text_neu.split(" "))
finder_tri_neu.apply_freq_filter(2)

trigram_neu = {" ".join(words): "_".join(words)
        for words in finder_tri_neu.above_score(trigram_measures.likelihood_ratio,min_score=.1)}
            



text_neg = " ".join(lem_words_neg) 
finder_tri_neg = TrigramCollocationFinder.from_words(text_neg.split(" "))
finder_tri_neg.apply_freq_filter(2)

trigram_neg = {" ".join(words): "_".join(words)
        for words in finder_tri_neg.above_score(trigram_measures.likelihood_ratio,min_score=.1)}  
            
            
            
            
text_pos = " ".join(lem_words_pos) 
finder_tri_pos = TrigramCollocationFinder.from_words(text_pos.split(" "))
finder_tri_pos.apply_freq_filter(2)

trigram_pos = {" ".join(words): "_".join(words)
        for words in finder_tri_pos.above_score(trigram_measures.likelihood_ratio,min_score=.1)}  
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
'''
            
            
            
from sklearn.feature_extraction.text import TfidfVectorizer
vectorizer = TfidfVectorizer()
dfToList = text_data['Text Response'][text_data['sentiment'] == 'neg'].tolist()

X = vectorizer.fit_transform(dfToList)
print(vectorizer.get_feature_names())


tfidf = TfidfVectorizer(vocabulary = dfToList, stop_words = 'english', ngram_range=(1,2))

feature_names = tfidf.get_feature_names()
corpus_index = [n for n in dfToList]
tfs = tfidf.fit_transform(dfToList)
rows, cols = tfs.nonzero()
for row, col in zip(rows, cols):
    print((feature_names[col], corpus_index[row]), tfs[row, col])
    
    
    
import pandas as pd
df = pd.DataFrame(tfs.T.todense(), index=feature_names, columns=corpus_index)
print(df)
'''
neg_corpus =  list(text_data['Text Response'][text_data['sentiment'] == 'neg'])
from sklearn.feature_extraction.text import TfidfVectorizer
tf = TfidfVectorizer(analyzer='word', ngram_range=(1,3), max_df = 0.85, stop_words = stop)


tfidf_matrix =  tf.fit_transform(neg_corpus)
feature_names = tf.get_feature_names() 
feature_names[:20]


dense = tfidf_matrix.todense()
episode = dense[0].tolist()[0]
phrase_scores = [pair for pair in zip(range(0, len(episode)), episode) if pair[1] > 0]

sorted(phrase_scores, key=lambda t: t[1] * -1)[:30]


sorted_phrase_scores = sorted(phrase_scores, key=lambda t: t[1] * -1)
for phrase, score in [(feature_names[word_id], score) for (word_id, score) in sorted_phrase_scores][:20]:
   print('{0: <20} {1}'.format(phrase, score))



##################### POSITIVE##########
pos_corpus =  list(text_data['Text Response'][text_data['sentiment'] == 'pos'])
from sklearn.feature_extraction.text import TfidfVectorizer
tf = TfidfVectorizer(analyzer='word', ngram_range=(1,3), max_df = 0.85, stop_words = stop)


tfidf_matrix =  tf.fit_transform(pos_corpus)
feature_names = tf.get_feature_names() 
feature_names[:20]


dense = tfidf_matrix.todense()
episode = dense[0].tolist()[0]
phrase_scores = [pair for pair in zip(range(0, len(episode)), episode) if pair[1] > 0]

sorted(phrase_scores, key=lambda t: t[1] * -1)[:30]


sorted_phrase_scores = sorted(phrase_scores, key=lambda t: t[1] * -1)
for phrase, score in [(feature_names[word_id], score) for (word_id, score) in sorted_phrase_scores][:20]:
   print('{0: <20} {1}'.format(phrase, score))

