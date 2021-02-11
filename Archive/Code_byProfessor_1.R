rm(list = ls())
#require(qdap) || install.packages("qdap") # ensure java is up to date!
#install.packages("qdap",dependencies = TRUE)
#install.packages('rJava', type = 'source', INSTALL_opts='--merge-multiarch')
#Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.7.0_80")
library(qdap)
library(wordcloud)
library(stringr)
library(ggplot2)
library(SnowballC)
library(tm)
library(tau)
library(data.table)
require(tidytext)
require(tidyr)
require(dplyr)
##############Loading the original file############################

overall=read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Galssdoor/Pros.csv", stringsAsFactors = FALSE)
head(overall)

overall[,17]

overall_new <- overall[,17]

#Analysis using QDAP Library

t1 = Sys.time()   # set timer
pol = qdap::polarity(overall_new)         # Calculate the polarity from qdap dictionary
wc = pol$all[,2]                  # Word Count in each doc
val = pol$all[,3]                 # average polarity score
p  = pol$all[,4]                  # Positive words info
n  = pol$all[,5]                  # Negative Words info  
Sys.time() - t1  # how much time did the above take?

head(pol$all)

head(pol$group)

positive_words = unique(setdiff(unlist(p),"-"))  # Positive words list
print(positive_words)  


negative_words = unique(setdiff(unlist(n),"-"))  # Negative words list
print(negative_words)  

######## Analysis using Tidytext

sentiments %>% 
  filter(lexicon == "AFINN") %>% 
  head()

textdf = data_frame(text = overall_new)   # convert to data frame

bing = get_sentiments("bing")   # put all of the bing sentiment dict into object 'bing'
bing     # view bing object

senti.bing = textdf %>%
  mutate(linenumber = row_number()) %>%   # build line num variable
  ungroup() %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, index = linenumber %/% 1, sort = FALSE) %>%
  mutate(method = "bing")    # creates a column with method name

######## get AFINN first

AFINN <- get_sentiments("afinn")
AFINN

# inner join AFINN words and scores with text tokens from corpus
senti.afinn = textdf %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, textdf) %>%
  inner_join(AFINN) %>%    # returns only intersection of wordlists and all columns
  group_by(index = linenumber %/% 1) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "afinn")

senti.afinn
