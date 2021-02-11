rm(list = ls())

library(rJava)
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

overall=read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/S&P Global reviews Glassdoor.csv", stringsAsFactors = FALSE)
head(overall)

overall[,20]

overall_new <- overall[,20]

textdf = data_frame(text = overall_new) 

textdf %>% unnest_tokens(sentence, overall_new, token = "sentences") %>% head() 

(textdf %>% unnest_tokens(sentence, text, token = "sentences"))$sentence[1:5]     # Nice. Sentence detection is good.

##Tokenizing df into bigrams below

textdf %>% unnest_tokens(ngram, text, token = "ngrams", n = 2) %>% head()     # yields (#tokens -1 ) bigrams

##few bigrams below

(textdf %>% unnest_tokens(ngram, text, token = "ngrams", n = 2))$ngram[5:10]


# use count() to see most common words
textdf %>% 
  unnest_tokens(word, text) %>% 
  count(word, sort = TRUE) %>%   #counts & sorts no. of occurrences of each item in 'word' column 
  rename(count = n) %>%     # renames the count column from 'n' (default name) to 'count'.
  head() 

data(stop_words)

textdf %>% 
  unnest_tokens(word, text) %>% 
  count(word, sort = TRUE) %>%   
  rename(count = n) %>%
  anti_join(stop_words) %>%    # try ?anti_join
  head()

# first, build a datafame
tidy_pros <- textdf %>% 
  unnest_tokens(word, text) %>%     # word tokenization 
  anti_join(stop_words)    # run ?join::dplyr 

tidy_pros %>%
  count(word, sort = TRUE) %>%
  filter(n > 20) %>%   # n is wordcount colname. 
  mutate(word = reorder(word, n)) %>%  # mutate() reorders columns & renames too
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()


### create doc id and group_by it
textdf_doc = textdf %>% mutate(doc = seq(1:nrow(textdf))) %>% group_by(doc)

pros_bigrams <- textdf %>%
  unnest_tokens(bigram, text, 
                token = "ngrams", n = 2)
pros_bigrams

# separate bigrams
bigrams_separated <- pros_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") 
bigrams_separated

# filtering the bigrams to remove stopwords
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigrams_filtered


# New bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts

bigram_output<-write.csv(bigram_counts,"C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/bigram_counts.csv",row.names = FALSE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united

bigrams_united<-write.csv(bigrams_united,"C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/bigrams_united.csv",row.names = FALSE)

####### Wordcloud fo Bigrams ##########

wordcloud(overall_out$word,(overall_out$word_count),scale=c(3,0.3),rot.per=0,colors=brewer.pal(8,"Dark2"))
