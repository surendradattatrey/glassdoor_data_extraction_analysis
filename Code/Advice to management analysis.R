rm(list=ls())

try(require(text2vec) || install.packages("text2vec"))
try(require(data.table) || install.packages("data.table"))
try(require(stringr) || install.packages("stringr"))
try(require(tm) || install.packages("tm"))
try(require(RWeka) || install.packages("RWeka"))
try(require(tokenizers) || install.packages("tokenizers"))
try(require(slam) || install.packages("slam"))
try(require(wordcloud) || install.packages("wordcloud"))
try(require(ggplot2) || install.packages("ggplot2"))

library(text2vec)
library(data.table)
library(stringr)
library(tm)
library(RWeka)
library(tokenizers)
library(slam)
library(wordcloud)
library(ggplot2)

text.clean = function(x)                    # text data
{ require("tm")
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  
  # Read Stopwords list
  stpw1 = readLines('https://raw.githubusercontent.com/sudhir-voleti/basic-text-analysis-shinyapp/master/data/stopwords.txt')# stopwords list
  stpw2 = tm::stopwords('english')      # tm package stop word list; tokenizer package has the same name function, hence 'tm::'
  comn  = unique(c(stpw1, stpw2))         # Union of two list
  stopwords = unique(gsub("'"," ",comn))  # final stop word lsit after removing punctuation
  
  x  =  removeWords(x,stopwords)            # removing stopwords created above
  x  =  stripWhitespace(x)                  # removing white space
  #  x  =  stemDocument(x)                   # can stem doc if needed.
  
  return(x)
}

file.spglobal=read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/advice_to_management_sentiments.csv")

#### Positive comment analysis

filter_data <- subset(file.spglobal, file.spglobal$sentiment_txt == 'pos')  # 5

  
data = data.frame(#id = 1:length(filter_data$Text.Response),  # creating doc IDs if name is not given
    text = filter_data$Advice.to.Sr..Management,  # filter_data$Pros
    stringsAsFactors = F)
  
data_df = data.frame(data)
  
  # pre-process data for cleaned dataset
x  = text.clean(data_df$text)                # applying func defined above to pre-process text corpus
  
#--------------------------------------------------------#
## Step 2: Create DTM and TCM using text2vec package             #
#--------------------------------------------------------#
  
build_dtm_tcm <- function(x){   # x is cleaned corpus
    require(text2vec)
    tok_fun = word_tokenizer  # using word & not space tokenizers
    it_0 = itoken( x,
                   #preprocessor = text.clean,
                   tokenizer = tok_fun,
                   ids = data$id,
                   progressbar = T)
    
    vocab = create_vocabulary(it_0,    #  func collects unique terms & corresponding statistics
                              ngram = c(2L, 2L))
    
    pruned_vocab = prune_vocabulary(vocab,  # filters input vocab & throws out v frequent & v infrequent terms
                                    term_count_min = 1)
    
    vectorizer = vocab_vectorizer(pruned_vocab) #  creates a text vectorizer func used in constructing a dtm/tcm/corpus
    
    dtm_0  = create_dtm(it_0, vectorizer) # high-level function for creating a document-term matrix
    
    # Sort bi-gram with decreasing order of freq
    tsum = as.matrix(t(slam::rollup(dtm_0, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
    tsum = tsum[order(tsum, decreasing = T),]       # terms in decreasing order of freq
    
    #-------------------------------------------------------
    # Code bi-grams as unigram in clean text corpus
    #-------------------------------------------------------
    
    text2 = x
    text2 = paste("",text2,"")
    
    pb <- txtProgressBar(min = 1, max = (length(tsum)), style = 3) ; 
    
    i = 0
    for (term in names(tsum)){
      i = i + 1
      focal.term = gsub("_", " ",term)        # in case dot was word-separator
      replacement.term = term
      text2 = gsub(paste("",focal.term,""),paste("",replacement.term,""), text2)
      setTxtProgressBar(pb, i)
    }
    
    
    it_m = itoken(text2,     # function creates iterators over input objects to vocabularies, corpora, DTM & TCM matrices
                  # preprocessor = text.clean,
                  tokenizer = tok_fun,
                  ids = data$id,
                  progressbar = T)
    
    vocab = create_vocabulary(it_m)     # vocab func collects unique terms and corresponding statistics
    pruned_vocab = prune_vocabulary(vocab,
                                    term_count_min = 1)
    
    vectorizer = vocab_vectorizer(pruned_vocab)
    
    dtm_m  = create_dtm(it_m, vectorizer)
    
    #dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)
    
    dtm <- tm::as.DocumentTermMatrix(slam::as.simple_triplet_matrix(dtm_m),
                                     weighting = weightTf)
    
    a0 = (apply(dtm, 1, sum) > 0)   # build vector to identify non-empty docs
    dtm = dtm[a0,]                  # drop empty docs
    
    vectorizer = vocab_vectorizer(pruned_vocab    # start with the pruned vocab
                                  #                                ,grow_dtm = FALSE    # doesn;t play well in R due to memory & over-writing issues
                                  #                                ,skip_grams_window = 5L  # window size = no. of terms to left & right of focal term
    )   
    
    tcm = create_tcm(it_m, vectorizer) # create_tcm() func to build a TCM
    
    out = list(dtm = dtm, tcm = tcm, dtm_sparse = dtm_m)
    
    return(out)  # output is list of length 3 containing dtm, tcm and a sparse dtm representation.
    
} # build_dtm_tcm func ends
  
  
out = build_dtm_tcm(x)    # dtm object 
dtm = out[[1]]    # first element of above function's output is the dtm
dim(dtm)
  
dtm = dtm[,order(apply(dtm, 2, sum), decreasing = T)]     # sorting dtm's columns in decreasing order of column sums
  
tcm = out[[2]]
  
a0 = apply(tcm, 1, sum) 
a1 = order(-a0)
  
tcm = tcm[a1, a1]
  
a0 = apply(dtm, 2, sum)
a1 = order(a0, decreasing = TRUE)
tsum = a0[a1]
  
# plot barchart for top tokens
df_chart <- data.frame(cbind(tsum[1:15]))
  
word_tokens<- rownames(df_chart)
pros_word_freq <- df_chart$cbind.tsum.1.15..
  
##############################Getting BiGrams in Dataframe######################################
pros_freq_data_bigrams <- data.frame(word_tokens, pros_word_freq)
pros_freq_data_bigrams

#fun_token (filter_data$Cons, 2L, 2L)

####### Negative comment analysis

filter_data <- subset(file.spglobal, file.spglobal$sentiment_txt == 'neg')  # 5


data = data.frame(#id = 1:length(filter_data$Text.Response),  # creating doc IDs if name is not given
  text = filter_data$Advice.to.Sr..Management,  # filter_data$Pros
  stringsAsFactors = F)

data_df = data.frame(data)

# pre-process data for cleaned dataset
x  = text.clean(data_df$text)                # applying func defined above to pre-process text corpus

#--------------------------------------------------------#
## Step 2: Create DTM and TCM using text2vec package             #
#--------------------------------------------------------#

build_dtm_tcm <- function(x){   # x is cleaned corpus
  require(text2vec)
  tok_fun = word_tokenizer  # using word & not space tokenizers
  it_0 = itoken( x,
                 #preprocessor = text.clean,
                 tokenizer = tok_fun,
                 ids = data$id,
                 progressbar = T)
  
  vocab = create_vocabulary(it_0,    #  func collects unique terms & corresponding statistics
                            ngram = c(2L, 2L))
  
  pruned_vocab = prune_vocabulary(vocab,  # filters input vocab & throws out v frequent & v infrequent terms
                                  term_count_min = 1)
  
  vectorizer = vocab_vectorizer(pruned_vocab) #  creates a text vectorizer func used in constructing a dtm/tcm/corpus
  
  dtm_0  = create_dtm(it_0, vectorizer) # high-level function for creating a document-term matrix
  
  # Sort bi-gram with decreasing order of freq
  tsum = as.matrix(t(slam::rollup(dtm_0, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
  tsum = tsum[order(tsum, decreasing = T),]       # terms in decreasing order of freq
  
  #-------------------------------------------------------
  # Code bi-grams as unigram in clean text corpus
  #-------------------------------------------------------
  
  text2 = x
  text2 = paste("",text2,"")
  
  pb <- txtProgressBar(min = 1, max = (length(tsum)), style = 3) ; 
  
  i = 0
  for (term in names(tsum)){
    i = i + 1
    focal.term = gsub("_", " ",term)        # in case dot was word-separator
    replacement.term = term
    text2 = gsub(paste("",focal.term,""),paste("",replacement.term,""), text2)
    setTxtProgressBar(pb, i)
  }
  
  
  it_m = itoken(text2,     # function creates iterators over input objects to vocabularies, corpora, DTM & TCM matrices
                # preprocessor = text.clean,
                tokenizer = tok_fun,
                ids = data$id,
                progressbar = T)
  
  vocab = create_vocabulary(it_m)     # vocab func collects unique terms and corresponding statistics
  pruned_vocab = prune_vocabulary(vocab,
                                  term_count_min = 1)
  
  vectorizer = vocab_vectorizer(pruned_vocab)
  
  dtm_m  = create_dtm(it_m, vectorizer)
  
  #dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)
  
  dtm <- tm::as.DocumentTermMatrix(slam::as.simple_triplet_matrix(dtm_m),
                                   weighting = weightTf)
  
  a0 = (apply(dtm, 1, sum) > 0)   # build vector to identify non-empty docs
  dtm = dtm[a0,]                  # drop empty docs
  
  vectorizer = vocab_vectorizer(pruned_vocab    # start with the pruned vocab
                                #                                ,grow_dtm = FALSE    # doesn;t play well in R due to memory & over-writing issues
                                #                                ,skip_grams_window = 5L  # window size = no. of terms to left & right of focal term
  )   
  
  tcm = create_tcm(it_m, vectorizer) # create_tcm() func to build a TCM
  
  out = list(dtm = dtm, tcm = tcm, dtm_sparse = dtm_m)
  
  return(out)  # output is list of length 3 containing dtm, tcm and a sparse dtm representation.
  
} # build_dtm_tcm func ends


out = build_dtm_tcm(x)    # dtm object 
dtm = out[[1]]    # first element of above function's output is the dtm
dim(dtm)

dtm = dtm[,order(apply(dtm, 2, sum), decreasing = T)]     # sorting dtm's columns in decreasing order of column sums

tcm = out[[2]]

a0 = apply(tcm, 1, sum) 
a1 = order(-a0)

tcm = tcm[a1, a1]

a0 = apply(dtm, 2, sum)
a1 = order(a0, decreasing = TRUE)
tsum = a0[a1]

# plot barchart for top tokens
df_chart <- data.frame(cbind(tsum[1:15]))

word_tokens<- rownames(df_chart)
pros_word_freq <- df_chart$cbind.tsum.1.15..

##############################Getting BiGrams in Dataframe######################################
pros_freq_data_bigrams <- data.frame(word_tokens, pros_word_freq)
pros_freq_data_bigrams

#fun_token (filter_data$Cons, 2L, 2L)
