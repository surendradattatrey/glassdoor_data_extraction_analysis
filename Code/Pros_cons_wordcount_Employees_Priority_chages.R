rm(list = ls())

#install.packages("qdap")
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
library(qdap)

##### Function for Text Cleaning
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

### Reading the Corpus

file.spglobal=read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/employee reviews.csv")
#filter_data <- subset(file.spglobal, file.spglobal$Current.Job == 'No')
n = min(nrow(file.spglobal))


data_cons = data.frame(text1 = file.spglobal$Cons[1:n],
                       #text2 = file.mi$text[1:n],
                       #text3 = file.lin$text[1:n],
                       stringsAsFactors = F)
dim(data_cons)
a  = text.clean(data_cons$text1)                # applying func defined above to pre-process text corpus
a = a[(a != "")]    # purge empty rows
length(a)

###########################################################################################################################
#################################### Getting Tri Grams from CONS ###########################################################


#--------------------------------------------------------#
## Step 2: Create DTM and TCM using text2vec package             #
#--------------------------------------------------------#

build_dtm_tcm <- function(x){   # x is cleaned corpus
  require(text2vec)
  tok_fun = word_tokenizer  # using word & not space tokenizers
  it_0 = itoken( x,
                 #preprocessor = text.clean,
                 tokenizer = tok_fun,
                 #ids = data$id,
                 progressbar = T)
  
  vocab = create_vocabulary(it_0,    #  func collects unique terms & corresponding statistics
                            ngram = c(3L, 3L))
  
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
                #ids = data$id,
                progressbar = T)
  
  vocab = create_vocabulary(it_m)     # vocab func collects unique terms and corresponding statistics
  pruned_vocab = prune_vocabulary(vocab,
                                  term_count_min = 1)
  
  vectorizer = vocab_vectorizer(pruned_vocab)
  
  dtm_m  = create_dtm(it_m, vectorizer)
  # dim(dtm_m)
  
  dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)
  a0 = (apply(dtm, 1, sum) > 0)   # build vector to identify non-empty docs
  dtm = dtm[a0,]                  # drop empty docs
  
  vectorizer = vocab_vectorizer(pruned_vocab     # start with the pruned vocab
                                #, grow_dtm = FALSE  # doesn;t play well in R due to memory & over-writing issues
                                #, skip_grams_window = 5L
                                )   # window size = no. of terms to left & right of focal term
  
  tcm = create_tcm(it_m, vectorizer) # create_tcm() func to build a TCM
  
  out = list(dtm = dtm, tcm = tcm, dtm_sparse = dtm_m)
  
  return(out)  # output is list of length 3 containing dtm, tcm and a sparse dtm representation.
  
} # build_dtm_tcm func ends

out = build_dtm_tcm(a)    # dtm object created

dtm = out[[1]]    # first element of above function's output is the dtm
dim(dtm)

dtm = dtm[,order(apply(dtm, 2, sum), decreasing = T)]     # sorting dtm's columns in decreasing order of column sums

inspect(dtm[1:5, 1:5])     # inspect() func used to view parts of a DTM object           

tcm = out[[2]]
dim(tcm)

a0 = apply(tcm, 1, sum) 
a1 = order(-a0)

tcm = tcm[a1, a1]
tcm[1:10, 1:10]

#--------------------------------------------------------#
## Step 3:     # Build word cloud                       #
#--------------------------------------------------------#
build_wordcloud <- function(dtm, 
                            max.words1, # max no. of words to accommodate
                            min.freq,   # min.freq of words to consider
                            title1){        # write within double quotes
  require(wordcloud)
  if (ncol(dtm) > 20000){   # if dtm is overly large, break into chunks and solve
    
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
      print(i)      } # i loop ends
    
    tsum = ss.col
    
  } else { tsum = apply(dtm, 2, sum) }
  
  tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
  # head(tsum)
  # tail(tsum)
  
  # windows()  # New plot window
  wordcloud(names(tsum), tsum,     # words, their freqs 
            scale = c(2, 0.5),     # range of word sizes
            min.freq,                     # min.freq of words to consider
            max.words = max.words1,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = title1)     # title for the wordcloud display
  
} # func ends

build_wordcloud(dtm, 100, 2, "TF wordcloud")


a0 = apply(dtm, 2, sum)
a1 = order(a0, decreasing = TRUE)
tsum = a0[a1]

# plot barchart for top tokens
df_chart <- data.frame(cbind(tsum[1:100]))
df_chart

word_tokens<- rownames(df_chart)
cons_word_freq <- df_chart$cbind.tsum.1.100..

cons_freq_data <- data.frame(word_tokens, cons_word_freq)
cons_freq_data

###########################################################################################################################
#################################### Getting Bi Grams from CONS ###########################################################

#--------------------------------------------------------#
## Step 2: Create DTM and TCM using text2vec package             #
#--------------------------------------------------------#

build_dtm_tcm <- function(x){   # x is cleaned corpus
  require(text2vec)
  tok_fun = word_tokenizer  # using word & not space tokenizers
  it_0 = itoken( x,
                 #preprocessor = text.clean,
                 tokenizer = tok_fun,
                 #ids = data$id,
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
                #ids = data$id,
                progressbar = T)
  
  vocab = create_vocabulary(it_m)     # vocab func collects unique terms and corresponding statistics
  pruned_vocab = prune_vocabulary(vocab,
                                  term_count_min = 1)
  
  vectorizer = vocab_vectorizer(pruned_vocab)
  
  dtm_m  = create_dtm(it_m, vectorizer)
  # dim(dtm_m)
  
  dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)
  a0 = (apply(dtm, 1, sum) > 0)   # build vector to identify non-empty docs
  dtm = dtm[a0,]                  # drop empty docs
  
  vectorizer = vocab_vectorizer(pruned_vocab     # start with the pruned vocab
                                #, grow_dtm = FALSE  # doesn;t play well in R due to memory & over-writing issues
                                #, skip_grams_window = 5L
  )   # window size = no. of terms to left & right of focal term
  
  tcm = create_tcm(it_m, vectorizer) # create_tcm() func to build a TCM
  
  out = list(dtm = dtm, tcm = tcm, dtm_sparse = dtm_m)
  
  return(out)  # output is list of length 3 containing dtm, tcm and a sparse dtm representation.
  
} # build_dtm_tcm func ends

out = build_dtm_tcm(a)    # dtm object created

dtm = out[[1]]    # first element of above function's output is the dtm
dim(dtm)

dtm = dtm[,order(apply(dtm, 2, sum), decreasing = T)]     # sorting dtm's columns in decreasing order of column sums

inspect(dtm[1:5, 1:5])     # inspect() func used to view parts of a DTM object           

tcm = out[[2]]
dim(tcm)

a0 = apply(tcm, 1, sum) 
a1 = order(-a0)

tcm = tcm[a1, a1]
tcm[1:10, 1:10]

#--------------------------------------------------------#
## Step 3:     # Build word cloud                       #
#--------------------------------------------------------#
build_wordcloud <- function(dtm, 
                            max.words1, # max no. of words to accommodate
                            min.freq,   # min.freq of words to consider
                            title1){        # write within double quotes
  require(wordcloud)
  if (ncol(dtm) > 20000){   # if dtm is overly large, break into chunks and solve
    
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
      print(i)      } # i loop ends
    
    tsum = ss.col
    
  } else { tsum = apply(dtm, 2, sum) }
  
  tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
  # head(tsum)
  # tail(tsum)
  
  # windows()  # New plot window
  wordcloud(names(tsum), tsum,     # words, their freqs 
            scale = c(2, 0.5),     # range of word sizes
            min.freq,                     # min.freq of words to consider
            max.words = max.words1,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = title1)     # title for the wordcloud display
  
} # func ends

build_wordcloud(dtm, 100, 2, "TF wordcloud")


a0 = apply(dtm, 2, sum)
a1 = order(a0, decreasing = TRUE)
tsum = a0[a1]

# plot barchart for top tokens
df_chart_bigrams <- data.frame(cbind(tsum[1:100]))
df_chart_bigrams

word_tokens<- rownames(df_chart_bigrams)
cons_word_freq <- df_chart_bigrams$cbind.tsum.1.100..

cons_freq_data_bigrams <- data.frame(word_tokens, cons_word_freq)

total <- rbind(cons_freq_data, cons_freq_data_bigrams)

common_words <- merge(cons_freq_data, cons_freq_data_bigrams, by.x = "word_tokens", by.y = "word_tokens", sort = TRUE)
#total <- total$word_tokens %in% common_words$word_tokens
total_unique <- total[!total$word_tokens %in% common_words$word_tokens,]

df <-total_unique[order(-total_unique$word_tokens),]


#################################################################################################################################
######################################### Analysis on Pros Data #################################################################


data_pros = data.frame(text1 = file.spglobal$Pros[1:n],
                       #text2 = file.mi$text[1:n],
                       #text3 = file.lin$text[1:n],
                       stringsAsFactors = F)
dim(data_pros)
a  = text.clean(data_pros$text1)                # applying func defined above to pre-process text corpus
a = a[(a != "")]    # purge empty rows
length(a)

#--------------------------------------------------------#
## Step 2: Create DTM and TCM using text2vec package             #
#--------------------------------------------------------#

build_dtm_tcm <- function(x){   # x is cleaned corpus
  require(text2vec)
  tok_fun = word_tokenizer  # using word & not space tokenizers
  it_0 = itoken( x,
                 #preprocessor = text.clean,
                 tokenizer = tok_fun,
                 #ids = data$id,
                 progressbar = T)
  
  vocab = create_vocabulary(it_0,    #  func collects unique terms & corresponding statistics
                            ngram = c(2L, 3L))
  
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
                #ids = data$id,
                progressbar = T)
  
  vocab = create_vocabulary(it_m)     # vocab func collects unique terms and corresponding statistics
  pruned_vocab = prune_vocabulary(vocab,
                                  term_count_min = 1)
  
  vectorizer = vocab_vectorizer(pruned_vocab)
  
  dtm_m  = create_dtm(it_m, vectorizer)
  # dim(dtm_m)
  
  dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)
  a0 = (apply(dtm, 1, sum) > 0)   # build vector to identify non-empty docs
  dtm = dtm[a0,]                  # drop empty docs
  
  vectorizer = vocab_vectorizer(pruned_vocab     # start with the pruned vocab
                                #, grow_dtm = FALSE  # doesn;t play well in R due to memory & over-writing issues
                                #, skip_grams_window = 5L
  )   # window size = no. of terms to left & right of focal term
  
  tcm = create_tcm(it_m, vectorizer) # create_tcm() func to build a TCM
  
  out = list(dtm = dtm, tcm = tcm, dtm_sparse = dtm_m)
  
  return(out)  # output is list of length 3 containing dtm, tcm and a sparse dtm representation.
  
} # build_dtm_tcm func ends

out = build_dtm_tcm(a)    # dtm object created

dtm = out[[1]]    # first element of above function's output is the dtm
dim(dtm)

dtm = dtm[,order(apply(dtm, 2, sum), decreasing = T)]     # sorting dtm's columns in decreasing order of column sums

inspect(dtm[1:5, 1:5])     # inspect() func used to view parts of a DTM object           

tcm = out[[2]]
dim(tcm)

a0 = apply(tcm, 1, sum) 
a1 = order(-a0)

tcm = tcm[a1, a1]
tcm[1:10, 1:10]

#--------------------------------------------------------#
## Step 3:     # Build word cloud                       #
#--------------------------------------------------------#
build_wordcloud <- function(dtm, 
                            max.words1, # max no. of words to accommodate
                            min.freq,   # min.freq of words to consider
                            title1){        # write within double quotes
  require(wordcloud)
  if (ncol(dtm) > 20000){   # if dtm is overly large, break into chunks and solve
    
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
      print(i)      } # i loop ends
    
    tsum = ss.col
    
  } else { tsum = apply(dtm, 2, sum) }
  
  tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
  # head(tsum)
  # tail(tsum)
  
  # windows()  # New plot window
  wordcloud(names(tsum), tsum,     # words, their freqs 
            scale = c(2, 0.5),     # range of word sizes
            min.freq,                     # min.freq of words to consider
            max.words = max.words1,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = title1)     # title for the wordcloud display
  
} # func ends

build_wordcloud(dtm, 100, 2, "TF wordcloud")


a0 = apply(dtm, 2, sum)
a1 = order(a0, decreasing = TRUE)
tsum = a0[a1]

# plot barchart for top tokens
df_chart_pros <- data.frame(cbind(tsum[1:100]))
df_chart_pros

word_tokens<- rownames(df_chart_pros)
pros_word_freq <- df_chart_pros$cbind.tsum.1.100..

pros_freq_data <- data.frame(word_tokens, pros_word_freq)
pros_freq_data

# windows()  # New plot window
#require(ggplot2)

ggplot(data = df_chart[order('Freq_count')], aes(x=rownames(df_chart),y= order(df_chart$cbind.tsum.1.100..))) +
  geom_bar(stat = "identity", fill = "Blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#################################################################################################################################
######################################## Combining both data freq dataframes ####################################################
#################################################################################################################################

pros_freq_data
cons_freq_data

final_dataset <- merge(pros_freq_data, cons_freq_data, by.x="word_tokens", by.y="word_tokens")
final_dataset$total_count <- final_dataset$pros_word_freq + final_dataset$cons_word_freq
final_dataset
sorted_dataset <- final_dataset[order(-final_dataset$total_count),]
sorted_dataset

library(reshape2)
df.long<-melt(sorted_dataset)
ggplot(df.long,aes(word_tokens,value,fill=variable))+
  geom_bar(stat="identity",position="dodge")+
  coord_flip() +
  geom_text(aes(label=value)) +
  ggtitle("Word Freq Dist")


###################################################################################################################
######################### What matters for employees ##############################################################

data_calc <- function(cntry_name, tenure_name, c_job)
{
  tenure_filter <- subset (file.spglobal, file.spglobal$Review.Country.Name == cntry_name)
  tenure_filter_tnre <- subset(tenure_filter, tenure_filter$Length.of.Employment == tenure_name)
  tenure_filter_tnre <- subset(tenure_filter_tnre, tenure_filter_tnre$Current.Job == c_job)
 
  a <- colMeans(tenure_filter_tnre[sapply(tenure_filter_tnre, is.numeric)])
  return(a)

}

data_calc('India','Less than a year','Yes')
data_calc('India','Less than a year','No')

### analysis for over all Population

data_calc_overall <- function(tenure_name, c_job)
{
  #tenure_filter <- subset (file.spglobal, file.spglobal$Review.Country.Name == cntry_name)
  tenure_filter_tnre <- subset(file.spglobal, file.spglobal$Length.of.Employment == tenure_name)
  tenure_filter_tnre <- subset(tenure_filter_tnre, tenure_filter_tnre$Current.Job == c_job)
  
  a <- colMeans(tenure_filter_tnre[sapply(tenure_filter_tnre, is.numeric)])
  return(a)
  
}

data_calc_overall('More than 10 years','Yes')
data_calc_overall('More than 10 years','No')


















