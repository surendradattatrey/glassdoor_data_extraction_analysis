rm(list = ls())

library(text2vec)
library(data.table)
library(stringr)
library(tm)
library(RWeka)
library(tokenizers)
library(slam)
library(wordcloud)
library(igraph)
library(maptpx)

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
  return(x)
}

##### Funcion - to create COG (Co-occurrence Graph)

distill.cog = function(mat1, # input TCM ADJ MAT
                       title, # title for the graph
                       s,    # no. of central nodes
                       k1){  # max no. of connections  
  
  a = colSums(mat1) # collect colsums into a vector obj a
  b = order(-a)     # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b,b]  #
  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:s){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # wow. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc,word)
  } # i1 loop ends
  
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  graph = simplify(graph) 
  V(graph)$color[1:s] = "green"
  V(graph)$color[(s+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ])
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title)
  
} # func ends

### Reading the Corpus

file.spglobal=read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/S&P Global reviews Glassdoor.csv")

n = min(nrow(file.spglobal))

############# Cleaning Pros Data

data = data.frame(text1 = file.spglobal$Pros[1:n],
                 #text2 = file.mi$text[1:n],
                 #text3 = file.lin$text[1:n],
                 stringsAsFactors = F)
dim(data)


#### Corpus Cleaning 

stpw1 = readLines('https://raw.githubusercontent.com/sudhir-voleti/basic-text-analysis-shinyapp/master/data/stopwords.txt')# stopwords list from git

stpw2 = tm::stopwords('english')               # tm package stop word list; tokenizer package has the same name function

comn  = unique(c(stpw1, stpw2))                 # Union of two list #'solid state chemistry','microeconomics','linguistic'
stopwords = unique(gsub("'"," ",comn))  # final stop word lsit after removing punctuation

x  = text.clean(data$text1)             # pre-process text corpus
x  =  removeWords(x,stopwords)            # removing stopwords created above
x  =  stripWhitespace(x)                  # removing white space

################Create DTM using text2vec package

t1 = Sys.time()

tok_fun = word_tokenizer

it_0 = itoken( x,
               #preprocessor = text.clean,
               tokenizer = tok_fun,
               ids = data$id,
               progressbar = F)

vocab = create_vocabulary(it_0,
                          ngram = c(2L, 3L)
                          #stopwords = stopwords
)

pruned_vocab = prune_vocabulary(vocab,
                                term_count_min = 10)

vectorizer = vocab_vectorizer(pruned_vocab)

dtm_0  = create_dtm(it_0, vectorizer)

# Sort bi-gram with decreasing order of freq
tsum = as.matrix(t(rollup(dtm_0, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum = tsum[order(tsum, decreasing = T),]       #terms in decreasing order of freq
head(tsum)

##########Wordcloud for complete corpus
#window()
wordcloud(names(tsum), tsum,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          6, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))


#############Create DTM after coding bi-gram as unigram in clean text corpus

text2 = x
text2 = paste("",text2,"")

pb <- txtProgressBar(min = 1, max = (length(tsum)), style = 3) ; i = 0

for (term in names(tsum)){
  i = i + 1
  focal.term = gsub("_", " ",term)        # in case dot was word-separator
  replacement.term = term
  text2 = gsub(paste("",focal.term,""),paste("",replacement.term,""), text2)
  #setTxtProgressBar(pb, i)
}


it_m = itoken(text2,
              # preprocessor = text.clean,
              tokenizer = tok_fun,
              ids = data$id,
              progressbar = F)

vocab = create_vocabulary(it_m
                          # ngram = c(2L, 2L),
                          #stopwords = stopwords
)

pruned_vocab = prune_vocabulary(vocab,
                                term_count_min = 2)
# doc_proportion_max = 0.5,
# doc_proportion_min = 0.001)

vectorizer = vocab_vectorizer(pruned_vocab)

dtm_m  = create_dtm(it_m, vectorizer)
dim(dtm_m)

########Apply RowSum and ColSum on DTM
dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)

#print(difftime(Sys.time(), t1, units = 'sec'))

# some basic clean-up ops
#dim(dtm)

a0 = apply(dtm, 1, sum)   # apply sum operation to dtm's rows. i.e. get rowSum
dtm = dtm[(a0 > 5),]    # retain only those rows with token rowSum >5, i.e. delete empty rows
dim(dtm); 
#rm(a0)        # delete a0 object


a0 = apply(dtm, 2, sum)   # use apply() to find colSUms this time
dtm = dtm[, (a0 > 4)]     # retain only those terms that occurred > 4 times in the corpus
dim(dtm);
#rm(a0)

# view summary wordlcoud
a0 = apply(dtm, 2, sum)     # colSum vector of dtm
a0[1:5]                   # view what a0 obj is like

a1 = order(as.vector(a0), decreasing = TRUE)     # vector of token locations
a0 = a0[a1]     # a0 ordered asper token locations
a0[1:5]         # view a0 now

##########Wordcloud for complete corpus
#window()
wordcloud(names(a0), a0,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          6, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))


############# Cleaning Cons Data

data_cons = data.frame(text1 = file.spglobal$Cons[1:n],
                  #text2 = file.mi$text[1:n],
                  #text3 = file.lin$text[1:n],
                  stringsAsFactors = F)
dim(data_cons)


#### Corpus Cleaning 

x_cons  = text.clean(data_cons$text1)             # pre-process text corpus
x_cons  =  removeWords(x_cons,stopwords)            # removing stopwords created above
x_cons  =  stripWhitespace(x_cons)                  # removing white space

################Create DTM using text2vec package

t1_cons = Sys.time()

tok_fun_cons = word_tokenizer

it_0_cons = itoken( x_cons,
               #preprocessor = text.clean,
               tokenizer = tok_fun_cons,
               ids = data_cons$id,
               progressbar = F)

vocab_cons = create_vocabulary(it_0_cons,
                          ngram = c(2L, 3L)
                          #stopwords = stopwords
)

pruned_vocab_cons = prune_vocabulary(vocab_cons,
                                term_count_min = 10)

vectorizer_cons = vocab_vectorizer(pruned_vocab_cons)

dtm_0_cons  = create_dtm(it_0_cons, vectorizer_cons)

# Sort bi-gram with decreasing order of freq
tsum_cons = as.matrix(t(rollup(dtm_0_cons, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum_cons = tsum_cons[order(tsum_cons, decreasing = T),]       #terms in decreasing order of freq
head(tsum_cons)

##########Wordcloud for complete corpus
#window()
wordcloud(names(tsum_cons), tsum_cons,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          6, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))

#############Create DTM after coding bi-gram as unigram in clean text corpus

text2_cons = x_cons
text2_cons = paste("",text2_cons,"")

pb_cons <- txtProgressBar(min = 1, max = (length(tsum_cons)), style = 3) ; i = 0

for (term in names(tsum_cons)){
  i = i + 1
  focal.term = gsub("_", " ",term)        # in case dot was word-separator
  replacement.term = term
  text2_cons = gsub(paste("",focal.term,""),paste("",replacement.term,""), text2_cons)
  #setTxtProgressBar(pb, i)
}


it_m_cons = itoken(text2_cons,
              # preprocessor = text.clean,
              tokenizer = tok_fun_cons,
              ids = data$id,
              progressbar = F)

vocab_cons = create_vocabulary(it_m_cons
                          # ngram = c(2L, 2L),
                          #stopwords = stopwords
)

pruned_vocab_cons = prune_vocabulary(vocab_cons,
                                term_count_min = 2)
# doc_proportion_max = 0.5,
# doc_proportion_min = 0.001)

vectorizer_cons = vocab_vectorizer(pruned_vocab_cons)

dtm_m_cons  = create_dtm(it_m_cons, vectorizer_cons)
dim(dtm_m_cons)

########Apply RowSum and ColSum on DTM
dtm_cons = as.DocumentTermMatrix(dtm_m_cons, weighting = weightTf)

#print(difftime(Sys.time(), t1, units = 'sec'))

# some basic clean-up ops
#dim(dtm)

a0_cons = apply(dtm_cons, 1, sum)   # apply sum operation to dtm's rows. i.e. get rowSum
dtm_cons = dtm_cons[(a0_cons > 5),]    # retain only those rows with token rowSum >5, i.e. delete empty rows
dim(dtm_cons); 
#rm(a0_cons)        # delete a0 object


a0_cons = apply(dtm_cons, 2, sum)   # use apply() to find colSUms this time
dtm_cons = dtm_cons[, (a0_cons > 4)]     # retain only those terms that occurred > 4 times in the corpus
dim(dtm_cons); 
#rm(a0)

# view summary wordlcoud
a0_cons = apply(dtm_cons, 2, sum)     # colSum vector of dtm
a0_cons[1:5]                   # view what a0 obj is like

a1_cons = order(as.vector(a0_cons), decreasing = TRUE)     # vector of token locations
a0_cons = a0_cons[a1_cons]     # a0 ordered asper token locations
a0_cons[1:5]         # view a0 now

##########Wordcloud for complete corpus
#window()
wordcloud(names(a0_cons), a0_cons,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          6, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))

##########################  Word Cloud for Countries ###################################################

head(file.spglobal)

unique(file.spglobal$Review.Country.Name)

file.spglobal.us <- subset(file.spglobal,Review.Country.Name == 'US')

n_country = min(nrow(file.spglobal.us))

############# Cleaning Pros Data

data_contries = data.frame(text1 = file.spglobal.us$Pros[1:n_country],
                  #text2 = file.mi$text[1:n],
                  #text3 = file.lin$text[1:n],
                  stringsAsFactors = F)
dim(data_contries)


#### Corpus Cleaning 

stpw1 = readLines('https://raw.githubusercontent.com/sudhir-voleti/basic-text-analysis-shinyapp/master/data/stopwords.txt')# stopwords list from git
stpw2 = tm::stopwords('english')               # tm package stop word list; tokenizer package has the same name function

comn  = unique(c(stpw1, stpw2))                 # Union of two list #'solid state chemistry','microeconomics','linguistic'
stopwords = unique(gsub("'"," ",comn))  # final stop word lsit after removing punctuation

x_countries  = text.clean(data_contries$text1)             # pre-process text corpus
x_countries  =  removeWords(x_countries,stopwords)            # removing stopwords created above
x_countries  =  stripWhitespace(x_countries)                  # removing white space

################ Create DTM using text2vec package

t1 = Sys.time()

tok_fun = word_tokenizer

it_0 = itoken( x_countries,
               #preprocessor = text.clean,
               tokenizer = tok_fun,
               ids = data_contries$id,
               progressbar = F)
?itoken
vocab = create_vocabulary(it_0,
                          ngram = c(2L, 3L)
                          #stopwords = stopwords
)

pruned_vocab = prune_vocabulary(vocab,
                                term_count_min = 2)

vectorizer = vocab_vectorizer(pruned_vocab)

dtm_0  = create_dtm(it_0, vectorizer)

# Sort bi-gram with decreasing order of freq
tsum = as.matrix(t(rollup(dtm_0, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum = tsum[order(tsum, decreasing = T),]       #terms in decreasing order of freq
head(tsum)


#############Create DTM after coding bi-gram as unigram in clean text corpus

text2 = x_countries
text2 = paste("",text2,"")

pb <- txtProgressBar(min = 1, max = (length(tsum)), style = 3) ; i = 0

for (term in names(tsum)){
  i = i + 1
  focal.term = gsub("_", " ",term)        # in case dot was word-separator
  replacement.term = term
  text2 = gsub(paste("",focal.term,""),paste("",replacement.term,""), text2,
  setTxtProgressBar(pb, i))
}


it_m = itoken(text2,
              # preprocessor = text.clean,
              tokenizer = tok_fun,
              ids = data_contries$id,
              progressbar = F)

vocab = create_vocabulary(it_m
                          # ngram = c(2L, 2L),
                          #stopwords = stopwords
)

pruned_vocab = prune_vocabulary(vocab,
                                term_count_min = 2)
# doc_proportion_max = 0.5,
# doc_proportion_min = 0.001)

vectorizer = vocab_vectorizer(pruned_vocab)

dtm_m  = create_dtm(it_m, vectorizer)
dim(dtm_m)

########Apply RowSum and ColSum on DTM
dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)

#print(difftime(Sys.time(), t1, units = 'sec'))

# some basic clean-up ops
#dim(dtm)

a0 = apply(dtm, 1, sum)   # apply sum operation to dtm's rows. i.e. get rowSum
dtm = dtm[(a0 > 2),]    # retain only those rows with token rowSum >5, i.e. delete empty rows
dim(dtm); 
#rm(a0)        # delete a0 object


a0 = apply(dtm, 2, sum)   # use apply() to find colSUms this time
dtm = dtm[, (a0 > 2)]     # retain only those terms that occurred > 4 times in the corpus
dim(dtm);
#rm(a0)

# view summary wordlcoud
a0 = apply(dtm, 2, sum)     # colSum vector of dtm
a0[1:5]                   # view what a0 obj is like

a1 = order(as.vector(a0), decreasing = TRUE)     # vector of token locations
a0 = a0[a1]     # a0 ordered asper token locations
a0[1:5]         # view a0 now

##########Wordcloud for complete corpus
#window()
wordcloud(names(a0), a0,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          6, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))


############# Cleaning Cons Data

data_cons = data.frame(text1 = file.spglobal.us$Cons[1:n_country],
                       #text2 = file.mi$text[1:n],
                       #text3 = file.lin$text[1:n],
                       stringsAsFactors = F)
dim(data_cons)


#### Corpus Cleaning 

x_cons  = text.clean(data_cons$text1)             # pre-process text corpus
x_cons  =  removeWords(x_cons,stopwords)            # removing stopwords created above
x_cons  =  stripWhitespace(x_cons)                  # removing white space

################Create DTM using text2vec package

t1_cons = Sys.time()

tok_fun_cons = word_tokenizer

it_0_cons = itoken( x_cons,
                    #preprocessor = text.clean,
                    tokenizer = tok_fun_cons,
                    ids = data_cons$id,
                    progressbar = F)

vocab_cons = create_vocabulary(it_0_cons,
                               ngram = c(2L, 2L)
                               #stopwords = stopwords
)

pruned_vocab_cons = prune_vocabulary(vocab_cons,
                                     term_count_min = 2)

vectorizer_cons = vocab_vectorizer(pruned_vocab_cons)

dtm_0_cons  = create_dtm(it_0_cons, vectorizer_cons)

# Sort bi-gram with decreasing order of freq
tsum_cons = as.matrix(t(rollup(dtm_0_cons, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum_cons = tsum_cons[order(tsum_cons, decreasing = T),]       #terms in decreasing order of freq
head(tsum_cons)


#############Create DTM after coding bi-gram as unigram in clean text corpus

text2_cons = x_cons
text2_cons = paste("",text2_cons,"")

pb_cons <- txtProgressBar(min = 1, max = (length(tsum_cons)), style = 3) ; i = 0

for (term in names(tsum_cons)){
  i = i + 1
  focal.term = gsub("_", " ",term)        # in case dot was word-separator
  replacement.term = term
  text2_cons = gsub(paste("",focal.term,""),paste("",replacement.term,""), text2_cons)
  #setTxtProgressBar(pb, i)
}


it_m_cons = itoken(text2_cons,
                   # preprocessor = text.clean,
                   tokenizer = tok_fun_cons,
                   ids = data_cons$id,
                   progressbar = F)

vocab_cons = create_vocabulary(it_m_cons
                               # ngram = c(2L, 2L),
                               #stopwords = stopwords
)

pruned_vocab_cons = prune_vocabulary(vocab_cons,
                                     term_count_min = 2)
# doc_proportion_max = 0.5,
# doc_proportion_min = 0.001)

vectorizer_cons = vocab_vectorizer(pruned_vocab_cons)

dtm_m_cons  = create_dtm(it_m_cons, vectorizer_cons)
dim(dtm_m_cons)

########Apply RowSum and ColSum on DTM
dtm_cons = as.DocumentTermMatrix(dtm_m_cons, weighting = weightTf)

#print(difftime(Sys.time(), t1, units = 'sec'))

# some basic clean-up ops
#dim(dtm)

a0_cons = apply(dtm_cons, 1, sum)   # apply sum operation to dtm's rows. i.e. get rowSum
dtm_cons = dtm_cons[(a0_cons > 5),]    # retain only those rows with token rowSum >5, i.e. delete empty rows
dim(dtm_cons); 
#rm(a0_cons)        # delete a0 object


a0_cons = apply(dtm_cons, 2, sum)   # use apply() to find colSUms this time
dtm_cons = dtm_cons[, (a0_cons > 4)]     # retain only those terms that occurred > 4 times in the corpus
dim(dtm_cons); 
#rm(a0)

# view summary wordlcoud
a0_cons = apply(dtm_cons, 2, sum)     # colSum vector of dtm
a0_cons[1:5]                   # view what a0 obj is like

a1_cons = order(as.vector(a0_cons), decreasing = TRUE)     # vector of token locations
a0_cons = a0_cons[a1_cons]     # a0 ordered asper token locations
a0_cons[1:5]         # view a0 now

##########Wordcloud for complete corpus
#window()
wordcloud(names(a0_cons), a0_cons,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          6, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))

