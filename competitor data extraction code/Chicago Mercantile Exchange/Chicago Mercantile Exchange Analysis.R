rm(list = ls())

#install.packages("qdap")

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
#require(qdap) ||  # ensure java is up to date!
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

file.spglobal.pros=read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/competitor data extraction code/Chicago Mercantile Exchange/Chicago Mercantile Exchange_pros_with_rating.csv")
file.spglobal.cons=read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/competitor data extraction code/Chicago Mercantile Exchange/Chicago Mercantile Exchange_cons_withratings.csv")


n_pros = min(nrow(file.spglobal.pros))
n_cons = min(nrow(file.spglobal.cons))

############# Cleaning Pros Data

data_pros = data.frame(text1 = file.spglobal.pros$pros_page1[1:n_pros],
                       #text2 = file.mi$text[1:n],
                       #text3 = file.lin$text[1:n],
                       stringsAsFactors = F)
dim(data_pros)


#### Corpus Cleaning 

stpw1 = readLines('https://raw.githubusercontent.com/sudhir-voleti/basic-text-analysis-shinyapp/master/data/stopwords.txt')# stopwords list from git

stpw2 = tm::stopwords('english')               # tm package stop word list; tokenizer package has the same name function

comn  = unique(c(stpw1, stpw2))                 # Union of two list #'solid state chemistry','microeconomics','linguistic'
stopwords = unique(gsub("'"," ",comn))  # final stop word list after removing punctuation

x_pros  = text.clean(data_pros$text1)             # pre-process text corpus
x_pros  =  removeWords(x_pros,stopwords)            # removing stopwords created above
x_pros  =  stripWhitespace(x_pros)                  # removing white space

################Create DTM using text2vec package

t1 = Sys.time()

tok_fun = word_tokenizer

it_0_pros = itoken( x_pros,
                    #preprocessor = text.clean,
                    tokenizer = tok_fun,
                    ids = data_pros$id,
                    progressbar = F)

vocab_pros = create_vocabulary(it_0_pros,
                               ngram = c(2L, 3L)
                               #stopwords = stopwords
)

# vocab_unigram = create_vocabulary(it_0,
#                           ngram = c(1L, 3L))

pruned_vocab_pros = prune_vocabulary(vocab_pros,
                                     term_count_min = 5)

vectorizer = vocab_vectorizer(pruned_vocab_pros)

dtm_0_pros  = create_dtm(it_0_pros, vectorizer)

# Sort bi-gram with decreasing order of freq
tsum_pros = as.matrix(t(rollup(dtm_0_pros, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum_pros = tsum_pros[order(tsum_pros, decreasing = T),]       #terms in decreasing order of freq
head(tsum_pros)

tsum_pros

#tsum_pros <- tsum_pros[-c(6,7,19,21,31,44,46,49,55,61,90)]

##########Wordcloud for complete corpus
windows()
wordcloud(names(tsum_pros), tsum_pros,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          4, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))

title(sub = "Top words in Pros")

############# Cleaning Cons Data

data_cons = data.frame(text1 = file.spglobal.cons$cons_page1[1:n_cons],
                       #text2 = file.mi$text[1:n],
                       #text3 = file.lin$text[1:n],
                       stringsAsFactors = F)
dim(data_cons)


############# Corpus Cleaning 

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
                                     term_count_min = 5)

vectorizer_cons = vocab_vectorizer(pruned_vocab_cons)

dtm_0_cons  = create_dtm(it_0_cons, vectorizer_cons)

# Sort bi-gram with decreasing order of freq
tsum_cons = as.matrix(t(rollup(dtm_0_cons, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum_cons = tsum_cons[order(tsum_cons, decreasing = T),]       #terms in decreasing order of freq
head(tsum_cons)
#tsum_cons <- tsum_cons[-c(1,2,26,27,43,51,65,70)]
#tsum_cons <- tsum_cons[-c(58)]
##########Wordcloud for complete corpus
windows()
wordcloud(names(tsum_cons), tsum_cons,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          5, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))

title(sub = "Top words in Cons")

###########################################################################################################################
######################## Pros Analysis based on employee Rating
filter_data <- subset(file.spglobal.pros, file.spglobal.pros$pros_page_rating_value == 3)
file.spglobal.pros

############# Cleaning Pros Data
n_1 = min(nrow(filter_data))

data_pros = data.frame(text1 = filter_data$pros_page1[1:n_1],
                       #text2 = file.mi$text[1:n],
                       #text3 = file.lin$text[1:n],
                       stringsAsFactors = F)
dim(data_pros)


x_pros  = text.clean(data_pros$text1)             # pre-process text corpus
x_pros  =  removeWords(x_pros,stopwords)            # removing stopwords created above
x_pros  =  stripWhitespace(x_pros)                  # removing white space

################Create DTM using text2vec package

t1 = Sys.time()

tok_fun = word_tokenizer

it_0_pros = itoken( x_pros,
                    #preprocessor = text.clean,
                    tokenizer = tok_fun,
                    ids = data_pros$id,
                    progressbar = F)

vocab_pros = create_vocabulary(it_0_pros,
                               ngram = c(2L, 3L)
                               #stopwords = stopwords
)

# vocab_unigram = create_vocabulary(it_0,
#                           ngram = c(1L, 3L))

pruned_vocab_pros = prune_vocabulary(vocab_pros,
                                     term_count_min = 5)

vectorizer = vocab_vectorizer(pruned_vocab_pros)

dtm_0_pros  = create_dtm(it_0_pros, vectorizer)

# Sort bi-gram with decreasing order of freq
tsum_pros = as.matrix(t(rollup(dtm_0_pros, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum_pros = tsum_pros[order(tsum_pros, decreasing = T),]       #terms in decreasing order of freq
head(tsum_pros)

tsum_pros

#tsum_pros <- tsum_pros[-c(6,7,19,21,31,44,46,49,55,61,90)]

##########Wordcloud for complete corpus
windows()
wordcloud(names(tsum_pros), tsum_pros,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          4, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))

title(sub = "Top words in Pros")
