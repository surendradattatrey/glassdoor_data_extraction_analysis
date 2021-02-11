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

#Index_GD = read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/Reviews/Index_GD.csv")
MI_GD = read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/Reviews/MI_GD.csv")
#Platts_GD = read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/Reviews/Platts_GD.csv")
#Rating_GD = read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/Reviews/Rating_GD.csv")
#SPGlobal_GlassDoor = read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/Reviews/S&PGlobal_GlassDoor.csv")
#file.spglobal=read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/S&P Global reviews Glassdoor.csv")

##?rbind

#file.spglobal <- rbind(Index_GD,MI_GD,Platts_GD, Rating_GD,SPGlobal_GlassDoor , deparse.level = 0 )
file.spglobal <- MI_GD
##file.spglobal=read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/S&P Global reviews Glassdoor.csv")

n = min(nrow(file.spglobal))
print(n)

final_combined_file <- write.csv(file.spglobal,"C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/final_combined_review.csv")
############# Cleaning Pros Data

data_pros = data.frame(text1 = file.spglobal$Pros[1:n],
                       #text2 = file.mi$text[1:n],
                       #text3 = file.lin$text[1:n],
                       stringsAsFactors = F)
dim(data_pros)


#### Corpus Cleaning 

stpw1 = readLines('https://raw.githubusercontent.com/sudhir-voleti/basic-text-analysis-shinyapp/master/data/stopwords.txt')# stopwords list from git

stpw2 = tm::stopwords('english')               # tm package stop word list; tokenizer package has the same name function
comn  = unique(c(stpw1, stpw2))                # Union of two list #'solid state chemistry','microeconomics','linguistic'
stopwords = unique(gsub("'"," ",comn))  # final stop word list after removing punctuation

#str <- c('from','less')
#str1<- list(c('from','work','good','life'))

#stopwords = removeWords(stopwords,str)
#stopwords = removeWords(stopwords,str1)
#stopwords = list.append(stopwords,c('from','work','good','life'))

#?grep()

typeof(stopwords)

x_pros  = text.clean(data_pros$text1)             # pre-process text corpus
x_pros  =  removeWords(x_pros,stopwords)            # removing stopwords created above
x_pros  =  stripWhitespace(x_pros)                  # removing white space

typeof(x_pros)
################Create DTM using text2vec package

t1 = Sys.time()

tok_fun = word_tokenizer

it_0_pros = itoken( x_pros,
                    #preprocessor = text.clean,
                    tokenizer = tok_fun,
                    ids = data_pros$id,
                    progressbar = F)

vocab_pros = create_vocabulary(it_0_pros,
                               ngram = c(1L, 4L)
                               #stopwords = stopwords
                               )

#######################################################

pruned_vocab_pros = prune_vocabulary(vocab_pros,term_count_min = 2)

words_to_remove <- list (c('work','good','life_balance','work_life','balance','life','working','good_work_life',
                     'place','nice','time','place_work','balance_good','home','life_balance_good','great_work_life','easy'))

for (word_list in words_to_remove)
{
  typeof(word_list)
  j = length(word_list)
  for (j in 1:length (word_list))
    {
      i =  min(which(pruned_vocab_pros$term == word_list [j]))
      print(pruned_vocab_pros$term_count[i])
      word_list[j]
      j = j+1
      pruned_vocab_pros$term_count[i] = 0  
      }
}


pruned_vocab_pros$term[pruned_vocab_pros$term_count >50]
pruned_vocab_pros$term_count [grep("life_balance",pruned_vocab_pros$term)]
vectorizer = vocab_vectorizer(pruned_vocab_pros)



dtm_0_pros  = create_dtm(it_0_pros, vectorizer)
typeof(dtm_0_pros)

# Sort bi-gram with decreasing order of freq
tsum_pros = as.matrix(t(rollup(dtm_0_pros, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum_pros = tsum_pros[order(tsum_pros, decreasing = T),]       #terms in decreasing order of freq
head(tsum_pros)

typeof(tsum_pros)

tsum_pros <- tsum_pros[-c(5,6,10,18,36,37,41)]
#tsum_pros <- tsum_pros[-c(27,37,51,71,83)]
tsum_pros[tsum_pros["great_work_life"]]

##########Wordcloud for complete corpus
windows()
wordcloud(names(tsum_pros), tsum_pros,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          10, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))

title(sub = "Top words in combined Pros")

############# Cleaning Cons Data

data_cons = data.frame(text1 = file.spglobal$Cons[1:n],
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
                               ngram = c(1L, 2L)
                               #stopwords = stopwords
)

pruned_vocab_cons = prune_vocabulary(vocab_cons,
                                     term_count_min = 2)
pruned_vocab_cons

vectorizer_cons = vocab_vectorizer(pruned_vocab_cons)

dtm_0_cons  = create_dtm(it_0_cons, vectorizer_cons)

# Sort bi-gram with decreasing order of freq
tsum_cons = as.matrix(t(rollup(dtm_0_cons, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum_cons = tsum_cons[order(tsum_cons, decreasing = T),]       #terms in decreasing order of freq
head(tsum_cons)

tsum_cons

tsum_cons <- tsum_cons[-c(77)]
tsum_cons

##########Wordcloud for complete corpus
windows()
wordcloud(names(tsum_cons), tsum_cons,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          6, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))

title(sub = "Top words in Cons")

################################################################################################################
############################################## US country Analysis #############################################
################################################################################################################

unique(file.spglobal$Review.Country.Name)

file.spglobal.us <- subset(file.spglobal,Review.Country.Name == 'US')

write.csv(file.spglobal.us,"C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/file.spglobal.us.csv",row.names = FALSE)

n_country = min(nrow(file.spglobal.us))

#file.spglobal.us$Pros

############# Cleaning Pros Data

data_contries = data.frame(text_country_data = file.spglobal.us$Pros[1:n_country],
                           #text2 = file.mi$text[1:n],
                           #text3 = file.lin$text[1:n],
                           stringsAsFactors = F)
dim(data_contries)


#### Corpus Cleaning 

x_countries  = text.clean(data_contries$text_country_data)             # pre-process text corpus
x_countries  =  removeWords(x_countries,stopwords)            # removing stopwords created above
x_countries  =  stripWhitespace(x_countries)                  # removing white space

################ Create DTM using text2vec package

t1 = Sys.time()

tok_fun = word_tokenizer

it_0_country = itoken( x_countries,
                       #preprocessor = text.clean,
                       tokenizer = tok_fun,
                       ids = data_contries$id,
                       progressbar = F)
#?itoken
vocab = create_vocabulary(it_0_country,
                          ngram = c(1L, 3L)
                          #stopwords = stopwords
)

pruned_vocab = prune_vocabulary(vocab,
                                term_count_min = 2)

vectorizer = vocab_vectorizer(pruned_vocab)

dtm_0_country  = create_dtm(it_0_country, vectorizer)

# Sort bi-gram with decreasing order of freq
tsum_country = as.matrix(t(rollup(dtm_0_country, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum_country = tsum_country[order(tsum_country, decreasing = T),]       #terms in decreasing order of freq
head(tsum_country)

tsum_country <- tsum_country[-c(1:4,5:6,10,17,23,40,42,55,57,59,66,71)]
tsum_country <- tsum_country[-c(63)]

##########Wordcloud for complete corpus
windows()
wordcloud(names(tsum_country), tsum_country,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          5, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))

title(sub = "Top words in Pros")

############# Cleaning Cons Data

data_contries_cons = data.frame(text_country_data = file.spglobal.us$Cons[1:n_country],
                                #text2 = file.mi$text[1:n],
                                #text3 = file.lin$text[1:n],
                                stringsAsFactors = F)
dim(data_contries_cons)


#### Corpus Cleaning 

x_countries_cons  = text.clean(data_contries_cons$text_country_data)             # pre-process text corpus
x_countries_cons  =  removeWords(x_countries_cons,stopwords)            # removing stopwords created above
x_countries_cons  =  stripWhitespace(x_countries_cons)                  # removing white space

################ Create DTM using text2vec package

t1 = Sys.time()

tok_fun = word_tokenizer

it_0_country_cons = itoken( x_countries_cons,
                            #preprocessor = text.clean,
                            tokenizer = tok_fun,
                            ids = data_contries_cons$id,
                            progressbar = F)
#?itoken
vocab_country_cons = create_vocabulary(it_0_country_cons,
                                       ngram = c(1L, 3L)
                                       #stopwords = stopwords
)

pruned_vocab_country_cons = prune_vocabulary(vocab_country_cons,
                                             term_count_min = 2)

vectorizer_country_cons = vocab_vectorizer(pruned_vocab_country_cons)

dtm_0_country_cons  = create_dtm(it_0_country_cons, vectorizer_country_cons)

# Sort bi-gram with decreasing order of freq
tsum_country_cons = as.matrix(t(rollup(dtm_0_country_cons, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum_country_cons = tsum_country_cons[order(tsum_country_cons, decreasing = T),]       #terms in decreasing order of freq
head(tsum_country_cons)
tsum_country_cons <- tsum_country_cons[-c(74,82,83,89)]
tsum_country_cons <- tsum_country_cons[-c(7,85,86,55,94,113)]

##########Wordcloud for complete corpus
windows()
wordcloud(names(tsum_country_cons), tsum_country_cons,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          5, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))

title(sub = "Top words in Cons")

################################################################################################################
############################################## INDIA country Analysis #############################################
################################################################################################################

unique(file.spglobal$Review.Country.Name)

file.spglobal.us <- subset(file.spglobal,Review.Country.Name == 'India')

write.csv(file.spglobal.us,"C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/file.spglobal.india.csv",row.names = FALSE)

n_country = min(nrow(file.spglobal.us))

#file.spglobal.us$Pros

############# Cleaning Pros Data

data_contries = data.frame(text_country_data = file.spglobal.us$Pros[1:n_country],
                           #text2 = file.mi$text[1:n],
                           #text3 = file.lin$text[1:n],
                           stringsAsFactors = F)
dim(data_contries)


#### Corpus Cleaning 

x_countries  = text.clean(data_contries$text_country_data)             # pre-process text corpus
x_countries  =  removeWords(x_countries,stopwords)            # removing stopwords created above
x_countries  =  stripWhitespace(x_countries)                  # removing white space

################ Create DTM using text2vec package

t1 = Sys.time()

tok_fun = word_tokenizer

it_0_country = itoken( x_countries,
                       #preprocessor = text.clean,
                       tokenizer = tok_fun,
                       ids = data_contries$id,
                       progressbar = F)
#?itoken
vocab = create_vocabulary(it_0_country,
                          ngram = c(1L, 3L)
                          #stopwords = stopwords
)

pruned_vocab = prune_vocabulary(vocab,
                                term_count_min = 2)

vectorizer = vocab_vectorizer(pruned_vocab)

dtm_0_country  = create_dtm(it_0_country, vectorizer)

# Sort bi-gram with decreasing order of freq
tsum_country = as.matrix(t(rollup(dtm_0_country, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum_country = tsum_country[order(tsum_country, decreasing = T),]       #terms in decreasing order of freq
head(tsum_country)

tsum_country <- tsum_country[-c(5,6,25,32,33,43,61,81,92.101,115,118,120,134)]

##########Wordcloud for complete corpus
windows()
wordcloud(names(tsum_country), tsum_country,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          5, # min.freq 
          max.words = 120,
          colors = brewer.pal(8, "Dark2"))

title(sub = "Top words in Pros")

############# Cleaning Cons Data

data_contries_cons = data.frame(text_country_data = file.spglobal.us$Cons[1:n_country],
                                #text2 = file.mi$text[1:n],
                                #text3 = file.lin$text[1:n],
                                stringsAsFactors = F)
dim(data_contries_cons)


#### Corpus Cleaning 

x_countries_cons  = text.clean(data_contries_cons$text_country_data)             # pre-process text corpus
x_countries_cons  =  removeWords(x_countries_cons,stopwords)            # removing stopwords created above
x_countries_cons  =  stripWhitespace(x_countries_cons)                  # removing white space

################ Create DTM using text2vec package

t1 = Sys.time()

tok_fun = word_tokenizer

it_0_country_cons = itoken( x_countries_cons,
                            #preprocessor = text.clean,
                            tokenizer = tok_fun,
                            ids = data_contries_cons$id,
                            progressbar = F)
#?itoken
vocab_country_cons = create_vocabulary(it_0_country_cons,
                                       ngram = c(1L, 3L)
                                       #stopwords = stopwords
)

pruned_vocab_country_cons = prune_vocabulary(vocab_country_cons,
                                             term_count_min = 2)

vectorizer_country_cons = vocab_vectorizer(pruned_vocab_country_cons)

dtm_0_country_cons  = create_dtm(it_0_country_cons, vectorizer_country_cons)

# Sort bi-gram with decreasing order of freq
tsum_country_cons = as.matrix(t(rollup(dtm_0_country_cons, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum_country_cons = tsum_country_cons[order(tsum_country_cons, decreasing = T),]       #terms in decreasing order of freq
head(tsum_country_cons)
#tsum_country_cons <- tsum_country_cons[-c(74,82,83,89)]

##########Wordcloud for complete corpus
windows()
wordcloud(names(tsum_country_cons), tsum_country_cons,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          5, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))

title(sub = "Top words in Cons")

############################################################################################################
########################## Finding Top 3 Themes - Model based analytics ##############################
############################################################################################################

