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


file.cr = read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/S&P Global reviews Glassdoor.csv")

data = data.frame(text1 = file.cr$Cons,
                  stringsAsFactors = F)
data$text = data$text1

dim(data)

stpw1 = readLines('https://raw.githubusercontent.com/sudhir-voleti/basic-text-analysis-shinyapp/master/data/stopwords.txt')# stopwords list from git
stpw2 = tm::stopwords('english')               # tm package stop word list; tokenizer package has the same name function

comn  = unique(c(stpw1, stpw2))                 # Union of two list #'solid state chemistry','microeconomics','linguistic'
stopwords = unique(gsub("'"," ",comn))  # final stop word lsit after removing punctuation

x  = text.clean(data$text)             # pre-process text corpus
x  =  removeWords(x,stopwords)            # removing stopwords created above
x  =  stripWhitespace(x)                  # removing white space

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
                                term_count_min = 2)

vectorizer = vocab_vectorizer(pruned_vocab)

dtm_0  = create_dtm(it_0, vectorizer)

# Sort bi-gram with decreasing order of freq
tsum = as.matrix(t(rollup(dtm_0, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum = tsum[order(tsum, decreasing = T),]       #terms in decreasing order of freq
head(tsum)


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
dtm_m[1:50]

dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)
dim(dtm)
a0 = apply(dtm, 1, sum)   # apply sum operation to dtm's rows. i.e. get rowSum
dtm = dtm[(a0 > 2),]    # retain only those rows with token rowSum >5, i.e. delete empty rows
dim(dtm); rm(a0)        # delete a0 object

a0 = apply(dtm, 2, sum)   # use apply() to find colSUms this time
dtm = dtm[, (a0 > 4)]     # retain only those terms that occurred > 4 times in the corpus
dim(dtm); rm(a0)

a0 = apply(dtm, 2, sum)     # colSum vector of dtm
a0[1:5]      

a1 = order(as.vector(a0), decreasing = TRUE)     # vector of token locations
a0 = a0[a1]     # a0 ordered asper token locations
a0[1:5]         # view a0 now


windows()
wordcloud(names(a0), a0,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          2, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))
title(sub = "Top words in Cons")
