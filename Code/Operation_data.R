
data_pros = data.frame(text1 = file.spglobal.2019.Operations$Objective,
                       #text2 = file.spglobal.2019$Objective.Category,
                       #text3 = file.spglobal.2019$segment,
                       #                       text3 = file.spglobal.rating.strength$Development.Item,
                       #                       text4 = file.spglobal.rating.strength$Additional.Information,
                       #                       text5 = file.spglobal.rating.strength$Relates.To,
                       #                       text6 = file.spglobal.rating.strength$Category,
                       stringsAsFactors = F)
dim(data_pros)

x  = text.clean(data_pros$text1)      

#### Corpus Cleaning 

stpw1 = readLines('https://raw.githubusercontent.com/sudhir-voleti/basic-text-analysis-shinyapp/master/data/stopwords.txt')# stopwords list from git

stpw2 = tm::stopwords('english')               # tm package stop word list; tokenizer package has the same name function

comn  = unique(c(stpw1, stpw2))                 # Union of two list #'solid state chemistry','microeconomics','linguistic'
stopwords = unique(gsub("'"," ",comn))  # final stop word list after removing punctuation

x_pros  = text.clean(data_pros$text1)             # pre-process text corpus
x_pros  =  removeWords(x_pros,stopwords)            # removing stopwords created above
x_pros  =  stripWhitespace(x_pros)                  # removing white space
#x_pros_corpus = Corpus(VectorSource(x_pros))


################Create DTM using text2vec package

t1 = Sys.time()

tok_fun = word_tokenizer

it_0_pros = itoken( x_pros,
                    #preprocessor = text.clean,
                    tokenizer = tok_fun,
                    ids = data_pros$id,
                    progressbar = F)

vocab_pros = create_vocabulary(it_0_pros,
                               ngram = c(2L, 2L)
                               #stopwords = stopwords
)


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
            scale = c(3.5, 0.5),     # range of word sizes
            min.freq,                     # min.freq of words to consider
            max.words = max.words1,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = title1)     # title for the wordcloud display
  
} # func ends
# vocab_unigram = create_vocabulary(it_0,
#                           ngram = c(1L, 3L))

pruned_vocab_pros = prune_vocabulary(vocab_pros,
                                     term_count_min = 50)
#pruned_vocab_pros <- pruned_vocab_pros[pruned_vocab_pros$term_count > 500 && pruned_vocab_pros$term_count < 2000 ]

vectorizer = vocab_vectorizer(pruned_vocab_pros)

dtm_0_pros  = create_dtm(it_0_pros, vectorizer)

# Sort bi-gram with decreasing order of freq
tsum_pros = as.matrix(t(rollup(dtm_0_pros, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum_pros = tsum_pros[order(tsum_pros, decreasing = T),]       #terms in decreasing order of freq
#typeof(tsum_pros)
#tsum_pros <- tsum_pros[tsum_pros]


##########Wordcloud for complete corpus
windows()
wordcloud(names(tsum_pros), 
          tsum_pros,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(6,1), 
          100, # min.freq 
          max.words = 500,
          colors = brewer.pal(8, "Dark2"))

title(sub = "Top words in Objective settings Operations")
