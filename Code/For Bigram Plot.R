#############Create DTM after coding bi-gram as unigram in clean text corpus

text2_pros = x
text2_pros = paste("",text2_pros,"")

pb <- txtProgressBar(min = 1, max = (length(tsum_pros)), style = 3) 
i = 0

for (term in names(tsum_pros)){
  i = i + 1
  focal.term = gsub(" ", " ",term)        # in case dot was word-separator
  replacement.term = term
  text2 = gsub(paste("",focal.term,""),paste("",replacement.term,""), text2_pros)
  #setTxtProgressBar(pb, i)
}


it_m = itoken(text2_pros,
              # preprocessor = text.clean,
              tokenizer = tok_fun,
              ids = data$id,
              progressbar = F)

vocab = create_vocabulary(it_m
                          # ngram = c(2L, 2L),
                          #stopwords = stopwords
)

pruned_vocab_cons = prune_vocabulary(vocab_pros,
                                     term_count_min = 2)
# doc_proportion_max = 0.5,
# doc_proportion_min = 0.001)

vectorizer = vocab_vectorizer(pruned_vocab_cons)

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
          4, # min.freq 
          max.words = 500,
          colors = brewer.pal(8, "Dark2"))