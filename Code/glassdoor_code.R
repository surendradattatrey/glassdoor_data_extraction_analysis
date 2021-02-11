rm(list = ls())

#############Importing the files##################
library(wordcloud)
library(stringr)
library(ggplot2)
library(SnowballC)
library(tm)
library(tau)
library(data.table)
library("RColorBrewer")

#install.packages('ggplot2', dependencies = TRUE)
##############Loading the original file############################

overall=read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/S&P Global reviews Glassdoor.csv")

#overall_new<-overall[overall$Year==2017,]
overall_new<-overall
head(overall_new)
#Generating the ngram file

tdm.generate <- function(overall,col_name,ng1,ng2,ng3,ng4,limit){
  
 if(col_name=="pros"){
   col_num<-20
 }
 else{
   col_num<-21
 }

  
  afile = overall[,c(col_num)]
  
  write.table(afile,"C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/dump.txt")
  
  afile=readLines("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/dump.txt")
  
  
  
  bigrams = textcnt(as.character(afile), n = ng1, method = "string")
  
  bigrams = bigrams[order(bigrams, decreasing = TRUE)]
  
  bigrams<-data.frame(bigrams)
  
  bigrams1<-cbind(bigrams, word=rownames(bigrams))
  
  row.names(bigrams1) <- NULL
  
  bigrams1$word_count<-ng1
  bigrams1$word <- as.character(bigrams1$word)
  
  bigrams2<-subset(bigrams1,nchar(word)>ng1)
  file1<-bigrams2
  
  
  
  bigrams = textcnt(as.character(afile), n = ng2, method = "string")
  
  bigrams = bigrams[order(bigrams, decreasing = TRUE)]
  
  bigrams<-data.frame(bigrams)
  
  bigrams1<-cbind(bigrams, word=rownames(bigrams))
  
  row.names(bigrams1) <- NULL
  
  bigrams1$word_count<-ng2
  bigrams1$word <- as.character(bigrams1$word)
  
  bigrams2<-subset(bigrams1,nchar(word)>ng2)
  file2<-bigrams2
  
  
  bigrams = textcnt(as.character(afile), n = ng3, method = "string")
  
  bigrams = bigrams[order(bigrams, decreasing = TRUE)]
  
  bigrams<-data.frame(bigrams)
  
  bigrams1<-cbind(bigrams, word=rownames(bigrams))
  
  row.names(bigrams1) <- NULL
  
  bigrams1$word_count<-ng3
  bigrams1$word <- as.character(bigrams1$word)
  
  bigrams2<-subset(bigrams1,nchar(word)>ng3)
  file3<-bigrams2
  
  
  bigrams = textcnt(as.character(afile), n = ng4, method = "string")
  
  bigrams = bigrams[order(bigrams, decreasing = TRUE)]
  
  bigrams<-data.frame(bigrams)
  
  bigrams1<-cbind(bigrams, word=rownames(bigrams))
  
  row.names(bigrams1) <- NULL
  
  bigrams1$word_count<-ng4
  bigrams1$word <- as.character(bigrams1$word)
  
  bigrams2<-subset(bigrams1,nchar(word)>ng4)
  file4<-bigrams2
  
  total<-rbind(file1,file2,file3,file4)
  
  total_1<-subset(total,bigrams>1)
  
  total_15<-setDT(total_1)[order(word_count,-bigrams), .SD[1:limit], by=word_count]
  
  total_15$attribute<-col_name
  
  return(total_15)
  
}


limit=200

total_15_1_out<-tdm.generate(overall_new,'pros',1,2,3,4,limit)
total_15_2_out<-tdm.generate(overall_new,'cons',1,2,3,4,limit)

total_15<-rbind(total_15_1_out,total_15_2_out)


write.csv(total_15,"C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/out_result.csv",row.names = FALSE)

overall_out<-read.csv("C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/out_result.csv")
#window()
overall_out <- overall_out[overall_out$attribute == 'pros',]

data(stop_words)

# first, build a datafame
tidy_pros <- overall_out %>% 
#  unnest_tokens(overall_out$word, text) %>%     # word tokenization 
  anti_join(stop_words)    # run ?join::dplyr 


#overall_out <- overall_out[overall_out$bigrams > 40]
wordcloud(tidy_pros$word,(tidy_pros$word_count),scale=c(3,0.3),rot.per=0,colors=brewer.pal(8,"Dark2"))

set.seed(1234)
wordcloud(tidy_pros$word, freq = tidy_pros$word_count, min.freq = 5,
          max.words=400, random.order=FALSE, rot.per=0, 
          colors=brewer.pal(8, "Dark2"))
          




