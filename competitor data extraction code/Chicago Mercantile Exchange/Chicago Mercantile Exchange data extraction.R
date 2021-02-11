rm(list=ls())
#read_html(baseurl)
library(xml2)
library(XML)
library(rvest)
library("xlsx")

baseurl <- "https://www.glassdoor.com/Reviews/Company-Reviews-"
companyNum <- "E7796"
sort <- ".htm?sort.sortType=RD&sort.descending=true"


full_url <- paste(baseurl, companyNum, sep = "")

df <- read_html(paste(baseurl, companyNum, sort, sep = ""))

total_review_count <- html_nodes(df, ".mt+ div strong")
num_review_count <- gsub('<strong>',"",total_review_count)
num_review_count <- gsub('</strong>',"",num_review_count)
num_review_count_numeric <- as.numeric (num_review_count)
total_pages_to_read <- round(num_review_count_numeric/10)
pros_list_page1 <- list()
pros_first_page_review <- data.frame()
#### Pros Data Extraction ##############
#####################################################  Page 1 Pros data extraction ###############################################
Pros <- html_nodes(df, ".mb-0+ .common__EiReviewTextStyles__allowLineBreaks .strong+ p")
pros_page1 <- html_text(Pros)
pros_page_rating_value <- html_attr(html_nodes(df, ".gdStars.gdRatings.sm .rating .value-title"), "title")
pros_first_page_review <- data.frame(1, pros_page1,pros_page_rating_value)
#####################################################  Page 2 to all page reading Pros data extraction   #######################################
pros_list <- list()
pros_list_ratings <- list()
df_pros_rating <- data.frame()
df_pros_ratingval <- data.frame()
df_pros_ratingtxt <- data.frame()
df_pros_overall <- data.frame()

pros_page_number_nodata <- list()
combined_page_number <- list()

for(i in 2:total_pages_to_read )  #####  total_pages_to_read-60
{
  print(i)
  baseurl <- "https://www.glassdoor.com/Reviews/Company-Reviews-"
  companyNum <- "E3096_P"
  page_number <- as.character(i)
  sort <- ".htm?sort.sortType=RD&sort.descending=true&countryRedirect=true"
  
  df_page <- read_html(paste(baseurl, companyNum,page_number, sort, sep = ""))
  Pros_oth <- html_nodes(df_page,".mb-0+ .common__EiReviewTextStyles__allowLineBreaks .strong+ p")
  pros_list <- html_text(Pros_oth)
  Pros_list_rating_val <- html_attr(html_nodes(df, ".gdStars.gdRatings.sm .rating .value-title"), "title")
  
  if (length(pros_list) != 0) {
    df_pros_withpagenumber <- data.frame(i,pros_list,Pros_list_rating_val)
    df_pros_overall <- rbind(df_pros_overall,df_pros_withpagenumber)
  }
  
  else {
    pros_page_number_nodata <- i
    combined_page_number <- append (combined_page_number,pros_page_number_nodata)
  }
}

names(df_pros_overall)[1]<-paste("X1")
names(df_pros_overall)[2]<-paste("pros_page1")
names(df_pros_overall)[3]<-paste("pros_page_rating_value") 

df_final_dataset <- rbind(pros_first_page_review,df_pros_overall)

write.csv(df_final_dataset, file = "C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/competitor data extraction code/Chicago Mercantile Exchange/Chicago Mercantile Exchange_Pros_withratings.csv")

#### Cons Data Extraction ##############
#####################################################  Page 1 Cons data extraction ###############################################
#cons <- html_nodes(df, ".common__EiReviewTextStyles__allowLineBreaks:nth-child(7) .strong+ p")
cons <- html_nodes(df, ".common__EiReviewTextStyles__allowLineBreaks+ .common__EiReviewTextStyles__allowLineBreaks .strong+ p")
cons_page1 <- html_text(cons)
typeof(cons_page1)
cons_page_rating_value <- html_attr(html_nodes(df, ".gdStars.gdRatings.sm .rating .value-title"), "title")
cons_first_page_review <- data.frame(1, cons_page1)
#####################################################  Page 2 to all page reading Pros data extraction   #######################################
cons_list <- list()
cons_list_ratings <- list()
df_cons_rating <- data.frame()
df_cons_ratingval <- data.frame()
df_cons_ratingtxt <- data.frame()
df_cons_overall <- data.frame()

cons_page_number_nodata <- list()
combined_page_number_cons <- list()

for(i in 2:total_pages_to_read )  #####  total_pages_to_read-60
{
  print(i)
  baseurl <- "https://www.glassdoor.com/Reviews/Company-Reviews-"
  companyNum <- "E7796_P"
  page_number <- as.character(i)
  sort <- ".htm?sort.sortType=RD&sort.descending=true&countryRedirect=true"
  
  df_page <- read_html(paste(baseurl, companyNum,page_number, sort, sep = ""))
  Cons_oth <- html_nodes(df_page,".common__EiReviewTextStyles__allowLineBreaks+ .common__EiReviewTextStyles__allowLineBreaks .strong+ p")
  #.common__EiReviewTextStyles__allowLineBreaks:nth-child(6) .strong+ p
  Cons_list <- html_text(Cons_oth)
  Cons_list_rating_val <- html_attr(html_nodes(df, ".gdStars.gdRatings.sm .rating .value-title"), "title")
  print(Cons_list)
  df_cons_withpagenumber <- data.frame(Cons_list) #Cons_list_rating_val
  df_cons_overall <- rbind(df_cons_overall,df_cons_withpagenumber)
}
cons_first_page_review$X1 <- NULL

names(df_cons_overall)[1]<-paste("cons_page1")
#names(df_cons_overall)[3]<-paste("cons_page_rating_value") 

df_final_dataset_cons <- rbind(cons_first_page_review,df_cons_overall)

#write.table(as.data.frame(com_pros_list),file="C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/Code/competitor data extraction code/FactSet_pros.csv", quote=F,sep=",",row.names=F)
write.csv(df_final_dataset_cons, file = "C:/Users/surendra_dattatrey/Desktop/S&P Data/Glassdoor/competitor data extraction code/Chicago Mercantile Exchange/Chicago Mercantile Exchange_cons_withratings.csv")
